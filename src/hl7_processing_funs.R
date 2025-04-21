library(dplyr)
library(stringr)

hl7_element_names <- function(v) {
  #' Get dataframe with element names for HL7 fields
  #' 
  #' Currently maps element names (at the field-level) for HL7 versions 2.3.1 and 2.5.1. Reads data from public github page.
  #' 
  #' Parameters:
  #'  v: version number. Should be "2.3.1" or "2.5.1". Defaults to 2.5.1
  #' Returns: df containing Segment (char), Name (char), and Field (int) fields

  if (v == "2.3.1") {
    hl7_mapping = read.csv(url("https://raw.githubusercontent.com/NW-PaGe/covid_seq_elr/refs/heads/main/data/hl7_231_mapping.csv"),
                           na.strings = c("", "NA"))
  } else {
    if (v != "2.5.1") warning("Version != 2.3.1 or 2.5.1; parsing with 2.5.1 schema")
    hl7_mapping = read.csv(url("https://raw.githubusercontent.com/NW-PaGe/covid_seq_elr/refs/heads/main/data/hl7_251_mapping.csv"),
                           na.strings = c("", "NA"))
  } 
  # Create additional rows for messages w/empty component/subcomponent even if they can exist:
  hl7_empty_subcomponent = hl7_mapping %>% mutate(Subcomponent = NA, Subcomponent_Name = NA)
  hl7_empty_component = hl7_empty_subcomponent %>% mutate(Component = NA, Component_Name = NA)
  # Combine all variations of component/subcomponent emptiness and get unique rows
  hl7_mapping_final = rbind(hl7_mapping, 
                            hl7_empty_subcomponent, 
                            hl7_empty_component) %>%
    distinct() %>% 
    mutate(
      Name = case_when(
        is.na(Component_Name) & is.na(Subcomponent_Name) ~ Field_Name,
        is.na(Subcomponent_Name) ~ paste(Field_Name, Component_Name, sep = ": "),
        T ~ paste(Field_Name, Component_Name, Subcomponent_Name, sep = ": ")
      )
    )
  
  return(hl7_mapping_final)
}

# Convert HL7 Strings into a table
parse_hl7 <- function(hl7, remove_pii=TRUE) {
  #' Create a dataframe from raw HL7 message
  #' 
  #' Segments should be separated by a carraige return and new line characters (\r\n)
  #' 
  #' Parameters:
  #'  hl7: string form of the HL7 message
  #'  remove_pii: default TRUE; if TRUE, remove potential PII from PID segment and other fields, including some provider information
  #' 
  #' Returns: df with a row for every distinct segment/field/repetition/component/subcomponent in the message. Output schema:
  #'  - Order (int): int sequence from 1-N for N total rows. The order of the elements in the message
  #'  - Value (char): the character value of the message element
  #'  - Position (char): Positional description of the element in the following format: Segment[Instance]-Field[Repetition].Component.Subcomponent
  #'    Instance, repetition, Component, and Subcomponent are excluded if they do not exist for that element.
  #'    (Instance is only include if >= 2 instances exist in the message for a given Segment)
  #'  - Name (char): The data element name for a given Segment's Field. Componenets and Subcomponents are not named in further detail.
  #'  - Segment (char): 3-character str containig Segment name
  #'  - Instance (int): the order the segment is in, relative to all other same-named segments. Value is NA is only one segment instance exists.
  #'  - Field (int): the order the field is in, relative to the other Fields in the same segment instance
  #'  - Repetition (int): the order the repetition is in, relative to the other Repetitions in the same Field instance
  #'  - Component(int): the order the Componenet is in, relative to the other Components in the same Field, or Repetition if it exists, instance
  #'  - Subcomponent (int): the order the Subcomponent is in, relative to the other Subcomponents in the same Component instance.

  hl7 = gsub('\\\\&amp', '\\\\&', hl7) # replace amp replacement with just amp
  hl7 = gsub('&amp;', '&', hl7, fixed=TRUE) # replace amp replacement with just amp
  hl7_fields_seps = gsub("^.{0,}MSH\\|", '', hl7) # get the MSH-2 elements which contain the componenet, rep, escape, and subcomponent chars
  hl7_fields_seps = substr(hl7_fields_seps, 1, 4) # keep only the separator chars
  hl7_seps_list = list("component" = substr(hl7_fields_seps, 1, 1), # set component separator char
                       "repetition" = substr(hl7_fields_seps, 2, 2), # set repetition separator char
                       "escape" = substr(hl7_fields_seps, 3, 3), # set escape char
                       "subcomponent" = substr(hl7_fields_seps, 4, 4)) # set subcomponent char
  hl7 = gsub(paste0("MSH|", hl7_fields_seps), "MSH|__**separators**__", hl7, fixed=TRUE) # temporarily remove MSH-2 so special chars don't get targeted during transformations
  # Get field names
  hl7_version = gsub("^.{0,}MSH(\\|[^|]{0,}){10}\\|([0-9\\.]{3,})", '\\2', hl7) # remove version prefix
  hl7_version = gsub("^([0-9\\.]+).+$", "\\1", hl7_version) # remove version suffix
  element_names_df = hl7_element_names(hl7_version) # get names for fields
  
  df = data.frame() # set empty df to capture message data
  
  segments = unlist(strsplit(hl7, "\r\n", fixed=TRUE)) # split segments by carriage returns\new line combos
  # Track occurrences of each segment type (used in naming of positions later)
  segment_counts <- table(sapply(segments, function(segment) unlist(strsplit(segment, "|", fixed=TRUE))[1])) # Get counts of segment occurance
  segment_instance_tracker <- list() # set empty list to track processing of segments
  
  for (segment in segments) {
    parts = unlist(strsplit(segment, "|", fixed=TRUE))
    segment_name = parts[1]
    if (segment_name != "MSH") {
      if (length(parts) == 1) { # if there are no fields outside of the field name, set to null
        parts = NULL
      } else {
        parts = parts[2:length(parts)]  # otherwise, cutoff field name
      }
    }
    
    # Track the instance number for the segment
    if (!segment_name %in% names(segment_instance_tracker)) { # if segment name hasn't been processed prev, set count to 1
      segment_instance_tracker[[segment_name]] <- 1
    } else { # otherwise, add 1 to processing count
      segment_instance_tracker[[segment_name]] <- segment_instance_tracker[[segment_name]] + 1
    }
    segment_instance <- segment_instance_tracker[[segment_name]] # After setting or adding to the processing count, get the count total
    
    for (field_i in seq_along(parts)) { # break each field up into it's repetitions
      repetitions = unlist(strsplit(parts[field_i], hl7_seps_list$repetition, fixed=TRUE))
      if (length(repetitions) == 0) repetitions = ""
      
      for (rep_i in seq_along(repetitions)) { # break each repetition up into it's components
        components = unlist(strsplit(repetitions[rep_i], hl7_seps_list$component, fixed=TRUE))
        if (length(components) == 0) components = ""
        
        for (comp_i in seq_along(components)) { # break each component up into it's subcomponents
          subcomponents = unlist(strsplit(components[comp_i], hl7_seps_list$subcomponent, fixed=TRUE))
          if (length(subcomponents) == 0) subcomponents = ""
          
          for (sub_i in seq_along(subcomponents)) {
            include_segment_instance = segment_counts[segment_name] > 1 # include an Instance Value if there are more than 1 segment instances in message
            new_df = data.frame("Segment" = segment_name, # set segment name
                                "Instance" = ifelse(include_segment_instance, segment_instance, NA), # set instance (if applicable)
                                "Field" = field_i, # Set field number
                                "Component" = ifelse(length(components) > 1, comp_i, NA), # set component number (if applicable)
                                "Subcomponent" = ifelse(length(subcomponents) > 1, sub_i, NA), # set subcomponent number (if applicable)
                                "Repetition" = ifelse(length(repetitions) > 1, rep_i, NA), # set repetition number (if applicable)
                                "Position" = paste0( # Create name for position in this format: segment[instance#]-field#[rep#].comp#.sub#
                                  segment_name, # include segment
                                  ifelse(include_segment_instance, paste0("[", segment_instance, "]"), ""), # include instance (if the segment occurs more than once)
                                  "-", field_i, # include the field
                                  ifelse(length(repetitions) > 1, paste0("[", rep_i, "]"), ""), # include the repetition (if field repeats more than once)
                                  ifelse(length(components) > 1, paste0(".", comp_i), ""), # include the component (if the field has more than one component)
                                  ifelse(length(subcomponents) > 1, paste0(".", sub_i), "") # include the subcomponent (if the component has more than one subcomponent)
                                ),
                                "Value" = subcomponents[sub_i] # set the raw value
            )
            df = rbind(df, new_df) # append the new df to the growing final df
          }
        }
      }
    }    
  }
  df$Order = seq_along(df$Segment) # Create Index to track absolute position within message
  # Join the field + component (if present) + subcomponent (if present) names as a new field ("Name"):
  df = left_join(df, element_names_df, by=c("Segment", "Field", "Component", "Subcomponent")) 
  # join missing names if needed:
  missing_name = filter(df, is.na(Name))
  if (nrow(missing_name) > 0) {
    missing_name = missing_name %>%
      left_join(element_names_df, by=c("Segment", "Field", "Component"), suffix = c("", "_drop")) %>% # Join names w/o subcomponent
      mutate(Name_no_sub = if_else(!is.na(Name_drop) & !is.na(Component_Name_drop), # create name by combining Field name and Component name
                                   paste(Name_drop, Component_Name_drop, sep = ": "),
                                   NA)) %>%
      select(-ends_with("_drop")) %>% 
      left_join(element_names_df, by=c("Segment", "Field"), suffix = c("", "_drop"), relationship = "many-to-many") %>% 
      rename(Name_no_comp = Field_Name_drop) %>% 
      select(-ends_with("_drop")) %>%
      mutate(Name = case_when(!is.na(Name_no_sub) ~ Name_no_sub, !is.na(Name_no_comp) ~ Name_no_comp)) %>% 
      select(-Name_no_sub, -Name_no_comp) %>% 
      distinct()
    # join rows with missing names (which should now be filled) with the rows that originally were not missing
    df = df %>% filter(!is.na(Name)) %>% rbind(missing_name) %>% arrange(Order)
  }
  # remove pii if indicated
  if (remove_pii) { # default is TRUE; if TRUE, remove potential PII
    date_name = grepl("date|time", df$Name, ignore.case=T) # find fields with date or time in name
    id_fields= c("ordering facility name", 
                 "address", 
                 "Message Control ID", 
                 "Security", 
                 "Ordering Provider", 
                 "Performing Organization Medical Director",
                 "Producer's ID", 
                 "Relevant Clinical Info", 
                 "ssn", 
                 "driver's license", 
                 "Mother's",
                 "Patient Identifier", 
                 "patient", 
                 "phone", 
                 "county", 
                 "race",
                 "sex", 
                 "marital", 
                 "religion", 
                 "ethnic",
                 "universal id",
                 "namespace id",
                 "entity identifier",
                 "organization",
                 "software",
                 "placer",
                 "filler",
                 "producer",
                 "observer",
                 "message type",
                 "observation method")
    id_name = grepl(paste0(id_fields, collapse="|"), df$Name, ignore.case=TRUE) # find fields that likely contain identifiers
    id_name = id_name | df$Segment == "PID" # include any PID segment fields
    # mask dates
    df[date_name, "Value"] = gsub("(^|\\D)(\\d{8})($|\\D)", # find YYYYMMDD strings 
                                      "\\1YYYYMMDD\\3", 
                                      df[date_name, "Value"])
    df[date_name, "Value"] = gsub("(^|\\D)(\\d{12})($|\\D)", # find YYYYMMDDHHMM strings
                                      "\\1YYYYMMDDHHMM\\3", 
                                      df[date_name, "Value"])
    df[date_name, "Value"] = gsub("(^|\\D)(\\d{14})($|\\D)", # find YYYYMMDDHHMMSS strings
                                      "\\1YYYYMMDDHHMMSS\\3", 
                                      df[date_name, "Value"])
    # mask identifiers
    df[id_name, "Value"] <- gsub("\\w|\\s", "__char__", df[id_name, "Value"])
    # replace long strings (8+) of removed characters with some length of X's (between 5 and 10 repeats) 
    while(any(grepl('(__char__){8,}', df$Value))) {
      df$Value = sub('__char____char____char____char____char____char____char__(__char__)+', 
                         paste(rep("X", round(min(10, max(5, rnorm(1, 7, 1.75))))), collapse=""), df$Value)
    }
    df$Value = gsub("__char__", "X", df$Value, fixed=TRUE) # replace any shorter length of removed values
  }
  df$Value[df$Position == "MSH-2"] = hl7_fields_seps # reset the separator text that was removed earlier
  # Order cols in return df:
  df = select(df, Order, Value, Position, Name, Segment, Instance, Field, Repetition, Component, Subcomponent)
  
  return(df)
}

# Convert HL7 Dataframes back into strings:
generate_hl7 <- function(df, field_sep="|", segment_sep="\r\n") {
  #' Flattens a df of HL7 elements into a single string
  #'
  #' Param:
  #'  - df: dataframe containing Value, Segment, Instance, Field, Repetition, Component, Subcomponent fields
  #'        (see parse_hl7 for field descriptions)
  #'  - field_sep: separator used between fields; default pipe ("|")
  #'  - segment_sep: separator used between segments; default carraige return + new line ("\r\n")
  #' 
  #' Returns: character element with df$Value joined with appropriate separators based on MSH-2 values
  #'
  hl7_fields_seps = df$Value[df$Segment == 'MSH' & df$Field == 2] # keep only the separator chars
  hl7_seps_list = list("component" = substr(hl7_fields_seps, 1, 1), # set component separator char
                       "repetition" = substr(hl7_fields_seps, 2, 2), # set repetition separator char
                       "escape" = substr(hl7_fields_seps, 3, 3), # set escape char
                       "subcomponent" = substr(hl7_fields_seps, 4, 4)) # set subcomponent char
  
  segments = df %>% 
    select(Value, Segment, Instance, Field, Repetition, Component, Subcomponent) %>% # only keep necessary fields
    group_by(Segment, Instance, Field, Repetition, Component) %>% # grouping excludes Value and Subcomponent 
    mutate(
      Value = paste(Value, collapse=hl7_seps_list$subcomponent), # Collapse values by Subcomponent separator
      Subcomponent = NULL # Drop Subcomponent so we can dedup below
    ) %>% 
    distinct() %>% # Remove duplicate rows which held subcomponent parts
    group_by(Segment, Instance, Field, Repetition) %>%  # Grouping excludes Component
    mutate(
      Value = paste(Value, collapse=hl7_seps_list$component), # Collapse values by Component separator
      Component = NULL # Drop Component so we can dedup below
    ) %>% 
    distinct() %>% # Remove duplicate rows which held Component parts
    group_by(Segment, Instance, Field) %>% # Grouping excludes repetition
    mutate(
      Value = paste(Value, collapse=hl7_seps_list$repetition), # Collapse values by Repetition separator
      Repetition = NULL # Drop Repetition so we can dedup below
    ) %>% 
    distinct() %>% # Remove duplicate rows which held Repetitions
    group_by(Segment, Instance) %>% # Grouping excludes Field
    mutate(
      Value = paste(Segment, paste(Value, collapse=field_sep), sep=field_sep), # Collapse values by Field separator; add the Separator str in front
      Field = NULL # Drop Field so we can dedup below
    ) %>% 
    distinct() # Remove duplicate rows which held Fields
  # We end with a df containing entire segment strings in the Value field
  
  message = paste(segments$Value, collapse=segment_sep) # flatten to one string
  message = gsub(paste0("^MSH.{", nchar(field_sep), "}MSH"), "MSH", message) # remove extra "MSH"
  
  return(message)
}
