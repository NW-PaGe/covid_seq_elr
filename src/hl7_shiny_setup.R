library(dplyr)
library(purrr)
library(kableExtra)
try(source('src/hl7_processing_funs.R'), silent=TRUE) # import custom hl7 funs (if run outside shinylive app)

# Here is code to read in csv file and create hl7 tables
# Since we already have these df created in the rds file, we will read that in, instead
if (FALSE) {
  hl7_messages <- read.csv('data/example_hl7.csv') %>% 
    mutate(hl7_table = map(hl7_string, function(x) parse_hl7(x)))
} else {
  hl7_messages <- readRDS('data/example_hl7.rds')
}

for (i in 1:nrow(hl7_messages)) {
  
  example_tbl <- hl7_messages$hl7_table[[i]]
  
  submitter <- hl7_messages$submitter[i]
  
  lineage <- stringr::str_extract(example_tbl$Value, 'SARS-CoV-2 (.+) lineage', group=1)
  lineage <- sort(lineage) # Remove NA values and sort the rest
  if (length(lineage) > 1) {
    warning(paste0("2+ lineage strings extracted from df. Continuing with first string:\t", lineage[1]))
  }
  
  lineage_mask <- grepl(lineage, example_tbl$Value, fixed=TRUE) # find values with full lineage (e.g. A.B.C.1.2.3)
  lineage_short = gsub('[^A-Z0-9]+', '', lineage, perl=TRUE) # create shortened lineage (e.g., ABC123)
  lineage_mask <- lineage_mask | grepl(lineage_short, example_tbl$Value, fixed=TRUE) # find values with shortened lineage
  example_tbl$Value[lineage_mask] <- text_spec(example_tbl$Value[lineage_mask], 'html', background='#aebccb', tooltip='Lineage') # add tooltip and highlight lineage
  
  strain_background <- ifelse(submitter == 'UW', '#969f84', '#bac0ae')
  strain_tooltip <- ifelse(submitter == 'UW', 'Full NCBI/GISAID strain name', 'Partial NCBI/GISAID strain name')
  strain_mask <- submitter %in% c('LabCorp', 'UW', 'Helix') & example_tbl$Value %in% example_tbl$Value[example_tbl$Position %in% 'OBX[2]-5']
  strain_mask <- strain_mask | (submitter == 'Quest' & example_tbl$Value %in% example_tbl$Value[example_tbl$Position %in% c('OBX[3]-5', 'SPM-2.2.1')])
  example_tbl$Value[strain_mask] <- text_spec(example_tbl$Value[strain_mask], 'html', background=strain_background, tooltip=strain_tooltip) # add tooltip and highlight strain name
  
  clin_acc_labcorp_mask <- submitter == 'Labcorp' & example_tbl$Value %in% example_tbl$Value[example_tbl$Position %in% 'OBR-3']
  clin_acc_quest_uw_mask <- submitter %in% c('Quest', 'UW') & example_tbl$Value %in% example_tbl$Value[example_tbl$Position %in% 'SPM-2.1.1']
  clin_acc_mask <- clin_acc_labcorp_mask | clin_acc_quest_uw_mask
  example_tbl$Value[clin_acc_mask] <- text_spec(example_tbl$Value[clin_acc_mask], 'html', background='#cbb3bf', tooltip='Clinical accession') # add tooltip and highlight clinical accession
  
  reason_mask <- grepl('**SEQREA**', example_tbl$Value, fixed=TRUE)
  example_tbl$Value[reason_mask] <- text_spec(example_tbl$Value[reason_mask], 'html', background='#c9b57e', tooltip='Sequence reason') # add tooltip and highlight reason
  
  
  hl7_messages$string_viz[i] <- gsub('\r\n', '<br>', generate_hl7(example_tbl), fixed=TRUE)
  
  example_tbl_viz <- example_tbl %>% 
    select(Value:Name) %>% 
    keep(function(x) sum(is.na(x)) != nrow(example_tbl)) %>% # Remove all-NA columns (may apply to Repetitions)
    mutate(across(where(is.character), function(x) if_else(is.na(x), '<em>NA</em>', x))) %>% 
    mutate(across(where(is.numeric), function(x) if_else(is.na(x), 0, x))) %>% 
    # # remove submitter row (this gets added later as a label as part of pack_rows)
    # select(-SUBMITTER) %>% 
    # create kable with caption
    kable(format = 'html', caption = '<strong>ELR Example Data (HL7 Formatting)</strong>', escape = F) %>% 
    # specify kable formatting
    kable_classic(full_width=TRUE, font_size=16) %>%
    # change formatting of headers
    row_spec(0, bold = TRUE) %>% 
    # Change row for every other case_id to have gray background 
    row_spec(which(example_tbl$Field %% 2 == 0), background = 'lightgray') %>% 
    # Add labs as label for groups:
    pack_rows(index = table(example_tbl$Segment)[unique(example_tbl$Segment)], # get table of Segment counts in order that counts occcur 
             label_row_css = 'text-align:center;',
             hline_before = T,
             hline_after = T,
             background = '#D0C7E1') %>% 
    # Create scroll box out of table so we don't have an too long or wide of a table
    scroll_box(height = '500px', width = '100%', fixed_thead = list(enabled = T, background = "#C3B1E1"))
  
  hl7_messages$table_viz[i] <- example_tbl_viz
}

viz_df <- data.frame(
  'Name' = paste0(hl7_messages$submitter, " (CASE ID ", hl7_messages$case_id, ")"), # Name to select tables by
  'Table' = hl7_messages$table_viz, # HTML-formatted kable table outputs
  'String' = hl7_messages$string_viz # HTML-formatted string outputs
)

# Write output 
write.csv(viz_df, 'data/example_hl7_viz.csv', row.names=FALSE)
