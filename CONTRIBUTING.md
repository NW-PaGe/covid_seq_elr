# Contributing Guidelines

*Pull requests, bug reports, and all other forms of contribution are welcomed and highly encouraged!*

### Contents
- [NEVER COMMIT PRIVATE INFORMATION](#warning-never-commit-private-information)
- [Opening an Issue](#lady_beetle-opening-an-issue)
- [Submitting Pull Requests](#repeat-submitting-pull-requests)
- [Writing Commit Messages](#memo-writing-commit-messages)

## :warning: NEVER COMMIT PRIVATE INFORMATION

This is intended to be a public repository shared with other public health organizations and available to the public. 

NEVER add any:
- Real case data: demographics, identifiers, test results, etc.
- Private health information
- Server names, keys, secrets, etc.

## :lady_beetle: Opening an Issue

A great way to contribute to the project is to send a detailed issue when you encounter a problem. We always appreciate a well-written, thorough bug report. Please include as much detail as possible in your issue so that the issue can easily be identified and reproduced, if applicable.

:sparkles: Feature requests and other issues are also welcome.

## :repeat: Submitting Pull Requests

This repo uses the [GitHub flow](https://docs.github.com/en/get-started/using-github/github-flow) as the main versioning workflow:
1. Fork the repository
2. Create a new branch for each feature, fix or improvement
3. Send a pull request from each feature branch to the main branch

It is very important to separate new features or improvements into separate feature branches, and to send a pull request for each branch.

## :memo: Writing Commit Messages

Please utilize [conventional commit](https://www.conventionalcommits.org/) syntax.

The commit message should be structured as follows:
```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```

`<type>` should be one of:
   - `feat`: New feature for the user, not a new feature for build script
   - `fix`: Bug fix for the user, not a fix to a build script
   - `docs`: Changes to the documentation
   - `style`: Formatting, missing semi colons, etc; no production code change
   - `refactor`: Refactoring production code, eg. renaming a variable
   - `test`: Adding missing tests, refactoring tests; no production code change
   - `chore`: Updating grunt tasks etc; no production code change

*When in doubt, pick whatever seems closest to your request.*
