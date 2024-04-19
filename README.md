# Isabelle Build System

## Installation
1. Clone Isabelle, init repository (see README_REPOSITORY in isabelle)
2. Clone this repository
3. Initialize: `isabelle components -u <this_repo_dir>`
4. Run with `isabelle build_system`

## IDE Setup
To develop this project, install the component and then run `isabelle scala_project`, e.g.:
```
isabelle scala -M -L -D scala_project
```
Then import the resulting (maven or gradle) project structure from the `scala_project` dir with your IDE.