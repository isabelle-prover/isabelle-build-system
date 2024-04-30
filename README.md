# Isabelle Build System

## Installation

1. If not done: Clone Isabelle, init repository (see README_REPOSITORY in isabelle)
2. Clone this repository
3. Initialize: `isabelle components -u <this_repo_dir>`

## Submitting builds

In your `~/.isabelle/etc/preferences`, set your ssh user to the Isabelle system (same user as for
repository access):
`build_system_ssh_user = "..."`
Then you can submit builds with `isabelle submit_build <...>`, where `<...>` are (most of) the
regular Isabelle build options, e.g.:
```
isabelle submit_build -P HOL-Analysis
```

## Development

### Starting server:

`isabelle build_system`

### IDE Setup

To develop this project, install the component and then run `isabelle scala_project`, e.g.:

```
isabelle scala -M -L -D scala_project
```

Then import the resulting (maven or gradle) project structure from the `scala_project` dir with your
IDE.