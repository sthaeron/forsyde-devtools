# Contribution Documentation

## Git management

All members should fork the repository and work on their local copy.
Changes should be kept on separate branches submitted as pull requests.

Add the main repository as a separate remote:

```
git remote add upstream git@github.com:sthaeron/forsyde-devtools.git
```

### Sync your fork

The local main branch can be updated e.g. by:

```
git pull upstream main
git push origin main
```

### Adding a new feature

Look for a corresponding issue if there exists one.
If there is none, look through [Chaos docs](chaos.md) and
consider creating one before starting to work so other members
of the project better know what is going on.

When that's done, create a separate branch, e.g if you have added
the main repo as the `upstream` remote:

```
git checkout -b work/feature upstream/main
# Do your work
git commit
git push origin work/feature
```

Ideally, you should separate commits for different parts of the project,
e.g. if you contributed actor11SDF, you should separate the commits
for e.g. the parser and the code generation.

Now, create a pull request from the branch on the main repo
(you can also use the link which appears when you push the new branch).

Wait for someone else to review and merge the pull request

### Commit Guideline

Each commit message should be concise, and contain commit types. More information in [conventional commits](https://www.conventionalcommits.org/en/v1.0.0/#summary). Additional messages can be placed in the body.

Typical commit types:

- fix
- feat
- build
- chore
- ci
- docs
- style
- refactor
- perf
- test

Example commit message:

```
fix: prevent racing of requests

Introduce a request id and a reference to latest request. Dismiss
incoming responses other than from latest request.
```

## Development environment

### Prerequisites

- [Nix](https://nixos.org/download.html) (multi-user or single-user installation)

### Setup

1. Install Nix by following the instructions at <https://nixos.org/download.html>

2. Enter the development environment:

   ```bash
   nix-shell
   ```

3. You now have all required dependencies (including OCaml and dune) available in your environment.

## Continuous Integration
