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

### Feature Contribution Rule

1. Separate commits for different parts of the project.
   E.g. if you contributed actor11SDF, you should separate the commits for e.g. the parser and the code generation.

2. Do the feature work in your own fork. You should also include at least one new test,
    see [Testing Documenation](testing.md) for more information.

3. Check that tests for other modules still complete successfully.
    If there are failing tests which are *expected* to fail as a result of your work,
    update the tests with the new expected behavior.

4. Create a pull request from the branch on the dev repo or via the link which appears when you push the new branch.

5. Wait for someone else to review, and address any resulting comments.

If there is no consensus on how to go forward it should be brought up
in the next team meeting.

Sometimes, a feature might depend on another feature or change
which needs to be handled before the pull request can be merged.

### Shared branches

If you are working together with someone, and find that e.g. meeting
in person and pair programming or sharing the screen is not enough,
you can create a shared branch on
[the main repository](https://github.com/sthaeron/forsyde-devtools).
It should be in the general form of `collab/<feature>`.

Another option is to invite the one you are collaborating with
into your own forked repository.

When the feature is ready, submit a pull request as usual.

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

There are two relevant features we are using for this project:

1. Local git hooks
2. Github actions

### Local git hooks

These need to be copied into your local `.git/hooks` directory.
If you use the nix environment, this is done automatically, but otherwise:

```bash
cp -r ./.githooks/. ./.git/hooks
```

At this point, they are used to ensure a consistent formatting of OCaml source code.

### Github Actions

Github Actions can be specified to run e.g. new commits on main or on pull requests.
They are yaml files which live in the `.github/workflows` directory.

At this point, we only have an automatic build configured for the compiler.
In the future, we should add running the compiler on our examples and comparing the
output from the binary when executed to the simulated output from the ForSyDe model.
