# Hooky

Minimal git hooks manager.

Features:
* Only runs on staged files
    * Supports partially staged and untracked files
* Hooks run in parallel
* Built-in support for auto-fixing files
* Show stdout while hook is in-progress
* Support for passing files to hooks via response files

<img src="scripts/demo/demo.gif" />

## Installation

### With Homebrew

```shell
brew install brandonchinn178/tap/hooky
```

### GitHub release

Download the appropriate binary from the [GitHub releases page](https://github.com/brandonchinn178/hooky/releases).

## Usage

* `hooky install [--mode MODE] [--format FORMAT]`
    * Registers `hooky` with git hooks.
    * `--mode` determines what to do when hooks modify files:
        * `check` (default) - Run the commands in `check` mode, which shouldn't modify files
        * `fix` - Run the commands in `fix` mode, which may modify files
        * `fix-add` - Same as `fix`, except add the changes to the stage and continue the commit
    * `--format` determines how to format the output:
        * `minimal` (default) - Only show full stdout of failed hooks.
        * `full` - Show passed/skipped hooks, without their stdout. Include durations for all hooks.
        * `verbose` - Show full stdout + duration of all hooks.
    * Configure defaults with the [Global configuration](#global-configuration)

* `hooky run [FILES ...] [-k HOOK]`
    * Manually runs the given hooks in `check` mode
    * Files may be specified as `@foo.txt`, where `foo.txt` contains one file per line.
    * Shortcuts:
        * `--modified`/`-m` = `@<(git diff --name-only --diff-filter=AMR)`
        * `--staged`/`-s` = `@<(git diff --staged --name-only --diff-filter=AMR)`
        * `--all`/`-a` = `@<(git ls-files)`
        * `--prev`/`-1` = `@<(git diff HEAD~1..HEAD --name-only --diff-filter=AMR)`
    * `--stash` stashes all unstaged changes before running
    * If no files are specified, equivalent to `--stash --staged`
    * `-k` may be specified multiple times, specifying the hooks to run

* `hooky fix [FILES ...] [-k HOOK]`
    * Same as `hooky run`, except runs in `fix` mode

### `.hooky.kdl` configuration

Configuration is specified with the [KDL](https://kdl.dev) language.

Each hook is specified in a `hook` section:

```kdl
hook name_of_hook {
    command my_formatter arg0 arg1 {
        check_args --mode check
        fix_args --mode fix
        pass_files file
    }
    files *.py *.txt
}
```

* `command`: The command to run
    * Can be written without quotes, but quotes are needed if special characters like `/` or spaces are used
* `check_args`: Additional arguments to pass when running in `check` mode
* `fix_args`: Additional arguments to pass when running in `fix` mode
* `pass_files`: How to pass files to the command
    * `xargs` (default) - Pass files to `xargs`, which will batch execute
    * `xargs_parallel` - Same as `xargs`, except run in parallel
    * `file` - Put files into a file (one file per line) and pass the list as an argument as `@path/to/file.txt`
    * `none` - Don't pass files to the command at all
* `files`: Globs of files that should trigger the hook
    * `*` matches any character except `/`
    * `**` matches zero or more directories
      * e.g. `a/**/*.txt` matches `a/foo.txt` and `a/b/foo.txt` but not `b/foo.txt`
    * Relative paths match anywhere in the repo
      * e.g. `*.txt` matches `foo.txt` and `foo/bar.txt`
      * In other words, `**/` is auto-prepended to the beginning
    * Absolute paths match from the root of the repo
      * e.g. `/*.txt` matches `foo.txt` but not `foo/bar.txt`
    * A glob can be negated with a `!` at the beginning
      * e.g. `!foo.txt` matches `bar.txt` but not `foo.txt` nor `a/foo.txt`
      * e.g. `!/foo.txt` matches `bar.txt` and `a/foo.txt` but not `foo.txt`
    * When multiple globs are specified, a file matches if:
      * Any positive globs match, AND
      * All negative globs don't match

You may also specify defaults in the `defaults` section. Available options are documented below:

```kdl
defaults {
    // Any globs specified here are inherited by all hooks
    files *.py
}
```

### Built-in hooks

For convenience, Hooky also ships with some general purpose hooks, which can be specified as:

```kdl
hook hooky {
    command hooky lint {
        fix_args --fix
        pass_files file
    }
    files *
}
```

`hooky lint` runs the rules specified under the `lint_rules` section, e.g.

```kdl
lint_rules {
    - check_broken_symlinks
    - no_commit_to_branch {
        branches {
            - main
            - release-*
        }
    }
    - trailing_whitespace {
        files "!foo.txt"
    }
}
```

All rules have the following configuration:

* `files`: Same as `files` in `hook` sections. If not specified, defaults to `*`

Available rules and rule-specific configuration:

* `check_broken_symlinks` - Check if any symlinks which do not point to anything
* `check_case_conflict` - Check if any files differ only by case, which would cause issues on case-insensitive filesystems like macOS
* `check_merge_conflict` - Check if any files contain merge conflict strings
* `end_of_file_fixer` - Ensure files end with exactly one newline
* `no_commit_to_branch` - Prevent commits to specific branches
    * `branches` - Branches to prevent commits to, as a glob pattern
* `trailing_whitespace` - Remove trailing whitespace

### Global configuration

Global configuration can also be specified at `$XDG_CONFIG_DIR/hooky/settings.kdl` (e.g. `~/.config/hooky/settings.kdl`).

Available configuration and their defaults is documented below:

```kdl
flags {
    // The default --mode to use when committing, if not overridden in `hooky install`
    --mode check

    // The default --format to use when committing, if not overridden in `hooky install`
    --format minimal
}

// The maximum number of output lines to show while a hook is running
max_output_lines 5

// The maximum number of hooks to run in parallel in `check` mode
max_parallel_hooks 5
```

### Skipping hooks

To temporarily skip hooks, use the `SKIP` env var, which takes a comma-delimited list of hooks to skip. This works for both `hook`s and `lint_rules`.

## Example hook configurations

### Ruff in a uv project

```kdl
hook ruff_check {
    command uv run ruff check {
        fix_args --fix
        pass_files file
    }
    files *.py
}

hook ruff_format {
    command uv run ruff format {
        check_args --check
        pass_files file
    }
    files *.py
}
```

### ESLint in an npm project

```kdl
hook eslint {
    command npx eslint {
        fix_args --fix
        pass_files xargs
    }
    files *.json *.js *.jsx *.ts *.tsx
}
```

## Comparison with other tools

### pre-commit

[`pre-commit`](https://pre-commit.com) is primarily a tool/environment manager ([ref](https://github.com/pre-commit/pre-commit/pull/3577), [ref2](https://github.com/pre-commit/pre-commit/issues/2316#issuecomment-1083643390)), not a command runner. IMO this is the wrong direction:
* `pre-commit` doesn't have any lockfiles, whereas your language's normal dependency management will likely support pinned dependencies
* With `pre-commit`, you have to re-configure the linter in `.pre-commit-config.yaml`, e.g. re-specifying `eslint` plugins

At the end of the day, git pre-commit hooks should just be a matter of taking commands you can already run manually and registering them with git hooks. That is the only thing Hooky cares about, and how you want to manage your tools is up to you.

Other notable differences:
* `hooky` installs as a compiled executable; no need to have the right Python environment
* Built-in functionality for fixing failures, with optional auto-add fixes in commit
* Hooks run in parallel
* Show hook output while it's running, to get more visibility into what's happening
* Support passing files via response files instead of using `xargs`

### husky

[Husky](https://typicode.github.io/husky/#/) is pretty specific to NodeJS projects. It's implemented in Javascript, so you need to install Husky with `npm`. By default, it will run the `prepare` script in `package.json`, but you can also add arbitrary commands to `.husky/pre-commit`. This is flexible, but completely open-ended, with no structure running/configuring each hook independently.

### prek

[prek](https://prek.j178.dev/) is essentially a reimplementation of [`pre-commit`](https://pre-commit.com), so it inherits the same issues.
