# Hooky

Minimal git hooks manager.

Features:
* Installs as a compiled executable; no need to have Python/Node/whatever installed to bootstrap.
* Only runs on staged files
    * Supports partially staged files
* Built-in support for auto-fixing files

## Installation

Download the appropriate binary from the GitHub release.

## Usage

* `hooky install [--mode=check|fix|fix-add]`
    * Registers `hooky` with git hooks.
    * `--mode` determines what to do when hooks modify files:
        * `check`: Run the commands in `check` mode, which shouldn't modify files
        * `fix`: Run the commands in `fix` mode, which may modify files
        * `fix-add`: Same as `fix`, except add the changes to the stage and continue the commit

* `hooky run hook1 hook2 -- [FILES ...]`
    * Manually runs the given hooks in `check` mode
    * Files may be specified as `@foo.txt`, where `foo.txt` contains one file per line.
    * Shortcuts:
        * `--modified`/`-m` = `@<(git diff --name-only --diff-filter=AMR)`
        * `--staged`/`-s` = `@<(git diff --staged --name-only --diff-filter=AMR)`
        * `--all`/`-a` = `@<(git ls-files)`
        * `--prev`/`-1` = `@<(git diff HEAD~1..HEAD --name-only --diff-filter=AMR)`
    * `--stash` stashes all unstaged changes before running
    * If no files are specified, equivalent to `--stash --staged`

* `hooky fix hook1 hook2 -- [FILES ...]`
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
    files *.txt
}
```

* `command`: TODO
* `check_args`: Additional arguments to pass when running in `check` mode
* `fix_args`: Additional arguments to pass when running in `fix` mode
* `pass_files`: How to pass files to the command
    * `xargs` (default) - Pass files to `xargs`, which will batch execute
    * `xargs_parallel` - Same as `xargs`, except run in parallel
    * `file` - Put files into a file (one file per line) and pass the list as an argument as `@path/to/file.txt`
    * `none` - Don't pass files to the command at all
* `files`: TODO
    * `"*.hs" => "**/*.hs"`
    * `"/*.hs" => "*.hs"`
    * `"foo/*.hs" => "**/foo/*.hs"`
    * `"/foo/*.hs" => "foo/*.hs"`
    * `"!foo.hs" => "!**/foo.hs"`

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

### Skipping hooks

To temporarily skip hooks, use the `SKIP` env var, which takes a comma-delimited list of hooks to skip. This works for both `hook`s and `lint_rules`.

## Comparison with other tools

### pre-commit

[`pre-commit`](https://pre-commit.com) is primarily a tool/environment manager ([ref](https://github.com/pre-commit/pre-commit/pull/3577), [ref2](https://github.com/pre-commit/pre-commit/issues/2316#issuecomment-1083643390)), not a command runner. IMO this is the wrong direction:
* `pre-commit` doesn't have any lockfiles, whereas your language's normal dependency management will likely support pinned dependencies
* With `pre-commit`, you have to re-configure the linter in `.pre-commit-config.yaml`, e.g. re-specifying `eslint` plugins

At the end of the day, git pre-commit hooks should just be a matter of taking commands you can already run manually and registering them with git hooks. That is the only thing Hooky cares about, and how you want to manage your tools is up to you.

### husky

TODO: https://typicode.github.io/husky/#/

### prek

TODO: https://prek.j178.dev/
