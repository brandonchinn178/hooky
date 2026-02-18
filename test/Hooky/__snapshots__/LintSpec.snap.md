# Hooky.Lint

## check_broken_symlinks / fails when a symlink is broken

```
foo-link.txt:
- [check_broken_symlinks] File is a broken symlink. Remove or exclude from rule
```

## check_case_conflict / fails when files conflict

```
foo.txt:
- [check_case_conflict] File conflicts with: FOO.TXT
```

## check_merge_conflict / fails when there are merge conflicts

```
foo.txt:
- [check_merge_conflict] Merge conflict string found at line 1: "<<<<<<< "
```

## end_of_file_fixer / autofixes when file has no trailing newlines

```
foo.txt:
- [end_of_file_fixer] FIXED
```

## end_of_file_fixer / fails when file has no trailing newlines

```
foo.txt:
- [end_of_file_fixer] file would be changed
```

## no_commit_to_branch / fails when committing on bad branch

```
FAILURES:
- [no_commit_to_branch] cannot commit to branch: main
```

## no_commit_to_branch / supports globs

```
FAILURES:
- [no_commit_to_branch] cannot commit to branch: release-2.0
```

## trailing_whitespace / autofixes trailing whitespace

```
end-space.txt:
- [trailing_whitespace] FIXED

end-tab.txt:
- [trailing_whitespace] FIXED
```

## trailing_whitespace / fails when line has trailing whitespace

```
end-space.txt:
- [trailing_whitespace] file would be changed

end-tab.txt:
- [trailing_whitespace] file would be changed
```
