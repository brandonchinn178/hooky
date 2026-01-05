# Hooky.Lint

## check_broken_symlinks / fails when a symlink is broken

```
foo-link.txt:
- [check_broken_symlinks] File is a broken symlink. Remove or exclude from rule
```

## check_case_conflict / fails when files conflict

```
FOO.TXT:
- [check_case_conflict] File conflicts with: foo.txt

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
