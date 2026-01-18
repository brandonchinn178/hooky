# test/Hooky/E2ESpec.hs

## hooky run / outputs --format=full

```
◈─── PASS hooky_pass (duration: XX.XXXs)
╭─── FAIL hooky_fail (duration: XX.XXXs)
│ ═══▶ Running: hooky lint
│ file.fail:
◈ - [end_of_file_fixer] file would be changed
◈─── SKIP hooky_skip (duration: XX.XXXs)
1 hook passed ✔
1 hook failed ✘
1 hook skipped ≫
```

## hooky run / outputs --format=minimal

```
╭─── FAIL hooky_fail (duration: XX.XXXs)
│ ═══▶ Running: hooky lint
│ file.fail:
◈ - [end_of_file_fixer] file would be changed
1 hook passed ✔
1 hook failed ✘
1 hook skipped ≫
```

## hooky run / outputs --format=verbose

```
╭─── PASS hooky_pass (duration: XX.XXXs)
│ ═══▶ Running: hooky lint
│ Hooks passed:
◈ - end_of_file_fixer
╭─── FAIL hooky_fail (duration: XX.XXXs)
│ ═══▶ Running: hooky lint
│ file.fail:
◈ - [end_of_file_fixer] file would be changed
╭─── SKIP hooky_skip (duration: XX.XXXs)
◈ ═══▶ Running: hooky lint
1 hook passed ✔
1 hook failed ✘
1 hook skipped ≫
```
