Output demo.gif

Set Width 1100
Set Height 800
Set FontSize 20
Set TypingSpeed 25ms

Set Margin 20
Set MarginFill "#674EFF"
Set BorderRadius 10

Require git
Require hooky

Hide
Type 'tmpdir=$(mktemp -d)' Enter
Type 'cp * "$tmpdir"' Enter
Type 'cp hooky.kdl "$tmpdir/.hooky.kdl"' Enter
Type 'cd "$tmpdir"' Enter
Type 'git init' Enter
Type 'git add .' Enter
Type 'git reset HEAD -- test.txt' Enter
Type 'git commit -m "Initial commit"' Enter
Env HOOKY_NO_GLOBAL '1'
Type 'clear' Enter
Show

Type 'hooky install'                             Sleep 0.5s Enter Wait Sleep 1s
Type 'git add . && git commit -m "Make changes"' Sleep 0.5s Enter Wait Sleep 3s
Type 'hooky fix'                                 Sleep 1.5s Enter Wait Sleep 3s
Type 'git add . && git commit -m "Make changes"' Sleep 0.5s Enter Wait Sleep 3s
Sleep 5s

Hide
Type 'rm -rf "$tmpdir"' Enter
