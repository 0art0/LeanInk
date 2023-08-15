error_indicator="ERROR - ERROR"
prefix="ERROR - ERROR executing LeanInk on"
postfix="\: Command"

files=$(grep "$error_indicator" -B 0 -C 0 ~/Downloads/tactic_extraction.log | awk -v a="$prefix" -v b="$postfix" 'match($0, a ".*" b) { print substr($0, RSTART + length(a), RLENGTH - length(a) - length(b)); }')

for file in $files
do
    ./build/bin/leanInk $file
done