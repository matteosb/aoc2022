set -eou pipefail

DAY=$1
FILE="day${DAY}.ml"
INPUT="input${DAY}.txt"

if test -f "$FILE"; then
    echo "$FILE exists."
    exit 1
fi


cat <<- EOF > "$FILE"
open! Core

let () =
  let _lines = In_channel.read_lines "./input${DAY}.txt" in
  printf "\nSolution Part 1: %d\n" 0;
  printf "Solution Part 2: %d\n" 0;

EOF

echo "" > $INPUT
