
cat ./tests/sudoku17.shuffled | timeout 1m $* | wc -l
exit 0

