
pdf(file="sudoku.pdf")
data <- read.table("bench.data")
barplot(as.matrix(data), beside=TRUE, legend.text = rownames(data), log="y")

