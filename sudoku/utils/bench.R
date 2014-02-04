
pdf(file="build/sudoku.pdf")
data <- read.table("build/bench.data")
barplot(as.matrix(data), beside=TRUE, legend.text = rownames(data),
        args.legend=list(x = "topright"), log="y")

