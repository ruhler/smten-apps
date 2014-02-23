
data <- read.table("data/icfp14.data")
pdf(file="build/sudoku_icfp14.pdf", height=5)
barplot(data[,2], log="y", names.arg=data[,1])


