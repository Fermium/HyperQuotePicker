#install.packages("doMC")
#install.packages("gtools")
#install.packages("tictoc")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("gplots")


# clean up varialble
rm(list = ls())

#### CONFIG
CPUCores <- 8
MaxCheapest <- 3 # Max number of cheapest lines for each amount of suppliers to shop display
inputFileName <- "quote.csv"
outputFileName <- "best.csv"
outputGraphsFileName <- "bests.pdf"


# Libraries
library(foreach)
library(doMC)
registerDoMC(CPUCores)
library(gtools)
library(tictoc)
require(ggplot2)
library(gridExtra)
library(gplots)
require(compiler)
enableJIT(3)
tic()



pdf("bests.pdf", paper="a4")

sprintf("The script will be run with %d CPU cores, be sure this matches the number of cores on your system", CPUCores)

data <- read.csv(inputFileName)

#Extract suppliers and pieces list
suppliers <- unique(data$Supplier)
pieces <- unique(data$Piece)

grid.table((suppliers))
grid.table(pieces)


#Only take the first five colums
data <- data[, 1:5]

# Remove commas and dollar sign from the prices
data$Price <- as.numeric(gsub('[$,€]', '', data$Price))
data$Tot <- as.numeric(gsub('[$,€]', '', data$Tot))

# The piece list contains every piece possible suppliers and prices
piece <- list()
for (pp in unique(data$Piece)) {
  piece[[pp]] <- data[data$Piece == pp, 2:5]
  piece[[pp]] <- piece[[pp]][order(piece[[pp]]$Supplier), ]
}

#Generate all possible combinations of pieces and suppliers
#These are only indexes, and will need to be threated accordingl
amountOfSuppliers <- length(suppliers)
amountOfPieces <- length(pieces)
x <-
  permutations(
    amountOfSuppliers,
    amountOfPieces,
    v = 1:length(suppliers),
    repeats.allowed = T
  )

sprintf(
  "there are a total of %d supplier for %d pieces for a total of %d combinations",
  amountOfSuppliers,
  amountOfPieces,
  nrow(x)
)

# Calculate the total price for each combination
totals <- as.data.frame(matrix(NA, 0, length(suppliers) + 2))

totals <- foreach (j=1:dim(x)[1], .combine=rbind) %dopar% {
  tot <- 0
  supps <- c()
  i <- 1
  rrow <- c()
  for (pp in pieces) {
    tot <- tot + piece[[pp]]$Tot[x[j, i]]
    supps <- c(supps, piece[[pp]]$Supplier[x[j, i]])
    rrow <- cbind(rrow, as.numeric(piece[[pp]]$Supplier[x[j, i]]))
    i <- i + 1
  }
  cbind(tot, length(unique(supps)), as.data.frame(rrow))
}

#Rename columns
colnames(totals) <- c('price', 'nOfSuppliers', levels(pieces))


p <- ggplot(totals, aes(x=price)) + geom_histogram(binwidth=25) +  ggtitle(sprintf("output for ALL suppliers"))
print(p, newpage = TRUE)
for(i in 1:MaxCheapest){
  tmp <- totals[totals$nOfSuppliers == i, ]
  p <- ggplot(tmp, aes(x=price)) + geom_histogram(binwidth=25) +   ggtitle(sprintf("output for %d suppliers", i))
  print(p, newpage = TRUE)

}

# Sort and get cheapest supplier from a single supplier to [number of pieces] suppliers
best <- as.data.frame(matrix(NA, 0, length(suppliers) + 2))
best <- foreach (i= 1:length(pieces), .combine=rbind) %dopar%{
  tmp <- totals[totals$nOfSuppliers == i, ]
  tmp <- tmp[order(tmp$price), ]
  tmp[1:MaxCheapest, ]
}

# Replace suppliers "number" with the name string
for (i in 3:(length(pieces) + 2)) {
  best[, i] <- levels(suppliers)[best[, i]]
}


write.csv(best, file = "best.csv")
dev.off()
toc()
