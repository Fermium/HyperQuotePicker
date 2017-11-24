#Install missing requirements and load them
rm(list = ls())
reqpackages <- c(
  "doMC",
  "gtools",
  "tictoc",
  "ggplot2",
  "data.table",
  "foreach",
  "compiler",
  "future",
  "optparse",
  "purrr",
  "tibble"
  )
new.packages <- reqpackages[!(reqpackages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(reqpackages, require, character.only = TRUE)

# clean up varialble
rm(list = ls())
option_list = list(
  make_option(c("-f", "--file"), type="character", default="example.csv", 
              help="csv format quoteDataset file name [default=%default]", metavar="character"),
  make_option(c("-o", "--out"), type="character", default="best", 
              help="output files name [default= %default]", metavar="character"),
  make_option(c("-n","--maxCheap"),type="integer",default = 3,
              help="number of output for each combination [default = %default",metavar="integer")
  )

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)
#### CONFIG
CPUCores <- availableCores()
MaxCheapest <- opt$maxCheap # Max number of cheapest lines for each amount of suppliers to shop display
inputFileName <- opt$file
outputFileName <- paste(opt$out,"csv",sep=".")
outputGraphsFileName <- paste(opt$out,"pdf",sep=".")

enableJIT(3)
tic()
pdf(outputGraphsFileName, paper="a4")

sprintf("The script will be run with %d CPU cores, be sure this matches the number of cores on your system", CPUCores)

quoteData <- read.csv(inputFileName)

#Extract suppliers and pieces list
suppliers <- unique(quoteData$Supplier)
pieces <- unique(quoteData$Piece)

#grid.table((suppliers))
#grid.table(pieces)


#Only take the first five colums
quoteData <- quoteData[, 1:5]

# Remove commas and dollar sign from the prices
quoteData$Price <- as.numeric(gsub('[$,€]', '', quoteData$Price))
quoteData$Tot <- as.numeric(gsub('[$,€]', '', quoteData$Tot))

# The piece list contains every piece possible suppliers and prices
pieceList <-split(quoteData,quoteData$Piece) %>% lapply(.,function(x){x[!is.na(x$Price),]})

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

registerDoMC(CPUCores)
# Calculate the total price for each combination
actualSuppliers<-sapply(pieceList,nrow)
means<-lapply(pieceList,function(pp){unname(quantile(pp$Tot,.75))})
sds<-lapply(pieceList,function(pp){sd(pp$Tot)})
x<-x[apply(x,1,function(y){all(y<=actualSuppliers)}),]
sprintf("Total combinations: %d",nrow(x))
totals <- foreach (j=1:dim(x)[1], .combine=rbind, .multicombine = TRUE) %dopar% {

  if(all(mapply(function(pp,index,i){pp$Tot[index[i]]},pieceList,x[j,])<means+sds)){
  tot <- sum(mapply(function(pp,index,i){pp$Tot[index[i]]},pieceList,x[j,],SIMPLIFY = T))
  supps <- mapply(function(pp,index,i){pp$Supplier[index[i]]},pieceList,x[j,],SIMPLIFY =T)

 c(tot, length(unique(supps)),supps)
  }

} %>% as_tibble(.)

#Rename columns
colnames(totals) <- c('price', 'nOfSuppliers', levels(pieces))


p <- ggplot(totals, aes(x=price)) + geom_histogram(binwidth=25)+  ggtitle(sprintf("output for ALL suppliers"))

print(p, newpage = TRUE)
for(i in 1:MaxCheapest){
  tmp <- totals[totals$nOfSuppliers == i, ]
  p <- ggplot(tmp, aes(x=price)) + geom_histogram(binwidth=25) +   ggtitle(sprintf("output for %d suppliers", i))
  print(p, newpage = TRUE)

}

# Sort and get cheapest supplier from a single supplier to [number of pieces] suppliers
best <- foreach (i= 1:min(length(suppliers),length(pieces)), .combine=rbind) %dopar%{
  tmp <- totals[totals$nOfSuppliers == i, ]
  tmp<-tmp[order(tmp$price), ]  
  tmp[1:MaxCheapest, ]
}

# Replace suppliers "number" with the name string
for (i in 3:(length(pieces) + 2)) {
  best[, i] <- levels(suppliers)[best[[i]]]
}


write.csv(best, file = outputFileName)
dev.off()
toc()
