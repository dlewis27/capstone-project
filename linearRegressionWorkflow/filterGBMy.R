library(tidyverse)
library(karyoploteR)
library(ggbio)
library(Homo.sapiens)

gbmLin = read_tsv("gbmCNVy.tsvlinRegressOutput.tsv")
sortedGbmLin = arrange(gbmLin, gbmLin$pValue)

brcaNeg <- scan("brca_neg_slope.txt", what="")

getList = function(inList){
  returnable = c()
  numFilteredOut = 0
  for(i in c(1:length(inList))){
    if(is.na(inList[i])){
      next
    }
    worked = try(genesymbol[inList[i]])
    if(isTRUE(class(worked)=="try-error")) {
      print(i)
      print(inList[i])
      numFilteredOut = numFilteredOut+1
      next } 
    else {
      returnable = c(returnable, inList[i])
    } 
  }
  print(paste0("number of CNVs filtered out: ", numFilteredOut, " of ", length(inList), " (",round((numFilteredOut/length(inList))*100, 3),"%)"))
  returnable
}

#plot list of genes onto chromosomes
#usage: plotList(brca$CNV), plotList(sigGenes$BRCA)
plotList = function(inList){
  
  data(genesymbol, package = "biovizBase")
  tryList = getList(inList)
  wh <- genesymbol[tryList] 
  #wh <- range(wh)
  kp <- plotKaryotype(genome="hg19",plot.type=1)
  # , chromosomes=c("chrY") plot type 3
  #chromosomes=c("chr1", "chr3", "chr7", "chr8", "chr20"))
  kpPlotRegions(kp, wh, col="orange")
}



first = sortedGbmLin$CNV[0:100]
sort(table(first))
plotList(unique(first))
plotList(first)

firstPro = sortedGbmLin$Protien[0:100]
sort(table(firstPro))

#
ggplot(sortedGbmLin[0:300,])+
  geom_histogram(aes(x=pValue, fill = Protien), bins = 100) +
  ggtitle("Histogram of the first 300 CNV Protien pairs") +
  geom_vline(xintercept= .00006, linetype = "dashed", color = 'darkred') +
  geom_text(x=.00006, label="p = 0.00006", y=25, colour="darkred", angle=90, vjust = 1.2, size = 4) +
  theme_bw()

filteredGbmLin = filter(sortedGbmLin, pValue <= .00006)

ggplot(filteredGbmLin)+
  geom_histogram(aes(x=pValue, fill = Protien), bins = 100) +
  ggtitle("Histogram CNV Protien pairs (p<0.00006)") +
  theme_bw()

ggplot(filteredGbmLin)+
  geom_histogram(aes(x=slope, fill = Protien), bins = 100) +
  ggtitle("Slope of Proteins in CNV") +
  theme_bw()

ggplot(sortedGbmLin[0:20,])+
  geom_histogram(aes(x=pValue, fill = CNV), bins = 100)



data(genesymbol, package = "biovizBase")
tryList = getList(brcaNeg)
wh <- genesymbol[tryList]
x = wh[wh@seqnames == "chr1"]
x = x@ranges@NAMES

