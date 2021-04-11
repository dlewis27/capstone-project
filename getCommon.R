library(tidyverse)
brca = read_tsv('brca_CNV_up.tsv')
ccrcc = read_tsv('ccrcc_CNV_up.tsv')
endo = read_tsv('en_CNV_up.tsv')
gbm = read_tsv('gbm_CNV_up.tsv')
hnscc = read_tsv('hnscc_CNV_up.tsv')
luad = read_tsv('luad_CNV_up.tsv')
ovarian = read_tsv('ovarian_CNV_up.tsv')

dataSets = c(brca2, ccrcc2, endo2, hnscc2, luad2, ovarian2)


ggplot(brca)+
  geom_tile(aes(x=Protien, y=CNV, fill=slope))


brca2 = mutate(brca, name = paste(CNV, Protien, sep = '/'))
ccrcc2 = mutate(ccrcc,name = paste(CNV, Protien, sep = '/'))
endo2 = mutate(endo, name = paste(CNV, Protien, sep = '/'))
gbm2 = mutate(gbm, name = paste(CNV, Protien, sep = '/'))
hnscc2 = mutate(hnscc, name = paste(CNV, Protien, sep = '/'))
luad2 = mutate(luad, name = paste(CNV, Protien, sep = '/'))
ovarian2 = mutate(ovarian, name = paste(CNV, Protien, sep = '/'))

common = Reduce(intersect, list(brca2$name,ccrcc2$name, endo2$name, hnscc2$name, luad2$name, ovarian2$name))

dataSets = filter(dataSets, name%in%common)
lapply(dataSets, function(x) print(x$name))

com = str_split(common, '/')
myList = unlist(com)
myList = myList[c(TRUE,FALSE)]

pairs <- data.frame(matrix(unlist(com), nrow=length(com), byrow=T))
unique(pairs$X2)
unique(pairs$X1)

x = pairs$X1
print(pairs$X1, sep='\n')
for(i in list(pairs$X1)){
  print(i)
  print("hello")
}





