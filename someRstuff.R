library(tidyverse)
brca = read_tsv('brca_CNV_up.tsv')
ccrcc = read_tsv('ccrcc_CNV_up.tsv')
endo = read_tsv('en_CNV_up.tsv')
gbm = read_tsv('gbm_CNV_up.tsv')
hnscc = read_tsv('hnscc_CNV_up.tsv')
luad = read_tsv('luad_CNV_up.tsv')
ovarian = read_tsv('ovarian_CNV_up.tsv')

dataSets = c(brca, ccrcc, endo, gbm,  hnscc, luad, ovarian)
commonCNVs = Reduce(intersect, list(brca$CNV,ccrcc$CNV, endo$CNV, gbm$CNV, hnscc$CNV, luad$CNV, ovarian$CNV))
commonProtiens = Reduce(intersect, list(brca$Protien, luad$Protien))
paste(shQuote(commonCNVs), collapse=", ")

brca2 = filter(brca, CNV%in%commonCNVs)
ccrcc2 = filter(ccrcc, CNV%in%commonCNVs)
endo2 = filter(endo, CNV%in%commonCNVs)
gbm2 = filter(gbm, CNV%in%commonCNVs)
hnscc2 = filter(hnscc, CNV%in%commonCNVs)
luad2 = filter(luad, CNV%in%commonCNVs)
ovarian2 = filter(ovarian, CNV%in%commonCNVs)

interGraph = function(data1, data2){
  comCNV = Reduce(intersect, list(data1$CNV, data2$CNV))
  comPro = Reduce(intersect, list(data1$Protien, data2$Protien))
  data1mod = filter(data1, CNV%in%comCNV, Protien%in%comPro)
  data2mod = filter(data2, CNV%in%comCNV, Protien%in%comPro)
  print(length(comCNV))
  print(length(comPro))
  ggplot(data1mod)+
    geom_tile(aes(x=Protien, y=CNV, fill=slope))
  
}

ggplot(ccrcc)+
  geom_tile(aes(x=Protien, y=CNV, fill=slope))
ggplot(luad)+
  geom_histogram(aes(x=rValue))



a = select(brca2, CNV, Protien, slope)
b = spread(a, Protien, slope)
df <- scale(mtcars)


