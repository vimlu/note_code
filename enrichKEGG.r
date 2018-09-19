source("https://bioconductor.org/biocLite.R")
biocLite("clusterProfiler")
require(clusterProfiler)

geneid_entrez<-read.table("example.geneid")
geneid_entrez2<-as.character(geneid_entrez[,1])
ego<-enrichGO(gene = geneid_entrez2, OrgDb = 'org.Hs.eg.db', keytype = "ENTREZID", ont = "MF", pvalueCutoff = 0.01, pAdjustMethod = "BH", qvalueCutoff = 0.05, readable = T)
ekk<-enrichKEGG(gene = geneid_entrez2, organism = 'hsa', keyType = 'kegg', pvalueCutoff = 0.01, pAdjustMethod = "BH", qvalueCutoff = 0.05)
write.csv(x = summary(ekk), file = "test_enrichKEGG.csv", row.names = F)
write.csv(x = summary(ego), file = "test_enrichGO.csv", row.names = F)

barplot(ego, showCategory=15,title="EnrichmentGO") #条形图
dotplot(ego,title="EnrichmentGO_dot")


ggplot(data=ego, mapping = aes(x=Description, y=Count))+geom_bar(aes(fill=ego$type_GO),stat = "identity")+coord_flip()
##保留原始数据排序!!
ggplot(data=ego, mapping = aes(x=factor(Description, levels = Description), y=Count))+geom_bar(aes(fill=ego$type_GO),stat = "identity")+coord_flip()
