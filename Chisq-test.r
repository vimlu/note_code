##==========to perform chisq.test in batch============##
## examples of input data by clipboard
# 基因	mut	wt	mut_AD	wt_AD
# EGFR	15	7	79	54
# TP53	9	13	93	40

read.table('clipboard',header = T)->test
test$Gene->rownames(test)
test[,-1]->test
as.matrix(test)->test
for(i in 1:length(test[,1])){
  tmp_matrix<-matrix(test[i,],ncol = 2,byrow = T)
  chisq.test(tmp_matrix)$p.value->pval[i]
  # cat(pval[i])
}
names(pval)<-rownames(test)
pval[pval<0.05]->pval_cand
