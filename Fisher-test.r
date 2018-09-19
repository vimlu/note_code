##==========to perform chisq.test in batch============##
## examples of input data by clipboard
# 基因	mut	wt	mut_AD	wt_AD
# EGFR	15	7	79	54
# TP53	9	13	93	40

read.table("clipboard", header = T) -> test4fisher

sink(file = "res_fisher_EGFR-RET_pathway.tsv")
print(paste("gene", "pval", "OR"), sep = " ")
for (i in 1:length(test4fisher[, 1])) {
  test4fisher[i, 1] -> geneid
  test4fisher[i, 2] -> mut
  test4fisher[i, 3] -> wt
  test4fisher[i, 4] -> mut_AD
  test4fisher[i, 5] -> wt_AD
  fisher.test(matrix(c(mut, wt, mut_AD, wt_AD), nrow = 2)) -> res_fisher
  ## print(paste(c(geneid,res_fisher$p,res_fisher$estimate), sep="\t"),digits=4, quote=F, append=T)
  
  print(paste(geneid, res_fisher$p, res_fisher$estimate, sep = " "),
        quote = F)
}
sink()

##for scatter plot of significant geneS
read.table("clipboard", header = T) -> test4dotplot
## plot all points with text
plot(
  x = log2(test4dotplot$OR),
  y = -log2(test4dotplot$pval),
  xlab = "Log2(Odds Ratio(OR))",
  ylab = "-Log(p value)"
)
points(
  x = log2(test4dotplot$OR),
  y = -log2(test4dotplot$pval),
  cex = 1,
  pch = 16,
  col = "red"
)
text(
  x = log2(test4dotplot$OR),
  y = -log2(test4dotplot$pval),
  test4dotplot$gene,
  cex = 1,
  pos = 1,
  col = "red"
)
abline(v = -log2(1),
       h = -log2(0.05),
       lty = 2)
## select to plot text of points
plot(
  x = log2(test4dotplot$OR),
  y = -log2(test4dotplot$pval),
  xlab = "Log2(Odds Ratio(OR))",
  ylab = "-Log(p value)"
)
points(
  x = log2(test4dotplot$OR[test4dotplot$pval < 0.05]),
  y = -log2(test4dotplot$pval[test4dotplot$pval < 0.05]),
  cex = 1,
  pch = 16,
  col = "red"
)
text(
  x = log2(test4dotplot$OR[test4dotplot$pval < 0.1]),
  y = -log2(test4dotplot$pval[test4dotplot$pval < 0.1]),
  test4dotplot$gene[test4dotplot$pval < 0.1],
  cex = 1,
  pos = 1,
  col = "red"
)
abline(v = -log2(1),
       h = -log2(0.05),
       lty = 2)
