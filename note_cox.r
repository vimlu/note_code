mod1 <- coxph(y ~ prison + dose + clinic, data=addicts)
summary(mod1)->summ_mod1
summ_mod1$coefficients->hr_pval
summ_mod1$conf.int->hr_95CI
write.table(hr_95CI, file = "hr_95CI.tsv", quote = F, sep = "\t")
write.table(hr_pval, file = "hr_pval.tsv", quote = F, sep = "\t")

setwd("E:\\gene+\\Gene+\\server\\project\\ctDNA_module-development\\test_cox")
require(survival)
addicts <- read.table("E:\\gene+\\Gene+\\server\\project\\ctDNA_module-development\\test_cox\\ADDICTS.txt",T)
y <- Surv(addicts$survt,addicts$status==1)
kmfit2 <- survfit(y~clinic, data=addicts)
kmfit2
unlist(kmfit2)

pval_cand<-survdiff(y~addicts$clinic)  # 检验危险因素是否是生存时间的独立因素显著性
chisq_val<-unlist(pval_cand)$chisq
pval<-pchisq(chisq_val, 1, lower.tail=F)
