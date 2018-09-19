read.table("clipboard", header = T)->p2t

ggplot(data = p2t_v2, mapping = aes(x=reorder(pid, rep(1, length(pid)),sum), fill=type))+geom_bar()
## 为保证按想要堆叠顺序进行（默认按字母顺序堆叠，因此源数据type应改为only_Tissue，Plasma+Tisssue）
ggplot(data = p2t_v2, mapping = aes(x=reorder(pid, rep(1, length(pid)),sum), fill=type))
+geom_bar(color="black") ##设置柱形边框颜色
+guides(fill=guide_legend(title=NULL))
+theme(legend.position = "top", axis.text = element_text(angle = 90))
+xlab(label = "Patient_ID")
+ylab(label = "No. of mutations")
+scale_fill_manual(values=c("grey","black"))  ##更改填充颜色


ggplot(p2t, aes(x=reorder(pid, rep(1, length(pid)), sum), fill=Plasma...))
+geom_bar()
+guides(fill=guide_legend(title = NULL))
+theme(legend.position = "top", axis.text.x = element_text(angle = 90))  ##or lengend.position=c(0.15, 0.9)
+xlab("Patient ID")
+scale_fill_discrete(labels=c("only_Tissue", "Plasma+Tissue"))  ##设置图例的标签

ggsave("p2t_consistentcy.png", width = 6, height = 6, type="cairo", dpi = 600)



ggplot(data=ego, mapping = aes(x=Description, y=Count))+geom_bar(aes(fill=ego$type_GO),stat = "identity")+coord_flip()
##保留原始数据排序!!
ggplot(data=ego, mapping = aes(x=factor(Description, levels = Description), y=Count))+geom_bar(aes(fill=ego$type_GO),stat = "identity")+coord_flip()
ggplot(data=ego, mapping = aes(x=factor(Description, levels = rev(Description)), y=Count))+geom_bar(aes(fill=ego$type_GO),stat = "identity")+coord_flip()
## 坐标轴倒序
ggplot(data=kegg_han, mapping = aes(x=factor(Description,levels = rev(Description)), y=Count))+geom_bar(aes(fill=pvalue),stat = "identity")+coord_flip()
+xlab("")
+scale_fill_gradient(low = 'red', high = 'blue')  ##自定义颜色梯度
+theme_bw()  ##去除背景灰色

##计算两向量相关性，并绘相关性图
rel_PB_PVB<-read.table("clipboard", header = T)
rel_PB_PVB$gene->rel_PB_PVB_rowname
rel_PB_PVB[-1]->rel_PB_PVB_2
rownames(rel_PB_PVB_2)<-rel_PB_PVB_rowname
qqnorm(rel_PB_PVB_2$BOPB)
qqnorm(rel_PB_PVB_2$PVB)
cor(rel_PB_PVB_2,method = "pearson")  ##计算pearson相关系数，即R值，【用于展示R2,R平方】
cor.test(rel_PB_PVB_2$BOPB, rel_PB_PVB_2$PVB, alternative = "two.side", method = "pearson")  ##相关性检验
## 绘图
ggplot(data = rel_PB_PVB, mapping = aes(x=BOPB, y=PVB))
+geom_point(alpha=0.3,position="jitter")  ##对重叠点添加扰动
+stat_smooth(method = "lm")  ##添加拟合曲线，及95%置信区间
+geom_text(data = subset(rel_PB_PVB,BOPB>7 & PVB >7), mapping = aes(label=gene),size=3)  ##标注特定数值标签
+theme_bw()  ##去掉灰色背景
+xlab("BOPB prevalence (%)")
+ylab("PVB prevalence (%)")
