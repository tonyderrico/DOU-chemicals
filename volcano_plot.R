#volcano plots
library(dplyr)
library(ggplot2)
library(ggrepel)

ggplot(data = x4, aes(x = est, y = -log10(pvalue), 
label = chemicals)) + 
geom_point() + theme_minimal() + 
geom_text_repel( data = subset(x4, pvalue < 0.2), size = 4) + 
geom_hline(yintercept=-log10(0.05), col="red", linetype = "dashed") + scale_color_manual(values=c("black", "red")) + 
geom_vline(xintercept = 0, col="blue", linetype = "dashed") + scale_y_continuous(breaks = c(0,0.5,1, 1.3,1.5,3), 
limits = c(0,4)) + scale_x_continuous(breaks = c(-1,0,1,2,3), limits = c(-1,1), ) + theme(legend.position="none")


library(dplyr)
x <- imp.df.v2.3 %>% filter(cohort == "KANC")
View(imp.df.v2.3)
View(x)
mean(imp_df_v2_3_m_adj$hs_mbzp_madj_Log2, na.rm = TRUE)

#heatmaps
library("pheatmap")
names(df_11.01_1)
x=children_chemicals_log_adjusted[3:48]
x=df_11.01_1[27:69]
x=as.matrix(d)
x <- cor(x, use = "complete.obs")
names(children_chemicals_log_adjusted)

pheatmap(x, clustering_distance_cols = "correlation", )

x1 <- as.data.frame(x)
write.csv(x1,file='D:/PhD/JEM_Exposome/corr_matrix_m_30.03.csv')
           
library(corrplot)  
a <- corrplot(x)
a <- as.data.frame(a$corrPos)
a1 <- filter(a, corr > 0.6 & corr < 1)
a1 <- select(a1, xName, yName, corr)

corrplot(x, tl.cex = 1.1,tl.col = "black", col=colorRampPalette(c("blue","white","red"))(20))
                                                                
d <- x %>% filter(urb_degree_dic == 0)                                                       
d <- d[1:43]
length(d$DDE)

library(gridExtra)
pdf("data_output.pdf", height=11, width=8.5)
grid.table(a1)
dev.off()
