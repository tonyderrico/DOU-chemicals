#PCA
library(dplyr)
library("factoextra")
library(FactoMineR)
library(corrr)
library(ggcorrplot)
library(forcats)
library(MASS) 
library(reshape2) 


#MOTHERS-------------------------------------
#PCA TOTALE
names(df_11.01_1)
df_03.04.23 <- df_11.01_1[c(1,26:70)]
df_03.04.23_norm <- scale(df_03.04.23[27:69])
corr_m <- cor(df_03.04.23_norm, use = "complete.obs")
ggcorrplot(corr_m)

data.pca <- princomp(na.omit(df_03.04.23), cor = TRUE)
summary(data.pca)
data.pca$loadings
pca.loading <- as.data.frame(data.pca$loadings[,1:11])
pca.loading <- cbind(rownames(pca.loading), pca.loading)
rownames(pca.loading) <- NULL
colnames(pca.loading) <- c("chemicals","PC1", "PC2", "PC3", "PC4","PC5", "PC6",
                        "PC7", "PC8", "PC9", "PC10","PC11")
loadings.pca <- melt(pca.loading, id="chemicals", 
                    measure=c("PC1", "PC2", "PC3", "PC4","PC5", "PC6",
                              "PC7", "PC8", "PC9", "PC10", "PC11"), 
                    variable.name="PC", value.name="Loading") 

ggplot(loadings.pca, aes(fct_rev(chemicals), abs(Loading), fill=Loading)) +
  facet_wrap(~ PC, nrow=1) + #place the factors in separate facets
  geom_bar(stat="identity") + #make the bars
  coord_flip() + #flip the axes so the test names can be horizontal  
  #define the fill color gradient: blue=positive, red=negative
  scale_fill_gradient2(name = "Loading", 
                       high = "red", mid = "white", low = "blue", 
                       midpoint=0, guide=F) +
  xlab("Chemicals") +
  ylab("Loading Strength") + #improve y-axis label
  theme_bw(base_size=10) #use a black-and0white theme with set font size


fviz_eig(data.pca, addlabels = TRUE)
fviz_pca_var(data.pca)

#corr per cohort

a <- cor(rhea_m[27:69], use = "complete.obs")
b <- cor(kanc_m[27:36], use = "complete.obs")
c <- cor(bib_m[27:69], use = "complete.obs")
d <- cor(eden_m[27:64], use = "complete.obs")
e <- cor(inma_m[27:56], use = "complete.obs" )

ggcorrplot(a)
ggcorrplot(b)
ggcorrplot(c)
ggcorrplot(d)
ggcorrplot(e)

#dataset + PCA per family - mothers

names(df_03.04.23)
summary(df_03.04.23)
pers_pest <- df_03.04.23[c(1:10)]
pers_pest1 <- pers_pest %>% filter(! Cohort %in% c("KANC")) #no DDT, PCB118, pcb170
pest_pest1_nm <- na.omit(pers_pest1) #compl case
pfas <- df_03.04.23[c(1:2,12:16)]
pfas1$PFUNDA <- NULL #no pfunda
pfas1_nm <- na.omit(pfas1) #compl case
phthalates <- df_03.04.23[c(1:2,22:32)] 
phthalates1 <- phthalates %>% filter(! Cohort %in% c("KANC")) #no sumdehp, mecpp, oxominp, ohminp
phthalates1_nm <- na.omit(phthalates1) #compl case
phenols <- df_03.04.23[c(1:2,33:39)]
phenols1 <- phenols %>% filter(! Cohort %in% c("KANC"))
phenols1_nm <- na.omit(phenols1)
op_pest <- df_03.04.23[c(1:2,40:44)]
op_pest1 <- op_pest %>% filter(! Cohort %in% c("KANC"))
op_pest1_nm <- na.omit(op_pest1) #compl case

#correlations
corr_pers_pest <- cor(pers_pest, use = "complete.obs")
corr_pfas <- cor(pfas, use = "complete.obs")
corr_phthalates <- cor(phthalates, use = "complete.obs")
corr_phenols <- cor(phenols, use = "complete.obs")
corr_op_pest <- cor(op_pest, use = "complete.obs")

x <- prcomp(na.exclude(op_pest), scale = TRUE)
x$rotation
summary(x)
#missing
View(pers_pest[!complete.cases(pers_pest), ])
library("naniar")
vis_miss(pers_pest)
gg_miss_var(pers_pest[2:10],Cohort,show_pct = TRUE)
gg_miss_var(pfas[2:6],Cohort,show_pct = TRUE)
gg_miss_var(phthalates[2:13],Cohort, show_pct = TRUE)
gg_miss_var(phenols[2:9],Cohort, show_pct = TRUE)
gg_miss_var(op_pest[2:7],Cohort, show_pct = TRUE)

#scores in dataset mothers
pers_pest1_pc <- princomp(pest_pest1_nm[3:7] , scores = TRUE)
summary(pers_pest1_pc)
pers_pest1_pc_scores <- pers_pest1_pc$scores[,1]
length(pers_pest1_pc_scores)
pest_pest1_nm <- cbind(pers_pest1_pc_scores, pest_pest1_nm)
x <- df_11.01_1 %>% dplyr::select("id", "urban_degree_dic")
pest_pest1_nm <- merge(x,pest_pest1_nm, by = "id" )
table(pest_pest1_nm$urban_degree_dic)
summary(pers_pest1_pc$loadings)

pfas1_pc <- princomp(pfas1_nm[3:6] , scores = TRUE)
summary(pfas1_pc)
pfas1_pc_scores <- pfas1_pc$scores[,1]
length(pfas1_nm[,1])
pfas1_nm <- cbind(pfas1_pc_scores, pfas1_nm)
pfas1_nm <- merge(x,pfas1_nm, by = "id")
table(pfas1_nm$urban_degree_dic)
summary(pfas1_pc$loadings)

phthalates1_pc <- princomp(phthalates1_nm[3:9], scores = TRUE)
summary(phthalates1_pc)
phthalates1_pc_scores <- phthalates1_pc$scores[,1]
length(phthalates1_pc_scores)
phthalates1_nm <- cbind(phthalates1_pc_scores,phthalates1_nm)
phthalates1_nm <- merge(x,phthalates1_nm, by = "id")
table(phthalates1_nm$urban_degree_dic)
summary(phthalates1_pc$loadings)

phenols1_pc <- princomp(phenols1_nm[3:9] , scores = TRUE)
summary(phenols1_pc)
phenols1_pc_scores <- phenols1_pc$scores[,1]
length(phenols1_pc_scores)
phenols1_nm <- cbind(phenols1_pc_scores,phenols1_nm)
phenols1_nm <- merge(x,phenols1_nm, by = "id" )
table(phenols1_nm$urban_degree_dic)
summary(phenols1_pc$loadings)

op_pest1_pc <- princomp(op_pest1_nm[3:7] , scores = TRUE)
summary(op_pest1_pc)
op_pest1_pc_scores <- op_pest1_pc$scores[,1]
length(op_pest1_pc_scores) 
op_pest1_nm <- cbind(op_pest1_pc_scores,op_pest1_nm)
op_pest1_nm <- merge(x, op_pest1_nm, by = "id")
table(op_pest1_nm$urban_degree_dic)
summary(op_pest1_pc$loadings)

#---- CHILDREN
names(children_chemicals_log_adjusted)
summary(children_chemicals_log_adjusted)

pers_pest_c <- children_chemicals_log_adjusted[c(1,3:12)]
pest_pest1_nm_c <- na.omit(pers_pest_c) #compl case

pfas_c <- children_chemicals_log_adjusted[c(1,14:18)]
pfas_nm_c <- na.omit(pfas_c) #compl case

phthalates_c <- children_chemicals_log_adjusted[c(1,24:34)] 
phthalates1_nm_c <- na.omit(phthalates_c) #compl case

phenols_c <- children_chemicals_log_adjusted[c(1,35:41)]
phenols_nm_c <- na.omit(phenols_c)

op_pest_c <- children_chemicals_log_adjusted[c(1,42:45)]
op_pest1_nm_c <- na.omit(op_pest_c) #compl case

#pca scores children
x <- children_chemicals_log_adjusted %>% dplyr::select("id", "urb_degree_dic", "Cohort", "sex")
x1 <- children_chemicals_log_adjusted %>% dplyr::select("id", "sex")

pers_pest_pc_c <- princomp(pest_pest1_nm_c[4:13], scores = TRUE)
pers_pest1_pc_scores_c <- pers_pest_pc_c$scores[,1]
length(pers_pest1_pc_scores_c)
pest_pest1_nm_c <- cbind(pers_pest1_pc_scores_c, pest_pest1_nm_c)
x <- children_chemicals_log_adjusted %>% dplyr::select("id", "urb_degree_dic", "Cohort")
pest_pest1_nm_c <- merge(x,pest_pest1_nm_c, by = "id" )
pest_pest1_nm_c <- merge(x1,pest_pest1_nm_c, by = "id" )
table(pest_pest1_nm_c$urb_degree_dic)
summary(pers_pest_pc_c)

pfas_pc_c <- princomp(pfas_nm_c[2:6] , scores = TRUE)
summary(pfas_pc_c)
pfas_pc_scores_c <- pfas_pc_c$scores[,1]
length(pfas_pc_scores_c)
pfas_nm_c <- cbind(pfas_pc_scores_c, pfas_nm_c)
pfas_nm_c <- merge(x1,pfas_nm_c, by = "id")
table(pfas_nm_c$urb_degree_dic)
summary(pfas_pc_c$loadings)

phthalates_pc_c <- princomp(phthalates1_nm_c[4:14], scores = TRUE)
summary(phthalates_pc_c)
phthalates_pc_scores_c <- phthalates_pc_c$scores[,1]
length(phthalates_pc_scores_c)
phthalates1_nm_c <- cbind(phthalates_pc_scores_c,phthalates1_nm_c)
phthalates1_nm_c <- merge(x1,phthalates1_nm_c, by = "id")
table(phthalates1_nm_c$urb_degree_dic)
summary(phthalates_pc_c$loadings)

phenols_pc_c <- princomp(phenols_nm_c[2:8] , scores = TRUE)
summary(phenols_pc_c)
phenols_pc_scores_c <- phenols_pc_c$scores[,1]
length(phenols_pc_scores_c)
phenols_nm_c <- cbind(phenols_pc_scores_c,phenols_nm_c)
phenols_nm_c <- merge(x1,phenols_nm_c, by = "id" )
table(phenols_nm_c$urb_degree_dic)
summary(phenols_pc_c$loadings)

op_pest_pc_c <- princomp(op_pest1_nm_c[2:5] , scores = TRUE)
summary(op_pest_pc_c)
op_pest_pc_scores_c <- op_pest_pc_c$scores[,1]
length(op_pest_pc_scores_c) 
op_pest1_nm_c <- cbind(op_pest_pc_scores_c,op_pest1_nm_c)
op_pest1_nm_c <- merge(x1, op_pest1_nm_c, by = "id")
table(op_pest1_nm_c$urb_degree_dic)
summary(op_pest_pc_c$loadings)
