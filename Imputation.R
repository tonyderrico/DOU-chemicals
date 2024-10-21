#IMPUTED DATASET

library(foreign)
library(mice)
library(dplyr)
library(FSA)
library(ggplot2)

write.dta(imp_df_v2_3_mod, "D:/PhD/HELIX/datasets/imp.df.v2.3_mod.dta")

#first dataset imputated

imp_df_v2_3_m_adj_1 <- imp_df_v2_3_m_adj %>% filter(`_imp` == 1)
names(id_urbd_cohort)[names(id_urbd_cohort) == "id"] <- "HelixID"
      
imp_df_v2_3_m_adj_1 <- merge(imp_df_v2_3_m_adj_1, id_urbd_cohort, by = "HelixID" )
a <- names(imp_df_v2_3_m_adj_1)[5:52]

#nobservations completed
names(imp_df_v2_3_m_adj_1)
x <- imp_df_v2_3_m_adj_1 %>% filter(urban_degree == 1) 
x <- x[5:52]
x1 <- colSums(is.na(x))
x1 <- as.data.frame(x1) - 233
x1 <- abs(x1)
nobs_rural_imp_1 <- x1$x1 

x <- imp_df_v2_3_m_adj_1 %>% filter(urban_degree == 2) 
x <- x[5:52]
x1 <- colSums(is.na(x))
x1 <- as.data.frame(x1) - 74
x1 <- abs(x1)
nobs_periurb_imp_1 <- x1$x1 

x <- imp_df_v2_3_m_adj_1 %>% filter(urban_degree == 3) 
x <- x[5:52]
x1 <- colSums(is.na(x))
x1 <- as.data.frame(x1) - 714
x1 <- abs(x1)
nobs_urb_imp_1 <- x1$x1 

#geometric sd

gsd_per_dou_m_adj_imp_1 <- aggregate(x=imp_df_v2_3_m_adj_1[,5:52]^2,
                               by=list(imp_df_v2_3_m_adj_1$urban_degree),
                               FUN = geosd, na.rm = TRUE)

gsd_per_dou_m_adj_imp_1 <- t(gsd_per_dou_m_adj_imp_1[,-1])
gsd_per_dou_m_adj_imp_1 <- as.data.frame(gsd_per_dou_m_adj_imp_1)
gsd_per_dou_m_adj_imp_1 <- cbind(rownames(gsd_per_dou_m_adj_imp_1), gsd_per_dou_m_adj_imp_1)
rownames(gsd_per_dou_m_adj_imp_1) <- NULL
names(gsd_per_dou_m_adj_imp_1) <- c("chemicals", "gsd_1", "gsd_2", "gsd_3")

#geometric mean
gm_per_dou_m_adj_imp_1 <- aggregate(x=imp_df_v2_3_m_adj_1[,5:52]^2,
                              by=list(imp_df_v2_3_m_adj_1$urban_degree),
                              FUN = geomean, na.rm = TRUE)

gm_per_dou_m_adj_imp_1 <- t(gm_per_dou_m_adj_imp_1[,-1])
gm_per_dou_m_adj_imp_1 <- as.data.frame(gm_per_dou_m_adj_imp_1)
gm_per_dou_m_adj_imp_1 <- cbind(rownames(gm_per_dou_m_adj_imp_1), gm_per_dou_m_adj_imp_1)
rownames(gm_per_dou_m_adj_imp_1) <- NULL
names(gm_per_dou_m_adj_imp_1) <- c("chemicals", "gm_1", "gm_2", "gm_3")


#analysis

x <- metacont(n.e = nobs_rural_imp_1,
              mean.e = gm_per_dou_m_adj_imp_1$gm_1,
              sd.e = gsd_per_dou_m_adj_imp_1$gsd_1,
              n.c = nobs_urb_imp_1,
              mean.c = gm_per_dou_m_adj_imp_1$gm_3,
              sd.c = gsd_per_dou_m_adj_imp_1$gsd_3,
              sm = "ROM",
              backtransf = TRUE)

s <- summary.meta(x)

out <- tibble(
  rom = exp(x$TE), 
  low_ci = exp(x$lower), 
  upp_ci = exp(x$upper))

str(s)

out <- as.data.frame(out)
out <- out[-c(41),]


#create forest plot
ggplot(data=out, aes(y=a[-c(41),], x=rom, xmin=low_ci, xmax=upp_ci)) +
  geom_point() + 
  geom_errorbarh(height=0) +
  labs(title='Pregnancy - Rural vs Urban', x='GMR (95% C.I.)', y = 'Environmental Contaminants') +
  geom_vline(xintercept=1, color='black', linetype='dashed', alpha=.5) +
  theme_classic()
