#MA

library(psych)
library(tidyverse)
library(desc)
library(metafor)
library(dmetar)
library(meta)
library(ggplot2)
library(gridExtra)
library(FSA)

#total summary MA adjusted by cohort

#dataframes need gm, gsd, n of obs

#rural n of obs
View(h1_bio_dou_m_urbdegree_cat_adj_metals_c)
x <- h1_bio_dou_m_urbdegree_cat_adj_metals_c %>% filter(urban_degree == 1) 
x <- x[4:48]
x1 <- colSums(is.na(x))
x1 <- as.data.frame(x1) - 233
x1 <- abs(x1)
nobs_rural_m_1 <- x1$x1

#suburban n of obs
x <- h1_bio_dou_m_urbdegree_cat_adj_metals_c %>% filter(urban_degree == 2) 
x <- x[4:48]
x2 <- colSums(is.na(x))
x2 <- as.data.frame(x2) - 74
x2 <- abs(x2)
nobs_rural_m_2 <- x2$x2
#urban n of obs
x <- h1_bio_dou_m_urbdegree_cat_adj_metals_c %>% filter(urban_degree == 3) 
x <- x[4:48]
x3 <- colSums(is.na(x))
x3 <- as.data.frame(x3) - 804
x3 <- abs(x3)
nobs_rural_m_3 <- x3$x3

#geometric sd

gsd_per_dou_m_adj <- aggregate(x=h1_bio_dou_m_urbdegree_cat_adj_metals_c[,4:48],
                          by=list(h1_bio_dou_m_urbdegree_cat_adj_metals_c$urban_degree),
                          FUN = geosd, na.rm = TRUE)

gsd_per_dou_m_adj <- t(gsd_per_dou_m_adj[,-1])
gsd_per_dou_m_adj <- as.data.frame(gsd_per_dou_m_adj)
gsd_per_dou_m_adj <- cbind(rownames(gsd_per_dou_m_adj), gsd_per_dou_m_adj)
rownames(gsd_per_dou_m_adj) <- NULL
names(gsd_per_dou_m_adj) <- c("chemicals", "gsd_1", "gsd_2", "gsd_3")
gsd_per_dou_m_adj[2:4] <- round(gsd_per_dou_m_adj[2:4],2)

library(gridExtra)
pdf("gm_sd.pdf", height=17, width=8.5)
grid.table(gsd_per_dou_m_adj)
dev.off()
library("flextable")
flextable(gsd_per_dou_m_adj)

#geometric mean
gm_per_dou_m_adj <- aggregate(x=h1_bio_dou_m_urbdegree_cat_adj_metals_c[,4:48],
                          by=list(h1_bio_dou_m_urbdegree_cat_adj_metals_c$urban_degree),
                          FUN = geometric.mean, na.rm = TRUE)

gm_per_dou_m_adj <- t(gm_per_dou_m_adj[,-1])
gm_per_dou_m_adj <- as.data.frame(gm_per_dou_m_adj)
gm_per_dou_m_adj <- cbind(rownames(gm_per_dou_m_adj), gm_per_dou_m_adj)
rownames(gm_per_dou_m_adj) <- NULL
names(gm_per_dou_m_adj) <- c("chemicals", "gm_1", "gm_2", "gm_3")


#missing
missing_adj <- colSums(is.na(h1_bio_dou_m_urbdegree_cat_adj_metals_c[4:48]))

n_obs_adj <- 1021 - missing_adj
n_obs_adj <- as.data.frame(n_obs_adj)
n_obs_adj <- cbind(rownames(n_obs_adj), n_obs_adj)
rownames(n_obs_adj) <- NULL
names(n_obs_adj) <- c("chemicals", "n_obs_adj")

#merge dataset

x <- merge(n_obs_adj, gm_per_dou_m_adj, by = "chemicals")
merged_sum_adj <- merge(x, gsd_per_dou_m_adj, by = "chemicals")
merged_sum_adj <- merged_sum_adj[-c(30),]


#analysis

x <- metacont(n.e = nobs_rural_m_2,
        mean.e = merged_sum_adj$gm_2,
        sd.e = merged_sum_adj$gsd_2,
        n.c = nobs_rural_m_3,
        mean.c = merged_sum_adj$gm_3,
        sd.c = merged_sum_adj$gsd_3,
        sm = "ROM",
        backtransf = TRUE)

s <- summary.meta(x)

out <- tibble(
    rom = exp(x$TE), 
    low_ci = exp(x$lower), 
    upp_ci = exp(x$upper))
  )
  
  str(s)

out <- as.data.frame(out)
out <- out[-c(30),]

#forest plots
sink("output.csv")
sink(NULL)
print(summary(x))



#create forest plot
ggplot(data=out, aes(y=merged_sum_adj$chemicals, x=rom, xmin=low_ci, xmax=upp_ci)) +
  geom_point() + 
  geom_errorbarh(height=0) +
  labs(title='Pregnancy - Periurban vs Urban', x='GMR (95% C.I.)', y = 'Environmental Contaminants') +
  geom_vline(xintercept=1, color='black', linetype='dashed', alpha=.5) +
  theme_classic()

