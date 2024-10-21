
#chemicals abbreviations
names(df_11.01_1)[27:69] <- c("DDE", "DDT", "HCB", "PCB-118", "PCB-138","PCB-153","PCB-170","PCB-180","sumPCBs","PFOA","PFNA","PFUNDA",
                              "PFHXS","PFOS", "As","Cd","Cu","Hg","Pb","MEP","MiBP","MnBP","MBzP","MEHP","MEHHP","MEOHP","MECPP","sumDEHP",
                              "oh-MiNP","oxoMiNP","MEPA","ETPA","PRPA","BPA","BUPA","OXBE","TCS","DMP","DMTP","DMDTP",
                              "DEP","DETP","COT")
level_order <- c("DDE", "DDT", "HCB", "PCB-118", "PCB-138","PCB-153","PCB-170","PCB-180","sumPCBs","PFOA","PFNA","PFUNDA",
                 "PFHXS","PFOS", "As","Cd","Cu","Hg","Pb","MEP","MiBP","MnBP","MBzP","MEHP","MEHHP","MEOHP","MECPP","sumDEHP",
                 "oh-MiNP","oxoMiNP","MEPA","ETPA","PRPA","BPA","BUPA","OXBE","TCS","DMP","DMTP","DMDTP",
                 "DEP","DETP","COT") 

#models TRIALs
m <- lmer(DDE~  urban_degree_dic+ h_parity +
               h_age + hs_bf1c + (1|cohort.y) , data = df_11.01_1) #intercept vary by cohort
m1 <- lmer(hs_pcb118_madj  ~ urban_degree_dic + (1|cohort.y), data = df_11.01_1) #random intercept and random slope
m2
m2 <- lm(hs_pcb118_madj  ~ urban_degree_dic + cohort.y, data = df_11.01_1)

#test for lmer
lrtest(m1,m2)
AIC(m1,m2)
lmerTest::ranova(m1)

summary(m1)
x <- ranef(m1) #intercepts and slopes coefficients per cohort

plot_model(x, type = "re", show.values = TRUE)
plot(m)
qqnorm(resid(m))
qqline(resid(m))


predict(m, re.form = NA)
summary(lmer(urban_degree ~ hs_dde_madj + (1 + hs_dde_madj |urban_degree), data = imp1_cexp))

fit <- with(dat2, lmer(hs_pcb153_cadj_Log2 ~ hs_green_dist_s_Sqrt + (1|cohort)))

#LMM - pregnancy with random intercept per cohort---------------------------------------------------------

summary()
names(df_11.01_2)
lmm_list <- list()

for(i in 27:65){
  reg = lmer(df_11.01_2[,i] ~ urban_degree_dic + (1|Cohort), data = df_11.01_2)
  sum_reg <- summary(reg)
  lmm_list[[i]] <- exp(sum_reg$coefficients[2,1])
  
}

x <- unlist(lmm_list)
x1 <- matrix(x, nrow=43, ncol=1, byrow = TRUE)
x2 <- as.data.frame(x1)
names(x2) <- c("est")
x3 <- round(x2, 3)

chemicals <- names(df_11.01_1[27:65])
x4 <- cbind(x3, chemicals)

write.csv(x4, "C:/Users/Tony/Desktop/R/Helix_DoU/estimates_LMM_preg_22.08_random_intercept.csv", row.names = F)

#Confidence interval pregnancy

names(df_11.01_2)
confidence_intervals_lmm <- list()

for(i in 27:65){
  reg = lmer(df_11.01_2[,i] ~ urban_degree_dic + (1|Cohort), data = df_11.01_2)
  confidence_intervals_lmm[[i]] <- exp(confint(reg)[4,])
}

x <- unlist(confidence_intervals_lmm[27:65])
x1 <- matrix(x, nrow=39, ncol=2, byrow = TRUE)
x2 <- as.data.frame(x1)
names(x2) <- c("ci 2.5", "ci 97.5")

write.csv(df, "C:/Users/Tony/Desktop/R/Helix_DoU/mothers_CI_preg_22.08.csv")

df <- cbind(x2,x4)

class(df$chemicals)
df$chemicals <- factor(chemicals, levels=unique(chemicals))
#LMM CHILDREN-------------------------------------------------------------------------------------

#chemicals renamed
chemicals <- names(df_11.01_2[27:65])
names(children_chemicals_log_adjusted)[2:44] <- c("DDE", "DDT", "HCB", "PBDE-47", "PBDE-153","PCB-118","PCB-138","PCB-153",
                                                  "PCB-170","PCB-180","sumPCBs","MEP","MiBP","MnBP","MbZP","MEHP", "MEHHP",
                                                  "MEOHP","MECPP","sumDEHP","oh-MiNP","oxoMiNP","MEPA","ETPA","PRPA","BPA",
                              "BUPA","OXBE","TCS","DMP","DMTP","DEP","DETP","PFUNDA","PFOA","PFOS","PFNA","PFHXS",
                              "Pb","Hg","Cu","Cd","As")

children_chemicals_log_adjusted <- children_chemicals_log_adjusted[, c("id","hs_prebf","DDE","DDT","HCB","PBDE-47","PBDE-153","PCB-118","PCB-138","PCB-153",
                                    "PCB-170","PCB-180","PFOA","PFNA","PFUNDA",
                                    "PFHXS","PFOS","As","Cd","Hg","Pb","MEP","MiBP","MnBP","MbZP","MEHP",
                                    "MEHHP","MEOHP","MECPP",
                                    "oh-MiNP","oxoMiNP","MEPA","ETPA","PRPA","BPA","BUPA","OXBE",
                                    "TCS","DMP","DMTP",
                                    "DEP","DETP","cohort.x","urban_degree","urb_degree_dic","urb_degree_cat",
                                    "hs_child_age_years","h_parity","h_race","hs_bf1c","sex")]

names(children_chemicals_log_adjusted)

lmm_c_list <- list()

for(i in 3:43){
  reg = lmer(children_chemicals_log_adjusted_2[,i] ~ urb_degree_dic + sex + (1|Cohort), data = children_chemicals_log_adjusted_2)
  sum_reg <- summary(reg)
  lmm_c_list[[i]] <- exp(sum_reg$coefficients[2,1])
  
}

x <- unlist(lmm_c_list)
x1 <- matrix(x, nrow=41, ncol=1, byrow = TRUE)
x2 <- as.data.frame(x1)
names(x2) <- c("est")
x3 <- round(x2, 3)

chemicals <- names(children_chemicals_log_adjusted_2[3:43])
x4 <- cbind(x3, chemicals)
x4$chemicals <- factor(x4$chemicals, levels=unique(x4$chemicals))

#differences between crude and adjusted
x <- llme_adj_c$est - x4$est
x <- as.data.frame(x)
x <- cbind(x,llme_adj_c$chemicals)
names(x)[1] <- "GMRdiff"

llme_adj_m$est - x4$est

write.csv(x, "C:/Users/Tony/Desktop/R/Helix_DoU/gmr_diff_c.csv", row.names = F)

#Confidence interval

names(children_chemicals_log_adjusted_2)
confidence_intervals_lmm_c <- list()

for(i in 3:43){
  reg = lmer(children_chemicals_log_adjusted_2[,i] ~ urb_degree_dic + sex + (1|Cohort), data = children_chemicals_log_adjusted_2)
  confidence_intervals_lmm_c[[i]] <- exp(confint(reg)[4,])
  
}

x <- unlist(confidence_intervals_lmm_c[3:43])
x1 <- matrix(x, nrow=41, ncol=2, byrow = TRUE)
x2 <- as.data.frame(x1)
names(x2) <- c("ci 2.5", "ci 97.5")

write.csv(df, "C:/Users/Tony/Desktop/R/Helix_DoU/lmm_children_22.08.csv")

df1 <- cbind(x4,x2)

df_11.01_1$urban_degree_dic
m <- plot(df_11.01_1$hs_pfoa_m ~ df_11.01_1$urban_degree_dic )


aaa <- children_chemicals_log_adjusted[,level_order]

level_order <- c("id","hs_prebf","DDE","DDT","HCB","PBDE-47","PBDE-153","PCB-118","PCB-138","PCB-153",
                 "PCB-170","PCB-180","sumPCBs","PFOA","PFNA","PFUNDA",
                 "PFHXS","PFOS","As","Cd","Cu","Hg","Pb","MEP","MiBP","MnBP","MbZP","MEHP",
                 "MEHHP","MEOHP","MECPP","sumDEHP",
                 "oh-MiNP","oxoMiNP","MEPA","ETPA","PRPA","BPA","BUPA","OXBE",
                 "TCS","DMP","DMTP",
                 "DEP","DETP","cohort.x","urban_degree","urb_degree_dic","urb_degree_cat",
                 "hs_child_age_years","h_parity","h_race","hs_bf1c")

#RANDOM SLOPE--------------------------------------------------------------------------------

#random slope - mothers confidence intervals

summary()
names(df_11.01_1)

confidence_intervals_lmm_rs <- list()

for(i in 27:69){
  reg = lmer(df_11.01_1[,i] ~ urban_degree_dic + (1|Cohort), data = df_11.01_1)
  confidence_intervals_lmm_rs[[i]] <- exp(confint(reg, method = "Wald")[4,])
}

x <- unlist(confidence_intervals_lmm[27:69])
x1 <- matrix(x, nrow=43, ncol=3, byrow = TRUE)
x2 <- as.data.frame(x1)
names(x2) <- c("estimate","ci 2.5", "ci 97.5")

write.csv(df, "C:/Users/Tony/Desktop/R/Helix_DoU/mothers_random_slope_03.07.csv")

#test with anova
library("VCA")
x <- anovaMM(DDE ~ urban_degree+ (cohort.y) +(cohort.y) ,df_11.01_1 )
varPlot(DDE ~ urban_degree+((cohort.y) +(cohort.y)) ,df_11.01_1) #plot variability
lmerSummary(reg)
remlVCA(reg)

reg1 = lmer(MiBP ~ urban_degree_dic + (urban_degree_dic|cohort.y),data = df_11.01_1)
reg = lmer(PFOA ~ urban_degree_dic + (1 + urban_degree_dic|Cohort),data = df_11.01_1)

reg2 = lmer(PFOA ~ urban_degree_dic + (1|Cohort), data = df_11.01_1)
summary(reg2)
anova(reg2,reg1, refit = FALSE)

coef(reg2)$cohort.y

#CI with bootstrap mothers

summary()
names(df_11.01_1)

confidence_intervals_lmm_rs <- list()

for(i in 27:69){
  reg = lmer(df_11.01_1[,i] ~ urban_degree_dic + (1|Cohort), data = df_11.01_1)
  confidence_intervals_lmm_rs[[i]] <- exp(confint.merMod(reg, method = "boot", level = 0.95, oldNames = FALSE)[4,])
}

x <- unlist(confidence_intervals_lmm[27:69])
x1 <- matrix(x, nrow=43, ncol=2, byrow = TRUE)
x2 <- as.data.frame(x1)
names(x2) <- c("ci 2.5", "ci 97.5")

write.csv(x2, "C:/Users/Tony/Desktop/R/Helix_DoU/bs_ci_mothers.csv")

#CI with bootstrap children

summary(children_chemicals_log_adjusted)
names(children_chemicals_log_adjusted)

confidence_intervals_lmm_bs_c <- list()

for(i in 3:45){
  reg = lmer(children_chemicals_log_adjusted[,i] ~ urb_degree_dic + sex + (1|cohort.x), 
             data = children_chemicals_log_adjusted)
  confidence_intervals_lmm_bs_c[[i]] <- exp(confint.merMod(reg, method = "boot", 
                                                           level = 0.95, oldNames = FALSE)[4,])
}

x <- unlist(confidence_intervals_lmm_bs_c[3:45])
x1 <- matrix(x, nrow=43, ncol=2, byrow = TRUE)
x2 <- as.data.frame(x1)
chemicals1 <- names(children_chemicals_log_adjusted[3:45])
x4 <- cbind(x2, chemicals1)
names(x4) <- c("ci_2.5_bs", "ci_97.5_bs", "chemicals")

write.csv(x4, "C:/Users/Tony/Desktop/R/Helix_DoU/bs_ci_children.csv")

#test
x <- lmer(children_chemicals_log_adjusted$DDE ~ urb_degree_dic + sex + (1|cohort.x), 
     data = children_chemicals_log_adjusted)
x1 <- exp(confint.merMod(x, method = "boot", level = 0.95, oldNames = FALSE))


#mothers with scores
m <- lmer(pers_pest1_pc_scores ~ urban_degree_dic + (1|Cohort), data = pest_pest1_nm)
m1 <- lmer(pfas1_pc_scores  ~ urban_degree_dic + (1|Cohort), data = pfas1_nm)
m2 <- lmer(phthalates1_pc_scores ~ urban_degree_dic + (1|Cohort), data = phthalates1_nm)
m3 <- lmer(phenols1_pc_scores ~ urban_degree_dic + (1|Cohort), data = phenols1_nm)
m4 <- lmer(op_pest1_pc_scores ~ urban_degree_dic + (1|Cohort), data = op_pest1_nm)

summary(m)
summary(m1)
summary(m2)
summary(m3)
summary(m4)

exp(m@beta)
exp(m1@beta)
exp(m2@beta)
exp(m3@beta)
exp(m4@beta)

confint(m)
confint(m1)
confint(m2)
confint(m3)
confint(m4)


confint.merMod(m, method = "boot", level = 0.95, oldNames = FALSE)
confint.merMod(m1, method = "boot", level = 0.95, oldNames = FALSE)
confint.merMod(m2, method = "boot", level = 0.95, oldNames = FALSE)
confint.merMod(m3, method = "boot", level = 0.95, oldNames = FALSE)
confint.merMod(m4, method = "boot", level = 0.95, oldNames = FALSE)

#children with scores
mc <- lmer(pers_pest1_pc_scores_c  ~ urb_degree_dic + sex + (1|Cohort), data = pest_pest1_nm_c)
m1c <- lmer(pfas_pc_scores_c ~ urb_degree_dic + sex + (1|Cohort), data = pfas_nm_c)
m2c <- lmer(phthalates_pc_scores_c ~ urb_degree_dic + sex + (1|Cohort), data = phthalates1_nm_c)
m3c <- lmer(phenols_pc_scores_c ~ urb_degree_dic + sex + (1|Cohort), data = phenols_nm_c)
m4c <- lmer(op_pest_pc_scores_c ~ urb_degree_dic + sex + (1|Cohort), data = op_pest1_nm_c)

exp(mc@beta)
exp(m1c@beta)
exp(m2c@beta)
exp(m3c@beta)
exp(m4c@beta)

confint(mc)
confint(m1c)
confint(m2c)
confint(m3c)
confint(m4c)

#----------------------------------------------------------------------------------------------
#LMM ADJUSTED mothers
names(df_11.01_1)
lmm_list <- list()

for(i in 27:64){
  reg = lmer(df_11.01_1[,i] ~ urban_degree_dic + h_edumc + h_age + h_parity + h_race + hs_prebf +
               (1|Cohort), data = df_11.01_1)
  sum_reg <- summary(reg)
  lmm_list[[i]] <- exp(sum_reg$coefficients[2,1])
  
}

x <- unlist(lmm_list)
x1 <- matrix(x, nrow=39, ncol=1, byrow = TRUE)
x2 <- as.data.frame(x1)
names(x2) <- c("est")
x3 <- round(x2, 3)

chemicals <- names(df_11.01_1[27:64])
x4 <- cbind(x3, chemicals)

write.csv(x4, "C:/Users/Tony/Desktop/R/Helix_DoU/estimates_LMM_preg_24.10_random_intercept_adj.csv", row.names = F)

#LMM ADJUSTED children
names(children_chemicals_log_adjusted)
lmm_c_list <- list()

for(i in 3:45){
  reg = lmer(children_chemicals_log_adjusted[,i] ~ urb_degree_dic + sex + h_edumc +
               h_race + h_parity + hs_child_age_years + hs_bf1c + (1|Cohort), data = children_chemicals_log_adjusted)
  sum_reg <- summary(reg)
  lmm_c_list[[i]] <- exp(sum_reg$coefficients[2,1])
  
}

x <- unlist(lmm_c_list)
x1 <- matrix(x, nrow=43, ncol=1, byrow = TRUE)
x2 <- as.data.frame(x1)
names(x2) <- c("est")
x3 <- round(x2, 3)
chemicals <- names(children_chemicals_log_adjusted[3:45])
x4 <- cbind(x3, chemicals)
write.csv(x4, "C:/Users/Tony/Desktop/R/Helix_DoU/lmm_children_24.10_adj.csv", row.names = F)

#CI children

confidence_intervals_lmm_c <- list()

for(i in 3:45){
  reg = lmer(children_chemicals_log_adjusted[,i] ~ urb_degree_dic + sex + h_edumc +
               h_race + h_parity + hs_child_age_years + hs_bf1c + (1|Cohort), data = children_chemicals_log_adjusted)
  confidence_intervals_lmm_c[[i]] <- exp(confint(reg)[4,])
  
}

x <- unlist(confidence_intervals_lmm_c[3:45])
x1 <- matrix(x, nrow=43, ncol=2, byrow = TRUE)g
x2 <- as.data.frame(x1)
names(x2) <- c("ci 2.5", "ci 97.5")

write.csv(x2, "C:/Users/Tony/Desktop/R/Helix_DoU/lmm_children_CI_24.10_adj.csv")
llme_adj_c <- cbind(x4,x2)
llme_adj_c <- cbind(llme_adj_c,group_chemicals1)

#CI mothers
names(df_11.01_1)
confidence_intervals_lmm_m <- list()

for(i in 27:69){
  reg = lmer(df_11.01_1[,i] ~ urban_degree_dic + h_edumc + h_age + h_parity + h_race + hs_prebf +
               (1|Cohort), data = df_11.01_1)
  confidence_intervals_lmm_m[[i]] <- exp(confint(reg)[4,])
  
}

x <- unlist(confidence_intervals_lmm_m[27:69])
x1 <- matrix(x, nrow=43, ncol=2, byrow = TRUE)
x2 <- as.data.frame(x1)
names(x2) <- c("ci 2.5", "ci 97.5")

chemicals <- names(df_11.01_1[27:69])
x4 <- cbind(x2, x4)
llme_adj_m <- x4
llme_adj_m <- llme_adj_m[-9,]
llme_adj_m <- cbind(llme_adj_m,group_chemicals)
write.csv(x4, "C:/Users/Tony/Desktop/R/Helix_DoU/CI_LMM_preg_24.10_adj.csv", row.names = F)

#intra class correlation------------------------------------------------------------------------------------------------------
library(psych)
library(sjstats)
library(car)
library(performance)

names(df_11.01_2)
perform <- list()
for(i in 27:65){
  reg = lmer(df_11.01_2[,i] ~ urban_degree_dic + (1|Cohort), data = df_11.01_2)
  a <- performance(reg)
  perform[[i]] <- a$ICC
}

x <- unlist(perform)
x1 <- matrix(x, nrow=38, ncol=1, byrow = TRUE)
x2 <- as.data.frame(x1)
names(x2) <- c("icc")
x3 <- round(x2, 3)
x3 <- cbind(x3, x6$chemicals )

write.csv(x3, "C:/Users/Tony/Desktop/R/Helix_DoU/preg_icc.csv", row.names = F)


for(i in 3:43){
  reg = lmer(children_chemicals_log_adjusted_2[,i] ~ urb_degree_dic + sex + (1|Cohort), data = children_chemicals_log_adjusted_2)
  a <- performance(reg)
  perform[[i]] <- a$ICC
}

x <- unlist(perform)
x1 <- matrix(x, nrow=41, ncol=1, byrow = TRUE)
x2 <- as.data.frame(x1)
names(x2) <- c("childhood_icc")
x3 <- round(x2, 3)
x4 <- cbind(x3, x5$chemicals)

write.csv(x4, "C:/Users/Tony/Desktop/R/Helix_DoU/childhood_icc.csv", row.names = F)


#extract SD of random effect------------------------------------------------------------------------------------------------
#mothers - x6 dataset
names(df_11.01_1)
sd_reg_list <- list()
names(df_11.01_2)


for(i in 27:65){
  reg = lmer(df_11.01_2[,i] ~ urban_degree_dic + (1|Cohort), data = df_11.01_2)
  sd_reg <- as.data.frame(VarCorr(reg))
  sd_reg_list[[i]] <- sd_reg$sdcor[1]
  }
x <- unlist(sd_reg_list)
x1 <- matrix(x, nrow=39, ncol=1, byrow = TRUE)
x2 <- as.data.frame(x1)
names(x2) <- c("sd_var")
x3 <- round(x2, 3)

chemicals <- names(df_11.01_2[27:65])
sd_dataset_c <- cbind(x3, chemicals)
sd_dataset_c$chemicals <- factor(sd_dataset_c$chemicals,levels=unique(sd_dataset_c$chemicals))
x6 <- cbind(group_chemicals,sd_dataset_c)
group_chemicals <- c("POPs","POPs","POPs","POPs","POPs","POPs","POPs","POPs","PFASs","PFASs","PFASs"
                     ,"PFASs","PFASs", "Metals", "Metals", "Metals", "Metals","Phthalates","Phthalates","Phthalates",
                     "Phthalates","Phthalates","Phthalates","Phthalates","Phthalates","Phthalates","Phthalates","Phthalates",
                     "Phenols","Phenols","Phenols","Phenols","Phenols","Phenols","Phenols", "OP Pesticides","OP Pesticides",
                     "OP Pesticides","OP Pesticides")
write.csv(x4, "C:/Users/Tony/Desktop/R/Helix_DoU/SD_RE_m.csv", row.names = F)
#extract confidence intervals of variance sd
for(i in 27:65){
  reg = lmer(df_11.01_2[,i] ~ urban_degree_dic + (1|Cohort), data = df_11.01_2)
  sd_reg <- as.data.frame(confint(reg))
  sd_reg_list[[i]] <- sd_reg[1,]
}

x <- unlist(sd_reg_list)
x1 <- matrix(x, nrow=39, ncol=2, byrow = TRUE)
x2 <- as.data.frame(x1)
names(x2) <- c("ci2.5","ci97.5")
x3 <- round(x2, 3)
x6 <- cbind(x6,x3)
#children - x5 dataset
names(children_chemicals_log_adjusted_2)
sd_reg_list <- list()

for(i in 3:43){
  reg = lmer(children_chemicals_log_adjusted_2[,i] ~ urb_degree_dic + sex + (1|Cohort), data = children_chemicals_log_adjusted_2)
  sd_reg <- as.data.frame(VarCorr(reg))
  sd_reg_list[[i]] <- sd_reg$sdcor[1]
  
}

x <- unlist(sd_reg_list)
x1 <- matrix(x, nrow=41, ncol=1, byrow = TRUE)
x2 <- as.data.frame(x1)
names(x2) <- c("sd_re")
x3 <- round(x2, 3)
chemicals <- names(children_chemicals_log_adjusted_2[3:43])
x4 <- cbind(x3, chemicals)
x4$chemicals <- factor(x4$chemicals,levels=unique(x4$chemicals))
group_chemicals1 <- c("POPs","POPs","POPs","POPs","POPs","POPs","POPs","POPs","POPs","POPs","PFASs","PFASs","PFASs"
                      ,"PFASs","PFASs", "Metals", "Metals", "Metals", "Metals","Phthalates","Phthalates","Phthalates",
                      "Phthalates","Phthalates","Phthalates","Phthalates","Phthalates","Phthalates","Phthalates","Phthalates",
                      "Phenols","Phenols","Phenols","Phenols","Phenols","Phenols","Phenols", "OP Pesticides","OP Pesticides",
                      "OP Pesticides","OP Pesticides")
x5 <- cbind(group_chemicals1,x4)

#confint variance
sd_reg_list <- list()

for(i in 3:43){
  reg = lmer(children_chemicals_log_adjusted_2[,i] ~ urb_degree_dic + sex + (1|Cohort), data = children_chemicals_log_adjusted_2)
  sd_reg <- as.data.frame(confint(reg))
  sd_reg_list[[i]] <- sd_reg[1,]
}

x <- unlist(sd_reg_list)
x1 <- matrix(x, nrow=41, ncol=2, byrow = TRUE)
x2 <- as.data.frame(x1)
names(x2) <- c("ci2.5","ci97.5")
x3 <- round(x2, 3)
x5 <- cbind(x5, x3)

#confronto randm intercept vs random slope

x = lmer(BUPA ~ urban_degree_dic + (1|Cohort),data = df_11.01_2)
x1 = lmer(BUPA ~ urban_degree_dic + (1 + urban_degree_dic|Cohort),data = df_11.01_2)
performance(x)
library(performance)
library(sjPlot)
sjp.l(x, type = "rs.ri")
anova(x,x1)
coef(x1)
tab_model(x1)
a <- plot_model(x, type="diag", show.values = T)
a[[2]]$Cohort
plot_model(x1,type="pred", terms=c("urban_degree_dic","Cohort"),pred.type = "re")
ggpredict(x1, terms = c("urban_degree_dic","Cohort"), type="re") %>% plot()
coef(x1)
