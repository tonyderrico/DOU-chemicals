#LR + LOGIT

#lin reg and confint
library(car) #confint + estimates

lapply(coexposures6[22:56], table)
names(coexposures6)

finale <- list()

for(i in 22:56){
  reg = glm(coexposures6$birth_weight ~ coexposures6[,i] + 
              coexposures6$parity + coexposures6$preg_smk +
              coexposures6$age_birth_m_y + coexposures6$abroad_mo +
              coexposures6$prepreg_weight + coexposures6$sex + 
              coexposures6$cohab_0)
  finale[[i]] <- Confint(reg)[2,]
  
}


y <- names(coexposures6[22:56])
y <- as.data.frame(y)
names(y)[1] <- "agent"

x <- unlist(finale)
x1 <- matrix(x, nrow=35, ncol=3, byrow = TRUE)
x2 <- as.data.frame(x1)
x2 <- cbind(x2, y$agent)
names(x2) <- c("est", "ci25", "ci95", "agent")

write.csv(x2, "D:/R/JEM/exwas/exwas 31.01/14.07_BW.csv")


#logit and conf int-------------------------------------------------------------------------------
library(car)
confidence_intv <- list()
names(coexposures6)
for(i in 22:56){
  reg = glm(coexposures6$ga_bj_dic ~ coexposures6[,i] + 
              coexposures6$parity + coexposures6$preg_smk +
              coexposures6$age_birth_m_y + coexposures6$abroad_mo +
              coexposures6$prepreg_weight + coexposures6$sex + 
              coexposures6$cohab_0, family = "binomial")
  sum_reg <- summary(reg)
  confidence_intv[[i]] <- exp(Confint(reg)[2,])
  
}

y <- names(coexposures6[22:56])
y <- as.data.frame(y)
names(y)[1] <- "agent"

x <- unlist(confidence_intv)
x1 <- matrix(x, nrow=35, ncol=3, byrow = TRUE)
x2 <- as.data.frame(x1)
x2 <- cbind(x2, y$agent)
names(x2) <- c("OR", "CI_25", "CI_95","agent")

write.csv(x2, "D:/R/JEM/exwas/exwas 31.01/14.07_preterm.csv")

#trials---------------------------------------------------------------

m <- glm(coexposures4$ga_bj_dic ~ coexposures4$chemioter_q + 
      coexposures4$parity + coexposures4$preg_smk +
      coexposures4$age_birth_m_y + coexposures4$abroad_mo +
      coexposures4$prepreg_weight + coexposures4$sex + 
      coexposures4$cohab_0, family = "binomial")
exp(Confint(m))

m <- glm(coexposures4$birth_weight ~ coexposures4$reduction_working_hours_3_months_q + 
           coexposures4$parity + coexposures4$preg_smk +
           coexposures4$age_birth_m_y + coexposures4$abroad_mo +
           coexposures4$prepreg_weight + coexposures4$sex + 
           coexposures4$cohab_0)
exp(Confint(m))

table(coexposures4$reduction_working_hours_3_months_q, coexposures4$birth_weight_dic)

m <- glm(jem_quest4$ga_bj_dic ~ jem_quest4$passive_smoking_WP + 
           jem_quest4$parity + jem_quest4$preg_smk +
           jem_quest4$age_birth_m_y + jem_quest4$abroad_mo +
           jem_quest4$prepreg_weight + jem_quest4$sex + 
           jem_quest4$cohab_0, family = "binomial")

exp(m$coefficients)
exp(confint(m))
table(jem_quest4$inks_q)
table(jem_quest8$inks_q)
table(coexposures5$noise_q)

lapply(coexposures6[22:55], table)

x <- ninfea_moccup_mar2021_v1 %>% select(id_child, q1_02__stress_attivita_tre_mesi,q1_04__inquinamento_acustico_lav)
coexposures6 <- merge(x, coexposures6, by = "id_child")
table(coexposures6$q1_04__inquinamento_acustico_lav)
coexposures6$q1_02__stress_attivita_tre_mesi[coexposures6$q1_02__stress_attivita_tre_mesi == 5] <- 1



m <- glm(coexposures6$adv_repr_out ~ coexposures6$oils_q + 
           coexposures6$parity + coexposures6$preg_smk +
           coexposures6$age_birth_m_y + coexposures6$abroad_mo +
           coexposures6$prepreg_weight + coexposures6$sex + 
           coexposures6$cohab_0 + coexposures6$adv_preg, family = "binomial")
exp(m$coefficients[2])
exp(confint(m))
summary(m)

#per tabella
library(dplyr)
library(car)
ftable(coexposures6$ga_bj_dic, coexposures6$prev_work_load)
lapply(coexposures6[22:56], ftable)
x <- coexposures6 %>% filter(sumscore == "0")
mean(x$birth_weight, na.rm = TRUE)
sd(x$birth_weight, na.rm = TRUE)

x <- coexposures6 %>% filter(formaline_q == 1)
ftable(x$mode_delivery)
View(table(x$q1_02__attivita_pre_gravidanza))

#loop for ftables
x <- list()
for(i in 22:56){
  x <- as.data.frame(ftable(coexposures6[i], coexposures6$mode_delivery))
  x[i] <- x
}

ftable(coexposures6$sumscore, coexposures6$mode_delivery)

summary(glm(coexposures6$mode_delivery ~ coexposures6$prev_rep_movements + 
      coexposures6$parity + coexposures6$preg_smk +
      coexposures6$age_birth_m_y + coexposures6$abroad_mo +
      coexposures6$prepreg_weight + coexposures6$sex + 
      coexposures6$cohab_0, family = "binomial"))

#parity stratificato
parity1$DUST
confidence_intv <- list()
names(parity1)
for(i in 2:32){
  reg = glm(parity1$ga_bj_dic ~ parity1[,i] + parity1$preg_smk +
              parity1$age_birth_m_y + parity1$abroad_mo +
              parity1$prepreg_weight  + 
              parity1$cohab_0, family = "binomial")
  sum_reg <- summary(reg)
  confidence_intv[[i]] <- exp(Confint(reg)[2,])
  
}

x <- unlist(confidence_intv)
x1 <- matrix(x, nrow=35, ncol=3, byrow = TRUE)
x2 <- as.data.frame(x1)
y <- names(parity1[22:56])
x2 <- cbind(y, x2)
names(x2) <- c("agent", "OR", "CI_25","CI_95")

write.csv(x2, "D:/R/JEM/exwas/exwas 31.01/11.07_aaro.csv")

library(lmtest)
coeftest(reg, vcov = vcovCL, cluster = ~id_mother)
reg <- glm(formula = ga_bj_dic ~ parity1$`REDUCTION WORKING HOURS` + preg_smk + 
             age_birth_m_y + abroad_mo + prepreg_weight + 
             cohab_0, family = "binomial", data = parity1)
summary(reg)

library(DALEX)
loss_accuracy(reg)
library(opera)
loss(reg)
