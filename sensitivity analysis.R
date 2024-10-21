#sensitivity analysis
a <- df_11.01_2 %>% filter(Cohort != "SAB")

lmm_list <- list()
for(i in 27:65){
  reg = lmer(a[,i] ~ urban_degree_dic + h_edumc + h_age +h_parity + h_race +
               (1|Cohort), data = a)
  sum_reg <- summary(reg)
  lmm_list[[i]] <- exp(sum_reg$coefficients[2,1])
  
}

x <- unlist(lmm_list[27:65])
x1 <- matrix(x, nrow=39, ncol=1, byrow = TRUE)
x3 <- as.data.frame(x1)
names(x3) <- c("est")
x2$chemicals <- names(a)[27:65]
x2$chemicals_adj <- x3
a$Cohort
x2$ratio <- x2$chemicals_adj/x2$est
names(a)
