#DATASETS

library(dplyr)
library(psych)
library(foreign)
library(FSA)

biomarker_child_database_version_20170719 #original
names(biomarker_child_database_version_20170719)[2] <- "id"

biomarker_children_c <- biomarker_mother_children[c(1:181)] #1293 children after the merge

biomarker_child_database_version_20170719_c #concentrations normal and adjusted
biomarkers_child_conc #cleaned for just normal concentrations

biomarker_child_database_version_dou <- merge(biomarker_children_c,
                                                       dou_all_cohorts, by = "id") #main dataset + degree of urbanization
biomarker_child_database_version_dou[1795:1797,] #with H1, S1, H2

#degree of urbanization + biomarkers

h1_bio_dou_c <- merge(dou_all_cohorts_H1, biomarker_children_c, by = "id") #merged h1 and only biomarkers conc
s1_bio_dou_c <- merge(dou_all_cohorts_S1, biomarker_children_c, by = "id") #merged s1 and only biomarkers conc

names(h1_bio_dou_c)


#ANALYSIS
#-------------------------------------------------------------------------------------------------------------------------------------

#summary of chemicals conc 
sum_h1_bio_dou <- describeBy(h1_bio_dou, group = h1_bio_dou$urban_degree, fast = TRUE)
sum_s1_bio_dou <- describeBy(s1_bio_dou, group = s1_bio_dou$urban_degree, fast = TRUE)
sum_bio_dou_cohort <- describeBy(h1_bio_dou, group = h1_bio_dou$cohort, fast = TRUE)

capture.output(print(sum_bib), file="sum_bib.csv")
capture.output(print(sum_sbd), file="sum_sbd.csv")
capture.output(print(sum_kanc), file="sum_kanc.csv")
capture.output(print(sum_eden), file="sum_eden.csv")
capture.output(print(sum_rhea), file="sum_rhea.csv")


write.dta(bib, "C:/Users/Tony/Desktop/R/Helix_DoU/bib.dta")
write.csv(imp1,"C:/Users/Tony/Desktop/R/Helix_DoU/imp1.csv")

#descrittive

table(h1_bio_dou_c$cohort.x, h1_bio_dou_c$urban_degree)
children_chemicals_adjusted <- h1_bio_dou_c[,grepl("adj", names(h1_bio_dou_c))]
x <- h1_bio_dou_c %>% dplyr::select(hs_pfunda_c,hs_pfoa_c,hs_pfos_c,hs_pfna_c,hs_pfhxs_c,hs_pb_c,hs_hg_c,hs_cu_c,hs_cd_c,hs_as_c,id,
                               cohort.x, urban_degree)
children_chemicals_adjusted <- data.frame(children_chemicals_adjusted, x)
names(children_chemicals_log_adjusted)
children_chemicals_log_adjusted[1:43] <- log(children_chemicals_adjusted[1:43]) 
children_chemicals_log_adjusted$urb_degree_cat <- NA
children_chemicals_log_adjusted$urb_degree_dic[children_chemicals_log_adjusted$urban_degree == 30] <- 3
children_chemicals_log_adjusted$urb_degree_dic[children_chemicals_log_adjusted$urban_degree == 23] <- 1
children_chemicals_log_adjusted$urb_degree_dic[children_chemicals_log_adjusted$urban_degree == 22] <- 1
children_chemicals_log_adjusted$urb_degree_dic[children_chemicals_log_adjusted$urban_degree == 21] <- 1
children_chemicals_log_adjusted$urb_degree_dic[children_chemicals_log_adjusted$urban_degree == 13] <- 1
children_chemicals_log_adjusted$urb_degree_dic[children_chemicals_log_adjusted$urban_degree == 12] <- 1
children_chemicals_log_adjusted$urb_degree_dic[children_chemicals_log_adjusted$urban_degree == 11] <- 1
children_chemicals_log_adjusted$urb_degree_dic[children_chemicals_log_adjusted$urban_degree == 10] <- 1

#see the means per cohort and degree of urb
x <- children_chemicals_log_adjusted %>% select(cohort.x, DETP, id, urb_degree_dic)
str(x)

x %>% group_by(cohort.x, urb_degree_dic) %>% summarise(mean=exp(mean(DETP, na.rm = TRUE)))

#covariates  children
covariates_children <- ap110_request_updated_02may_2022 %>% dplyr::select(id,hs_child_age_years,h_parity,h_race)
                                     
names(breadfeeeding_infoquest_10jun_2022)
breastfeeding_1c <- breadfeeeding_infoquest_10jun_2022 %>% dplyr::select(id,hs_bf1c)

covariates_children <- merge(covariates_children,breastfeeding_1c, by = "id" )
names(covariates_children)

children_chemicals_log_adjusted <- merge(children_chemicals_log_adjusted, x, by = "id")
names(children_chemicals_log_adjusted)

library(psych)
x <- describeBy(children_chemicals_log_adjusted[3:45],group = children_chemicals_log_adjusted$urb_degree_dic)

capture.output(print(x), file="children_descriptive.csv")

table(children_chemicals_log_adjusted$h_race, children_chemicals_log_adjusted$Cohort)

#dataframe with inverse ICC for plot
df_11.12_c$revICC <-  c("0.05", "0.10", "0.02", "0.02", "0.07", "0.15", "0.07", "0.08", "0.13", "0.00",
                        "0.04", "0.10", "0.08", "0.37", "0.32", "0.30", "0.16", "0.17", "0.27", "0.25",
                        "0.14", "0.01", "0.38", "0.02", "0.14", "0.21", "0.50", "0.09", "0.12", "0.11",
                        "0.28", "0.21", "0.30", "0.34", "0.43", "0.08", "0.06", "0.61", "0.39", "0.41")


table(children_chemicals_log_adjusted$urb_degree_dic,df_11.01$urban_degree_dic)
