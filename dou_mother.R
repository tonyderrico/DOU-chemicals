library(dplyr)
library(psych)
library(foreign)
library(FSA)
library(ggplot2)
library(plyr)

#DATASETS MOTHER

biomarker_mother_database_20170719 #original
names(biomarker_mother_database_20170719)[2] <- "id"

biomarkers_mothers_h1 <- merge(biomarker_mother_database_20170719, dou_all_cohorts_H1, 
      by = "id")

biomarker_mother_children <- merge(biomarker_child_database_version_20170719, biomarker_mother_database_20170719, 
                                   by = "id") #dataset mothers + children

biomarker_mother_database_20170719_m <- biomarker_mother_children[-c(2:181)] #only mother merged with children for ID

*biomarker_mother_database_20170719_c #concentrations normal and adjusted
*biomarker_mother_database_20170719_not_adjusted #cleaned for just normal concentrations

biomarker_mother_database_20170719_dou <- merge(biomarker_mother_database_20170719_m,
                                                       dou_all_cohorts, by = "id") #main dataset + degree of urbanization

h1_bio_dou_m <- merge(dou_all_cohorts_H1, biomarker_mother_database_20170719_m, by = "id") #merged h1 and only biomarkers conc

write.dta(h1_bio_dou_m, "C:/Users/Tony/Desktop/R/Helix_DoU/h1_bio_dou_m.dta")

#ANALYSIS
#-------------------------------------------------------------------------------------------------------------------------------------

#summary of chemicals conc 
sum_h1_bio_dou <- describeBy(h1_bio_dou_m, group = h1_bio_dou_m$urban_degree, fast = TRUE)
sum_s1_bio_dou <- describeBy(s1_bio_dou_c, group = s1_bio_dou_c$urban_degree, fast = TRUE)
sum_bio_dou_cohort_m <- describeBy(h1_bio_dou_m, group = h1_bio_dou_m$cohort, fast = TRUE)


capture.output(print(sum_bib), file="sum_bib.csv")
capture.output(print(sum_sbd), file="sum_sbd.csv")
capture.output(print(sum_kanc), file="sum_kanc.csv")
capture.output(print(sum_eden), file="sum_eden.csv")
capture.output(print(sum_rhea), file="sum_rhea.csv")

#median and GM calculations

#dataset mother with just concentrations
#gm_mean = function(x, na.rm=TRUE){exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))}

median_per_cohort_dou_c <- aggregate(x=h1_bio_dou_m_cat_c[,3:57],
          by=list(h1_bio_dou_m_cat_c$urban_degree,
                  h1_bio_dou_m_cat_c$cohort),
          FUN = median, na.rm = TRUE)

median_per_cohort_c <- aggregate(x=h1_bio_dou_m_cat_c[,3:57],
                            by=list(h1_bio_dou_m_cat_c$cohort),
                            FUN = median, na.rm = TRUE)

median_per_dou_c <- aggregate(x=h1_bio_dou_m_cat_c[,3:57],
                            by=list(h1_bio_dou_m_cat_c$urban_degree),
                            FUN = median, na.rm = TRUE)

sd_per_dou_c <- aggregate(x=h1_bio_dou_m_cat_c[,3:57],
                          by=list(h1_bio_dou_m_cat_c$urban_degree),
                          FUN = geosd, na.rm = TRUE)

gm_per_dou_c <- aggregate(x=h1_bio_dou_m_cat_c[,3:57],
                        by=list(h1_bio_dou_m_cat_c$urban_degree),
                        FUN = geometric.mean, na.rm = TRUE)

gm_per_dou_cohort_c <- aggregate(x=h1_bio_dou_m_cat_c[,3:57],
          by=list(h1_bio_dou_m_cat_c$urban_degree,
                  h1_bio_dou_m_cat_c$cohort),
          FUN = geometric.mean, na.rm = TRUE)

gm_per_cohort_c <- aggregate(x=h1_bio_dou_m_cat_c[,3:57],
                           by=list(h1_bio_dou_m_cat_c$cohort),
                           FUN = geometric.mean, na.rm = TRUE)



#dataframe to table-------------------------------------------------------------------------------------

n <- c("2 BIB", "3 BIB", "1 EDEN", "2 EDEN", "3 EDEN", "1 KANC", "2 KANC", "3 KANC", "1 RHEA",
       "2 RHEA", "3 RHEA", "1 SAB", "2 SAB", "3 SAB")
gm_per_dou_cohort_c$Group.2 <- NULL
x <-t(gm_per_dou_c[,-1])
colnames(x) <- c("1", "2", "3")
x <- as.data.frame(x)

mydf <- cbind(rownames(x),x)
rownames(mydf) <- NULL
colnames(mydf) <- c(names(mydf)) #to not write all the column names

names(mydf)[1] <- c("Chemicals")
names(mydf)
mydf[mydf == 1.0] <- 0

is.num <- sapply(mydf, is.numeric)
mydf[is.num] <- lapply(mydf[is.num], round, 2)
mydf[mydf == "0"] <- ""

library(gt)
library(webshot)
library(reporter)

y <- gt(mydf ) 
y %>% tab_options(table.width = pct(10)) %>% 
  tab_header(title =  "GM per degree of urbanization")  %>%
  gtsave("gm_per_dou.pdf")
  

#categorize degree of urbanization h1_mother_m and h1_mother_cleaned----------------------------------------

h1_bio_dou_m_cat <- h1_bio_dou_m
h1_bio_dou_m_cat$urban_degree <- as.factor(h1_bio_dou_m_cat$urban_degree)

h1_bio_dou_m_cat$urban_degree[h1_bio_dou_m_cat$urban_degree == 30] <- 3
h1_bio_dou_m_cat$urban_degree[h1_bio_dou_m_cat$urban_degree == 23] <- 2
h1_bio_dou_m_cat$urban_degree[h1_bio_dou_m_cat$urban_degree == 22] <- 2
h1_bio_dou_m_cat$urban_degree[h1_bio_dou_m_cat$urban_degree == 21] <- 2
h1_bio_dou_m_cat$urban_degree[h1_bio_dou_m_cat$urban_degree == 13] <- 1
h1_bio_dou_m_cat$urban_degree[h1_bio_dou_m_cat$urban_degree == 12] <- 1
h1_bio_dou_m_cat$urban_degree[h1_bio_dou_m_cat$urban_degree == 11] <- 1
h1_bio_dou_m_cat$urban_degree[h1_bio_dou_m_cat$urban_degree == 10] <- 1

h1_bio_dou_m_cat[-c(1,2,3,6,7)]

library(foreign)
write.dta(h1_bio_dou_m_cat, "C:/Users/Tony/Desktop/R/Helix_DoU/h1_bio_dou_m_urbdegree_cat.dta")

#-------------------------------------------------------------------------------------------------------------------
#Analysis for adjusted chemicals + normal for metals

#plot the missing
library(pacman)
pacman::p_load(naniar)
gg_miss_var(h1_bio_dou_m_urbdegree_cat_adj_metals_c)
summary(h1_bio_dou_m_urbdegree_cat_adj_metals_c)

#log transformation
log_h1_bio_dou_m_urbdegree_cat_adj_metals_c[4:63] <- log(log_h1_bio_dou_m_urbdegree_cat_adj_metals_c[4:63])

multi.hist(h1_bio_dou_m_urbdegree_cat_adj_metals_c[4:63])

#hist for normal values
df <- gather(h1_bio_dou_m_urbdegree_cat_adj_metals_c[31:63], key = "name", value = "value")
# plot histograms per name
ggplot(df) +
  facet_wrap(~name, ncol = 5, scales = "free_x") +
  geom_histogram(aes(value))

#df + covariates
names(conf)[names(conf) == "HelixID"] <- "id"
df_11.01 <- merge(conf, h1_bio_dou_m_urbdegree_cat_adj_metals_c, by ="id")
df_11.01$urban_degree_dic <- df_11.01_1$urban_degree
df_11.01$hs_pbde153_madj <- NULL
df_11.01$hs_pbde47_madj <- NULL

x %>% tab_options(table.width = pct(10)) %>% 
  tab_header(title = "GM per degree of urbanization")  %>%
  gtsave("gm_per_dou.pdf")
df_11.01_1$urban_degree[df_11.01_1$urban_degree == 2] <- 1
df_11.01_1$hs_pbde153_madj <- NULL
df_11.01_1$hs_pbde47_madj <- NULL

names(breadfeeeding_infoquest_10jun_2022)[names(breadfeeeding_infoquest_10jun_2022) == "HelixID"] = "id"
breadfeeeding_infoquest_10jun_2022 <- as.data.frame(breadfeeeding_infoquest_10jun_2022)
x <- breadfeeeding_infoquest_10jun_2022 %>% select_("id","hs_bf1c") 
names(df_11.01)

#descrittive
df_11.01$urban_degree
names(dou_all_cohorts_H1)[4] <- "urban_degree_cat"
x <- dou_all_cohorts_H1 %>% dplyr::select("id", "urban_degree_cat")
df_11.01_1 <- merge(x, df_11.01_1, by = "id")

#adding previous breastfeeding for mothers
x <- breadfeeeding_infoquest_18ene_2023 %>% select(hs_prebf, HelixID)
names(x)[2] <- "id"
x <- merge(x, df_11.01_1, by="id")
df_11.01_1 <- x

x <- table(df_11.01_1$cohort.y, df_11.01_1$urban_degree_cat)
x <- as.data.frame.matrix(x)

x <- describeBy(df_11.01_1[27:69], group = df_11.01_1$urban_degree)
capture.output(print(x), file="pregnancy_biomarkers_dou_dic_descriptive.csv")
names(df_11.01_1)

#DATASET SPLIT PER COHORT 28.04.23

rhea_m <- df_11.01_1 %>% filter(cohort.y == "RHEA")
bib_m <- df_11.01_1 %>% filter(cohort.y == "BIB")
inma_m <- df_11.01_1 %>% filter(cohort.y == "SAB")
eden_m <- df_11.01_1 %>% filter(cohort.y == "EDEN")
kanc_m <- df_11.01_1 %>% filter(cohort.y == "KANC")

#adding ICC values in a new column for forest plots
df_11.12_m$revICC <- c("0.01", "0.04", "0.16", "0.20", "0.17", "0.05", "0.07", "0.07", "0.04", "0.09", 
                    "0.02", "0.06", "0.18", "0.11", "0.18", "0.22", "0.16", "0.42", "0.18", "0.11", 
                    "0.09", "0.06", "0.51", "0.12", "0.19", "0.36", "0.33", "0.60", "0.41", "0.33", 
                    "0.73", "0.75", "0.72", "0.75", "0.72", "0.55", "0.14", "0.42")

class(df_11.12_m$ICC)

