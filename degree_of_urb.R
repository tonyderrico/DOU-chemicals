#DEGREE OF URBANIZATION 

dou_all_cohorts <- rbind(Degree_of_Urbanization_bib_sub, Degree_of_Urbanization_kanc_sub,Degree_of_Urbanization_poi_sub,
                         Degree_of_Urbanization_rhea_sub, Degree_of_Urbanization_sbd_sub) #dou for all cohorts
dou_all_cohorts_H1 <- dou_all_cohorts %>% filter(id_ad == "H1") #dou h1
dou_all_cohorts_S1 <- dou_all_cohorts %>% filter(id_ad == "S1") #dou s1
dou_all_cohorts_H2 <- dou_all_cohorts %>% filter(id_ad == "H2") #dou s1

write.dta(dou_all_cohorts,"D:/PhD/HELIX/datasets/dou_all_cohorts.dta")

#degree of urbanization per cohort H1
sbd_h1 <- Degree_of_Urbanization_sbd_sub %>% filter(id_ad == "H1")
bib_h1 <- Degree_of_Urbanization_bib_sub %>% filter(id_ad == "H1")
kanc_h1 <- Degree_of_Urbanization_kanc_sub %>% filter(id_ad == "H1")
eden_h1 <- Degree_of_Urbanization_poi_sub %>% filter(id_ad == "H1")
rhea_h1 <- Degree_of_Urbanization_rhea_sub %>% filter(id_ad == "H1")

#degree of urbanization per cohort S1
sbd_s1 <- Degree_of_Urbanization_sbd_sub %>% filter(id_ad == "S1")
bib_s1 <- Degree_of_Urbanization_bib_sub %>% filter(id_ad == "S1")
kanc_s1 <- Degree_of_Urbanization_kanc_sub %>% filter(id_ad == "S1")
eden_s1 <- Degree_of_Urbanization_poi_sub %>% filter(id_ad == "S1")
rhea_s1 <- Degree_of_Urbanization_rhea_sub %>% filter(id_ad == "S1")


#ANALYSIS
#-------------------------------------------------------------------------------------------------------------------------------------

#dou descriptive

data.frame(table(sbd_h1$urban_degree))
data.frame(table(bib_h1$urban_degree))
data.frame(table(kanc_h1$urban_degree))
data.frame(table(rhea_s1$urban_degree))
data.frame(table(eden_h1$urban_degree))


