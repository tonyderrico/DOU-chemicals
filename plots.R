#plots
library(ggplot2)
library(ggpubr)
library(officer)
library(export)
#create forest plot pregnancy
m <- ggplot(data=df_11.12_m, aes(y=chemicals, x=est, xmin=`ci 2.5`, xmax=`ci 97.5`,color=group_chemicals)) +
  geom_point() + 
  geom_errorbarh(height=.6,linewidth=0.6) +
  labs(x='GMR (95% C.I.)', y = 'Environmental Contaminants', color= "Contaminants Groups") +
  geom_text(aes(label = ICC), hjust = -6, size = 3, color = "black", fontface = "bold", nudge_y = .15) + # Add multiple text annotations+
  geom_vline(xintercept=1, color='black', linetype='solid', alpha=.5, size = 1) +
  scale_x_continuous(breaks = c(0.5,1,2), limits = c(0.5,3), trans = "log") +
  theme_classic() +
  scale_y_discrete(limits=rev)
m

#with ICC values
m <- ggplot(data=df_11.12_m, aes(y=chemicals, x=est, xmin =ci2.5, xmax= ci97.5, color=group_chemicals)) +
  geom_point() + 
  geom_errorbarh(height=.6, linewidth=0.6) +
  geom_text(data = df_11.12_m, aes(label = revICC, x = 1.4, y = as.numeric(factor(chemicals))), size = 3, 
            color = "black", hjust = -9) +
  labs(x='GMR (95% C.I.)', y = 'Environmental Contaminants', color= "Contaminants Groups") +
  geom_vline(xintercept=1, color='black', linetype='solid', alpha=.5, size = 1) +
  scale_x_continuous(breaks = c(0.5,1,2), limits = c(0.5,3.5), trans = "log") +
  theme_classic() +
  scale_y_discrete(limits=rev)
m


#create forest plot children
m1 <- ggplot(data=df_11.12_c, aes(y=chemicals, x=est, xmin=`ci 2.5`, xmax=`ci 97.5`,color=group_chemicals1)) +
  geom_point() + 
  geom_errorbarh(height=.6,linewidth=.6) +
  labs(x='GMR (95% C.I.)', y = '',color= "Contaminants Groups") +
  geom_vline(xintercept=1, color='black', linetype='solid', alpha=.5, linewidth = 1) +
  scale_x_continuous(breaks = c(0.5,1,2), limits = c(0.5,3), trans = "log") +
  theme_classic() +
  scale_y_discrete(limits=rev)

#with icc text
m1 <- ggplot(data=df_11.12_c, aes(y=chemicals, x=est, xmin=`ci 2.5`, xmax=`ci 97.5`,color=group_chemicals1)) +
  geom_point() + 
  geom_errorbarh(height=.6,linewidth=.6) +
  geom_text(aes(label = revICC, x = 1.6, y = as.numeric(factor(chemicals))), size = 3, 
            color = "black", hjust = -6) +
  labs(x='GMR (95% C.I.)', y = '',color= "Contaminants Groups") +
  geom_vline(xintercept=1, color='black', linetype='solid', alpha=.5, linewidth = 1) +
  scale_x_continuous(breaks = c(0.5,1,2), limits = c(0.5,3), trans = "log") +
  theme_classic() +
  scale_y_discrete(limits=rev)
m1

ggobj <- ggarrange(m,m1, labels = c("A","B"), common.legend = TRUE,legend="right")
ggobj

ggsave(filename = "ggobj.tiff", width = 15, height = 9, device='tiff', dpi=1000)
tiff("outfile.tif", compression = "lzw")
#overlapping distribution plots per chemical------------------------------------------

# Assuming you have a vector of variable names for x
par(mfrow=c(6,6))
library(ggplot2)
library(ggpubr)

variable_names_mothers <- c("DDE", "DDT", "HCB", "PCB-118", "PCB-138", "PCB-153", "PCB-170", 
                            "PCB-180", "PFOA", "PFNA", "PFUNDA", "PFHXS", "PFOS", "As", "Cd", 
                            "Hg", "Pb", "MEP", "MiBP", "MnBP", "MBzP", "MEHP", "MEHHP", "MEOHP", 
                            "MECPP", "sumDEHP", "oh-MiNP", "oxoMiNP", "MEPA", "ETPA", "PRPA", 
                            "BPA", "BUPA", "OXBE", "TCS", "DMP", "DMTP", "DEP", "DETP")

plots <- lapply(variable_names_mothers, function(variable) {
  ggplot(df_11.01_2, aes(x = get(variable), fill = factor(urban_degree_dic))) +
    geom_density(alpha = 0.2) +
    labs(
      x = variable,
      y = "",
      fill="Urban Degree"
    ) +
    scale_fill_manual(values = c("green", "red"), labels=c("Non-Urban","Urban")) +
    theme_dark() +
    theme(axis.text = element_text(size=7), axis.title.x = element_text(size = 9),
          axis.title.y=element_blank()) +
    scale_x_continuous(breaks = pretty(df_11.01_2[[variable]], n = num_breaks)) +
    facet_wrap(~Cohort)
})

ggobject <- ggarrange(plotlist = plots[36:39], common.legend = TRUE) 
annotate_figure(ggobject, left="Density")

par(mfrow=c(1,2))

plots[[3]]

library("export")
graph2ppt(file="plot.pptx", width=10, height=6)
library(cowplot)

#DP for children
names(children_chemicals_log_adjusted_2)
variable_names_children <- c("DDE", "DDT", "HCB", "PBDE-47","PBDE-153","PCB-118", "PCB-138", "PCB-153", 
                             "PCB-170", 
                             "PCB-180", "PFOA", "PFNA", "PFUNDA", "PFHXS", "PFOS", "As", "Cd", 
                             "Hg", "Pb", "MEP", "MiBP", "MnBP", "MbZP", "MEHP", "MEHHP", "MEOHP", 
                             "MECPP", "sumDEHP", "oh-MiNP", "oxoMiNP", "MEPA", "ETPA", "PRPA", 
                             "BPA", "BUPA", "OXBE", "TCS", "DMP", "DMTP", "DEP", "DETP")

plots_c <- lapply(variable_names_children, function(variable) {
  ggplot(children_chemicals_log_adjusted_2, aes(x = get(variable), fill = factor(urb_degree_dic))) +
    geom_density(alpha = 0.2) +
    labs(
      x = variable,
      y = "",
      fill="Urban Degree"
    ) +
    scale_fill_manual(values = c("green", "red"), labels=c("Non-Urban","Urban")) +
    theme_dark() +
    theme(axis.text = element_text(size=7), axis.title.x = element_text(size = 9),
          axis.title.y=element_blank()) +
    scale_x_continuous(breaks = pretty(children_chemicals_log_adjusted_2[[variable]], n = num_breaks)) +
    facet_wrap(~Cohort)
})

ggobject_c <- ggarrange(plotlist = plots_c[38:41], common.legend = TRUE) 
annotate_figure(ggobject_c, left="Density")
#boxplot sd - chemicals------------------------------------------------------------------------------------------------
library(ggplot2)
group_chemicals <- c("POPs","POPs","POPs","POPs","POPs","POPs","POPs","POPs","PFASs","PFASs","PFASs"
                     ,"PFASs","PFASs", "Metals", "Metals", "Metals", "Metals","Phthalates","Phthalates","Phthalates",
                     "Phthalates","Phthalates","Phthalates","Phthalates","Phthalates","Phthalates","Phthalates","Phthalates",
                     "Phenols","Phenols","Phenols","Phenols","Phenols","Phenols","Phenols", "OP Pesticides","OP Pesticides",
                     "OP Pesticides","OP Pesticides")
p <- ggplot(x5, aes(x=sd_re_exp, y=chemicals, color=group_chemicals1))

z <- p  +
  geom_errorbar(aes(xmin=ci2.5, xmax=ci97.5), width=.7, 
                position=position_dodge(0.05), linewidth=.6) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8), limits = c(1,8)) +
  geom_point() +
  labs(y = "", x= "Random Effect (SD Between Cohorts)",color= "Contaminants Groups") +
  scale_y_discrete(limits=rev)

#combine plots------------------------------------------------------------------------------------------------------------------------
#preg - df dataframe
r <- ggplot(data=df, aes(y=chemicals, x=est, xmin=`ci 2.5`, xmax=`ci 97.5`, color=group_chemicals)) +
  geom_point() + 
  geom_errorbarh(height=0.7, linewidth=0.6) +
  labs(x='GMR (95% C.I.)', y = 'Environmental Contaminants',color= "Contaminants Groups") +
  geom_vline(xintercept=1, color='black', linetype='solid', alpha=.5, size = 1) +
  scale_x_continuous(breaks = c(0.5,1,1.5,2), limits = c(0.5,2.5), trans = "log") +
  scale_y_discrete(limits=rev) 
#children -  df1 dataframe
m <- ggplot(data=df1, aes(y=chemicals, x=est, xmin=`ci 2.5`, xmax=`ci 97.5`, color=group_chemicals1)) +
  geom_point() + 
  geom_errorbarh(height=0.7, linewidth=0.6) +
  labs(x='GMR (95% C.I.)', y = 'Environmental Contaminants',color= "Contaminants Groups") +
  geom_vline(xintercept=1, color='black', linetype='solid', alpha=.5, linewidth = 1) +
  scale_x_continuous(breaks = c(0.5,1,1.5,2), limits = c(0.5,2.5), trans = "log") +
  scale_y_discrete(limits=rev) 

library(gridExtra)
library(ggpubr)
library(patchwork)
#children
ggobj <- ggarrange(m,z, labels = c("A","B"), common.legend = TRUE,legend="right")
#preg
q <- ggplot(data=df, aes(y=chemicals, x=est, xmin=`ci 2.5`, xmax=`ci 97.5`,color=group_chemicals)) +
  geom_point() + 
  geom_errorbarh(height=0.7,linewidth=0.6) +
  labs(x='GMR (95% C.I.)', y = 'Environmental Contaminants',color= "Contaminants Groups") +
  geom_vline(xintercept=1, color='black', linetype='solid', alpha=.5, linewidth = 1) +
  scale_x_continuous(breaks = c(0.5,1,1.5,2), limits = c(0.5,2.7), trans = "log") +
  scale_y_discrete(limits=rev) 

#to export to pptx
library("export")
graph2ppt(file="plot.pptx", width=10, height=6)

x6 %>% filter(group_chemicals == "OP Pesticides") %>% summarise(mean = mean(sd_var ))
x5 %>% filter(group_chemicals1 == "OP Pesticides") %>% summarise(mean = mean(sd_re_exp))

#plots for adjusted models----------------------------------------------------------------------
#for order on y axis
llme_adj_c$chemicals <- factor(llme_adj_c$chemicals, levels = c(llme_adj_c$chemicals))
llme_adj_m$chemicals <- factor(llme_adj_m$chemicals, levels = c(llme_adj_m$chemicals))

# Create the ggplot with the specified order
m <- ggplot(data = llme_adj_m, aes(y = chemicals, x = est, xmin = `ci 2.5`, xmax = `ci 97.5`, color = group_chemicals)) +
  geom_point() + 
  geom_errorbarh(height = .6, linewidth = 0.6) +
  labs(x = 'GMR (95% C.I.)', y = 'Environmental Contaminants', color = 'Contaminants Groups') +
  geom_vline(xintercept = 1, color = 'black', linetype = 'solid', alpha = .5, size = 1) +
  scale_x_continuous(breaks = c(0.5, 1, 2), limits = c(0.4, 4), trans = 'log') +
  theme_classic() + 
  scale_y_discrete(limits=rev)

#create forest plot children
m1 <- ggplot(data=llme_adj_c, aes(y=chemicals, x=est, xmin=`ci 2.5`, xmax=`ci 97.5`,color=group_chemicals1)) +
  geom_point() + 
  geom_errorbarh(height=.6,linewidth=.6) +
  labs(x='GMR (95% C.I.)', y = '',color= "Contaminants Groups") +
  geom_vline(xintercept=1, color='black', linetype='solid', alpha=.5, linewidth = 1) +
  scale_x_continuous(breaks = c(0.5,1,2), limits = c(0.4,4), trans = "log") +
  theme_classic() +
  scale_y_discrete(limits=rev)


ggobj <- ggarrange(m,m1, labels = c("A","B"), common.legend = TRUE,legend="right")

#plotcorrelations
library(corrplot)
library(polycor)
df_11.01_2$hs_c_weight
names(df_11.01_2)
df_11.01_2[2:25]
M <- hetcor(df_11.01_2[2:25])
corrplot(M$correlations)
