
library(dplyr)
library(afex)
library(tidyr)
library(tidyverse)
library(generics)
library(emmeans)
library(PROscorerTools)
library(ggpubr)
library(pander)

setwd("C:/Users/fintan.smith/Documents/Diss/Validation")

df <- read.csv("VALIDATION.csv")

val_df <- select(df, splitsample, ID, Scape_remain:emotional_control_leave_jpg, pastvote_EURef)
  

val_df_remain_long <- val_df %>%
  pivot_longer(cols= contains("remain"),
               values_to= "Response",
               values_drop_na= TRUE)


val_df_leave_long <- val_df %>%
  pivot_longer(cols= contains("leave"),
               values_to= "Response",
               values_drop_na= TRUE)


val_df_leave_long <- val_df_leave_long%>%
  select(ID, splitsample, Response, name, pastvote_EURef)
  
val_df_remain_long <- val_df_remain_long%>%
  select(ID, splitsample, Response, name, pastvote_EURef)

val_df_comb <-merge(val_df_leave_long, val_df_remain_long, all.x = TRUE, all.y = TRUE)

val_df_comb <- val_df_comb%>%
  mutate(Stimuli=name)

val_df_comb <- val_df_comb%>%
  mutate(Type= if_else(str_detect(Stimuli,"cont"), "Non-derogatory", "Derogatory"))

val_df_comb$Stimuli <- as.factor(as.character(val_df_comb$Stimuli))

val_df_comb <-val_df_comb%>% 
  mutate(Response=revcode(Response, mn=1, mx=5))

val_df_comb <- val_df_comb %>%
  mutate(brexside= if_else(str_detect(Stimuli,"main"), "Congruent_with_remain", "Congruent_with_leave"))
  
  
  val_df_comb$brexside <- as.factor(as.character(val_df_comb$brexside))

val_df_comb$pastvote_EURef <- factor(val_df_comb$pastvote_EURef, levels=c(1:2), labels =c("Remain", "Leave"))


  
###ANOVA with derogatory measure 1-5 as outcome, type stimuli (derogatory or non) as between subs condition

ANOVA <- aov_ez("ID",
       dv = "Response",
       val_df_comb,
       between = "Type",
       within=NULL,
       print.formula=TRUE)

ANOVA

#EMMeans for ANOVA2

ANOVA_EM <- emmeans::lsmeans(ANOVA, specs= "Type")

print(ANOVA_EM)


#Post hoc contrasts for ANOVA2
emmeans::contrast(ANOVA_EM, specs= "Type")


#ANOVA with derogatory measure 1-5 as outcome, specific stimuli seen as between subs condition
ANOVA3 <- aov_ez("ID", dv="Response", val_df_comb, between="Stimuli", within=NULL, print.formla=TRUE)

nice(ANOVA3, es="pes")

ANOVA_3_plot <- emmip(ANOVA3, ~Stimuli, CIs=TRUE)+
  theme_pubclean()+
  labs(x= "Stimuli")

plot(ANOVA_3_plot)

#EMMEans for ANOVA3
ANOVA3_EM <- emmeans::lsmeans(ANOVA3, specs= "Stimuli")

print(ANOVA3_EM)

ANOVA3_contrast <- pairs(ANOVA3_EM, specs="Stimuli")

ANOVA3_contrast <- as.data.frame(ANOVA3_contrast)

#Print pair contrasts for ANOVA 3
pander(ANOVA3_contrast)






#ANOVA4 - Check whether leavers and remainers perceive derogatory vs non more or less negatively

ANOVA4 <- aov_ez ("ID", data=val_df_comb, dv= "Response", between = c("Type", "pastvote_EURef", "brexside"))

nice(ANOVA4, es="pes")

ANOVA4_plot <-emmip(ANOVA4, brexside~Type | pastvote_EURef, CIs=TRUE)+
  theme_pubclean()+
  labs(x="Stimuli type", y="Extent to which derogatory (1-5)", color="Stimuli faction")
  
ANOVA4_EM <- emmeans::lsmeans(ANOVA4, specs= c("pastvote_EURef", "Type", "brexside"))

print(ANOVA4_EM)

ANOVA4_EM_pairs <- pairs(ANOVA4_EM)

ANOVA4_EM_pairs <- as.data.frame(ANOVA4_EM_pairs)

pander(ANOVA4_EM_pairs)


plot(ANOVA4_plot)



  
 





