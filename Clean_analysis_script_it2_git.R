
library(pacman)

p_load(tidyverse,sjlabelled,ggpubr,psych,afex,emmeans,cowplot,lavaan,effects,wesanderson,stargazer,corrplot, naniar, interactions)



#Read in data

setwd('C:/Users/fintan.smith/Documents/Diss')

df <- read.csv("2nd_iteration_final.csv", header=TRUE)


#Selects relevant variables for analysis and wrangling and assigns to object 'cond_outcomes_df'

cond_outcomes_df <- df%>%  dplyr::select(ID, brexdentity, split_video,age, profile_gender,  scapegoating_leave_sharing, t1_1, t1_2, split_trials, contains("AOT"), contains ("CRT"), contains("epistemic"), contains("NFC"), contains("sharing_1"), contains ("click_1"), contains("reaction"), contains("CRT"),contains("_identity_strength"))

# Filter cond_outcomes_df to only include rows where split_video is levels 1:2 meaning we exclude those who don't meet inclusion criteria






# Wrangling additional measures -------------------------------------------



#WRANGLING FOR cognitive reflection test

cond_outcomes_df <- cond_outcomes_df %>%
  mutate(CRT1_score= ifelse(CRT1==.05, 1, 0))%>%
  mutate(CRT2_score=ifelse(CRT2==5,1,0))%>%
  mutate(CRT3_score=ifelse(CRT3==47, 1,0))%>%
  mutate(CRT_SCORE= CRT1_score+CRT2_score+CRT3_score)

#WRANGLING FOR AOT

cond_outcomes_df <- cond_outcomes_df%>%
  replace_with_na(., replace=list("AOT17_1"=97,"AOT17_2"=97,"AOT17_3"=97,"AOT17_4"=97,"AOT17_5"=97,"AOT17_6"=97,"AOT17_7"=97,"AOT17_8"=97,"AOT17_9"=97,"AOT17_10"=97,"AOT17_11"=97,"AOT17_12"=97,"AOT17_13"=97,"AOT17_14"=97,"AOT17_15"=97,"AOT17_16"=97,"AOT17_17"=97))%>%
  mutate(AOT17_1=  PROscorerTools::revcode(AOT17_1,1,6))%>%
  mutate(AOT17_2=  PROscorerTools::revcode(AOT17_2,1,6))%>%
  mutate(AOT17_3=  PROscorerTools::revcode(AOT17_3,1,6))%>%
  mutate(AOT17_4=  PROscorerTools::revcode(AOT17_4,1,6))%>%
  mutate(AOT17_5=  PROscorerTools::revcode(AOT17_5,1,6))%>%
  mutate(AOT17_6=  PROscorerTools::revcode(AOT17_6,1,6))%>%
  mutate(AOT17_7=  PROscorerTools::revcode(AOT17_7,1,6))%>%
  mutate(AOT17_8=  PROscorerTools::revcode(AOT17_8,1,6))%>%
  mutate(AOT17_9=  PROscorerTools::revcode(AOT17_9,1,6))%>%
  mutate(AOT17_15=  PROscorerTools::revcode(AOT17_15,1,6))%>%
  mutate(AOT17_16=  PROscorerTools::revcode(AOT17_16,1,6))%>%
  mutate(AOT17_17=  PROscorerTools::revcode(AOT17_17,1,6))%>%
  mutate(AOT17_SCORE= rowSums(.[,c(11:27)], na.rm=TRUE))


#WRANGLING FOR NEED FOR CHAOS

cond_outcomes_df <- cond_outcomes_df%>%
  mutate(NFC_8= PROscorerTools::revcode(NFC_8, 1,7))%>%
  mutate(NFC_SCORE= rowSums(.[,c(39:46)], na.rm=TRUE))


#replaces missing values of not sure and not asked with 'NA'   

cond_outcomes_df[cond_outcomes_df==999] <- NA   
#cond_outcomes_df[cond_outcomes_df==5] <- NA  

cond_outcomes_df_for_polar<- cond_outcomes_df%>%
   unite(outgroup_polar, c(t1_1, t1_2), na.rm=T)

cond_outcomes_df_for_polar$outgroup_polar[cond_outcomes_df_for_polar$outgroup_polar==997] <- NA  

cond_outcomes_df_for_polar$outgroup_polar<- as.numeric(cond_outcomes_df_for_polar$outgroup_polar)






# Wrangling: clicking likelihood outcome -----------------------------------




#Creates DF with just clicking as outcome 

cond_clicking_df <- cond_outcomes_df%>% dplyr::select(ID, split_video, brexdentity, contains("click_1"))


#Gets data into long format and performs various recodes


cond_clicking_df_long <- cond_clicking_df %>%
  pivot_longer(cols = contains ("click"),
               names_to = "Condition" ,
               values_to = "Clicking_likelihood",
               values_drop_na = TRUE)%>%
  mutate(Stimuli = ifelse(str_detect(Condition, "_control_"), "Non_derogatory", "Derogatory"))%>%  #creates new variable that indicates video condition of that observation 
  mutate(Video_condition= ifelse(split_video==1, "Inoculation", "Control video"))%>%  #creates new variable that indicates video condition based on value of split_video
  mutate(brexdentity = ifelse(brexdentity==1, "Leaver", "Remainer"))



cond_clicking_df_long$Stimuli<- as.factor((as.character(cond_clicking_df_long$Stimuli))) #Defines stimuli as a factor
cond_clicking_df_long$Video_condition<- as.factor((as.character(cond_clicking_df_long$Video_condition))) #Defines video condition as a factor
cond_clicking_df_long$brexdentity<- as.factor((as.character(cond_clicking_df_long$brexdentity))) # Defines brexdentity as a factor


#Create technique variable and then label

cond_clicking_df_long <- cond_clicking_df_long%>%
  mutate(scapegoating = ifelse(str_detect(Condition,"scape"), 1, NA))%>%
  mutate(adhom = ifelse(str_detect(Condition,"hom"), 2, NA))%>%
  mutate(emotional = ifelse(str_detect(Condition,"emotio"), 3, NA))

cond_clicking_df_long <- cond_clicking_df_long%>%
  unite(Technique, c(scapegoating,adhom, emotional), na.rm=TRUE)

cond_clicking_df_long <- cond_clicking_df_long%>%
  add_labels(Technique, labels = c("Scapegoating" = 1))%>%
  add_labels(Technique, labels = c("Ad_hominem" = 2))%>%
  add_labels(Technique, labels = c("Emotional_language" = 3))

cond_clicking_df_long$Technique <- factor (cond_clicking_df_long$Technique, 
                                           levels= c(1,2,3),
                                           labels = c("Scapegoating", "Ad_Hominem", "Emotional_language"))


cond_clicking_df_long$Condition <- NULL

cond_clicking_df_long$split_video <- NULL









# Wrangling: sharing as outcome -------------------------------------------


#Creates DF with just sharing as outcome 

cond_sharing_df <- cond_outcomes_df%>% dplyr::select(ID, split_video, brexdentity, contains("sharing_1"))

#Codes values of 9 and 5 in these variables as 'NA'

cond_sharing_df[cond_sharing_df==999] <- NA   
#cond_sharing_df[cond_sharing_df==5] <- NA 


#Gets data into long format and performs various recodes


cond_sharing_df_long <- cond_sharing_df %>%
  pivot_longer(cols = contains("sharing"),
               names_to = "Condition",
               values_to = "Sharing_likelihood",
               values_drop_na = TRUE)%>%
  mutate(Stimuli = ifelse(str_detect(Condition, "_control_"), "Non derogatory", "Derogatory"))%>%  #creates new variable that indicates video condition of that observation 
  mutate(Video_condition= ifelse(split_video==1, "Inoculation", "Control video"))%>%#creates new variable that indicates video condition based on value of split_video
  mutate(brexdentity = ifelse(brexdentity==1, "Leaver", "Remainer"))


cond_sharing_df_long$Stimuli<- as.factor((as.character(cond_sharing_df_long$Stimuli))) #Defines stimuli as a factor
cond_sharing_df_long$Video_condition<- as.factor((as.character(cond_sharing_df_long$Video_condition))) #Defines video condition as a factor
cond_sharing_df_long$brexdentity<- as.factor((cond_sharing_df_long$brexdentity)) # Defines brexdentity as a factor


#Create technique variable and then label

cond_sharing_df_long <- cond_sharing_df_long%>%
  mutate(scapegoating = ifelse(str_detect(Condition,"scape"), 1, NA))%>%
  mutate(adhom = ifelse(str_detect(Condition,"hom"), 2, NA))%>%
  mutate(emotional = ifelse(str_detect(Condition,"emotio"), 3, NA))

cond_sharing_df_long <- cond_sharing_df_long%>%
  unite(Technique, c(scapegoating,adhom, emotional), na.rm=TRUE)

cond_sharing_df_long$Technique <- factor (cond_sharing_df_long$Technique, 
                                          levels= c(1,2,3),
                                          labels = c("Scapegoating", "Ad_Hominem", "Emotional_language"))

#Set 'condition' and 'split_video' variables to NULL

cond_sharing_df_long$Condition <- NULL
cond_sharing_df_long$split_video <- NULL



# Wrangling - Reactions to long form --------------------------------------


cond_reactions_df_long <- dplyr::select(cond_outcomes_df, ID, contains ("reaction"), split_video)



cond_reactions_df_long <- cond_reactions_df_long %>%
  replace_with_na(., replace=list("scapegoating_remain_reaction"=99, "scapegoating_leave_reaction"=99, "adhominem_leave_reaction"=99, "adhominem_remain_reaction"=99, 
                                  "emotional_leave_reaction"=99, "emotional_remain_reaction"=99,"scape_control_leave_reaction"=99, "scape_control_remain_reaction"=99, "adhom_control_remain_reaction"=99, "adhom_control_leave_reaction"=99,
                                  "emotional_control_leave_reaction"=99, "emotional_control_remain_reaction"=99))

cond_reactions_df_long <- cond_reactions_df_long%>%
  pivot_longer(contains("reaction"),
               names_to = "Condition",
               values_to = "Reaction",
               values_drop_na = T)

cond_reactions_df_long <- cond_reactions_df_long%>%
  mutate(Stimuli = ifelse(str_detect(Condition, "control"), 0, 1))%>%  #creates new variable that indicates video condition of that observation 
  mutate(Video_condition= ifelse(split_video==1, "Inoculation", "Control video"))%>%   #creates new variable that indicates video condition based on value of split_video
  mutate(angry_react= ifelse(Reaction==1, 1, 0))%>%
  mutate(wow_react= ifelse(Reaction==2, 1, 0))%>%
  mutate(sad_react= ifelse(Reaction==3, 1, 0))%>%
  mutate(Love_react= ifelse(Reaction==4, 1, 0))%>%
  mutate(like_react= ifelse(Reaction==5, 1, 0))%>%
  mutate(haha_react= ifelse(Reaction==6, 1, 0))%>%
  mutate(care_react= ifelse(Reaction==7, 1, 0))%>%
  mutate(reacted = ifelse(Reaction==8,0,1))%>%
  mutate(Stimuli = ifelse(Stimuli==1, "Derogatory", "Non derogatory"))

# Wrangling: Wide data incl. clicking sharing and reactions data ----------------------

#creates an object 'cond_sharing_polar_df' as a dataframe containing the named variables

cond_polar_df <- cond_outcomes_df%>% dplyr::select(ID, split_video, brexdentity, t1_1, t1_2, scapegoating_remain_sharing_1, scapegoating_leave_sharing_1, adhominem_leave_sharing_1,adhominem_remain_sharing_1,
                                                   emotional_leave_sharing_1, emotional_remain_sharing_1, scape_control_leave_sharing_1, scape_control_remain_sharing_1, adhom_control_leave_sharing_1, adhom_control_remain_sharing_1,
                                                   emotional_control_leave_sharing_1, emotional_control_remain_sharing_1, contains("reaction"), contains("strength"))





#replaces missing values of not sure and not asked with 'NA'   
cond_polar_df <- cond_polar_df%>%
  replace_with_na(replace = list(scapegoating_remain_sharing_1=c(999), scapegoating_leave_sharing_1=c(999), adhominem_remain_sharing_1=c(999), adhominem_leave_sharing_1=c(999), 
                                 emotional_remain_sharing_1=c(999),emotional_leave_sharing_1=c(999), t1_1=c(999,997), t1_2=c(999,997),scape_control_leave_sharing_1= c(999), scape_control_remain_sharing_1= c(999), adhom_control_remain_sharing_1= c(999), adhom_control_leave_sharing_1= c(999),
                                 emotional_control_leave_sharing_1= c(999), emotional_control_remain_sharing_1 = c(999)))%>%
  replace_with_na(., replace=list("scapegoating_remain_reaction"=99, "scapegoating_leave_reaction"=99, "adhominem_leave_reaction"=99, "adhominem_remain_reaction"=99, 
                                  "emotional_leave_reaction"=99, "emotional_remain_reaction"=99,"scape_control_leave_reaction"=99, "scape_control_remain_reaction"=99, "adhom_control_remain_reaction"=99, "adhom_control_leave_reaction"=99,
                                  "emotional_control_leave_reaction"=99, "emotional_control_remain_reaction"=99))%>%
  unite(derogatory_reaction, c(scapegoating_remain_reaction, scapegoating_leave_reaction, adhominem_leave_reaction, adhominem_remain_reaction, 
                               emotional_leave_reaction, emotional_remain_reaction),na.rm=TRUE, remove=TRUE)%>%
  unite(control_reaction, c(scape_control_leave_reaction, scape_control_remain_reaction, adhom_control_remain_reaction, adhom_control_leave_reaction,
                            emotional_control_leave_reaction, emotional_control_remain_reaction), na.rm = TRUE, remove = TRUE)%>%
  mutate(reacted_derogatory= ifelse(derogatory_reaction==8,0,1))%>%
  mutate(reacted_control= ifelse(control_reaction==8,1, 10))%>%
  mutate(derogatory_angry_react= ifelse(derogatory_reaction==1, "Selected", "Not selected"))%>%
  mutate(derogatory_wow_react= ifelse(derogatory_reaction==2, "Selected", "Not selected"))%>%
  mutate(derogatory_sad_react= ifelse(derogatory_reaction==3, "Selected", "Not selected"))%>%
  mutate(derogatory_Love_react= ifelse(derogatory_reaction==4, "Selected", "Not selected"))%>%
  mutate(derogatory_like_react= ifelse(derogatory_reaction==5, "Selected", "Not selected"))%>%
  mutate(derogatory_haha_react= ifelse(derogatory_reaction==6, "Selected", "Not selected"))%>%
  mutate(derogatory_care_react= ifelse(derogatory_reaction==7, "Selected", "Not selected"))%>%
  as_factor(., derogatory_angry_react, derogatory_haha_react,derogatory_like_react,derogatory_Love_react,derogatory_care_react,derogatory_wow_react)



#Unites all derogatory sharing likelihood condition values into one variable and then codes missing as NA + unites polarization score

cond_polar_df <- cond_polar_df%>%
  unite(Derogatory_sharing_1, c(scapegoating_remain_sharing_1, scapegoating_leave_sharing_1, adhominem_leave_sharing_1, adhominem_remain_sharing_1, 
                                emotional_leave_sharing_1, emotional_remain_sharing_1), na.rm=TRUE)%>%
  unite(Non_derogatory_sharing_1, c(scape_control_leave_sharing_1, scape_control_remain_sharing_1, adhom_control_remain_sharing_1, adhom_control_leave_sharing_1,
                                    emotional_control_leave_sharing_1, emotional_control_remain_sharing_1), na.rm=TRUE)%>%
  unite(col=Polarization_score, t1_1:t1_2,na.rm=TRUE)%>%
  mutate_all(na_if,"")

cond_polar_df$Polarization_score <- as.numeric(as.character(cond_polar_df$Polarization_score))  
cond_polar_df$Derogatory_sharing <- as.numeric(as.character(cond_polar_df$Derogatory_sharing))  
cond_polar_df$Non_derogatory_sharing <- as.numeric(as.character(cond_polar_df$Non_derogatory_sharing))
cond_polar_df$split_video <- as.numeric(as.character(cond_polar_df$split_video))


#Reverse coding polarization score

cond_polar_df <- cond_polar_df%>%
  mutate(Polarization_score=  (101-Polarization_score))

#Recoding split video variable into dummy 0, 1

cond_polar_df<- cond_polar_df%>%
  mutate(split_video = ifelse(split_video==1, 1, 0))


# Wrangles identity strenght measure

cond_polar_df <- cond_polar_df %>%
  unite(identity_str_1, contains("strength_1"), na.rm=TRUE)%>%
  unite(identity_str_4, contains("strength_4"), na.rm=TRUE)%>%
  unite(identity_str_5, contains("strength_5"), na.rm=TRUE)

cond_polar_df$identity_str_1 <- as.numeric(cond_polar_df$identity_str_1)
cond_polar_df$identity_str_4 <- as.numeric(cond_polar_df$identity_str_4)
cond_polar_df$identity_str_5 <- as.numeric(cond_polar_df$identity_str_5)

#Averages brexit identity score items to produce score

cond_polar_df <- cond_polar_df %>%
  mutate(identity_strength_SCORE= rowSums(.[,c(10:12)], na.rm=TRUE))%>%
  mutate(identity_strength_SCORE = identity_strength_SCORE/3)



# Primary analyses ----------------------------------------------------




#ANOVA1 - sharing as outcome

ANOVA1 <- afex::aov_ez(id = "ID",
                       dv = "Sharing_likelihood",
                       between = "Video_condition",
                       within = "Stimuli",
                       na.rm=TRUE,
                       data = cond_sharing_df_long,
                       es="pes",
                       detailed=TRUE)

#Produces a nice table of results

nice(ANOVA1, es="ges") #table of results

em_mean1 <- emmeans(ANOVA1, ~Video_condition:Stimuli)

print(em_mean1)




#PLOTS ANOVA MODEL 1

ANOVA1_plot <- emmip(ANOVA1, Stimuli ~ Video_condition, CIs=TRUE, Plot=TRUE)+
  theme_pubclean()+
  labs(title = 'Estimated Marginal Means For Sharing Likelihood',
       x='Video condition',
       color = "Stimuli",
       y = "Sharing likelihood (1-10)") + scale_color_manual(values=wes_palette("Darjeeling1",2))
plot(ANOVA1_plot)





#Performing ANOVA 1 but on clicking likelihood

ANOVA_1_click <- afex::aov_ez(id = "ID",
                              dv = "Clicking_likelihood",
                              between = "Video_condition",
                              within = "Stimuli",
                                na.rm=TRUE,
                              data = cond_clicking_df_long,
                              detailed=TRUE)


ANOVA1_click_plot <- emmip(ANOVA_1_click, Stimuli ~ Video_condition, CIs=TRUE, Plot=TRUE)+
  theme_pubclean()+
  labs(title = 'Estimated Marginal Means For Clicking Likelihood',
       x='Video condition',
       color = "Stimuli",
       y = "Clicking likelihood (1-4)")+ scale_color_manual(values=wes_palette("Darjeeling1",2))


plot(ANOVA1_click_plot)


# MEDIATED REGRESSION - M= polarization score; y= sharing likelihood for derogatory stimuli trials


Med_Reg<- process (data = cond_polar_df, y = "Derogatory_sharing", x = "split_video", m ="Polarization_score", model = 4, effsize =1, total =1, stand =1, boot = 5000 , modelbt = 1, seed = 654321, hc=3)



# LOGISTIC REGRESSION MODELS - impact of intervention (video condition) upon probability of reaction


#Effect of video condition and stimuli type on different reactions

angry_log_mod <- glm(angry_react~Video_condition + Stimuli + Video_condition*Stimuli,family=binomial, data=cond_reactions_df_long)
love_log__mod <- glm(Love_react ~Video_condition + Stimuli + Video_condition*Stimuli,family=binomial, data=cond_reactions_df_long)
care_log__mod <- glm(care_react ~Video_condition + Stimuli + Video_condition*Stimuli,family=binomial, data=cond_reactions_df_long)
like_log__mod <- glm(like_react ~Video_condition + Stimuli + Video_condition*Stimuli,family=binomial, data=cond_reactions_df_long)
haha_log__mod <- glm(haha_react ~Video_condition + Stimuli + Video_condition*Stimuli,family=binomial, data=cond_reactions_df_long)
sad_log__mod <- glm(sad_react ~Video_condition + Stimuli + Video_condition*Stimuli,family=binomial, data=cond_reactions_df_long)
reacted_log__mod <- glm(reacted ~ Video_condition + Stimuli + Video_condition*Stimuli,family=binomial, data=cond_reactions_df_long)



angry_plot<- cat_plot(angry_log_mod, pred = Video_condition, modx = Stimuli, geom = "line", point.shape = TRUE)+ scale_color_manual(values=wes_palette("Darjeeling1",2)) + labs(y="Probability of 'angry' reaction", x="Video condition", title = "Effect of video condition and stimuli type on probability of 'angry' reaction") + theme_pubclean()
love_plot <- cat_plot(love_log__mod, pred = Video_condition, modx = Stimuli, geom = "line", point.shape = TRUE)+ scale_color_manual(values=wes_palette("Darjeeling1",2)) + labs(y="Probability of 'love' reaction", x="Video condition", title = "Effect of video condition and stimuli type on probability of 'love' reaction") + theme_pubclean()
care_plot <- cat_plot(care_log__mod, pred = Video_condition, modx = Stimuli, geom = "line", point.shape = TRUE)+ scale_color_manual(values=wes_palette("Darjeeling1",2)) + labs(y="Probability of 'care' reaction", x="Video condition", title = "Effect of video condition and stimuli type on probability of 'care' reaction") + theme_pubclean()
like_plot <- cat_plot(like_log__mod, pred = Video_condition, modx = Stimuli, geom = "line", point.shape = TRUE)+ scale_color_manual(values=wes_palette("Darjeeling1",2)) + labs(y="Probability of 'like' reaction", x="Video condition", title = "Effect of video condition and stimuli type on probability of 'like' reaction") + theme_pubclean()
haha_plot <- cat_plot(haha_log__mod, pred = Video_condition, modx = Stimuli, geom = "line", point.shape = TRUE)+ scale_color_manual(values=wes_palette("Darjeeling1",2)) + labs(y="Probability of 'haha' reaction", x="Video condition", title = "Effect of video condition and stimuli type on probability of 'haha' reaction") + theme_pubclean()
sad_plot <- cat_plot(sad_log__mod, pred = Video_condition, modx = Stimuli, geom = "line", point.shape = TRUE)+ scale_color_manual(values=wes_palette("Darjeeling1",2)) + labs(y="Probability of 'sad' reaction", x="Video condition", title = "Effect of video condition and stimuli type on probability of 'sad' reaction") + theme_pubclean()
love_plot <- cat_plot(love_log__mod, pred = Video_condition, modx = Stimuli, geom = "line", point.shape = TRUE)+ scale_color_manual(values=wes_palette("Darjeeling1",2)) + labs(y="Probability of 'love' reaction", x="Video condition", title = "Effect of video condition and stimuli type on probability of 'angry' reaction") + theme_pubclean()
reacted_plot <- cat_plot(reacted_log__mod, pred = Video_condition, modx = Stimuli, geom = "line", point.shape = TRUE)+ scale_color_manual(values=wes_palette("Darjeeling1",2)) + labs(y="Probability of any reaction", x="Video condition", title = "Effect of video condition and stimuli type on probability of any reaction") + theme_pubclean()


# Produce latex regression tables using stargazer


stargazer(reacted_log__mod, angry_log_mod,love_log__mod,care_log__mod, align = TRUE,dep.var.labels=c("Reacted","Angry reaction","Love reaction","Care reaction"),
          covariate.labels=c("Video condition","Stimuli type", "Video condition * Stimuli type"), ci=TRUE, ci.level = 0.95,single.row = FALSE)
                                                                                                                                                                                   
stargazer(like_log__mod, haha_log__mod,sad_log__mod, align = TRUE,dep.var.labels=c("Like reaction","Haha reaction","Sad reaction"),
          covariate.labels=c("Video condition","Stimuli type", "Video condition * Stimuli type"), ci=TRUE, ci.level = 0.95,single.row = FALSE)





r2_nagelkerke(angry_log_mod)
r2_nagelkerke(reacted_log__mod)
r2_nagelkerke(like_log__mod)
r2_nagelkerke(love_log__mod)
r2_nagelkerke(care_log__mod)
r2_nagelkerke(haha_log__mod)
r2_nagelkerke(sad_log__mod)



