library("tidyverse")
library("MASS")
library("glmnet")
library("fastDummies")
library("nnet")

# Set file paths
data_path = "D:/Work/Premise/Data/"
data_file = function(x){paste0(data_path,x)}

# Load raw data
CC_22 = read.csv(data_file("Climate Change Perceptions 2 - 08.09.22.csv"))
C19_22 = read.csv(data_file("COVID-19 Vaccine Perceptions 2 08.09.22.csv"))

# Renaming - please check these are the correct columns
CC_22 = as_tibble(CC_22) %>%
  rename(Emotion = Response.Label.or.Image.List.Distinct.14,
         Sharing = Response.Label.or.Image.List.Distinct.22,
         Truthfulness = Response.Label.or.Image.List.Distinct.17,
         Platform = Response.Label.or.Image.List.Distinct.4) %>%
  mutate(Sharing_any = !(Sharing == ""),
         Opinion_changed = Response.Label.or.Image.List.Distinct.18 == "Yes") %>%
  mutate(Sharing_recode = case_when(grepl("Followers",Sharing,ignore.case = T) ~ 3,
                                    grepl("Close friends",Sharing,ignore.case = T) ~ 2,
                                    grepl("Family",Sharing,ignore.case = T) ~ 1,
                                    TRUE ~ 0),
         Twitter = grepl("Twitter",Platform,ignore.case = T),
         Facebook = grepl("Facebook",Platform,ignore.case = T),
         Instagram = grepl("Instagram",Platform,ignore.case = T),
         YouTube = grepl("Youtube",Platform,ignore.case = T),
         TikTok = grepl("TikTok",Platform,ignore.case = T),
         Other = grepl("Other",Platform,ignore.case = T))

C19_22 = as_tibble(C19_22) %>%
  rename(Emotion = Response.Label.or.Image.List.Distinct.14,
         Sharing = Response.Label.or.Image.List.Distinct.22,
         Truthfulness = Response.Label.or.Image.List.Distinct.17,
         Platform = Response.Label.or.Image.List.Distinct.4) %>%
  mutate(Sharing_any = !(Sharing == ""),
         Opinion_changed = Response.Label.or.Image.List.Distinct.18 == "Yes") %>%
  mutate(Sharing_recode = case_when(grepl("Followers",Sharing,ignore.case = T) ~ 3,
                                    grepl("Close friends",Sharing,ignore.case = T) ~ 2,
                                    grepl("Family",Sharing,ignore.case = T) ~ 1,
                                    TRUE ~ 0),
         Twitter = grepl("Twitter",Platform,ignore.case = T),
         Facebook = grepl("Facebook",Platform,ignore.case = T),
         Instagram = grepl("Instagram",Platform,ignore.case = T),
         YouTube = grepl("Youtube",Platform,ignore.case = T),
         TikTok = grepl("TikTok",Platform,ignore.case = T),
         Other = grepl("Other",Platform,ignore.case = T))

# Check frequencies in strata
CC_22 %>% group_by(Gender) %>% summarise(Freq = mean(Sharing_any))
CC_22 %>% group_by(Truthfulness) %>% summarise(Freq = mean(Sharing_any))
CC_22 %>% group_by(Emotion) %>% summarise(Freq = mean(Sharing_any))
CC_22 %>% group_by(Emotion) %>% summarise(Freq = mean(Opinion_changed))
CC_22 %>% group_by(Opinion_changed) %>% summarise(Freq = mean(Sharing_any))


C19_22 %>% group_by(Gender) %>% summarise(Freq = mean(Sharing_any))
C19_22 %>% group_by(Truthfulness) %>% summarise(Freq = mean(Sharing_any))
C19_22 %>% group_by(Emotion) %>% summarise(Freq = mean(Sharing_any))
C19_22 %>% group_by(Emotion) %>% summarise(Freq = mean(Opinion_changed))
C19_22 %>% group_by(Opinion_changed) %>% summarise(Freq = mean(Sharing_any))
# Tests
group_chisq <- function(a,b){
  chisq.test(x = a,
             y = b,simulate.p.value = T)$p.value %>% round(digits = 3) %>% return
}

group_lm <- function(data){
  lm(Sharing_any ~ Emotion,data = data) %>% summary %>%  return
}

group_lm_oc <- function(data){
  lm(Sharing_any ~ Opinion_changed,data = data) %>% summary %>%  return
}


# Gender vs. 
# chisq.test(x = CC_22$Gender,y = CC_22$Sharing_any)
# chisq.test(x = CC_22 %>% filter(Truthfulness != "Unsure") %>% pull(Truthfulness),y = CC_22 %>% filter(Truthfulness != "Unsure") %>% pull(Sharing_any))
# chisq.test(x = CC_22$Emotion,y = CC_22$Sharing_any)
# 
# 
# polr(as.factor(Sharing_recode) ~ Gender,data = CC_22)
# polr(as.factor(Sharing_recode) ~ Emotion,data = CC_22)
# 
# lm(Sharing_any ~ Truthfulness,data = CC_22) %>% summary
# lm(Sharing_any ~ Emotion,data = CC_22) %>% summary
# lm(Sharing_any ~ Gender,data = CC_22) %>% summary

# Germany, Greece, Romania do not have enough variety in their responses, all are 
CC_22 %>% group_by(Country.Name) %>% 
  filter(!is.na(Country.Name),!Country.Name %in% c("#N/A","0","Greece","Germany","Romania"),Truthfulness != "Unsure") %>% 
  summarise(p_chi_sq = group_chisq(Truthfulness,Sharing_any))

C19_22 %>% group_by(Country.Name) %>% 
  filter(!is.na(Country.Name),!Country.Name %in% c("#N/A","0","Germany","Venezuela"),Truthfulness != "Unsure") %>% 
  summarise(p_chi_sq = group_chisq(Truthfulness,Sharing_any))

# Emotion vs Sharing stratified by country
# CC
data = CC_22 %>% group_by(Country.Name) %>% 
  filter(!is.na(Country.Name),!Country.Name %in% c("#N/A","0","Greece","Germany","Romania")) %>% 
  group_split(Country.Name)

CC_22_countries_emotion_sharing = lapply(data,FUN = group_lm)
names(CC_22_countries_emotion_sharing) = lapply(data,FUN = function(x){unique(x$Country.Name)}) %>% unlist

CC_22_countries_emotion_sharing

# C19
data = C19_22 %>% group_by(Country.Name) %>% 
  filter(!is.na(Country.Name),!Country.Name %in% c("#N/A","0","France","Venezuela")) %>% 
  group_split(Country.Name)

C19_22_countries_emotion_sharing = lapply(data,FUN = group_lm)
names(C19_22_countries_emotion_sharing) = lapply(data,FUN = function(x){unique(x$Country.Name)}) %>% unlist

C19_22_countries_emotion_sharing

# Emotion vs Sharing stratified by gender
# CC
data = CC_22 %>% group_by(Gender) %>% 
  filter(!is.na(Gender),!is.na(Country.Name),!Country.Name %in% c("#N/A","0","Greece","Germany","Romania")) %>% 
  group_split(Gender)

CC_22_gender_emotion_sharing = lapply(data,FUN = group_lm)
names(CC_22_gender_emotion_sharing) = lapply(data,FUN = function(x){unique(x$Gender)}) %>% unlist

CC_22_gender_emotion_sharing

# C19
data = C19_22 %>% group_by(Gender) %>% 
  filter(!is.na(Country.Name),!Country.Name %in% c("#N/A","0","France","Venezuela")) %>% 
  group_split(Country.Name)

C19_22_countries_emotion_sharing = lapply(data,FUN = group_lm)
names(C19_22_countries_emotion_sharing) = lapply(data,FUN = function(x){unique(x$Country.Name)}) %>% unlist

C19_22_countries_emotion_sharing

# Emotion influencing sharing behaviour - non stratified
# C22
data = CC_22 %>% filter(!is.na(Country.Name),!Country.Name %in% c("#N/A","0","Greece","Germany","Romania")) 
lm(Sharing_any ~ Emotion,data = data) %>% summary

# C19
data = C19_22 %>%filter(!is.na(Country.Name),!Country.Name %in% c("#N/A","0","France","Venezuela"))
lm(Sharing_any ~ Emotion,data = data) %>% summary


# Opinion change influencing sharing behaviour
# C22
data = CC_22 %>% filter(!is.na(Country.Name),!Country.Name %in% c("#N/A","0","Greece","Germany","Romania")) 
chisq.test(x = data$Opinion_changed,y = data$Sharing_any)
lm(Sharing_any ~ Opinion_changed,data = data) %>% summary

# C19
data = C19_22 %>%filter(!is.na(Country.Name),!Country.Name %in% c("#N/A","0","France","Venezuela"))
chisq.test(x = data$Opinion_changed,y = data$Sharing_any)
lm(Sharing_any ~ Opinion_changed,data = data) %>% summary

#Truthfulness impacting emotion - there's definitely a better way to do this if we want to pursue. If we just want to know whether there is a 
# difference in the impact of information on emotion, 
# data = CC_22 %>% filter(!is.na(Country.Name),!Country.Name %in% c("#N/A","0","Greece","Germany","Romania")) 
# 
# lm(Emotion == "Anxiety" ~ Truthfulness,data) %>% summary
# lm(Emotion == "Curiosity" ~ Truthfulness,data) %>% summary
# lm(Emotion == "Fear" ~ Truthfulness,data) %>% summary
# lm(Emotion == "Happiness" ~ Truthfulness,data) %>% summary
# lm(Emotion == "Indifference" ~ Truthfulness,data) %>% summary
# lm(Emotion == "Other" ~ Truthfulness,data) %>% summary
# lm(Emotion == "Relief" ~ Truthfulness,data) %>% summary
# lm(Emotion == "Sadness" ~ Truthfulness,data) %>% summary
# 
# data = C19_22 %>%filter(!is.na(Country.Name),!Country.Name %in% c("#N/A","0","France","Venezuela")) 
# 
# lm(Emotion == "Anxiety" ~ Truthfulness,data) %>% summary
# lm(Emotion == "Curiosity" ~ Truthfulness,data) %>% summary
# lm(Emotion == "Fear" ~ Truthfulness,data) %>% summary
# lm(Emotion == "Happiness" ~ Truthfulness,data) %>% summary
# lm(Emotion == "Indifference" ~ Truthfulness,data) %>% summary
# lm(Emotion == "Other" ~ Truthfulness,data) %>% summary
# lm(Emotion == "Relief" ~ Truthfulness,data) %>% summary
# lm(Emotion == "Sadness" ~ Truthfulness,data) %>% summary

# ---------------- Platform

data = CC_22 %>% filter(!is.na(Country.Name),!Country.Name %in% c("#N/A","0","Greece","Germany","Romania")) 
multinom(formula = Emotion ~ Twitter + Facebook + YouTube + Instagram + TikTok + Other,data = data) %>% summary

data = C19_22 %>%filter(!is.na(Country.Name),!Country.Name %in% c("#N/A","0","France","Venezuela")) 
multinom(formula = Emotion ~ Twitter + Facebook + YouTube + Instagram + TikTok + Other,data = data)

# ---------------- Combined predictive model, including socio economic factors
data = CC_22 %>% filter(!is.na(Country.Name),!Country.Name %in% c("#N/A","0","Greece","Germany","Romania")) 

lm(Sharing_any ~ Age.Group + Emotion + Financial.Situation + Country.Name + Gender + Truthfulness,data) %>% summary
gl = cv.glmnet(y = data$Sharing_any %>% as.numeric,as.matrix(dummy_cols(.data = data %>% dplyr::select(Age.Group,Emotion,Financial.Situation,Country.Name,Gender,Truthfulness),remove_selected_columns = TRUE))
,family = "binomial",alpha = 1)
gl %>% coef

# C19
data = C19_22 %>%filter(!is.na(Country.Name),!Country.Name %in% c("#N/A","0","France","Venezuela")) 

lm(Sharing_any ~ Age.Group + Emotion + Financial.Situation + Country.Name + Gender + Truthfulness,data) %>% summary
gl = cv.glmnet(y = data$Sharing_any %>% as.numeric,as.matrix(dummy_cols(.data = data %>% dplyr::select(Age.Group,Emotion,Financial.Situation,Country.Name,Gender,Truthfulness),remove_selected_columns = TRUE))
               ,family = "binomial",alpha = 1)
gl %>% coef


#######################################
CC_22 %>% group_by(Sharing_any) %>% tally
CC_22 %>% group_by(Sharing_any,Gender) %>% tally
CC_22 %>% group_by(Sharing_any,Emotion) %>% tally

CC_22 %>% group_by(Sharing_any,Twitter) %>% tally
CC_22 %>% group_by(Sharing_any,Facebook) %>% tally
CC_22 %>% group_by(Sharing_any,YouTube) %>% tally
CC_22 %>% group_by(Sharing_any,TikTok) %>% tally
CC_22 %>% group_by(Sharing_any,Instagram) %>% tally


range_maker <- function(x){
  range(x)
  
  paste0(mean(x)," [",,,)
  
  
  return()
}


Make_Table_1 <- function(data){
  n = data %>% nrow
  n_resp = data %>% group_by(User.ID) %>% n_groups()
  
  data %>% group_by(User.ID) %>% summarise(n = n()) %>% pull(n)
}
