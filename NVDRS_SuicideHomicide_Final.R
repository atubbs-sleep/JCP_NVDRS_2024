#### Load Packages ####
library(ggpubr)
library(gt)
library(gtsummary)
library(rmarkdown)
library(sandwich)
library(lmtest)
library(lubridate)
library(magrittr)
library(wec)
library(broom)
library(car)
library(tidyverse)

#### Load ATUS Data ####
# This creates a 24x17 tibble where rows are hours, the first column is the intercept column
# (the average wakefulness per hour across age/sex/race/ethnicity)
# and additional columns are values to change the wakefulness when specifying specific ages, sexes, races, or ethnicities
atus.mins <- read_csv("c:/users/atala/sync/research/data/ATUS/atus_2003_2017.csv") %>%
  transmute("Time" = c(sort(rep(8:47,38)),sort(rep(0:7,38))),
            Parameter,
            "Minutes" = round(Estimate,4),
            "Proportion" = round(Estimate/30,4)) %>%
  arrange(Time) %>% pivot_wider(id_cols = "Time",names_from = "Parameter",values_from = "Minutes") %>%
  mutate("Time" = factor(floor(Time/2),0:23)) %>% group_by(Time) %>% 
  summarize(across(.cols=everything(),.fns = ~sum(.x))) %>% 
  mutate("Intercept" = abs(60-Intercept),across(.cols=3:39, .fns = ~ .x*-1),
         "age15to24" = -1*(age25to34+age35to44+age45to54+age55to64+age65to74+ageGE75),
         "Female" = -1*Male,
         "White" = -1*(Black+Hispanic+Asian+OtherRace),
         "Non-Hispanic" = -1*(Hispanic),
         "M01" = -1*(M02+M03+M04+M05+M06+M07+M08+M09+M10+M11+M12),
         "Y03" = -1*(Y04+Y05+Y06+Y07+Y08+Y09+Y10+Y11+Y12+Y13+Y14+Y15+Y16+Y17),
         "weekdays" = -1*weekendsholidays
  ) %>% select(
    Time, Intercept, starts_with("age"),
    "sexMale" = Male,"sexFemale" = Female,
    "raceWhite" = White,"raceBlack" = Black,"raceAsian" = Asian,"raceOtherRace" = OtherRace,
    "ethNon-Hispanic" = `Non-Hispanic`,"ethHispanic" = Hispanic) 
# Separate Estimates by Demographic
# This creates separate tibbles for each demographic in advance
atus.mins.base <- atus.mins %>% select(Time,Intercept)
atus.mins.age <- atus.mins %>% select(Time,starts_with("age")) %>% 
  pivot_longer(cols=2:8, names_to = "Age",values_to = "MinsAge") %>% 
  mutate("Age" = factor(str_remove(str_replace(Age,pattern = "to","-"),"age"),
                        levels = c("15-24","25-34","35-44","45-54","55-64","65-74","GE75"),
                        labels = c("15-24","25-34","35-44","45-54","55-64","65-74","75+")))
atus.mins.sex <- atus.mins %>% select(Time,starts_with("sex")) %>% 
  pivot_longer(cols=2:3, names_to = "Sex",values_to = "MinsSex") %>% 
  mutate("Sex" = factor(str_remove(Sex,"sex"),levels = c("Male","Female")))
atus.mins.race <- atus.mins %>% select(Time,starts_with("race")) %>% 
  pivot_longer(cols=2:5, names_to = "Race",values_to = "MinsRace") %>% 
  mutate("Race" = factor(str_remove(Race,"race"),levels = c("White","Black","Asian","OtherRace")))
atus.mins.eth <- atus.mins %>% select(Time,starts_with("eth")) %>% 
  pivot_longer(cols=2:3, names_to = "Ethnicity",values_to = "MinsEthnicity") %>% 
  mutate("Ethnicity" = factor(str_remove(Ethnicity, "eth"), levels = c("Non-Hispanic","Hispanic")))

#### Load NVDRS Data ####
nvdrs <- read_csv("c:/users/atala/sync/research/data/NVDRS/grandner_171_nvdrs_2017.csv") %>% 
  # Prepare individual variables
  transmute(
    "ID" = IncidentID,
    "Type" = factor(AbstractorDeathmanner_c, levels = c("Suicide or intentional self-harm","Homicide"),labels = c("Suicide","Homicide")),
    "MurderSuicide" = factor(case_when(PersonType=="Both victim and suspect"~1,PersonType=="Victim"~0),0:1),
    "SelfDefense" = factor(if_else(JustifiableSelfDefense_c=="Yes",1,0),0:1),
    "MaritalStatus" = factor(MaritalStatus,levels = c("Divorced","Married/Civil Union/Domestic Partnership","Married/Civil Union/Domestic Partnership, but separated",
                                                      "Never Married","Single, not otherwise specified","Widowed"),
                             labels = c("Divorced/Separated","Married","Divorced/Separated","Single","Single","Widowed")),
    "Education" = factor(EducationLevel, levels = c("8th grade or less","9th to 12th grade, no diploma","Associate's degree","Bachelor's degree",
                                                    "Doctorate or Professional degree","High school graduate or GED completed","Master's degree",
                                                    "Some college credit, but no degree"),
                         labels = c("Less than HS","Less than HS","College","College","Postgraduate","High School","Postgraduate","High School")),
    "Military" = case_when(Military=="No"~0,Military=="Yes"~1),
    "DepressedMood" = factor(if_else(DepressedMood_c=="Yes",1,0),0:1),
    "MentalHealthProblem" = factor(if_else(MentalHealthProblem_c=="Yes",1,0),0:1),
    "PartnerConflict" = if_else(IntimatePartnerProblem_c=="Yes",1,0),
    "AnyCrisis" = if_else(AnyCrisis_c=="Yes",1,0),
    "PriorSI" = if_else(SuicideThoughtHistory_c=="Yes",1,0),
    "PriorSA" = if_else(SuicideAttemptHistory_c=="Yes",1,0),
    "Argument" = if_else(Argument_c=="Yes",1,0),
    "Year" = str_trunc(IncidentYear,width=2,side="left",ellipsis = "") %>% as.numeric() %>% factor(3:17),
    "Yearcat" = if_else(Year %in% 3:10,"2003-2010","2011-2017") %>% factor(levels=c("2003-2010","2011-2017")),
    "Month" = mdy(InjuryDate) %>% month() %>% factor(1:12),
    "Day" = mdy(InjuryDate) %>% wday(),
    "Weekday" = if_else(Day==1 | Day==7, 1,0) %>% factor(levels = 0:1),
    "Date" = mdy(InjuryDate),
    "Time" = as.numeric(InjuryTime) %>% str_pad(.,4,"left",pad=0) %>% strptime("%H%M") %>% hour(),
    "Minute" = as.numeric(InjuryTime) %>% str_pad(.,4,"left",pad=0) %>% strptime("%H%M") %>% minute(),
    "ToD" = factor(case_when(Time %in% c(23,0:4) ~ "Night",
                             Time %in% 5:10 ~ "Morning",
                             Time %in% 11:16 ~ "Afternoon",
                             Time %in% 17:22 ~ "Evening"),
                   levels = c("Morning","Afternoon","Evening","Night")),
    "Night" = factor(if_else(Time %in% c(23,0,1,2,3,4), 1, 0), 0:1, c("No","Yes")),
    "Age" = case_when(as.numeric(AgeYears_c) < 15 ~ NA_real_, as.numeric(AgeYears_c) %in% 15:74 ~ as.numeric(AgeYears_c), as.numeric(AgeYears_c) > 74 ~ 75),
    "Age7cat" = factor(case_when(Age %in% 15:24 ~ "15-24",
                             Age %in% 25:34 ~ "25-34",
                             Age %in% 35:44 ~ "35-44",
                             Age %in% 45:54 ~ "45-54",
                             Age %in% 55:64 ~ "55-64",
                             Age %in% 65:74 ~ "65-74",
                             Age %in% 75    ~ "75+"),
                   levels = c("15-24","25-34","35-44","45-54","55-64","65-74","75+")),
    "Age3cat" = factor(case_when(Age %in% 15:34 ~ "Young",Age %in% 35:64 ~ "Middle",Age %in% 65:75 ~ "Older"),
                      levels = c("Middle","Young","Older")),
    "Sex" = factor(Sex, levels = c("Male","Female")),
    "Race" = factor(Race_c,
                    levels = c("White","Black or African American","Asian/Pacific Islander",
                               "American Indian/Alaska Native","Other/Unspecified","Two or more races"),
                    labels = c("White","Black","Asian","OtherRace","OtherRace","OtherRace")),
    "Ethnicity" = factor(Ethnicity, levels = c("Not hispanic","Hispanic"),
                         labels = c("Non-Hispanic","Hispanic")),
    "Education" = factor(EducationLevel, levels = c("8th grade or less","9th to 12th grade, no diploma","High school graduate or GED completed",
                                                    "Some college credit, but no degree","Associate's degree","Bachelor's degree","Master's degree",
                                                    "Doctorate or Professional degree"),
                         labels = c("Less than HS","Less than HS","HS","Some college","Associates","Bachelors","Postgraduate","Postgraduate")),
    "Alcohol" = case_when(AlcoholResult=="Not present" ~ "No", AlcoholResult=="Present"~"Yes") %>% factor(c("No","Yes")),
    "Marijuana" = case_when(MarijuanaResult=="Not present" ~ "No", MarijuanaResult=="Present"~"Yes")%>% factor(c("No","Yes")),
    "BAC" = case_when(BloodAlcoholContent_c == "Below the detection limit of the test (<0.01% or Non-detectable)" ~ 0,
                      BloodAlcoholContent_c == "Unknown" ~ NA_real_,
                      BloodAlcoholContent_c == "Not applicable, no testing" ~ NA_real_,
                      BloodAlcoholContent_c == "10" ~ NA_real_,
                      BloodAlcoholContent_c == "100" ~ NA_real_,
                      is.na(as.numeric(BloodAlcoholContent_c))==F ~ as.numeric(BloodAlcoholContent_c)),
    "BACcat" = factor(if_else(BAC>=0, 1, 0),0:1,c("No","Yes")),
    "BACcat2" = factor(case_when(BAC == 0 ~ "No Alcohol",
                                 BAC > 0 & BAC <0.08 ~ "< 0.08",
                                 BAC >=0.08 ~ ">=0.08"),
                       levels = c("No Alcohol","< 0.08",">=0.08"))
  )
# Separate data into suicides and homicides
suicide <- nvdrs %>% filter(Type=="Suicide" & is.na(Time)==F) 
homicide <- nvdrs %>% filter(Type=="Homicide" & is.na(Time)==F) 
# remove the overall dataset to save memory
rm(nvdrs)

#### Table 1 ####
# The resulting values were actually separated in Excel to create Tables 1 and 2 in the JCP paper. 
tbl1 <- tbl_merge(tab_spanner = c("Suicide","Homicide"), list(
  tbl_merge(list(
    tbl_summary(
      data = suicide %>% select(Age7cat,Sex,Race,Ethnicity,Military,BACcat2,Marijuana,
                                DepressedMood,PartnerConflict,PriorSI,PriorSA),
      statistic = list(all_continuous() ~ "{mean} ({sd})",
                       all_categorical() ~ "{n} ({p}%)"),
      type = list(Ethnicity ~ "categorical",Military ~ "categorical",Marijuana ~ "categorical",PartnerConflict ~ "categorical",
                  DepressedMood ~ "categorical",PriorSI ~ "categorical",PriorSA ~ "categorical"),
      digits = list(all_continuous() ~ c(1,2),
                    all_categorical() ~ c(0,1)),
      percent = "column"),
    tbl_summary(
      data = suicide %>% select(ToD,Age7cat,Sex,Race,Ethnicity,Military,BACcat2,Marijuana,
                                DepressedMood,PartnerConflict,PriorSI,PriorSA),
      by = "ToD",
      statistic = list(all_continuous() ~ "{mean} ({sd})",
                       all_categorical() ~ "{n} ({p}%)"),
      type = list(Ethnicity ~ "categorical",Military ~ "categorical",Marijuana ~ "categorical",PartnerConflict ~ "categorical",
                  DepressedMood ~ "categorical",PriorSI ~ "categorical",PriorSA ~ "categorical"),
      digits = list(all_continuous() ~ c(1,2),
                    all_categorical() ~ c(0,1)),
      percent = "column") %>% 
      add_p(test = list(all_continuous() ~ "aov",
                        all_categorical() ~ "chisq.test"))
  )),
  tbl_merge(list(
    tbl_summary(
      data = homicide %>% select(Age7cat,Sex,Race,Ethnicity,Military,BACcat2,Marijuana,
                                DepressedMood,PartnerConflict,PriorSI,PriorSA),
      statistic = list(all_continuous() ~ "{mean} ({sd})",
                       all_categorical() ~ "{n} ({p}%)"),
      type = list(Ethnicity ~ "categorical",Military ~ "categorical",Marijuana ~ "categorical",PartnerConflict ~ "categorical",
                  DepressedMood ~ "categorical",PriorSI ~ "categorical",PriorSA ~ "categorical"),
      digits = list(all_continuous() ~ c(1,2),
                    all_categorical() ~ c(0,1)),
      percent = "column"),
    tbl_summary(
      data = homicide %>% select(ToD,Age7cat,Sex,Race,Ethnicity,Military,BACcat2,Marijuana,
                                DepressedMood,PartnerConflict,PriorSI,PriorSA),
      by = "ToD",
      statistic = list(all_continuous() ~ "{mean} ({sd})",
                       all_categorical() ~ "{n} ({p}%)"),
      type = list(Ethnicity ~ "categorical",Military ~ "categorical",Marijuana ~ "categorical",PartnerConflict ~ "categorical",
                  DepressedMood ~ "categorical",PriorSI ~ "categorical",PriorSA ~ "categorical"),
      digits = list(all_continuous() ~ c(1,2),
                    all_categorical() ~ c(0,1)),
      percent = "column") %>% 
      add_p(test = list(all_continuous() ~ "aov",
                        all_categorical() ~ "chisq.test"))
  ))
))

# tbl1 %>% as_tibble() %>% 
# write_csv("c:/users/atala/sync/research/projects/to publish/nvdrs new data/submission/table1.csv")

#### Analysis 1) Hourly IRRs for all years ####
### Suicide ####
## Unadjusted Models ####
# Create summary dataset
suicide.tab.m1 <- suicide %>% 
  group_by(Time,Age7cat,Sex,Race,Ethnicity) %>% 
  summarize("Count" = n()) %>% 
  ungroup() %>%
  mutate("Time" = factor(Time, 0:23)) %>%
  left_join(.,atus.mins.base,by="Time") %>% 
  left_join(.,atus.mins.age,by=c("Time","Age7cat" = "Age")) %>%
  left_join(.,atus.mins.sex,by=c("Time","Sex")) %>%
  left_join(.,atus.mins.race,by=c("Time","Race")) %>%
  left_join(.,atus.mins.eth,by=c("Time","Ethnicity")) %>%
  rowwise() %>% 
  mutate("Mins" = sum(Intercept,MinsAge,MinsSex,MinsRace,MinsEthnicity,na.rm=T)) %>%
  select(-c(Intercept,MinsAge,MinsSex,MinsRace,MinsEthnicity))
# Create weighted effect coding to average across clock hours (rather than a reference hour)
suicide.tab.m1$Timewec2 <- suicide.tab.m1$Timewec1 <- suicide.tab.m1$Time
# Because WEC contrasts require one level to be omitted in each model, we have to duplicate steps to get all the levels (hours)
contrasts(suicide.tab.m1$Timewec1) <- contr.wec(suicide.tab.m1$Time,omitted = 12)
contrasts(suicide.tab.m1$Timewec2) <- contr.wec(suicide.tab.m1$Time,omitted = 0)
# Model IRRs for all hours in two parts: all hours except noon, and then just noon
suicide.m1 <- bind_rows(
  # coeftest allows use of sandwich errors
  coeftest(
    # model hourly counts as a function of weighted effect coded time with wakefulness as an offset variable using poisson distribution
    glm(Count ~ Timewec1+offset(log(Mins)),data=suicide.tab.m1,family="poisson"), 
    # apply sandwich estimator to the model to obtain robust standard errors
    sandwich(glm(Count ~ Timewec1+offset(log(Mins)),data=suicide.tab.m1,family="poisson"))) %>%
    # generate confidence intervals
    tidy(conf.int=T) %>% 
    # clean up the resulting tibble for use
    filter(str_detect(term, "Timewec1")==T) %>%
    transmute("Time" = term %>% str_remove("Timewec1"),
              "IRR" = estimate %>% exp() %>% round(.,2),
              "Lower" = conf.low %>% exp() %>% round(.,2),
              "Upper" = conf.high %>% exp() %>% round(.,2),
              "95% CI" = paste("[",Lower,", ",Upper, "]",sep=""),
              "P" = round(p.value, 4)
    ),
  # repeat the modeling scheme to include the 12 o'clock hour omitted in the previous model
  coeftest(glm(Count ~ Timewec2+offset(log(Mins)),data=suicide.tab.m1,family="poisson"), 
           sandwich(glm(Count ~ Timewec2+offset(log(Mins)),data=suicide.tab.m1,family="poisson"))) %>%
    tidy(conf.int=T) %>% filter(term=="Timewec212") %>%
    transmute("Time" = term %>% str_remove("Timewec2"),
              "IRR" = estimate %>% exp() %>% round(.,2),
              "Lower" = conf.low %>% exp() %>% round(.,2),
              "Upper" = conf.high %>% exp() %>% round(.,2),
              "95% CI" = paste("[",Lower,", ",Upper, "]",sep=""),
              "P" = round(p.value, 4)))  %>% 
  # Organize the resulting tibble and add in base wakefulness estimates
  mutate("Time" = factor(Time,0:23)) %>% 
  left_join(., suicide.tab.m1 %>% group_by(Time) %>% summarize(n=sum(Count)), by="Time") %>%
  left_join(., atus.mins.base,by="Time") %>%
  mutate("Mins" = Intercept,
         Intercept=NULL,
         "Count" = n/max(n))
## Adjusted for Age, Sex, Race, Ethnicity, Military, Depressed Mood, PartnerConflict, PriorSI, PriorSA ####
# Create summary dataset
suicide.tab.m2 <- suicide %>% 
  group_by(Time,Age7cat,Sex,Race,Ethnicity,Military,DepressedMood,PartnerConflict,PriorSI,PriorSA) %>% 
  summarize("Count" = n()) %>% 
  ungroup() %>%
  mutate("Time" = factor(Time, 0:23)) %>%
  left_join(.,atus.mins.base,by="Time") %>% 
  left_join(.,atus.mins.age,by=c("Time","Age7cat" = "Age")) %>%
  left_join(.,atus.mins.sex,by=c("Time","Sex")) %>%
  left_join(.,atus.mins.race,by=c("Time","Race")) %>%
  left_join(.,atus.mins.eth,by=c("Time","Ethnicity")) %>%
  rowwise() %>% 
  mutate("Mins" = sum(Intercept,MinsAge,MinsSex,MinsRace,MinsEthnicity,na.rm=T)) %>%
  select(-c(Intercept,MinsAge,MinsSex,MinsRace,MinsEthnicity))
# Create weighted effect coding
suicide.tab.m2$Timewec2 <- suicide.tab.m2$Timewec1 <- suicide.tab.m2$Time
contrasts(suicide.tab.m2$Timewec1) <- contr.wec(suicide.tab.m2$Time,omitted = 12)
contrasts(suicide.tab.m2$Timewec2) <- contr.wec(suicide.tab.m2$Time,omitted = 0)
# Model IRRs for all hours in two parts: all hours except noon, and then just noon
suicide.m2 <- bind_rows(
  # All hours except 12
  coeftest(glm(Count ~ Timewec1+Age7cat+Sex+Race+Ethnicity+Military+DepressedMood+PartnerConflict+PriorSI+PriorSA+offset(log(Mins)),data=suicide.tab.m2,family="poisson"), 
           sandwich(glm(Count ~ Timewec1+Age7cat+Sex+Race+Ethnicity+Military+DepressedMood+PartnerConflict+PriorSI+PriorSA+offset(log(Mins)),data=suicide.tab.m2,family="poisson"))) %>%
    tidy(conf.int=T) %>% filter(str_detect(term, "Timewec1")==T) %>%
    transmute("Time" = term %>% str_remove("Timewec1"),
              "IRR" = estimate %>% exp() %>% round(.,2),
              "Lower" = conf.low %>% exp() %>% round(.,2),
              "Upper" = conf.high %>% exp() %>% round(.,2),
              "95% CI" = paste("[",Lower,", ",Upper, "]",sep=""),
              "P" = round(p.value, 4)
    ),
  # Only Hour 12
  coeftest(glm(Count ~ Timewec2+Age7cat+Sex+Race+Ethnicity+Military+DepressedMood+PartnerConflict+PriorSI+PriorSA+offset(log(Mins)),data=suicide.tab.m2,family="poisson"), 
           sandwich(glm(Count ~ Timewec2+Age7cat+Sex+Race+Ethnicity+Military+DepressedMood+PartnerConflict+PriorSI+PriorSA+offset(log(Mins)),data=suicide.tab.m2,family="poisson"))) %>%
    tidy(conf.int=T) %>% filter(term=="Timewec212") %>%
    transmute("Time" = term %>% str_remove("Timewec2"),
              "IRR" = estimate %>% exp() %>% round(.,2),
              "Lower" = conf.low %>% exp() %>% round(.,2),
              "Upper" = conf.high %>% exp() %>% round(.,2),
              "95% CI" = paste("[",Lower,", ",Upper, "]",sep=""),
              "P" = round(p.value, 4)))  %>% 
  mutate("Time" = factor(Time,0:23)) %>% 
  left_join(., suicide.tab.m2 %>% group_by(Time) %>% summarize(n=sum(Count)), by="Time") %>%
  left_join(., atus.mins.base,by="Time") %>%
  mutate("Mins" = Intercept,
         Intercept=NULL,
         "Count" = n/max(n))

### Homicide ####
## Unadjusted Models ####
# Create summary dataset
homicide.tab.m1 <- homicide %>% 
  group_by(Time,Age7cat,Sex,Race,Ethnicity) %>% 
  summarize("Count" = n()) %>% 
  ungroup() %>%
  mutate("Time" = factor(Time, 0:23)) %>%
  left_join(.,atus.mins.base,by="Time") %>% 
  left_join(.,atus.mins.age,by=c("Time","Age7cat" = "Age")) %>%
  left_join(.,atus.mins.sex,by=c("Time","Sex")) %>%
  left_join(.,atus.mins.race,by=c("Time","Race")) %>%
  left_join(.,atus.mins.eth,by=c("Time","Ethnicity")) %>%
  rowwise() %>% 
  mutate("Mins" = sum(Intercept,MinsAge,MinsSex,MinsRace,MinsEthnicity,na.rm=T)) %>%
  select(-c(Intercept,MinsAge,MinsSex,MinsRace,MinsEthnicity))
# Create weighted effect coding
homicide.tab.m1$Timewec2 <- homicide.tab.m1$Timewec1 <- homicide.tab.m1$Time
contrasts(homicide.tab.m1$Timewec1) <- contr.wec(homicide.tab.m1$Time,omitted = 12)
contrasts(homicide.tab.m1$Timewec2) <- contr.wec(homicide.tab.m1$Time,omitted = 0)
# Model IRRs for all hours in two parts: all hours except noon, and then just noon
homicide.m1 <- bind_rows(
  coeftest(glm(Count ~ Timewec1+offset(log(Mins)),data=homicide.tab.m1,family="poisson"), 
           sandwich(glm(Count ~ Timewec1+offset(log(Mins)),data=homicide.tab.m1,family="poisson"))) %>%
    tidy(conf.int=T) %>% filter(str_detect(term, "Timewec1")==T) %>%
    transmute("Time" = term %>% str_remove("Timewec1"),
              "IRR" = estimate %>% exp() %>% round(.,2),
              "Lower" = conf.low %>% exp() %>% round(.,2),
              "Upper" = conf.high %>% exp() %>% round(.,2),
              "95% CI" = paste("[",Lower,", ",Upper, "]",sep=""),
              "P" = round(p.value, 4)
    ),
  coeftest(glm(Count ~ Timewec2+offset(log(Mins)),data=homicide.tab.m1,family="poisson"), 
           sandwich(glm(Count ~ Timewec2+offset(log(Mins)),data=homicide.tab.m1,family="poisson"))) %>%
    tidy(conf.int=T) %>% filter(term=="Timewec212") %>%
    transmute("Time" = term %>% str_remove("Timewec2"),
              "IRR" = estimate %>% exp() %>% round(.,2),
              "Lower" = conf.low %>% exp() %>% round(.,2),
              "Upper" = conf.high %>% exp() %>% round(.,2),
              "95% CI" = paste("[",Lower,", ",Upper, "]",sep=""),
              "P" = round(p.value, 4)))  %>% 
  mutate("Time" = factor(Time,0:23)) %>% 
  left_join(., homicide.tab.m1 %>% group_by(Time) %>% summarize(n=sum(Count)), by="Time") %>%
  left_join(., atus.mins.base,by="Time") %>%
  mutate("Mins" = Intercept,
         Intercept=NULL,
         "Count" = n/max(n))
## Adjusted for Age, Sex, Race, Ethnicity, Military, Depressed Mood, PartnerConflict, PriorSI, PriorSA ####
# Create summary dataset
homicide.tab.m2 <- homicide %>% 
  group_by(Time,Age7cat,Sex,Race,Ethnicity,Military,DepressedMood,PartnerConflict,PriorSI,PriorSA) %>% 
  summarize("Count" = n()) %>% 
  ungroup() %>%
  mutate("Time" = factor(Time, 0:23)) %>%
  left_join(.,atus.mins.base,by="Time") %>% 
  left_join(.,atus.mins.age,by=c("Time","Age7cat" = "Age")) %>%
  left_join(.,atus.mins.sex,by=c("Time","Sex")) %>%
  left_join(.,atus.mins.race,by=c("Time","Race")) %>%
  left_join(.,atus.mins.eth,by=c("Time","Ethnicity")) %>%
  rowwise() %>% 
  mutate("Mins" = sum(Intercept,MinsAge,MinsSex,MinsRace,MinsEthnicity,na.rm=T)) %>%
  select(-c(Intercept,MinsAge,MinsSex,MinsRace,MinsEthnicity))
# Create weighted effect coding
homicide.tab.m2$Timewec2 <- homicide.tab.m2$Timewec1 <- homicide.tab.m2$Time
contrasts(homicide.tab.m2$Timewec1) <- contr.wec(homicide.tab.m2$Time,omitted = 12)
contrasts(homicide.tab.m2$Timewec2) <- contr.wec(homicide.tab.m2$Time,omitted = 0)
# Model IRRs for all hours in two parts: all hours except noon, and then just noon
homicide.m2 <- bind_rows(
  # All hours except 12
  coeftest(glm(Count ~ Timewec1+Age7cat+Sex+Race+Ethnicity+Military+DepressedMood+PartnerConflict+PriorSI+PriorSA+offset(log(Mins)),data=homicide.tab.m2,family="poisson"), 
           sandwich(glm(Count ~ Timewec1+Age7cat+Sex+Race+Ethnicity+Military+DepressedMood+PartnerConflict+PriorSI+PriorSA+offset(log(Mins)),data=homicide.tab.m2,family="poisson"))) %>%
    tidy(conf.int=T) %>% filter(str_detect(term, "Timewec1")==T) %>%
    transmute("Time" = term %>% str_remove("Timewec1"),
              "IRR" = estimate %>% exp() %>% round(.,2),
              "Lower" = conf.low %>% exp() %>% round(.,2),
              "Upper" = conf.high %>% exp() %>% round(.,2),
              "95% CI" = paste("[",Lower,", ",Upper, "]",sep=""),
              "P" = round(p.value, 4)
    ),
  # Only Hour 12
  coeftest(glm(Count ~ Timewec2+Age7cat+Sex+Race+Ethnicity+Military+DepressedMood+PartnerConflict+PriorSI+PriorSA+offset(log(Mins)),data=homicide.tab.m2,family="poisson"), 
           sandwich(glm(Count ~ Timewec2+Age7cat+Sex+Race+Ethnicity+Military+DepressedMood+PartnerConflict+PriorSI+PriorSA+offset(log(Mins)),data=homicide.tab.m2,family="poisson"))) %>%
    tidy(conf.int=T) %>% filter(term=="Timewec212") %>%
    transmute("Time" = term %>% str_remove("Timewec2"),
              "IRR" = estimate %>% exp() %>% round(.,2),
              "Lower" = conf.low %>% exp() %>% round(.,2),
              "Upper" = conf.high %>% exp() %>% round(.,2),
              "95% CI" = paste("[",Lower,", ",Upper, "]",sep=""),
              "P" = round(p.value, 4)))  %>% 
  mutate("Time" = factor(Time,0:23)) %>% 
  left_join(., homicide.tab.m2 %>% group_by(Time) %>% summarize(n=sum(Count)), by="Time") %>%
  left_join(., atus.mins.base,by="Time") %>%
  mutate("Mins" = Intercept,
         Intercept=NULL,
         "Count" = n/max(n))

### Figure 1 ####
## Plot Suicide Model ####
suic.denom.m1 <- max(suicide.m1$Upper)
# Duplicate data for double plotting
suic.plot.table1 <- bind_rows(suicide.m1,
                              suicide.m1 %>% mutate(Time = factor(Time, 0:23,24:47)),
                              suicide.m1 %>% filter(Time==0) %>% mutate(Time = factor(Time, 0,48)))
suic.plot.table2 <- bind_rows(suicide.m2,
                              suicide.m2 %>% mutate(Time = factor(Time, 0:23,24:47)),
                              suicide.m2 %>% filter(Time==0) %>% mutate(Time = factor(Time, 0,48)))
# This code creates Figure 1A in the JCP paper
fig1.1 <- ggplot(suic.plot.table1, aes(x=Time))+theme_pubr()+
  geom_col(aes(y=Count*suic.denom.m1,fill="% Max Deaths"),width = .8)+
  geom_area(aes(y=Mins/60*suic.denom.m1,group=1,fill="% Population Wakefulness"),alpha=0.25,color="black",size=.1,show.legend = TRUE)+
  geom_point(aes(y=IRR,color="Unadjusted"),size=2)+
  geom_line(aes(y=IRR,color="Unadjusted",group=1),linewidth=1)+
  geom_ribbon(aes(y=IRR,ymin=Lower,ymax=Upper,group=1,color="Unadjusted"),size=0,fill="goldenrod2",alpha=0.5)+
  geom_point(data=suic.plot.table2,aes(y=IRR,color="Adjusted"),size=2)+
  geom_line(data=suic.plot.table2,aes(y=IRR,color="Adjusted",group=1),linewidth=1)+
  geom_ribbon(data=suic.plot.table2,aes(y=IRR,ymin=Lower,ymax=Upper,group=1,color="Adjusted"),size=0,fill="red3",alpha=0.25)+
  geom_hline(yintercept = 1,linetype=2,linewidth=1)+
  scale_fill_manual(name = "", values = c("% Suicides (scaled)" = "grey55",
                                          "% Population Wakefulness" = "dodgerblue"))+
  scale_color_manual("",values=c("Unadjusted" = "goldenrod2", "Adjusted" = "red3"))+
  scale_x_discrete(breaks = c(0,6,12,18,24,30,36,42,48),
                   labels = c("Midnight","6AM","Noon","6PM","Midnight","6AM","Noon","6PM","Midnight"))+
  scale_y_continuous(name = "Suicide IRR",
                     sec.axis = sec_axis(~./suic.denom.m1, name = "% Max Suicides / Wakefulness",
                                         labels = scales::percent,breaks = c(0,.25,.5,.75,1)))+
  xlab("")+guides(color=guide_legend(order=2,reverse = T),fill=guide_legend(order=1))

## Plot Homicide Model ####
hom.denom.m1 <- max(homicide.m1$Upper)
hom.plot.table1 <- bind_rows(homicide.m1,
                              homicide.m1 %>% mutate(Time = factor(Time, 0:23,24:47)),
                              homicide.m1 %>% filter(Time==0) %>% mutate(Time = factor(Time, 0,48)))
hom.plot.table2 <- bind_rows(homicide.m2,
                              homicide.m2 %>% mutate(Time = factor(Time, 0:23,24:47)),
                              homicide.m2 %>% filter(Time==0) %>% mutate(Time = factor(Time, 0,48)))
# This code creates Figure 1B in the paper
fig1.2 <- ggplot(hom.plot.table1, aes(x=Time))+theme_pubr()+
  geom_col(aes(y=Count*hom.denom.m1,fill="% Max Deaths"),width = .8)+
  geom_area(aes(y=Mins/60*hom.denom.m1,group=1,fill="% Population Wakefulness"),alpha=0.25,color="black",size=.1,show.legend = TRUE)+
  geom_point(aes(y=IRR,color="Unadjusted"),size=2)+
  geom_line(aes(y=IRR,color="Unadjusted",group=1),linewidth=1)+
  geom_ribbon(aes(y=IRR,ymin=Lower,ymax=Upper,group=1,color="Unadjusted"),size=0,fill="goldenrod2",alpha=0.5)+
  geom_point(data=hom.plot.table2,aes(y=IRR,color="Adjusted"),size=2)+
  geom_line(data=hom.plot.table2,aes(y=IRR,color="Adjusted",group=1),linewidth=1)+
  geom_ribbon(data=hom.plot.table2,aes(y=IRR,ymin=Lower,ymax=Upper,group=1,color="Adjusted"),size=0,fill="red3",alpha=0.25)+
  geom_hline(yintercept = 1,linetype=2,linewidth=1)+
  scale_fill_manual(name = "", values = c("% Max Deaths" = "grey55",
                                          "% Population Wakefulness" = "dodgerblue"))+
  scale_color_manual("",values=c("Unadjusted" = "goldenrod2", "Adjusted" = "red3"))+
  scale_x_discrete(breaks = c(0,6,12,18,24,30,36,42,48),
                   labels = c("Midnight","6AM","Noon","6PM","Midnight","6AM","Noon","6PM","Midnight"))+
  scale_y_continuous(name = "Homicide IRR",
                     sec.axis = sec_axis(~./hom.denom.m1, name = "% Max Homicide / Wakefulness",
                                         labels = scales::percent,breaks = c(0,.25,.5,.75,1)))+
  xlab("")+guides(color=guide_legend(order=2,reverse = T),fill=guide_legend(order=1))

# Model Table
# This code specifies the underlying values for Figures 1A and B and was intended to be included as a supplemental table except it cost money
# I can provide this table upon request. 
tblS1 <- bind_rows(crossing("Model" = "Suicide Unadjusted",suicide.m1),
                   crossing("Model" = "Suicide Adjusted",suicide.m2),
                   crossing("Model" = "Homicide Unadjusted (no-offset)", homicide.m1),
                   crossing("Model" = "Homicide Adjusted", homicide.m2))
# write_csv(tblS1, "c:/users/atala/sync/research/projects/to publish/nvdrs new data/submission/tableS1.csv")          

# Figure 1
# Combine 1A and 1B to make Figure 1
fig1 <- ggarrange(
  fig1.1,fig1.2,
  nrow=2,labels=c("A","B"),
  common.legend = T)

# Print Figure 1 to a TIFF file
# tiff("c:/users/atala/sync/research/projects/to publish/nvdrs new data/submission/fig1.tiff",
#      height=2100,width=3500,res=300)
# ggpar(fig1, font.tickslab = c(12,"plain","black"),
#             font.x = c(12,"plain","black"),
#             font.y = c(12,"plain","black"),
#       font.legend = c(12,"plain","black"))
# dev.off()

#### Analysis 2) Subgroup Testing ####
### Suicide ####
## ANOVA testing for subgroups ####
# Demographic subgroups
subgroup.suicpvals <- c()
suicide.demo <- suicide %>%
  group_by(Time,Age7cat,Sex,Race,Ethnicity) %>% 
  summarize("Count" = n()) %>% 
  ungroup() %>%
  mutate("Time" = factor(Time, 0:23)) %>%
  left_join(.,atus.mins.base,by="Time") %>% 
  left_join(.,atus.mins.age,by=c("Time","Age7cat" = "Age")) %>%
  left_join(.,atus.mins.sex,by=c("Time","Sex")) %>%
  left_join(.,atus.mins.race,by=c("Time","Race")) %>%
  left_join(.,atus.mins.eth,by=c("Time","Ethnicity")) %>%
  rowwise() %>% 
  mutate("Mins" = sum(Intercept,MinsAge,MinsSex,MinsRace,MinsEthnicity,na.rm=T),
         "Age7cat" = relevel(Age7cat,ref = "45-54")) %>%
  select(-c(Intercept,MinsAge,MinsSex,MinsRace,MinsEthnicity))
suicide.demo$Timewec2 <- suicide.demo$Timewec1 <- suicide.demo$Time
contrasts(suicide.demo$Timewec1) <- contr.wec(suicide.demo$Time,omitted = 12)
contrasts(suicide.demo$Timewec2) <- contr.wec(suicide.demo$Time,omitted = 0)
# For convenience, two-way ANOVAs were tested using the quasipoisson distribution (which uses a dispersion parameter to get 
# around model misspecification) to test for significant interactions rather than using sandwich estimators. 
# Age 
sub.age <- anova(glm(Count ~ Timewec1*Age7cat+Sex+Race+Ethnicity+Mins,data=suicide.demo,family="quasipoisson"),test="Chisq") 
subgroup.suicpvals <- c(subgroup.suicpvals,last(sub.age$`Pr(>Chi)`))
# Sex 
sub.sex <- anova(glm(Count ~ Timewec1*Sex+Age7cat+Race+Ethnicity+Mins,data=suicide.demo,family="quasipoisson"),test="Chisq") 
subgroup.suicpvals <- c(subgroup.suicpvals,last(sub.sex$`Pr(>Chi)`))
# Race
sub.race <- anova(glm(Count ~ Timewec1*Race+Age7cat+Sex+Ethnicity+Mins,data=suicide.demo,family="quasipoisson"),test="Chisq") 
subgroup.suicpvals <- c(subgroup.suicpvals,last(sub.race$`Pr(>Chi)`))
# Ethnicity 
sub.eth <- anova(glm(Count ~ Timewec1*Ethnicity+Age7cat+Sex+Race+Mins,data=suicide.demo,family="quasipoisson"),test="Chisq") 
subgroup.suicpvals <- c(subgroup.suicpvals,last(sub.eth$`Pr(>Chi)`))
## Clinical Subgroups
# This code uses a loop to sequentially test each of the following models in the "subgroups" object
subgroups <- c("Military","BACcat2","Marijuana","DepressedMood","PartnerConflict","PriorSI","PriorSA")
temp1 <- tibble()
for(idx in 1:length(subgroups)){
  temp1 <- suicide %>%
    group_by(Time,Age7cat,Sex,Race,Ethnicity,!!as.symbol(subgroups[idx])) %>% 
    summarize("Count" = n()) %>% 
    ungroup() %>%
    mutate("Time" = factor(Time, 0:23)) %>%
    left_join(.,atus.mins.base,by="Time") %>% 
    left_join(.,atus.mins.age,by=c("Time","Age7cat" = "Age")) %>%
    left_join(.,atus.mins.sex,by=c("Time","Sex")) %>%
    left_join(.,atus.mins.race,by=c("Time","Race")) %>%
    left_join(.,atus.mins.eth,by=c("Time","Ethnicity")) %>%
    rowwise() %>% 
    mutate("Mins" = sum(Intercept,MinsAge,MinsSex,MinsRace,MinsEthnicity,na.rm=T)) %>%
    select(-c(Intercept,MinsAge,MinsSex,MinsRace,MinsEthnicity))
  temp1$Timewec2 <- temp1$Timewec1 <- temp1$Time
  contrasts(temp1$Timewec1) <- contr.wec(temp1$Time,omitted = 12)
  contrasts(temp1$Timewec2) <- contr.wec(temp1$Time,omitted = 0)
  temp2 <- anova(glm(as.formula(paste0(
    "Count ~ Timewec1*",subgroups[idx],"+Age7cat+Sex+Race+Ethnicity+Mins")),
    data=temp1,family="quasipoisson"),test="Chisq")
  subgroup.suicpvals <- c(subgroup.suicpvals,last(temp2$`Pr(>Chi)`))
}
# This aggregated all of the ANOVA results into a single tibble and then adjusted for multiple comparisons to identify 
# significant interactions which are tested below. 
subgroup.suicsum <- tibble("Subgroup" = c("Age","Sex","Race","Ethnicity","Military","BACcat2","Marijuana",
                      "DepressedMood","PartnerConflict","PriorSI","PriorSA"),
       "Pval" = p.adjust(subgroup.suicpvals,method="holm"))

## Individual testing of Age, Ethnicity, BACcat2, PartnerConflict ####
# Coding loops allow for measurement of effects at all time by covariate levels
# Age
agemod1 <- glm(Count ~ Timewec1*Age7cat+Sex+Race+Ethnicity+offset(log(Mins)),data=suicide.demo,family="poisson")
agemod2 <- glm(Count ~ Timewec2*Age7cat+Sex+Race+Ethnicity+offset(log(Mins)),data=suicide.demo,family="poisson")
agelevels <- paste("Age7cat",levels(suicide.demo$Age7cat),sep="")[-1]
timelevels <- paste("Timewec1",levels(suicide.demo$Timewec1),sep="")[-13]
agesubs <- tibble()
for(jdx in 1:length(agelevels)){
  for(idx in 1:length(timelevels)){
    agesubs <- bind_rows(
      agesubs,
      tibble("Age" = str_remove(agelevels[jdx],"Age7cat"),
             "Time" = timelevels[idx],
             deltaMethod(
               agemod1,vcov. = sandwich(agemod1),
               g.= paste0("`",timelevels[idx],":",agelevels[jdx],"`")
             )
      )
    )
  }
  agesubs <- bind_rows(
    agesubs,
    tibble("Age" = str_remove(agelevels[jdx],"Age7cat"),
           "Time" = "Timewec112",
           deltaMethod(
             agemod2, vcov. = sandwich(agemod2),
             g.= paste0("`Timewec212:",agelevels[jdx],"`")
           )
    )
  )
}
agesubs.pdat <- agesubs %>% 
  transmute(Age = factor(Age),
            Time = Time %>% str_remove(., "Timewec1"),
            IRR = round(exp(Estimate),2),
            Lower = round(exp(`2.5 %`),2),
            Upper = round(exp(`97.5 %`),2),
            `95% CI` = paste0("[",Lower,", ",Upper,"]")
  ) %>% mutate(Time = factor(Time, levels = c(0:23))) %>% 
  left_join(., suicide.demo %>% group_by(Time,Age7cat) %>% summarize(n=sum(Count)), by=c("Time","Age" = "Age7cat")) %>%
  left_join(., atus.mins.base,by="Time") %>%
  left_join(., atus.mins.age,by=c("Time","Age")) %>%
  group_by(Age) %>% 
  mutate("Mins" = Intercept+MinsAge,
         Intercept=NULL,MinsAge=NULL,
         "Count" = n/max(n))
# Plot
# Code to create Figure 2A
agemod.p1 <- ggplot(agesubs.pdat %>% filter(Age %in% c("15-24","25-34")), aes(x=Time,group=Age))+theme_pubr()+
  geom_point(aes(y=IRR,color=Age,group=Age),size=2)+
  geom_line(aes(y=IRR,color=Age,group=Age),linewidth=1)+
  geom_ribbon(aes(y=IRR,ymin=Lower,ymax=Upper,color=Age,fill=Age,group=Age),size=0,alpha=0.25)+
  scale_x_discrete(breaks = c(0,4,8,12,16,20,23),
                   labels = c("12AM","4AM","8AM","12PM","4PM","8PM","11PM"))+
  scale_discrete_manual(name = "",
                        labels = c("15-24","25-34"),
                        values = c("seagreen4","goldenrod3"),
                        aesthetics = c("color","fill"))+
  geom_hline(aes(yintercept = 1,linetype="45-54"),size=1)+
  scale_linetype_manual(values = c(`45-54` = "dashed"))+
  guides(linetype = guide_legend("",order = 1))+
  xlab("")+ ylab("Marginal IRR")+ylim(0,4.5)
# Figure 2B
agemod.p2 <-ggplot(agesubs.pdat %>% filter(Age %in% c("35-44","55-64")), aes(x=Time,group=Age))+theme_pubr()+
  geom_point(aes(y=IRR,color=Age,group=Age),size=2)+
  geom_line(aes(y=IRR,color=Age,group=Age),linewidth=1)+
  geom_ribbon(aes(y=IRR,ymin=Lower,ymax=Upper,color=Age,fill=Age,group=Age),size=0,alpha=0.25)+
  scale_x_discrete(breaks = c(0,4,8,12,16,20,23),
                   labels = c("12AM","4AM","8AM","12PM","4PM","8PM","11PM"))+
  scale_discrete_manual(name = "",
                        labels = c("35-44","55-64"),
                        values = c("darkorange3","red3"),
                        aesthetics = c("color","fill"))+
  geom_hline(aes(yintercept = 1,linetype="45-54"),size=1)+
  scale_linetype_manual(values = c(`45-54` = "dashed"))+
  guides(linetype = guide_legend("",order=1))+
  xlab("")+ ylab("Marginal IRR")+ylim(0,4.5)
#Figure 2C
agemod.p3 <-ggplot(agesubs.pdat %>% filter(Age %in% c("65-74","75+")), aes(x=Time,group=Age))+theme_pubr()+
  geom_point(aes(y=IRR,color=Age,group=Age),size=2)+
  geom_line(aes(y=IRR,color=Age,group=Age),linewidth=1)+
  geom_ribbon(aes(y=IRR,ymin=Lower,ymax=Upper,color=Age,fill=Age,group=Age),size=0,alpha=0.25)+
  scale_x_discrete(breaks = c(0,4,8,12,16,20,23),
                   labels = c("12AM","4AM","8AM","12PM","4PM","8PM","11PM"))+
  scale_discrete_manual(name = "",
                        labels = c("65-74","75+"),
                        values = c("deeppink3","darkorchid3"),
                        aesthetics = c("color","fill"))+
  geom_hline(aes(yintercept = 1,linetype="45-54"),size=1)+
  scale_linetype_manual(values = c(`45-54` = "dashed"))+
  guides(linetype = guide_legend("",order=1))+
  xlab("")+ ylab("Marginal IRR")+ylim(0,4.5)

## Ethnicity
ethmod1 <- glm(Count ~ Timewec1*Ethnicity+Age7cat+Sex+Race+offset(log(Mins)),data=suicide.demo,family="poisson")
ethmod2 <- glm(Count ~ Timewec2*Ethnicity+Age7cat+Sex+Race+offset(log(Mins)),data=suicide.demo,family="poisson")
ethlevels <- paste("Ethnicity",levels(suicide.demo$Ethnicity),sep="")[-1]
timelevels <- paste("Timewec1",levels(suicide.demo$Timewec1),sep="")[-13]
ethsubs <- tibble()
for(jdx in 1:length(ethlevels)){
  for(idx in 1:length(timelevels)){
    ethsubs <- bind_rows(
      ethsubs,
      tibble("Ethnicity" = str_remove(ethlevels[jdx],"Ethnicity"),
             "Time" = timelevels[idx],
             deltaMethod(
               ethmod1, vcov. = sandwich(ethmod1),
               g.= paste0("`",timelevels[idx],":",ethlevels[jdx],"`")
             )
      )
    )
  }
  ethsubs <- bind_rows(
    ethsubs,
    tibble("Ethnicity" = str_remove(ethlevels[jdx],"Ethnicity"),
           "Time" = "Timewec112",
           deltaMethod(
             ethmod2, vcov. = sandwich(ethmod2),
             g.= paste0("`Timewec212:",ethlevels[jdx],"`")
           )
    )
  )
}
ethsubs.pdat <- ethsubs %>%
  transmute(Ethnicity = factor(Ethnicity, c("Non-Hispanic","Hispanic")),
            Time = Time %>% str_remove(., "Timewec1") %>% factor(.,0:23),
            IRR = round(exp(Estimate),2),
            Lower = round(exp(`2.5 %`),2),
            Upper = round(exp(`97.5 %`),2),
            `95% CI` = paste0("[",Lower,", ",Upper,"]")
  ) %>% mutate(Time = factor(Time, levels = c(0:23))) %>% 
  left_join(., suicide.demo %>% group_by(Time,Ethnicity) %>% summarize(n=sum(Count)), by=c("Time","Ethnicity")) %>%
  left_join(., atus.mins.base,by="Time") %>%
  left_join(., atus.mins.eth,by=c("Time","Ethnicity")) %>%
  group_by(Ethnicity) %>% 
  mutate("Mins" = Intercept+MinsEthnicity,
         Intercept=NULL,MinsEthnicity=NULL,
         "Count" = n/max(n))
# Figure 2D
ethmod.p <- ggplot(ethsubs.pdat, aes(x=Time,group=Ethnicity))+theme_pubr()+
  geom_point(aes(y=IRR,color=Ethnicity,group=Ethnicity),size=2)+
  geom_line(aes(y=IRR,color=Ethnicity,group=Ethnicity),size=1)+
  geom_ribbon(aes(y=IRR,ymin=Lower,ymax=Upper,color=Ethnicity,fill=Ethnicity),size=0,alpha=0.25)+
  scale_x_discrete(breaks = c(0,4,8,12,16,20,23),
                   labels = c("12AM","4AM","8AM","12PM","4PM","8PM","11PM"))+
  scale_discrete_manual(name = "",
                        labels = "Hispanic",
                        values = "peru",
                        aesthetic = c('fill','color'))+
  geom_hline(aes(yintercept = 1,linetype="Non-Hispanic"),size=1)+
  scale_linetype_manual(values = c(`Non-Hispanic` = "dashed"))+
  guides(linetype = guide_legend("",order = 1))+
  xlab("")+ ylab("Marginal IRR")+ylim(0,3.5)

## Blood Alcohol Level
bacsum <- suicide %>%
  group_by(Time,Age7cat,Sex,Race,Ethnicity,BACcat2) %>% 
  summarize("Count" = n()) %>% 
  ungroup() %>%
  mutate("Time" = factor(Time, 0:23)) %>%
  left_join(.,atus.mins.base,by="Time") %>% 
  left_join(.,atus.mins.age,by=c("Time","Age7cat" = "Age")) %>%
  left_join(.,atus.mins.sex,by=c("Time","Sex")) %>%
  left_join(.,atus.mins.race,by=c("Time","Race")) %>%
  left_join(.,atus.mins.eth,by=c("Time","Ethnicity")) %>%
  rowwise() %>% 
  mutate("Mins" = sum(Intercept,MinsAge,MinsSex,MinsRace,MinsEthnicity,na.rm=T)) %>%
  select(-c(Intercept,MinsAge,MinsSex,MinsRace,MinsEthnicity))
bacsum$Timewec2 <- bacsum$Timewec1 <- bacsum$Time
contrasts(bacsum$Timewec1) <- contr.wec(bacsum$Time,omitted = 12)
contrasts(bacsum$Timewec2) <- contr.wec(bacsum$Time,omitted = 0)
bacmod1 <- glm(Count ~ Timewec1*BACcat2+Ethnicity+Age7cat+Sex+Race+offset(log(Mins)),data=bacsum,family="poisson")
bacmod2 <- glm(Count ~ Timewec2*BACcat2+Ethnicity+Age7cat+Sex+Race+offset(log(Mins)),data=bacsum,family="poisson")
baclevels <- paste("BACcat2",levels(bacsum$BACcat2),sep="")[-1]
timelevels <- paste("Timewec1",levels(bacsum$Timewec1),sep="")[-13]
bacsubs <- tibble()
for(jdx in 1:length(baclevels)){
  for(idx in 1:length(timelevels)){
    bacsubs <- bind_rows(
      bacsubs,
      tibble("BACcat2" = str_remove(baclevels[jdx],"BACcat2"),
             "Time" = timelevels[idx],
             deltaMethod(
               bacmod1, vcov. = sandwich(bacmod1),
               g.= paste0("`",timelevels[idx],":",baclevels[jdx],"`")
             )
      )
    )
  }
  bacsubs <- bind_rows(
    bacsubs,
    tibble("BACcat2" = str_remove(baclevels[jdx],"BACcat2"),
           "Time" = "Timewec112",
           deltaMethod(
             bacmod2, vcov. = sandwich(bacmod2),
             g.= paste0("`Timewec212:",baclevels[jdx],"`")
           )
    )
  )
}
bacsubs.pdat <- bacsubs %>%
  transmute(BACcat2 = factor(BACcat2, c("< 0.08",">=0.08")),
            Time = Time %>% str_remove(., "Timewec1") %>% factor(.,0:23),
            IRR = round(exp(Estimate),2),
            Lower = round(exp(`2.5 %`),2),
            Upper = round(exp(`97.5 %`),2),
            `95% CI` = paste0("[",Lower,", ",Upper,"]")
  ) %>% mutate(Time = factor(Time, levels = c(0:23))) %>% 
  left_join(., bacsum %>% group_by(Time,BACcat2) %>% summarize(n=sum(Count)), by=c("Time","BACcat2")) %>%
  left_join(., atus.mins.base,by="Time") %>%
  group_by(BACcat2) %>% 
  mutate(BACcat2 = factor(BACcat2, c("< 0.08",">=0.08"),c("< 80 mg/dL",">= 80 mg/dL")),
         "Mins" = Intercept,
         Intercept=NULL,
         "Count" = n/max(n))
# Figure 2E
bacmod.p <- ggplot(bacsubs.pdat, aes(x=Time,group=BACcat2))+theme_pubr()+
  geom_point(aes(y=IRR,color=BACcat2,group=BACcat2),size=2)+
  geom_line(aes(y=IRR,color=BACcat2,group=BACcat2),linewidth=1)+
  geom_ribbon(aes(y=IRR,ymin=Lower,ymax=Upper,color=BACcat2,fill=BACcat2),size=0,alpha=0.25)+
  scale_x_discrete(breaks = c(0,4,8,12,16,20,23),
                   labels = c("12AM","4AM","8AM","12PM","4PM","8PM","11PM"))+
  scale_discrete_manual(name = "",
                        labels = c("< 80 mg/dL",">= 80 mg/dL"),
                        values = c("darkcyan","firebrick3"),
                        aesthetics = c("color","fill"))+
  geom_hline(aes(yintercept = 1,linetype="No Alcohol"),size=1)+
  scale_linetype_manual(values = c(`No Alcohol` = "dashed"))+
  guides(linetype = guide_legend("",order = 1))+
  xlab("")+ ylab("Marginal IRR")+ylim(0,4.5)

## Partner Conflict
ptnrsum <- suicide %>%
  group_by(Time,Age7cat,Sex,Race,Ethnicity,PartnerConflict) %>% 
  summarize("Count" = n()) %>% 
  ungroup() %>%
  mutate("Time" = factor(Time, 0:23)) %>%
  left_join(.,atus.mins.base,by="Time") %>% 
  left_join(.,atus.mins.age,by=c("Time","Age7cat" = "Age")) %>%
  left_join(.,atus.mins.sex,by=c("Time","Sex")) %>%
  left_join(.,atus.mins.race,by=c("Time","Race")) %>%
  left_join(.,atus.mins.eth,by=c("Time","Ethnicity")) %>%
  rowwise() %>% 
  mutate("Mins" = sum(Intercept,MinsAge,MinsSex,MinsRace,MinsEthnicity,na.rm=T),
         "PartnerConflict" = factor(PartnerConflict,0:1)) %>%
  select(-c(Intercept,MinsAge,MinsSex,MinsRace,MinsEthnicity))
ptnrsum$Timewec2 <- ptnrsum$Timewec1 <- ptnrsum$Time
contrasts(ptnrsum$Timewec1) <- contr.wec(ptnrsum$Time,omitted = 12)
contrasts(ptnrsum$Timewec2) <- contr.wec(ptnrsum$Time,omitted = 0)
ptnrmod1 <- glm(Count ~ Timewec1*PartnerConflict+Ethnicity+Age7cat+Sex+Race+offset(log(Mins)),data=ptnrsum,family="poisson")
ptnrmod2 <- glm(Count ~ Timewec2*PartnerConflict+Ethnicity+Age7cat+Sex+Race+offset(log(Mins)),data=ptnrsum,family="poisson")
ptnrlevels <- "PartnerConflict1"
timelevels <- paste("Timewec1",levels(ptnrsum$Timewec1),sep="")[-13]
ptnrsubs <- tibble()
for(jdx in 1:length(ptnrlevels)){
  for(idx in 1:length(timelevels)){
    ptnrsubs <- bind_rows(
      ptnrsubs,
      tibble("PartnerConflict" = str_remove(ptnrlevels[jdx],"PartnerConflict"),
             "Time" = timelevels[idx],
             deltaMethod
             (ptnrmod1, vcov. = sandwich(ptnrmod1),
               g.= paste0("`",timelevels[idx],":",ptnrlevels[jdx],"`")
             )
      )
    )
  }
  ptnrsubs <- bind_rows(
    ptnrsubs,
    tibble("PartnerConflict" = str_remove(ptnrlevels[jdx],"PartnerConflict"),
           "Time" = "Timewec112",
           deltaMethod(
             ptnrmod2, vcov. = sandwich(ptnrmod2),
             g.= paste0("`Timewec212:",ptnrlevels[jdx],"`")
           )
    )
  )
}
ptnrsubs.pdat <- ptnrsubs %>%
  transmute(PartnerConflict = factor(PartnerConflict),
            Time = Time %>% str_remove(., "Timewec1") %>% factor(.,0:23),
            IRR = round(exp(Estimate),2),
            Lower = round(exp(`2.5 %`),2),
            Upper = round(exp(`97.5 %`),2),
            `95% CI` = paste0("[",Lower,", ",Upper,"]")
  ) %>% mutate(Time = factor(Time, levels = c(0:23)))%>% 
  left_join(., ptnrsum %>% group_by(Time,PartnerConflict) %>% summarize(n=sum(Count)), by=c("Time","PartnerConflict")) %>%
  left_join(., atus.mins.base,by="Time") %>%
  group_by(PartnerConflict) %>% 
  mutate(PartnerConflict = factor(PartnerConflict, 1,"Partner Conflict"),
         "Mins" = Intercept,
         Intercept=NULL,
         "Count" = n/max(n))
# Figure 2F
ptnrmod.p <- ggplot(ptnrsubs.pdat, aes(x=Time,group=PartnerConflict))+theme_pubr()+
  geom_point(aes(y=IRR,color=PartnerConflict),size=2)+
  geom_line(aes(y=IRR,color=PartnerConflict),size=1)+
  geom_ribbon(aes(y=IRR,ymin=Lower,ymax=Upper,color=PartnerConflict,fill=PartnerConflict),size=0,alpha=0.25)+
  scale_x_discrete(breaks = c(0,4,8,12,16,20,23),
                   labels = c("12AM","4AM","8AM","12PM","4PM","8PM","11PM"))+
  scale_discrete_manual(name = "",
                        values = "darkblue",
                        labels = "Partner Conflict",
                        aesthetics = c("color","fill"))+
  geom_hline(aes(yintercept = 1,linetype="No Partner Conflict"),size=1)+
  scale_linetype_manual(values = c(`No Partner Conflict` = "dashed"))+
  guides(linetype = guide_legend("",order = 1))+
  xlab("")+ ylab("Marginal IRR")+ylim(0,4.5)

### Figure 2 ####
# Arrange figures and print to TIFF file
fig2 <- ggarrange(ggpar(agemod.p1,xlim=c(1,24.5)),ggpar(agemod.p2,xlim=c(1,24.5)),ggpar(agemod.p3,xlim=c(1,24.5)),
                  ggpar(ethmod.p,xlim=c(1,24.5),ylim=c(0,4)),ggpar(bacmod.p,xlim=c(1,24.5)),ggpar(ptnrmod.p,xlim=c(1,24.5)),
                  nrow=2,ncol=3,labels = "AUTO")
# tiff("c:/users/atala/sync/research/projects/to publish/nvdrs new data/submission/fig2.tiff",
#      height=2100,width=4000,res=300)
# fig2
# dev.off()

### Homicide ####
## ANOVA testing for subgroups ####
# Demographic subgroups
subgroup.hompvals <- c()
homicide.demo <- homicide %>%
  group_by(Time,Age7cat,Sex,Race,Ethnicity) %>% 
  summarize("Count" = n()) %>% 
  ungroup() %>%
  mutate("Time" = factor(Time, 0:23)) %>%
  left_join(.,atus.mins.base,by="Time") %>% 
  left_join(.,atus.mins.age,by=c("Time","Age7cat" = "Age")) %>%
  left_join(.,atus.mins.sex,by=c("Time","Sex")) %>%
  left_join(.,atus.mins.race,by=c("Time","Race")) %>%
  left_join(.,atus.mins.eth,by=c("Time","Ethnicity")) %>%
  rowwise() %>% 
  mutate("Mins" = sum(Intercept,MinsAge,MinsSex,MinsRace,MinsEthnicity,na.rm=T),
         "Age7cat" = relevel(Age7cat,ref = "45-54")) %>%
  select(-c(Intercept,MinsAge,MinsSex,MinsRace,MinsEthnicity))
homicide.demo$Timewec2 <- homicide.demo$Timewec1 <- homicide.demo$Time
contrasts(homicide.demo$Timewec1) <- contr.wec(homicide.demo$Time,omitted = 12)
contrasts(homicide.demo$Timewec2) <- contr.wec(homicide.demo$Time,omitted = 0)
# Age 
sub.age <- anova(glm(Count ~ Timewec1*Age7cat+Sex+Race+Ethnicity+Mins,data=homicide.demo,family="quasipoisson"),test="Chisq") 
subgroup.hompvals <- c(subgroup.hompvals,last(sub.age$`Pr(>Chi)`))
# Sex 
sub.sex <- anova(glm(Count ~ Timewec1*Sex+Age7cat+Race+Ethnicity+Mins,data=homicide.demo,family="quasipoisson"),test="Chisq") 
subgroup.hompvals <- c(subgroup.hompvals,last(sub.sex$`Pr(>Chi)`))
# Race
sub.race <- anova(glm(Count ~ Timewec1*Race+Age7cat+Sex+Ethnicity+Mins,data=homicide.demo,family="quasipoisson"),test="Chisq") 
subgroup.hompvals <- c(subgroup.hompvals,last(sub.race$`Pr(>Chi)`))
# Ethnicity 
sub.eth <- anova(glm(Count ~ Timewec1*Ethnicity+Age7cat+Sex+Race+Mins,data=homicide.demo,family="quasipoisson"),test="Chisq") 
subgroup.hompvals <- c(subgroup.hompvals,last(sub.eth$`Pr(>Chi)`))
## Clinical Subgroups
subgroups <- c("Military","BACcat2","Marijuana","DepressedMood","PartnerConflict","PriorSI","PriorSA")
temp1 <- tibble()
for(idx in 1:length(subgroups)){
  temp1 <- homicide %>%
    group_by(Time,Age7cat,Sex,Race,Ethnicity,!!as.symbol(subgroups[idx])) %>% 
    summarize("Count" = n()) %>% 
    ungroup() %>%
    mutate("Time" = factor(Time, 0:23)) %>%
    left_join(.,atus.mins.base,by="Time") %>% 
    left_join(.,atus.mins.age,by=c("Time","Age7cat" = "Age")) %>%
    left_join(.,atus.mins.sex,by=c("Time","Sex")) %>%
    left_join(.,atus.mins.race,by=c("Time","Race")) %>%
    left_join(.,atus.mins.eth,by=c("Time","Ethnicity")) %>%
    rowwise() %>% 
    mutate("Mins" = sum(Intercept,MinsAge,MinsSex,MinsRace,MinsEthnicity,na.rm=T)) %>%
    select(-c(Intercept,MinsAge,MinsSex,MinsRace,MinsEthnicity))
  temp1$Timewec2 <- temp1$Timewec1 <- temp1$Time
  contrasts(temp1$Timewec1) <- contr.wec(temp1$Time,omitted = 12)
  contrasts(temp1$Timewec2) <- contr.wec(temp1$Time,omitted = 0)
  temp2 <- anova(glm(as.formula(paste0(
    "Count ~ Timewec1*",subgroups[idx],"+Age7cat+Sex+Race+Ethnicity+Mins")),
    data=temp1,family="quasipoisson"),test="Chisq")
  subgroup.hompvals <- c(subgroup.hompvals,last(temp2$`Pr(>Chi)`))
}
subgroup.hom <- tibble("Subgroup" = c("Age","Sex","Race","Ethnicity","Military","BACcat2","Marijuana",
                                      "DepressedMood","PartnerConflict","PriorSI","PriorSA"),
                       "Pval" = p.adjust(subgroup.hompvals,method="holm"))

## Sex
sexmod1 <- glm(Count ~ Timewec1*Sex+Age7cat+Ethnicity+Race+offset(log(Mins)),data=homicide.demo,family="poisson")
sexmod2 <- glm(Count ~ Timewec2*Sex+Age7cat+Ethnicity+Race+offset(log(Mins)),data=homicide.demo,family="poisson")
sexlevels <- paste("Sex",levels(homicide.demo$Sex),sep="")[-1]
timelevels <- paste("Timewec1",levels(homicide.demo$Timewec1),sep="")[-13]
sexsubs <- tibble()
for(jdx in 1:length(sexlevels)){
  for(idx in 1:length(timelevels)){
    sexsubs <- bind_rows(
      sexsubs,
      tibble("Sex" = str_remove(sexlevels[jdx],"Sex"),
             "Time" = timelevels[idx],
             deltaMethod(
               sexmod1, vcov. = sandwich(sexmod1),
               g.= paste0("`",timelevels[idx],":",sexlevels[jdx],"`")
             )
      )
    )
  }
  sexsubs <- bind_rows(
    sexsubs,
    tibble("Sex" = str_remove(sexlevels[jdx],"Sex"),
           "Time" = "Timewec112",
           deltaMethod(
             sexmod2, vcov. = sandwich(sexmod2),
             g.= paste0("`Timewec212:",sexlevels[jdx],"`")
           )
    )
  )
}
sexsubs.pdat <- sexsubs %>%
  transmute(Sex = factor(Sex, c("Male","Female")),
            Time = Time %>% str_remove(., "Timewec1") %>% factor(.,0:23),
            IRR = round(exp(Estimate),2),
            Lower = round(exp(`2.5 %`),2),
            Upper = round(exp(`97.5 %`),2),
            `95% CI` = paste0("[",Lower,", ",Upper,"]")
  ) %>% mutate(Time = factor(Time, levels = c(0:23))) %>% 
  left_join(., suicide.demo %>% group_by(Time,Sex) %>% summarize(n=sum(Count)), by=c("Time","Sex")) %>%
  left_join(., atus.mins.base,by="Time") %>%
  left_join(., atus.mins.sex,by=c("Time","Sex")) %>%
  group_by(Sex) %>% 
  mutate("Mins" = Intercept+MinsSex,
         Intercept=NULL,MinsSex=NULL,
         "Count" = n/max(n))
# Figure 3A
sexmod.p <- ggplot(sexsubs.pdat, aes(x=Time,group=Sex))+theme_pubr()+
  geom_point(aes(y=IRR,color=Sex,group=Sex),size=2)+
  geom_line(aes(y=IRR,color=Sex,group=Sex),size=1)+
  geom_ribbon(aes(y=IRR,ymin=Lower,ymax=Upper,color=Sex,fill=Sex),size=0,alpha=0.25)+
  scale_x_discrete(breaks = c(0,4,8,12,16,20,23),
                   labels = c("12AM","4AM","8AM","12PM","4PM","8PM","11PM"))+
  scale_discrete_manual(name = "",
                        labels = "Female",
                        values = "violetred1",
                        aesthetic = c('fill','color'))+
  geom_hline(aes(yintercept = 1,linetype="Male"),size=1)+
  scale_linetype_manual(values = c(`Male` = "dashed"))+
  guides(linetype = guide_legend("",order = 1))+
  xlab("")+ ylab("Marginal IRR")+ylim(0,3.5)

## Blood Alcohol Level
bacsum.hom <- homicide %>%
  group_by(Time,Age7cat,Sex,Race,Ethnicity,BACcat2) %>% 
  summarize("Count" = n()) %>% 
  ungroup() %>%
  mutate("Time" = factor(Time, 0:23)) %>%
  left_join(.,atus.mins.base,by="Time") %>% 
  left_join(.,atus.mins.age,by=c("Time","Age7cat" = "Age")) %>%
  left_join(.,atus.mins.sex,by=c("Time","Sex")) %>%
  left_join(.,atus.mins.race,by=c("Time","Race")) %>%
  left_join(.,atus.mins.eth,by=c("Time","Ethnicity")) %>%
  rowwise() %>% 
  mutate("Mins" = sum(Intercept,MinsAge,MinsSex,MinsRace,MinsEthnicity,na.rm=T)) %>%
  select(-c(Intercept,MinsAge,MinsSex,MinsRace,MinsEthnicity))
bacsum.hom$Timewec2 <- bacsum.hom$Timewec1 <- bacsum.hom$Time
contrasts(bacsum.hom$Timewec1) <- contr.wec(bacsum.hom$Time,omitted = 12)
contrasts(bacsum.hom$Timewec2) <- contr.wec(bacsum.hom$Time,omitted = 0)
bacmod1 <- glm(Count ~ Timewec1*BACcat2+Ethnicity+Age7cat+Sex+Race+offset(log(Mins)),data=bacsum.hom,family="poisson")
bacmod2 <- glm(Count ~ Timewec2*BACcat2+Ethnicity+Age7cat+Sex+Race+offset(log(Mins)),data=bacsum.hom,family="poisson")
baclevels <- paste("BACcat2",levels(bacsum.hom$BACcat2),sep="")[-1]
timelevels <- paste("Timewec1",levels(bacsum.hom$Timewec1),sep="")[-13]
bacsubs <- tibble()
for(jdx in 1:length(baclevels)){
  for(idx in 1:length(timelevels)){
    bacsubs <- bind_rows(
      bacsubs,
      tibble("BACcat2" = str_remove(baclevels[jdx],"BACcat2"),
             "Time" = timelevels[idx],
             deltaMethod(
               bacmod1, vcov. = sandwich(bacmod1),
               g.= paste0("`",timelevels[idx],":",baclevels[jdx],"`")
             )
      )
    )
  }
  bacsubs <- bind_rows(
    bacsubs,
    tibble("BACcat2" = str_remove(baclevels[jdx],"BACcat2"),
           "Time" = "Timewec112",
           deltaMethod(
             bacmod2, vcov. = sandwich(bacmod2),
             g.= paste0("`Timewec212:",baclevels[jdx],"`")
           )
    )
  )
}
bacsubs.hompdat <- bacsubs %>%
  transmute(BACcat2 = factor(BACcat2, c("< 0.08",">=0.08")),
            Time = Time %>% str_remove(., "Timewec1") %>% factor(.,0:23),
            IRR = round(exp(Estimate),2),
            Lower = round(exp(`2.5 %`),2),
            Upper = round(exp(`97.5 %`),2),
            `95% CI` = paste0("[",Lower,", ",Upper,"]")
  ) %>% mutate(Time = factor(Time, levels = c(0:23))) %>% 
  left_join(., bacsum.hom %>% group_by(Time,BACcat2) %>% summarize(n=sum(Count)), by=c("Time","BACcat2")) %>%
  left_join(., atus.mins.base,by="Time") %>%
  group_by(BACcat2) %>% 
  mutate(BACcat2 = factor(BACcat2, c("< 0.08",">=0.08"),c("< 80 mg/dL",">= 80 mg/dL")),
         "Mins" = Intercept,
         Intercept=NULL,
         "Count" = n/max(n))
# Figure 3B
bacmod.homp <- ggplot(bacsubs.hompdat, aes(x=Time,group=BACcat2))+theme_pubr()+
  geom_point(aes(y=IRR,color=BACcat2,group=BACcat2),size=2)+
  geom_line(aes(y=IRR,color=BACcat2,group=BACcat2),linewidth=1)+
  geom_ribbon(aes(y=IRR,ymin=Lower,ymax=Upper,color=BACcat2,fill=BACcat2),size=0,alpha=0.25)+
  scale_x_discrete(breaks = c(0,4,8,12,16,20,23),
                   labels = c("12AM","4AM","8AM","12PM","4PM","8PM","11PM"))+
  scale_discrete_manual(name = "",
                        labels = c("< 80 mg/dL",">= 80 mg/dL"),
                        values = c("darkcyan","firebrick3"),
                        aesthetics = c("color","fill"))+
  geom_hline(aes(yintercept = 1,linetype="No Alcohol"),size=1)+
  scale_linetype_manual(values = c(`No Alcohol` = "dashed"))+
  guides(linetype = guide_legend("",order = 1))+
  xlab("")+ ylab("Marginal IRR")+ylim(0,4.5)

#### Figure 3 ####
fig3 <- ggarrange(ggpar(sexmod.p,xlim=c(1,24.5),ylim=c(0,4)),
                  ggpar(bacmod.homp,xlim=c(1,24.5),ylim=c(0,4)),
                  nrow=1,ncol=2,labels="AUTO")
# tiff("c:/users/atala/sync/research/projects/to publish/nvdrs new data/submission/fig3.tiff",
#      height=1150,width=3000,res=300)
# fig3
# dev.off()
#### Table S2 and S3 ####
tblS2 <- bind_rows(agesubs.pdat,ethsubs.pdat,bacsubs.pdat,ptnrsubs.pdat)
tblS3 <- bind_rows(sexsubs.pdat,bacsubs.hompdat)
# write_csv(tblS2,"c:/users/atala/sync/research/projects/to publish/nvdrs new data/submission/tableS2.csv")
# write_csv(tblS3,"c:/users/atala/sync/research/projects/to publish/nvdrs new data/submission/tableS3.csv")

# tiff("c:/users/atala/sync/research/projects/to publish/nvdrs new data/submission/fig2.tiff",
#      height=2000,width=5000,res=300)
# ggpar(fig2, font.tickslab = c(12,"plain","black"),
#             font.x = c(12,"plain","black"),
#             font.y = c(12,"plain","black"),
#       font.legend = c(12,"plain","black"))
# dev.off()

# tiff("c:/users/atala/sync/research/projects/to publish/nvdrs new data/submission/fig3.tiff",
#      height=2100,width=4000,res=300)
# ggpar(fig3, font.tickslab = c(12,"plain","black"),
#       font.x = c(12,"plain","black"),
#       font.y = c(12,"plain","black"),
#       font.legend = c(12,"plain","black"))
# dev.off()

ggplot(suicide.m2, aes(x=Time))+theme_pubr()+
  geom_col(aes(y=n/4864*6),fill="grey55",color="black",width = .8)+
  geom_area(aes(y=Mins/60*6,group=1,fill="% Population Wakefulness"),alpha=0.25,color="black",size=.1,show.legend = TRUE)+
  geom_point(aes(y=IRR,color="IRR"),size=2)+
  geom_line(aes(y=IRR,group=1),color="red3",linewidth=1)+
  geom_ribbon(aes(y=IRR,ymin=Lower,ymax=Upper,group=1),size=0,color="black",fill="red3",alpha=0.5)+
  geom_hline(yintercept = 1,linetype=2,linewidth=1)+
  scale_fill_manual(name = "", values = c("% Population Wakefulness" = "dodgerblue"))+
  xlim(0,24)+
  guides(color = "none")+
  scale_x_discrete(breaks = c(0,6,12,18,24),
                   labels = c("Midnight","6AM","Noon","6PM","Midnight"))+
  scale_y_continuous(name = "Incident Risk Ratio for Suicide",
                     sec.axis = sec_axis(~.*4864*6, name = "Raw Suicide Count",labels = c(0,1000,2000,3000,4000)))+
  xlab("")+theme(legend.position = "top",legend.direction = "horizontal",
                 axis.text.y.left = element_text(size=18,color="red3"),
                 axis.text.y.right = element_text(size=18,color="black"),
                 axis.text.x = element_text(size=18),
                 legend.text = element_text(size=20),
                 axis.title.y.left = element_text(size=20,colour = "red3"),
                 axis.title.y.right = element_text(size=20,colour = "black"))


