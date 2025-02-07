######## Source Clean Data ##############
source(file="~/Desktop/Data Processing.R")

######## Select Age Groups and Data in 2019 ############
### Risk Factors Data
risk_19<-risk %>% filter(Year=="2019" & Age.group.name %in% c("60 to 64","40 to 44","50 to 54","70 to 74","65 to 69","35 to 39","30 to 34","55 to 59","75 to 79","45 to 49") & Sex!="Both")
length(unique(risk_19$iso))

### Prevalence Data
prev_19<-prev %>% filter(Year=="2019" & Age.group.name %in% c("60 to 64","40 to 44","50 to 54","70 to 74","65 to 69","35 to 39","30 to 34","55 to 59","75 to 79","45 to 49") & Sex!="Both")
length(unique(prev_19$iso))

### Death Rates Data
mort_19<-mort %>% filter(Year=="2019" & Age.group.name %in% c("60 to 64","40 to 44","50 to 54","70 to 74","65 to 69","35 to 39","30 to 34","55 to 59","75 to 79","45 to 49") & Sex!="Both")
length(unique(mort_19$iso))

######## Set Data on Risk Fact, Prev, Mort and Cascade ############
### Risk Factors Data
risk_iso1<-risk_19
length(unique(risk_iso1$iso))

### Prevalence Data
prev_iso1<-prev_19
length(unique(prev_iso1$iso))

### Death Rates Data
mort_iso1<-mort_19
length(unique(mort_iso1$iso))

######## Analysis of Country-specific Sex Differences - Risk Factors ############
### Stratify Data by Sex
fem_risk<-risk_iso1 %>% filter(Sex=="Female");mal_risk<-risk_iso1 %>% filter(Sex=="Male")

### Remove intimate partner violence from risk factors (available only for females)
fem_risk<-fem_risk[which(fem_risk$Definition!="Prevalence of physical and/or sexual violence by current or former intimate partner since age 15"),]

### Sort data by country, age group, and risk factor
fem_risk<-fem_risk[order(fem_risk[,"iso"],fem_risk[,"Age.group.name"],fem_risk[,"Risk.factor"]),]
mal_risk<-mal_risk[order(mal_risk[,"iso"],mal_risk[,"Age.group.name"],mal_risk[,"Risk.factor"]),]

### Combine male and female data
male_to_female_risk<-data.frame(mal=mal_risk,fem=fem_risk)

##### Average daily sodium intake (in g/day) - Sodium ######
male_to_female_risk_sel<-male_to_female_risk %>% filter(mal.Definition=="Average daily sodium intake (in g/day)")

### Male higher than Female
male_higher<-male_to_female_risk_sel %>% filter(mal.Lower.bound.of.95..UI>fem.Upper.bound.of.95..UI)
length(unique(male_higher$mal.iso))   ### N countries
cbind(table(male_higher$mal.Age.group.name))   ### N countries by age group
table(male_higher$mal.Age.group.name,male_higher$mal.iso)   ### Countries by age group

### Female higher than Male
female_higher<-male_to_female_risk_sel %>% filter(fem.Lower.bound.of.95..UI>mal.Upper.bound.of.95..UI)
length(unique(female_higher$mal.iso))   ### N countries
table(female_higher$fem.Age.group.name,female_higher$mal.iso)   ### N countries by age group
cbind(table(female_higher$fem.Age.group.name))   ### Countries by age group

male_higher_sodium<-male_higher
female_higher_sodium<-female_higher

##### Mean FPG level (in mmol/L) - Glucose ##### 
male_to_female_risk_sel<-male_to_female_risk %>% filter(mal.Definition=="Mean FPG level (in mmol/L)")

### Male higher than Female
male_higher<-male_to_female_risk_sel %>% filter(mal.Lower.bound.of.95..UI>fem.Upper.bound.of.95..UI)
length(unique(male_higher$mal.iso))   ### N countries
cbind(table(male_higher$mal.Age.group.name))   ### N countries by age group
table(male_higher$mal.Age.group.name,male_higher$mal.iso)   ### Countries by age group

### Female higher than Male
female_higher<-male_to_female_risk_sel %>% filter(fem.Lower.bound.of.95..UI>mal.Upper.bound.of.95..UI)
length(unique(female_higher$mal.iso))   ### N countries
table(female_higher$fem.Age.group.name,female_higher$mal.iso)   ### N countries by age group
cbind(table(female_higher$fem.Age.group.name))   ### Countries by age group

male_higher_glucose<-male_higher
female_higher_glucose<-female_higher

##### Overweight ##### 
male_to_female_risk_sel<-male_to_female_risk %>% filter(mal.Definition=="Prevalence of BMI >= 25 kg/m^2")

### Male higher than Female
male_higher<-male_to_female_risk_sel %>% filter(mal.Lower.bound.of.95..UI>fem.Upper.bound.of.95..UI)
length(unique(male_higher$mal.iso))   ### N countries
cbind(table(male_higher$mal.Age.group.name))   ### N countries by age group
table(male_higher$mal.Age.group.name,male_higher$mal.iso)   ### Countries by age group

### Female higher than Male
female_higher<-male_to_female_risk_sel %>% filter(fem.Lower.bound.of.95..UI>mal.Upper.bound.of.95..UI)
length(unique(female_higher$mal.iso))   ### N countries
table(female_higher$fem.Age.group.name,female_higher$mal.iso)   ### N countries by age group
cbind(table(female_higher$fem.Age.group.name))   ### Countries by age group

male_higher_overweight<-male_higher
female_higher_overweight<-female_higher

##### Obesity ##### 
male_to_female_risk_sel<-male_to_female_risk %>% filter(mal.Definition=="Prevalence of BMI >= 30 kg/m^2")

### Male higher than Female
male_higher<-male_to_female_risk_sel %>% filter(mal.Lower.bound.of.95..UI>fem.Upper.bound.of.95..UI)
length(unique(male_higher$mal.iso))   ### N countries
cbind(table(male_higher$mal.Age.group.name))   ### N countries by age group
table(male_higher$mal.Age.group.name,male_higher$mal.iso)   ### Countries by age group

### Female higher than Male
female_higher<-male_to_female_risk_sel %>% filter(fem.Lower.bound.of.95..UI>mal.Upper.bound.of.95..UI)
length(unique(female_higher$mal.iso))   ### N countries
table(female_higher$fem.Age.group.name,female_higher$mal.iso)   ### N countries by age group
cbind(table(female_higher$fem.Age.group.name))   ### Countries by age group

male_higher_obesity<-male_higher
female_higher_obesity<-female_higher

##### Smoking ##### 
male_to_female_risk_sel<-male_to_female_risk %>% filter(mal.Definition=="Prevalence of current smoking of any tobacco product")

### Male higher than Female
male_higher<-male_to_female_risk_sel %>% filter(mal.Lower.bound.of.95..UI>fem.Upper.bound.of.95..UI)
length(unique(male_higher$mal.iso))   ### N countries
cbind(table(male_higher$mal.Age.group.name))   ### N countries by age group
table(male_higher$mal.Age.group.name,male_higher$mal.iso)   ### Countries by age group

### Female higher than Male
female_higher<-male_to_female_risk_sel %>% filter(fem.Lower.bound.of.95..UI>mal.Upper.bound.of.95..UI)
length(unique(female_higher$mal.iso))   ### N countries
table(female_higher$fem.Age.group.name,female_higher$mal.iso)   ### N countries by age group
cbind(table(female_higher$fem.Age.group.name))   ### Countries by age group

male_higher_smoking<-male_higher
female_higher_smoking<-female_higher

##### Average weekly total metabolic equivalent (MET) minutes - Physical Activity ##### 
male_to_female_risk_sel<-male_to_female_risk %>% filter(mal.Definition=="Average weekly total metabolic equivalent (MET) minutes")

### Male higher than Female
male_higher<-male_to_female_risk_sel %>% filter(mal.Lower.bound.of.95..UI>fem.Upper.bound.of.95..UI)
length(unique(male_higher$mal.iso))   ### N countries
cbind(table(male_higher$mal.Age.group.name))   ### N countries by age group
table(male_higher$mal.Age.group.name,male_higher$mal.iso)   ### Countries by age group

### Female higher than Male
female_higher<-male_to_female_risk_sel %>% filter(fem.Lower.bound.of.95..UI>mal.Upper.bound.of.95..UI)
length(unique(female_higher$mal.iso))   ### N countries
table(female_higher$fem.Age.group.name,female_higher$mal.iso)   ### N countries by age group
cbind(table(female_higher$fem.Age.group.name))   ### Countries by age group

male_higher_inactivity<-male_higher
female_higher_inactivity<-female_higher

##### Prevalence of drug use disorders ##### 
male_to_female_risk_sel<-male_to_female_risk %>% filter(mal.Definition=="Prevalence of drug use disorders")

### Male higher than Female
male_higher<-male_to_female_risk_sel %>% filter(mal.Lower.bound.of.95..UI>fem.Upper.bound.of.95..UI)
length(unique(male_higher$mal.iso))   ### N countries
cbind(table(male_higher$mal.Age.group.name))   ### N countries by age group
table(male_higher$mal.Age.group.name,male_higher$mal.iso)   ### Countries by age group

### Female higher than Male
female_higher<-male_to_female_risk_sel %>% filter(fem.Lower.bound.of.95..UI>mal.Upper.bound.of.95..UI)
length(unique(female_higher$mal.iso))   ### N countries
table(female_higher$fem.Age.group.name,female_higher$mal.iso)   ### N countries by age group
cbind(table(female_higher$fem.Age.group.name))   ### Countries by age group

male_higher_drug_use<-male_higher
female_higher_drug_use<-female_higher

##### Proportion of HIV/AIDS due to unsafe sex ##### 
male_to_female_risk_sel<-male_to_female_risk %>% filter(mal.Definition=="Proportion of HIV/AIDs due to unsafe sex")

### Male higher than Female
male_higher<-male_to_female_risk_sel %>% filter(mal.Lower.bound.of.95..UI>fem.Upper.bound.of.95..UI)
length(unique(male_higher$mal.iso))   ### N countries
cbind(table(male_higher$mal.Age.group.name))   ### N countries by age group
table(male_higher$mal.Age.group.name,male_higher$mal.iso)   ### Countries by age group

### Female higher than Male
female_higher<-male_to_female_risk_sel %>% filter(fem.Lower.bound.of.95..UI>mal.Upper.bound.of.95..UI)
length(unique(female_higher$mal.iso))   ### N countries
table(female_higher$fem.Age.group.name,female_higher$mal.iso)   ### N countries by age group
cbind(table(female_higher$fem.Age.group.name))   ### Countries by age group

male_higher_unsafesex<-male_higher
female_higher_unsafesex<-female_higher

######## Analysis of Country-specific Sex Differences - Prevalence ############
### Stratify Data by Sex
fem_prev<-prev_iso1 %>% filter(Sex=="Female");mal_prev<-prev_iso1 %>% filter(Sex=="Male")

### Sort data by country, age group, and prev
fem_prev<-fem_prev[order(fem_prev[,"iso"],fem_prev[,"Age.group.name"],fem_prev[,"Cause.name"]),]
mal_prev<-mal_prev[order(mal_prev[,"iso"],mal_prev[,"Age.group.name"],mal_prev[,"Cause.name"]),]

### Combine male and female data
male_to_female_prev<-data.frame(mal=mal_prev,fem=fem_prev)

##### Hypertension ##### 
male_to_female_prev_sel<-male_to_female_prev %>% filter(mal.Cause.name=="Prevalence of high systolic blood pressure (> 140 mmHg)")

### Male higher than Female
male_higher<-male_to_female_prev_sel %>% filter(mal.Lower.bound.of.95..UI>fem.Upper.bound.of.95..UI)
length(unique(male_higher$mal.iso))   ### N countries
cbind(table(male_higher$mal.Age.group.name))   ### N countries by age group
table(male_higher$mal.Age.group.name,male_higher$mal.iso)   ### Countries by age group

### Female higher than Male
female_higher<-male_to_female_prev_sel %>% filter(fem.Lower.bound.of.95..UI>mal.Upper.bound.of.95..UI)
length(unique(female_higher$mal.iso))   ### N countries
table(female_higher$fem.Age.group.name,female_higher$mal.iso)   ### N countries by age group
cbind(table(female_higher$fem.Age.group.name))   ### Countries by age group

male_higher_hyp_prev<-male_higher
female_higher_hyp_prev<-female_higher

##### Diabetes mellitus ##### 
male_to_female_prev_sel<-male_to_female_prev %>% filter(mal.Cause.name=="Diabetes mellitus")

### Male higher than Female
male_higher<-male_to_female_prev_sel %>% filter(mal.Lower.bound.of.95..UI>fem.Upper.bound.of.95..UI)
length(unique(male_higher$mal.iso))   ### N countries
cbind(table(male_higher$mal.Age.group.name))   ### N countries by age group
table(male_higher$mal.Age.group.name,male_higher$mal.iso)   ### Countries by age group

### Female higher than Male
female_higher<-male_to_female_prev_sel %>% filter(fem.Lower.bound.of.95..UI>mal.Upper.bound.of.95..UI)
length(unique(female_higher$mal.iso))   ### N countries
table(female_higher$fem.Age.group.name,female_higher$mal.iso)   ### N countries by age group
cbind(table(female_higher$fem.Age.group.name))   ### Countries by age group

male_higher_diab_prev<-male_higher
female_higher_diab_prev<-female_higher

##### HIV/AIDS ##### 
male_to_female_prev_sel<-male_to_female_prev %>% filter(mal.Cause.name=="HIV/AIDS")

### Male higher than Female
male_higher<-male_to_female_prev_sel %>% filter(mal.Lower.bound.of.95..UI>fem.Upper.bound.of.95..UI)
length(unique(male_higher$mal.iso))   ### N countries
cbind(table(male_higher$mal.Age.group.name))   ### N countries by age group
table(male_higher$mal.Age.group.name,male_higher$mal.iso)   ### Countries by age group

### Female higher than Male
female_higher<-male_to_female_prev_sel %>% filter(fem.Lower.bound.of.95..UI>mal.Upper.bound.of.95..UI)
length(unique(female_higher$mal.iso))   ### N countries
table(female_higher$fem.Age.group.name,female_higher$mal.iso)   ### N countries by age group
cbind(table(female_higher$fem.Age.group.name))   ### Countries by age group

male_higher_HIV_AIDS_prev<-male_higher
female_higher_HIV_AIDS_prev<-female_higher

######## Analysis of Country-specific Sex Differences - Death Rates ############
### Stratify Data by Sex
fem_mort<-mort_iso1 %>% filter(Sex=="Female");mal_mort<-mort_iso1 %>% filter(Sex=="Male")

### Sort data by country, age group, and mort
fem_mort<-fem_mort[order(fem_mort[,"iso"],fem_mort[,"Age.group.name"],fem_mort[,"Cause"]),]
mal_mort<-mal_mort[order(mal_mort[,"iso"],mal_mort[,"Age.group.name"],mal_mort[,"Cause"]),]

### Combine male and female data
male_to_female_mort<-data.frame(mal=mal_mort,fem=fem_mort)

##### Hypertension ##### 
male_to_female_mort_sel<-male_to_female_mort %>% filter(mal.Cause=="High systolic blood pressure")

### Male higher than Female
male_higher<-male_to_female_mort_sel %>% filter(mal.Lower.bound.of.95..UI>fem.Upper.bound.of.95..UI)
length(unique(male_higher$mal.iso))   ### N countries
cbind(table(male_higher$mal.Age.group.name))   ### N countries by age group
table(male_higher$mal.Age.group.name,male_higher$mal.iso)   ### Countries by age group

### Female higher than Male
female_higher<-male_to_female_mort_sel %>% filter(fem.Lower.bound.of.95..UI>mal.Upper.bound.of.95..UI)
length(unique(female_higher$mal.iso))   ### N countries
table(female_higher$fem.Age.group.name,female_higher$mal.iso)   ### N countries by age group
cbind(table(female_higher$fem.Age.group.name))   ### Countries by age group

male_higher_hyp_mort<-male_higher
female_higher_hyp_mort<-female_higher

##### Diabetes mellitus ##### 
male_to_female_mort_sel<-male_to_female_mort %>% filter(mal.Cause=="Diabetes mellitus")

### Male higher than Female
male_higher<-male_to_female_mort_sel %>% filter(mal.Lower.bound.of.95..UI>fem.Upper.bound.of.95..UI)
length(unique(male_higher$mal.iso))   ### N countries
cbind(table(male_higher$mal.Age.group.name))   ### N countries by age group
table(male_higher$mal.Age.group.name,male_higher$mal.iso)   ### Countries by age group

### Female higher than Male
female_higher<-male_to_female_mort_sel %>% filter(fem.Lower.bound.of.95..UI>mal.Upper.bound.of.95..UI)
length(unique(female_higher$mal.iso))   ### N countries
table(female_higher$fem.Age.group.name,female_higher$mal.iso)   ### N countries by age group
cbind(table(female_higher$fem.Age.group.name))   ### Countries by age group

male_higher_diab_mort<-male_higher
female_higher_diab_mort<-female_higher

##### HIV/AIDS ##### 
male_to_female_mort_sel<-male_to_female_mort %>% filter(mal.Cause=="HIV/AIDS")

### Male higher than Female
male_higher<-male_to_female_mort_sel %>% filter(mal.Lower.bound.of.95..UI>fem.Upper.bound.of.95..UI)
length(unique(male_higher$mal.iso))   ### N countries
cbind(table(male_higher$mal.Age.group.name))   ### N countries by age group
table(male_higher$mal.Age.group.name,male_higher$mal.iso)   ### Countries by age group

### Female higher than Male
female_higher<-male_to_female_mort_sel %>% filter(fem.Lower.bound.of.95..UI>mal.Upper.bound.of.95..UI)
length(unique(female_higher$mal.iso))   ### N countries
table(female_higher$fem.Age.group.name,female_higher$mal.iso)   ### N countries by age group
cbind(table(female_higher$fem.Age.group.name))   ### Countries by age group

male_higher_HIV_AIDS_mort<-male_higher
female_higher_HIV_AIDS_mort<-female_higher


######## Analysis of Country-specific Sex Differences - Cascade of Care ############
##### Hypertension #####
### Stratify Data by Sex
hyp$Age <- ifelse(hyp$Age == "30-34", "30 to 34",
                  ifelse(hyp$Age == "35-39", "35 to 39",
                         ifelse(hyp$Age == "40-44", "40 to 44",
                                ifelse(hyp$Age == "45-49", "45 to 49",
                                       ifelse(hyp$Age == "50-54", "50 to 54",
                                              ifelse(hyp$Age == "55-59", "55 to 59",
                                                     ifelse(hyp$Age == "60-64", "60 to 64",
                                                            ifelse(hyp$Age == "65-69", "65 to 69",
                                                                   ifelse(hyp$Age == "70-74", "70 to 74",
                                                                          ifelse(hyp$Age == "75-79", "75 to 79", hyp$Age))))))))))

fem_hyp<-hyp %>% filter(Sex=="Women" & Year==2019);mal_hyp<-hyp %>% filter(Sex=="Men" & Year==2019)

### Sort data by country, age group
fem_hyp<-fem_hyp[order(fem_hyp[,"ISO"],fem_hyp[,"Year"],fem_hyp[,"Age"]),]
mal_hyp<-mal_hyp[order(mal_hyp[,"ISO"],mal_hyp[,"Year"],mal_hyp[,"Age"]),]

### Combine male and female data
male_to_female_hyp<-data.frame(mal=mal_hyp,fem=fem_hyp)

##### Hypertension - Step 1 ####
### Male higher than Female
step1_male_higher<-male_to_female_hyp %>% filter(mal.Proportion.of.diagnosed.hypertension.among.all.hypertension.lower.95..uncertainty.interval>fem.Proportion.of.diagnosed.hypertension.among.all.hypertension.upper.95..uncertainty.interval)
length(unique(step1_male_higher$mal.ISO))   ### N countries
cbind(table(step1_male_higher$mal.Age))   ### N countries by age group
table(step1_male_higher$mal.Age,step1_male_higher$mal.ISO)   ### Countries by age group

### Female higher than Male
step1_female_higher<-male_to_female_hyp %>% filter(fem.Proportion.of.diagnosed.hypertension.among.all.hypertension.lower.95..uncertainty.interval>mal.Proportion.of.diagnosed.hypertension.among.all.hypertension.upper.95..uncertainty.interval)
length(unique(step1_female_higher$mal.ISO))   ### N countries
cbind(table(step1_female_higher$mal.Age))   ### N countries by age group
table(step1_female_higher$mal.Age,step1_female_higher$mal.ISO)   ### Countries by age group

step1_male_higher_hyp<-step1_male_higher
step1_female_higher_hyp<-step1_female_higher

##### Hypertension - Step 2 ####
### Male higher than Female
step2_male_higher<-male_to_female_hyp %>% filter(mal.Proportion.of.treated.hypertension.among.all.hypertension.lower.95..uncertainty.interval>fem.Proportion.of.treated.hypertension.among.all.hypertension.upper.95..uncertainty.interval)
length(unique(step2_male_higher$mal.ISO))   ### N countries
cbind(table(step2_male_higher$mal.Age))   ### N countries by age group
table(step2_male_higher$mal.Age,step2_male_higher$mal.ISO)   ### Countries by age group

### Female higher than Male
step2_female_higher<-male_to_female_hyp %>% filter(fem.Proportion.of.treated.hypertension.among.all.hypertension.lower.95..uncertainty.interval>mal.Proportion.of.treated.hypertension.among.all.hypertension.upper.95..uncertainty.interval)
length(unique(step2_female_higher$mal.ISO))   ### N countries
cbind(table(step2_female_higher$mal.Age))   ### N countries by age group
table(step2_female_higher$mal.Age,step2_female_higher$mal.ISO)   ### Countries by age group

step2_male_higher_hyp<-step2_male_higher
step2_female_higher_hyp<-step2_female_higher

##### Hypertension - Step 3 ####
### Male higher than Female
step3_male_higher<-male_to_female_hyp %>% filter(mal.Proportion.of.controlled.hypertension.among.all.hypertension.lower.95..uncertainty.interval>fem.Proportion.of.controlled.hypertension.among.all.hypertension.upper.95..uncertainty.interval)
length(unique(step3_male_higher$mal.ISO))   ### N countries
cbind(table(step3_male_higher$mal.Age))   ### N countries by age group
table(step3_male_higher$mal.Age,step3_male_higher$mal.ISO)   ### Countries by age group

### Female higher than Male
step3_female_higher<-male_to_female_hyp %>% filter(fem.Proportion.of.controlled.hypertension.among.all.hypertension.lower.95..uncertainty.interval>mal.Proportion.of.controlled.hypertension.among.all.hypertension.upper.95..uncertainty.interval)
length(unique(step3_female_higher$mal.ISO))   ### N countries
cbind(table(step3_female_higher$mal.Age))   ### N countries by age group
table(step3_female_higher$mal.Age,step3_female_higher$mal.ISO)   ### Countries by age group

step3_male_higher_hyp<-step3_male_higher
step3_female_higher_hyp<-step3_female_higher

##### Hypertension - Step 2 conditional to Step 1 ####
### Male higher than Female
step2.1_male_higher<-male_to_female_hyp %>% filter((mal.Proportion.of.treated.hypertension.among.all.hypertension.lower.95..uncertainty.interval/mal.Proportion.of.diagnosed.hypertension.among.all.hypertension)>(fem.Proportion.of.treated.hypertension.among.all.hypertension.upper.95..uncertainty.interval/fem.Proportion.of.diagnosed.hypertension.among.all.hypertension))
length(unique(step2.1_male_higher$mal.ISO))   ### N countries
cbind(table(step2.1_male_higher$mal.Age))   ### N countries by age group
table(step2.1_male_higher$mal.Age,step2.1_male_higher$mal.ISO)   ### Countries by age group

### Female higher than Male
step2.1_female_higher<-male_to_female_hyp %>% filter((fem.Proportion.of.treated.hypertension.among.all.hypertension.lower.95..uncertainty.interval/fem.Proportion.of.diagnosed.hypertension.among.all.hypertension)>(mal.Proportion.of.treated.hypertension.among.all.hypertension.upper.95..uncertainty.interval/mal.Proportion.of.diagnosed.hypertension.among.all.hypertension))
length(unique(step2.1_female_higher$mal.ISO))   ### N countries
cbind(table(step2.1_female_higher$mal.Age))   ### N countries by age group
table(step2.1_female_higher$mal.Age,step2.1_female_higher$mal.ISO)   ### Countries by age group

step2.1_male_higher_hyp<-step2.1_male_higher
step2.1_female_higher_hyp<-step2.1_female_higher

##### Hypertension - Step 3 conditional to Step 2 ####
### Male higher than Female
step3.2_male_higher<-male_to_female_hyp %>% filter((mal.Proportion.of.controlled.hypertension.among.all.hypertension.lower.95..uncertainty.interval/mal.Proportion.of.treated.hypertension.among.all.hypertension)>(fem.Proportion.of.controlled.hypertension.among.all.hypertension.upper.95..uncertainty.interval/fem.Proportion.of.treated.hypertension.among.all.hypertension))
length(unique(step3.2_male_higher$mal.ISO))   ### N countries
cbind(table(step3.2_male_higher$mal.Age))   ### N countries by age group
table(step3.2_male_higher$mal.Age,step3.2_male_higher$mal.ISO)   ### Countries by age group

### Female higher than Male
step3.2_female_higher<-male_to_female_hyp %>% filter((fem.Proportion.of.controlled.hypertension.among.all.hypertension.lower.95..uncertainty.interval/fem.Proportion.of.treated.hypertension.among.all.hypertension)>(mal.Proportion.of.controlled.hypertension.among.all.hypertension.upper.95..uncertainty.interval/mal.Proportion.of.treated.hypertension.among.all.hypertension))
length(unique(step3.2_female_higher$mal.ISO))   ### N countries
cbind(table(step3.2_female_higher$mal.Age))   ### N countries by age group
table(step3.2_female_higher$mal.Age,step3.2_female_higher$mal.ISO)   ### Countries by age group

step3.2_male_higher_hyp<-step3.2_male_higher
step3.2_female_higher_hyp<-step3.2_female_higher

##### Diabetes mellitus ##### 
### Stratify Data by Sex
fem_diab<-diab %>% filter(Sex=="Women" );mal_diab<-diab %>% filter(Sex=="Men" )

### Sort data by country, age group
fem_diab<-fem_diab[order(fem_diab[,"location"],fem_diab[,"Age"]),]
mal_diab<-mal_diab[order(mal_diab[,"location"],mal_diab[,"Age"]),]

### Combine male and female data
male_to_female_diab<-data.frame(mal=mal_diab,fem=fem_diab)

##### Diabetes mellitus - Step 1 ####
### Male higher than Female
step1_male_higher<-male_to_female_diab %>% filter(mal.step1_lower>fem.step1_upper)
length(unique(step1_male_higher$mal.iso3))   ### N countries
cbind(table(step1_male_higher$mal.Age))   ### N countries by age group
table(step1_male_higher$mal.Age,step1_male_higher$mal.iso3)   ### Countries by age group

### Female higher than Male
step1_female_higher<-male_to_female_diab %>% filter(fem.step1_lower>mal.step1_upper)
length(unique(step1_female_higher$mal.iso3))   ### N countries
cbind(table(step1_female_higher$mal.Age))   ### N countries by age group
table(step1_female_higher$mal.Age,step1_female_higher$mal.iso3)   ### Countries by age group

step1_male_higher_diab<-step1_male_higher
step1_female_higher_diab<-step1_female_higher

##### Diabetes mellitus - Step 2 ####
### Male higher than Female
step2_male_higher<-male_to_female_diab %>% filter(mal.step2_lower>fem.step2_upper)
length(unique(step2_male_higher$mal.iso3))   ### N countries
cbind(table(step2_male_higher$mal.Age))   ### N countries by age group
table(step2_male_higher$mal.Age,step2_male_higher$mal.iso3)   ### Countries by age group

### Female higher than Male
step2_female_higher<-male_to_female_diab %>% filter(fem.step2_lower>mal.step2_upper)
length(unique(step2_female_higher$mal.iso3))   ### N countries
cbind(table(step2_female_higher$mal.Age))   ### N countries by age group
table(step2_female_higher$mal.Age,step2_female_higher$mal.iso3)   ### Countries by age group

step2_male_higher_diab<-step2_male_higher
step2_female_higher_diab<-step2_female_higher

##### Diabetes mellitus - Step 3 ####
### Male higher than Female
step3_male_higher<-male_to_female_diab %>% filter(mal.step3_lower>fem.step3_upper)
length(unique(step3_male_higher$mal.iso3))   ### N countries
cbind(table(step3_male_higher$mal.Age))   ### N countries by age group
table(step3_male_higher$mal.Age,step3_male_higher$mal.iso3)   ### Countries by age group

### Female higher than Male
step3_female_higher<-male_to_female_diab %>% filter(fem.step3_lower>mal.step3_upper)
length(unique(step3_female_higher$mal.iso3))   ### N countries
cbind(table(step3_female_higher$mal.Age))   ### N countries by age group
table(step3_female_higher$mal.Age,step3_female_higher$mal.iso3)   ### Countries by age group

step3_male_higher_diab<-step3_male_higher
step3_female_higher_diab<-step3_female_higher

##### Diabetes mellitus - Step 2 conditional to Step 1 ####
### Male higher than Female
step2c_male_higher<-male_to_female_diab %>% filter(mal.step2c_lower>fem.step2c_upper)
length(unique(step2c_male_higher$mal.iso3))   ### N countries
cbind(table(step2c_male_higher$mal.Age))   ### N countries by age group
table(step2c_male_higher$mal.Age,step2c_male_higher$mal.iso3)   ### Countries by age group

### Female higher than Male
step2c_female_higher<-male_to_female_diab %>% filter(fem.step2c_lower>mal.step2c_upper)
length(unique(step2c_female_higher$mal.iso3))   ### N countries
cbind(table(step2c_female_higher$mal.Age))   ### N countries by age group
table(step2c_female_higher$mal.Age,step2c_female_higher$mal.iso3)   ### Countries by age group

step2.1_male_higher_diab<-step2.1_male_higher
step2.1_female_higher_diab<-step2.1_female_higher

##### Diabetes mellitus - Step 3 conditional to Step 2 ####
### Male higher than Female
step3c_male_higher<-male_to_female_diab %>% filter(mal.step3c_lower>fem.step3c_upper)
length(unique(step3c_male_higher$mal.iso3))   ### N countries
cbind(table(step3c_male_higher$mal.Age))   ### N countries by age group
table(step3c_male_higher$mal.Age,step3c_male_higher$mal.iso3)   ### Countries by age group

### Female higher than Male
step3c_female_higher<-male_to_female_diab %>% filter(fem.step3c_lower>mal.step3c_upper)
length(unique(step3c_female_higher$mal.iso3))   ### N countries
cbind(table(step3c_female_higher$mal.Age))   ### N countries by age group
table(step3c_female_higher$mal.Age,step3c_female_higher$mal.iso3)   ### Countries by age group

step3.2_male_higher_diab<-step3.2_male_higher
step3.2_female_higher_diab<-step3.2_female_higher

##### HIV/AIDS ##### 
### Stratify Data by Sex
HIV_AIDS_female<-HIV_AIDS %>% filter(Sex=="Female");HIV_AIDS_male<-HIV_AIDS %>% filter(Sex=="Male")

### Sort data by country, age group
HIV_AIDS_female<-HIV_AIDS_female[order(HIV_AIDS_female[,"location"]),]
HIV_AIDS_male<-HIV_AIDS_male[order(HIV_AIDS_male[,"location"]),]

### Combine male and female data
male_to_female_HIV_AIDS<-data.frame(mal=HIV_AIDS_male,fem=HIV_AIDS_female)
male_to_female_HIV_AIDS$mal.Percent.of.people.living.with.HIV.who.know.their.status_lower
male_to_female_HIV_AIDS$fem.Percent.of.people.living.with.HIV.who.know.their.status_upper

##### HIV/AIDS - Step 1 ####
### Male higher than Female
step1_male_higher<-male_to_female_HIV_AIDS %>% filter(mal.Percent.of.people.living.with.HIV.who.know.their.status_lower>fem.Percent.of.people.living.with.HIV.who.know.their.status_upper)
length(unique(step1_male_higher$mal.Country))   ### N countries

### Female higher than Male
step1_female_higher<-male_to_female_HIV_AIDS %>% filter(fem.Percent.of.people.living.with.HIV.who.know.their.status_lower>mal.Percent.of.people.living.with.HIV.who.know.their.status_upper)
length(unique(step1_female_higher$mal.Country))   ### N countries

step1_male_higher_HIV_AIDS<-step1_male_higher
step1_female_higher_HIV_AIDS<-step1_female_higher

##### HIV/AIDS - Step 2 ####
### Male higher than Female
step2_male_higher<-male_to_female_HIV_AIDS %>% filter(mal.Coverage.of.people.living.with.HIV.receiving.ART_lower>fem.Coverage.of.people.living.with.HIV.receiving.ART_upper)
length(unique(step2_male_higher$mal.Country))   ### N countries

### Female higher than Male
step2_female_higher<-male_to_female_HIV_AIDS %>% filter(fem.Coverage.of.people.living.with.HIV.receiving.ART_lower>mal.Coverage.of.people.living.with.HIV.receiving.ART_upper)
length(unique(step2_female_higher$mal.Country))   ### N countries

step2_male_higher_HIV_AIDS<-step2_male_higher
step2_female_higher_HIV_AIDS<-step2_female_higher

##### HIV/AIDS - Step 3 ####
### Male higher than Female
step3_male_higher<-male_to_female_HIV_AIDS %>% filter(mal.Percent.of.people.living.with.HIV.who.have.suppressed.viral.loads_lower>fem.Percent.of.people.living.with.HIV.who.have.suppressed.viral.loads_upper)
length(unique(step3_male_higher$mal.Country))   ### N countries

### Female higher than Male
step3_female_higher<-male_to_female_HIV_AIDS %>% filter(fem.Percent.of.people.living.with.HIV.who.have.suppressed.viral.loads_lower>mal.Percent.of.people.living.with.HIV.who.have.suppressed.viral.loads_upper)
length(unique(step3_female_higher$mal.Country))   ### N countries

step3_male_higher_HIV_AIDS<-step3_male_higher
step3_female_higher_HIV_AIDS<-step3_female_higher

##### HIV/AIDS - Step 2 conditional to Step 1 ####
### Male higher than Female
step2.1_male_higher<-male_to_female_HIV_AIDS %>% filter((mal.Coverage.of.people.living.with.HIV.receiving.ART_lower/mal.Percent.of.people.living.with.HIV.who.know.their.status)>(fem.Coverage.of.people.living.with.HIV.receiving.ART_lower/fem.Percent.of.people.living.with.HIV.who.know.their.status))
length(unique(step2.1_male_higher$mal.ISO))   ### N countries
cbind(table(step2.1_male_higher$mal.Age))   ### N countries by age group
table(step2.1_male_higher$mal.Age,step2.1_male_higher$mal.ISO)   ### Countries by age group

### Female higher than Male
step2.1_female_higher<-male_to_female_HIV_AIDS %>% filter((fem.Coverage.of.people.living.with.HIV.receiving.ART_lower/fem.Percent.of.people.living.with.HIV.who.know.their.status)>(mal.Coverage.of.people.living.with.HIV.receiving.ART_lower/mal.Percent.of.people.living.with.HIV.who.know.their.status))
length(unique(step2.1_female_higher$mal.ISO))   ### N countries
cbind(table(step2.1_female_higher$mal.Age))   ### N countries by age group
table(step2.1_female_higher$mal.Age,step2.1_female_higher$mal.ISO)   ### Countries by age group

step2.1_male_higher_HIV_AIDS<-step2.1_male_higher
step2.1_female_higher_HIV_AIDS<-step2.1_female_higher

##### HIV/AIDS - Step 3 conditional to Step 2 ####
### Male higher than Female
step3.2_male_higher<-male_to_female_HIV_AIDS %>% filter((mal.Percent.of.people.living.with.HIV.who.have.suppressed.viral.loads_lower/mal.Coverage.of.people.living.with.HIV.receiving.ART_lower)>(fem.Percent.of.people.living.with.HIV.who.have.suppressed.viral.loads_lower/fem.Coverage.of.people.living.with.HIV.receiving.ART_lower))
length(unique(step3.2_male_higher$mal.ISO))   ### N countries
cbind(table(step3.2_male_higher$mal.Age))   ### N countries by age group
table(step3.2_male_higher$mal.Age,step3.2_male_higher$mal.ISO)   ### Countries by age group

### Female higher than Male
step3.2_female_higher<-male_to_female_HIV_AIDS %>% filter((fem.Percent.of.people.living.with.HIV.who.have.suppressed.viral.loads_lower/fem.Coverage.of.people.living.with.HIV.receiving.ART_lower)>(mal.Percent.of.people.living.with.HIV.who.have.suppressed.viral.loads_lower/mal.Coverage.of.people.living.with.HIV.receiving.ART_lower))
length(unique(step3.2_female_higher$mal.ISO))   ### N countries
cbind(table(step3.2_female_higher$mal.Age))   ### N countries by age group
table(step3.2_female_higher$mal.Age,step3.2_female_higher$mal.ISO)   ### Countries by age group

step3.2_male_higher_HIV_AIDS<-step3.2_male_higher
step3.2_female_higher_HIV_AIDS<-step3.2_female_higher


######################
######## Stratify Results Health Pathways by Income group ############
male_higher_sodium_HI<-male_higher_sodium %>% filter(mal.wbinc2023=="H")
male_higher_sodium_UM<-male_higher_sodium %>% filter(mal.wbinc2023=="UM")
male_higher_sodium_LM<-male_higher_sodium %>% filter(mal.wbinc2023=="LM")
male_higher_sodium_LO<-male_higher_sodium %>% filter(mal.wbinc2023=="L")
female_higher_sodium_HI<-female_higher_sodium %>% filter(mal.wbinc2023=="H")
female_higher_sodium_UM<-female_higher_sodium %>% filter(mal.wbinc2023=="UM")
female_higher_sodium_LM<-female_higher_sodium %>% filter(mal.wbinc2023=="LM")
female_higher_sodium_LO<-female_higher_sodium %>% filter(mal.wbinc2023=="L")

male_higher_glucose_HI<-male_higher_glucose %>% filter(mal.wbinc2023=="H")
male_higher_glucose_UM<-male_higher_glucose %>% filter(mal.wbinc2023=="UM")
male_higher_glucose_LM<-male_higher_glucose %>% filter(mal.wbinc2023=="LM")
male_higher_glucose_LO<-male_higher_glucose %>% filter(mal.wbinc2023=="L")
female_higher_glucose_HI<-female_higher_glucose %>% filter(mal.wbinc2023=="H")
female_higher_glucose_UM<-female_higher_glucose %>% filter(mal.wbinc2023=="UM")
female_higher_glucose_LM<-female_higher_glucose %>% filter(mal.wbinc2023=="LM")
female_higher_glucose_LO<-female_higher_glucose %>% filter(mal.wbinc2023=="L")

male_higher_overweight_HI<-male_higher_overweight %>% filter(mal.wbinc2023=="H")
male_higher_overweight_UM<-male_higher_overweight %>% filter(mal.wbinc2023=="UM")
male_higher_overweight_LM<-male_higher_overweight %>% filter(mal.wbinc2023=="LM")
male_higher_overweight_LO<-male_higher_overweight %>% filter(mal.wbinc2023=="L")
female_higher_overweight_HI<-female_higher_overweight %>% filter(mal.wbinc2023=="H")
female_higher_overweight_UM<-female_higher_overweight %>% filter(mal.wbinc2023=="UM")
female_higher_overweight_LM<-female_higher_overweight %>% filter(mal.wbinc2023=="LM")
female_higher_overweight_LO<-female_higher_overweight %>% filter(mal.wbinc2023=="L")

male_higher_obesity_HI<-male_higher_obesity %>% filter(mal.wbinc2023=="H")
male_higher_obesity_UM<-male_higher_obesity %>% filter(mal.wbinc2023=="UM")
male_higher_obesity_LM<-male_higher_obesity %>% filter(mal.wbinc2023=="LM")
male_higher_obesity_LO<-male_higher_obesity %>% filter(mal.wbinc2023=="L")
female_higher_obesity_HI<-female_higher_obesity %>% filter(mal.wbinc2023=="H")
female_higher_obesity_UM<-female_higher_obesity %>% filter(mal.wbinc2023=="UM")
female_higher_obesity_LM<-female_higher_obesity %>% filter(mal.wbinc2023=="LM")
female_higher_obesity_LO<-female_higher_obesity %>% filter(mal.wbinc2023=="L")

male_higher_smoking_HI<-male_higher_smoking %>% filter(mal.wbinc2023=="H")
male_higher_smoking_UM<-male_higher_smoking %>% filter(mal.wbinc2023=="UM")
male_higher_smoking_LM<-male_higher_smoking %>% filter(mal.wbinc2023=="LM")
male_higher_smoking_LO<-male_higher_smoking %>% filter(mal.wbinc2023=="L")
female_higher_smoking_HI<-female_higher_smoking %>% filter(mal.wbinc2023=="H")
female_higher_smoking_UM<-female_higher_smoking %>% filter(mal.wbinc2023=="UM")
female_higher_smoking_LM<-female_higher_smoking %>% filter(mal.wbinc2023=="LM")
female_higher_smoking_LO<-female_higher_smoking %>% filter(mal.wbinc2023=="L")

male_higher_inactivity_HI<-male_higher_inactivity %>% filter(mal.wbinc2023=="H")
male_higher_inactivity_UM<-male_higher_inactivity %>% filter(mal.wbinc2023=="UM")
male_higher_inactivity_LM<-male_higher_inactivity %>% filter(mal.wbinc2023=="LM")
male_higher_inactivity_LO<-male_higher_inactivity %>% filter(mal.wbinc2023=="L")
female_higher_inactivity_HI<-female_higher_inactivity %>% filter(mal.wbinc2023=="H")
female_higher_inactivity_UM<-female_higher_inactivity %>% filter(mal.wbinc2023=="UM")
female_higher_inactivity_LM<-female_higher_inactivity %>% filter(mal.wbinc2023=="LM")
female_higher_inactivity_LO<-female_higher_inactivity %>% filter(mal.wbinc2023=="L")

male_higher_drug_use_HI<-male_higher_drug_use %>% filter(mal.wbinc2023=="H")
male_higher_drug_use_UM<-male_higher_drug_use %>% filter(mal.wbinc2023=="UM")
male_higher_drug_use_LM<-male_higher_drug_use %>% filter(mal.wbinc2023=="LM")
male_higher_drug_use_LO<-male_higher_drug_use %>% filter(mal.wbinc2023=="L")
female_higher_drug_use_HI<-female_higher_drug_use %>% filter(mal.wbinc2023=="H")
female_higher_drug_use_UM<-female_higher_drug_use %>% filter(mal.wbinc2023=="UM")
female_higher_drug_use_LM<-female_higher_drug_use %>% filter(mal.wbinc2023=="LM")
female_higher_drug_use_LO<-female_higher_drug_use %>% filter(mal.wbinc2023=="L")

male_higher_unsafesex_HI<-male_higher_unsafesex %>% filter(mal.wbinc2023=="H")
male_higher_unsafesex_UM<-male_higher_unsafesex %>% filter(mal.wbinc2023=="UM")
male_higher_unsafesex_LM<-male_higher_unsafesex %>% filter(mal.wbinc2023=="LM")
male_higher_unsafesex_LO<-male_higher_unsafesex %>% filter(mal.wbinc2023=="L")
female_higher_unsafesex_HI<-female_higher_unsafesex %>% filter(mal.wbinc2023=="H")
female_higher_unsafesex_UM<-female_higher_unsafesex %>% filter(mal.wbinc2023=="UM")
female_higher_unsafesex_LM<-female_higher_unsafesex %>% filter(mal.wbinc2023=="LM")
female_higher_unsafesex_LO<-female_higher_unsafesex %>% filter(mal.wbinc2023=="L")

male_higher_hyp_prev_HI<-male_higher_hyp_prev %>% filter(mal.wbinc2023=="H")
male_higher_hyp_prev_UM<-male_higher_hyp_prev %>% filter(mal.wbinc2023=="UM")
male_higher_hyp_prev_LM<-male_higher_hyp_prev %>% filter(mal.wbinc2023=="LM")
male_higher_hyp_prev_LO<-male_higher_hyp_prev %>% filter(mal.wbinc2023=="L")
female_higher_hyp_prev_HI<-female_higher_hyp_prev %>% filter(mal.wbinc2023=="H")
female_higher_hyp_prev_UM<-female_higher_hyp_prev %>% filter(mal.wbinc2023=="UM")
female_higher_hyp_prev_LM<-female_higher_hyp_prev %>% filter(mal.wbinc2023=="LM")
female_higher_hyp_prev_LO<-female_higher_hyp_prev %>% filter(mal.wbinc2023=="L")

male_higher_diab_prev_HI<-male_higher_diab_prev %>% filter(mal.wbinc2023=="H")
male_higher_diab_prev_UM<-male_higher_diab_prev %>% filter(mal.wbinc2023=="UM")
male_higher_diab_prev_LM<-male_higher_diab_prev %>% filter(mal.wbinc2023=="LM")
male_higher_diab_prev_LO<-male_higher_diab_prev %>% filter(mal.wbinc2023=="L")
female_higher_diab_prev_HI<-female_higher_diab_prev %>% filter(mal.wbinc2023=="H")
female_higher_diab_prev_UM<-female_higher_diab_prev %>% filter(mal.wbinc2023=="UM")
female_higher_diab_prev_LM<-female_higher_diab_prev %>% filter(mal.wbinc2023=="LM")
female_higher_diab_prev_LO<-female_higher_diab_prev %>% filter(mal.wbinc2023=="L")

male_higher_HIV_AIDS_prev_HI<-male_higher_HIV_AIDS_prev %>% filter(mal.wbinc2023=="H")
male_higher_HIV_AIDS_prev_UM<-male_higher_HIV_AIDS_prev %>% filter(mal.wbinc2023=="UM")
male_higher_HIV_AIDS_prev_LM<-male_higher_HIV_AIDS_prev %>% filter(mal.wbinc2023=="LM")
male_higher_HIV_AIDS_prev_LO<-male_higher_HIV_AIDS_prev %>% filter(mal.wbinc2023=="L")
female_higher_HIV_AIDS_prev_HI<-female_higher_HIV_AIDS_prev %>% filter(mal.wbinc2023=="H")
female_higher_HIV_AIDS_prev_UM<-female_higher_HIV_AIDS_prev %>% filter(mal.wbinc2023=="UM")
female_higher_HIV_AIDS_prev_LM<-female_higher_HIV_AIDS_prev %>% filter(mal.wbinc2023=="LM")
female_higher_HIV_AIDS_prev_LO<-female_higher_HIV_AIDS_prev %>% filter(mal.wbinc2023=="L")

male_higher_hyp_mort_HI<-male_higher_hyp_mort %>% filter(mal.wbinc2023=="H")
male_higher_hyp_mort_UM<-male_higher_hyp_mort %>% filter(mal.wbinc2023=="UM")
male_higher_hyp_mort_LM<-male_higher_hyp_mort %>% filter(mal.wbinc2023=="LM")
male_higher_hyp_mort_LO<-male_higher_hyp_mort %>% filter(mal.wbinc2023=="L")
female_higher_hyp_mort_HI<-female_higher_hyp_mort %>% filter(mal.wbinc2023=="H")
female_higher_hyp_mort_UM<-female_higher_hyp_mort %>% filter(mal.wbinc2023=="UM")
female_higher_hyp_mort_LM<-female_higher_hyp_mort %>% filter(mal.wbinc2023=="LM")
female_higher_hyp_mort_LO<-female_higher_hyp_mort %>% filter(mal.wbinc2023=="L")

male_higher_diab_mort_HI<-male_higher_diab_mort %>% filter(mal.wbinc2023=="H")
male_higher_diab_mort_UM<-male_higher_diab_mort %>% filter(mal.wbinc2023=="UM")
male_higher_diab_mort_LM<-male_higher_diab_mort %>% filter(mal.wbinc2023=="LM")
male_higher_diab_mort_LO<-male_higher_diab_mort %>% filter(mal.wbinc2023=="L")
female_higher_diab_mort_HI<-female_higher_diab_mort %>% filter(mal.wbinc2023=="H")
female_higher_diab_mort_UM<-female_higher_diab_mort %>% filter(mal.wbinc2023=="UM")
female_higher_diab_mort_LM<-female_higher_diab_mort %>% filter(mal.wbinc2023=="LM")
female_higher_diab_mort_LO<-female_higher_diab_mort %>% filter(mal.wbinc2023=="L")

male_higher_HIV_AIDS_mort_HI<-male_higher_HIV_AIDS_mort %>% filter(mal.wbinc2023=="H")
male_higher_HIV_AIDS_mort_UM<-male_higher_HIV_AIDS_mort %>% filter(mal.wbinc2023=="UM")
male_higher_HIV_AIDS_mort_LM<-male_higher_HIV_AIDS_mort %>% filter(mal.wbinc2023=="LM")
male_higher_HIV_AIDS_mort_LO<-male_higher_HIV_AIDS_mort %>% filter(mal.wbinc2023=="L")
female_higher_HIV_AIDS_mort_HI<-female_higher_HIV_AIDS_mort %>% filter(mal.wbinc2023=="H")
female_higher_HIV_AIDS_mort_UM<-female_higher_HIV_AIDS_mort %>% filter(mal.wbinc2023=="UM")
female_higher_HIV_AIDS_mort_LM<-female_higher_HIV_AIDS_mort %>% filter(mal.wbinc2023=="LM")
female_higher_HIV_AIDS_mort_LO<-female_higher_HIV_AIDS_mort %>% filter(mal.wbinc2023=="L")

step1_male_higher_hyp_HI<-step1_male_higher_hyp %>% filter(mal.wbinc2023=="H")
step1_male_higher_hyp_UM<-step1_male_higher_hyp %>% filter(mal.wbinc2023=="UM")
step1_male_higher_hyp_LM<-step1_male_higher_hyp %>% filter(mal.wbinc2023=="LM")
step1_male_higher_hyp_LO<-step1_male_higher_hyp %>% filter(mal.wbinc2023=="L")
step1_female_higher_hyp_HI<-step1_female_higher_hyp %>% filter(mal.wbinc2023=="H")
step1_female_higher_hyp_UM<-step1_female_higher_hyp %>% filter(mal.wbinc2023=="UM")
step1_female_higher_hyp_LM<-step1_female_higher_hyp %>% filter(mal.wbinc2023=="LM")
step1_female_higher_hyp_LO<-step1_female_higher_hyp %>% filter(mal.wbinc2023=="L")

step2_male_higher_hyp_HI<-step2_male_higher_hyp %>% filter(mal.wbinc2023=="H")
step2_male_higher_hyp_UM<-step2_male_higher_hyp %>% filter(mal.wbinc2023=="UM")
step2_male_higher_hyp_LM<-step2_male_higher_hyp %>% filter(mal.wbinc2023=="LM")
step2_male_higher_hyp_LO<-step2_male_higher_hyp %>% filter(mal.wbinc2023=="L")
step2_female_higher_hyp_HI<-step2_female_higher_hyp %>% filter(mal.wbinc2023=="H")
step2_female_higher_hyp_UM<-step2_female_higher_hyp %>% filter(mal.wbinc2023=="UM")
step2_female_higher_hyp_LM<-step2_female_higher_hyp %>% filter(mal.wbinc2023=="LM")
step2_female_higher_hyp_LO<-step2_female_higher_hyp %>% filter(mal.wbinc2023=="L")

step3_male_higher_hyp_HI<-step3_male_higher_hyp %>% filter(mal.wbinc2023=="H")
step3_male_higher_hyp_UM<-step3_male_higher_hyp %>% filter(mal.wbinc2023=="UM")
step3_male_higher_hyp_LM<-step3_male_higher_hyp %>% filter(mal.wbinc2023=="LM")
step3_male_higher_hyp_LO<-step3_male_higher_hyp %>% filter(mal.wbinc2023=="L")
step3_female_higher_hyp_HI<-step3_female_higher_hyp %>% filter(mal.wbinc2023=="H")
step3_female_higher_hyp_UM<-step3_female_higher_hyp %>% filter(mal.wbinc2023=="UM")
step3_female_higher_hyp_LM<-step3_female_higher_hyp %>% filter(mal.wbinc2023=="LM")
step3_female_higher_hyp_LO<-step3_female_higher_hyp %>% filter(mal.wbinc2023=="L")

step1_male_higher_diab_HI<-step1_male_higher_diab %>% filter(mal.wbinc2023=="H")
step1_male_higher_diab_UM<-step1_male_higher_diab %>% filter(mal.wbinc2023=="UM")
step1_male_higher_diab_LM<-step1_male_higher_diab %>% filter(mal.wbinc2023=="LM")
step1_male_higher_diab_LO<-step1_male_higher_diab %>% filter(mal.wbinc2023=="L")
step1_female_higher_diab_HI<-step1_female_higher_diab %>% filter(mal.wbinc2023=="H")
step1_female_higher_diab_UM<-step1_female_higher_diab %>% filter(mal.wbinc2023=="UM")
step1_female_higher_diab_LM<-step1_female_higher_diab %>% filter(mal.wbinc2023=="LM")
step1_female_higher_diab_LO<-step1_female_higher_diab %>% filter(mal.wbinc2023=="L")

step2_male_higher_diab_HI<-step2_male_higher_diab %>% filter(mal.wbinc2023=="H")
step2_male_higher_diab_UM<-step2_male_higher_diab %>% filter(mal.wbinc2023=="UM")
step2_male_higher_diab_LM<-step2_male_higher_diab %>% filter(mal.wbinc2023=="LM")
step2_male_higher_diab_LO<-step2_male_higher_diab %>% filter(mal.wbinc2023=="L")
step2_female_higher_diab_HI<-step2_female_higher_diab %>% filter(mal.wbinc2023=="H")
step2_female_higher_diab_UM<-step2_female_higher_diab %>% filter(mal.wbinc2023=="UM")
step2_female_higher_diab_LM<-step2_female_higher_diab %>% filter(mal.wbinc2023=="LM")
step2_female_higher_diab_LO<-step2_female_higher_diab %>% filter(mal.wbinc2023=="L")

step3_male_higher_diab_HI<-step3_male_higher_diab %>% filter(mal.wbinc2023=="H")
step3_male_higher_diab_UM<-step3_male_higher_diab %>% filter(mal.wbinc2023=="UM")
step3_male_higher_diab_LM<-step3_male_higher_diab %>% filter(mal.wbinc2023=="LM")
step3_male_higher_diab_LO<-step3_male_higher_diab %>% filter(mal.wbinc2023=="L")
step3_female_higher_diab_HI<-step3_female_higher_diab %>% filter(mal.wbinc2023=="H")
step3_female_higher_diab_UM<-step3_female_higher_diab %>% filter(mal.wbinc2023=="UM")
step3_female_higher_diab_LM<-step3_female_higher_diab %>% filter(mal.wbinc2023=="LM")
step3_female_higher_diab_LO<-step3_female_higher_diab %>% filter(mal.wbinc2023=="L")

step1_male_higher_HIV_AIDS_HI<-step1_male_higher_HIV_AIDS %>% filter(mal.wbinc2023=="H")
step1_male_higher_HIV_AIDS_UM<-step1_male_higher_HIV_AIDS %>% filter(mal.wbinc2023=="UM")
step1_male_higher_HIV_AIDS_LM<-step1_male_higher_HIV_AIDS %>% filter(mal.wbinc2023=="LM")
step1_male_higher_HIV_AIDS_LO<-step1_male_higher_HIV_AIDS %>% filter(mal.wbinc2023=="L")
step1_female_higher_HIV_AIDS_HI<-step1_female_higher_HIV_AIDS %>% filter(mal.wbinc2023=="H")
step1_female_higher_HIV_AIDS_UM<-step1_female_higher_HIV_AIDS %>% filter(mal.wbinc2023=="UM")
step1_female_higher_HIV_AIDS_LM<-step1_female_higher_HIV_AIDS %>% filter(mal.wbinc2023=="LM")
step1_female_higher_HIV_AIDS_LO<-step1_female_higher_HIV_AIDS %>% filter(mal.wbinc2023=="L")

step2_male_higher_HIV_AIDS_HI<-step2_male_higher_HIV_AIDS %>% filter(mal.wbinc2023=="H")
step2_male_higher_HIV_AIDS_UM<-step2_male_higher_HIV_AIDS %>% filter(mal.wbinc2023=="UM")
step2_male_higher_HIV_AIDS_LM<-step2_male_higher_HIV_AIDS %>% filter(mal.wbinc2023=="LM")
step2_male_higher_HIV_AIDS_LO<-step2_male_higher_HIV_AIDS %>% filter(mal.wbinc2023=="L")
step2_female_higher_HIV_AIDS_HI<-step2_female_higher_HIV_AIDS %>% filter(mal.wbinc2023=="H")
step2_female_higher_HIV_AIDS_UM<-step2_female_higher_HIV_AIDS %>% filter(mal.wbinc2023=="UM")
step2_female_higher_HIV_AIDS_LM<-step2_female_higher_HIV_AIDS %>% filter(mal.wbinc2023=="LM")
step2_female_higher_HIV_AIDS_LO<-step2_female_higher_HIV_AIDS %>% filter(mal.wbinc2023=="L")

step3_male_higher_HIV_AIDS_HI<-step3_male_higher_HIV_AIDS %>% filter(mal.wbinc2023=="H")
step3_male_higher_HIV_AIDS_UM<-step3_male_higher_HIV_AIDS %>% filter(mal.wbinc2023=="UM")
step3_male_higher_HIV_AIDS_LM<-step3_male_higher_HIV_AIDS %>% filter(mal.wbinc2023=="LM")
step3_male_higher_HIV_AIDS_LO<-step3_male_higher_HIV_AIDS %>% filter(mal.wbinc2023=="L")
step3_female_higher_HIV_AIDS_HI<-step3_female_higher_HIV_AIDS %>% filter(mal.wbinc2023=="H")
step3_female_higher_HIV_AIDS_UM<-step3_female_higher_HIV_AIDS %>% filter(mal.wbinc2023=="UM")
step3_female_higher_HIV_AIDS_LM<-step3_female_higher_HIV_AIDS %>% filter(mal.wbinc2023=="LM")
step3_female_higher_HIV_AIDS_LO<-step3_female_higher_HIV_AIDS %>% filter(mal.wbinc2023=="L")

######## Stratify Results Health Pathways by Regions ############
male_higher_sodium_ECA<-male_higher_sodium %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
male_higher_sodium_SAS<-male_higher_sodium %>% filter(mal.World.Bank.Region=="South Asia")
male_higher_sodium_SSA<-male_higher_sodium %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
male_higher_sodium_MEN<-male_higher_sodium %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
male_higher_sodium_LAT<-male_higher_sodium %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
male_higher_sodium_EAP<-male_higher_sodium %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
male_higher_sodium_NOA<-male_higher_sodium %>% filter(mal.World.Bank.Region=="North America")
female_higher_sodium_ECA<-female_higher_sodium %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
female_higher_sodium_SAS<-female_higher_sodium %>% filter(mal.World.Bank.Region=="South Asia")
female_higher_sodium_SSA<-female_higher_sodium %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
female_higher_sodium_MEN<-female_higher_sodium %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
female_higher_sodium_LAT<-female_higher_sodium %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
female_higher_sodium_EAP<-female_higher_sodium %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
female_higher_sodium_NOA<-female_higher_sodium %>% filter(mal.World.Bank.Region=="North America")

male_higher_glucose_ECA<-male_higher_glucose %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
male_higher_glucose_SAS<-male_higher_glucose %>% filter(mal.World.Bank.Region=="South Asia")
male_higher_glucose_SSA<-male_higher_glucose %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
male_higher_glucose_MEN<-male_higher_glucose %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
male_higher_glucose_LAT<-male_higher_glucose %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
male_higher_glucose_EAP<-male_higher_glucose %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
male_higher_glucose_NOA<-male_higher_glucose %>% filter(mal.World.Bank.Region=="North America")
female_higher_glucose_ECA<-female_higher_glucose %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
female_higher_glucose_SAS<-female_higher_glucose %>% filter(mal.World.Bank.Region=="South Asia")
female_higher_glucose_SSA<-female_higher_glucose %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
female_higher_glucose_MEN<-female_higher_glucose %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
female_higher_glucose_LAT<-female_higher_glucose %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
female_higher_glucose_EAP<-female_higher_glucose %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
female_higher_glucose_NOA<-female_higher_glucose %>% filter(mal.World.Bank.Region=="North America")

male_higher_overweight_ECA<-male_higher_overweight %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
male_higher_overweight_SAS<-male_higher_overweight %>% filter(mal.World.Bank.Region=="South Asia")
male_higher_overweight_SSA<-male_higher_overweight %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
male_higher_overweight_MEN<-male_higher_overweight %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
male_higher_overweight_LAT<-male_higher_overweight %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
male_higher_overweight_EAP<-male_higher_overweight %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
male_higher_overweight_NOA<-male_higher_overweight %>% filter(mal.World.Bank.Region=="North America")
female_higher_overweight_ECA<-female_higher_overweight %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
female_higher_overweight_SAS<-female_higher_overweight %>% filter(mal.World.Bank.Region=="South Asia")
female_higher_overweight_SSA<-female_higher_overweight %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
female_higher_overweight_MEN<-female_higher_overweight %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
female_higher_overweight_LAT<-female_higher_overweight %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
female_higher_overweight_EAP<-female_higher_overweight %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
female_higher_overweight_NOA<-female_higher_overweight %>% filter(mal.World.Bank.Region=="North America")

male_higher_obesity_ECA<-male_higher_obesity %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
male_higher_obesity_SAS<-male_higher_obesity %>% filter(mal.World.Bank.Region=="South Asia")
male_higher_obesity_SSA<-male_higher_obesity %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
male_higher_obesity_MEN<-male_higher_obesity %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
male_higher_obesity_LAT<-male_higher_obesity %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
male_higher_obesity_EAP<-male_higher_obesity %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
male_higher_obesity_NOA<-male_higher_obesity %>% filter(mal.World.Bank.Region=="North America")
female_higher_obesity_ECA<-female_higher_obesity %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
female_higher_obesity_SAS<-female_higher_obesity %>% filter(mal.World.Bank.Region=="South Asia")
female_higher_obesity_SSA<-female_higher_obesity %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
female_higher_obesity_MEN<-female_higher_obesity %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
female_higher_obesity_LAT<-female_higher_obesity %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
female_higher_obesity_EAP<-female_higher_obesity %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
female_higher_obesity_NOA<-female_higher_obesity %>% filter(mal.World.Bank.Region=="North America")

male_higher_smoking_ECA<-male_higher_smoking %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
male_higher_smoking_SAS<-male_higher_smoking %>% filter(mal.World.Bank.Region=="South Asia")
male_higher_smoking_SSA<-male_higher_smoking %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
male_higher_smoking_MEN<-male_higher_smoking %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
male_higher_smoking_LAT<-male_higher_smoking %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
male_higher_smoking_EAP<-male_higher_smoking %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
male_higher_smoking_NOA<-male_higher_smoking %>% filter(mal.World.Bank.Region=="North America")
female_higher_smoking_ECA<-female_higher_smoking %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
female_higher_smoking_SAS<-female_higher_smoking %>% filter(mal.World.Bank.Region=="South Asia")
female_higher_smoking_SSA<-female_higher_smoking %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
female_higher_smoking_MEN<-female_higher_smoking %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
female_higher_smoking_LAT<-female_higher_smoking %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
female_higher_smoking_EAP<-female_higher_smoking %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
female_higher_smoking_NOA<-female_higher_smoking %>% filter(mal.World.Bank.Region=="North America")

male_higher_inactivity_ECA<-male_higher_inactivity %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
male_higher_inactivity_SAS<-male_higher_inactivity %>% filter(mal.World.Bank.Region=="South Asia")
male_higher_inactivity_SSA<-male_higher_inactivity %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
male_higher_inactivity_MEN<-male_higher_inactivity %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
male_higher_inactivity_LAT<-male_higher_inactivity %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
male_higher_inactivity_EAP<-male_higher_inactivity %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
male_higher_inactivity_NOA<-male_higher_inactivity %>% filter(mal.World.Bank.Region=="North America")
female_higher_inactivity_ECA<-female_higher_inactivity %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
female_higher_inactivity_SAS<-female_higher_inactivity %>% filter(mal.World.Bank.Region=="South Asia")
female_higher_inactivity_SSA<-female_higher_inactivity %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
female_higher_inactivity_MEN<-female_higher_inactivity %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
female_higher_inactivity_LAT<-female_higher_inactivity %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
female_higher_inactivity_EAP<-female_higher_inactivity %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
female_higher_inactivity_NOA<-female_higher_inactivity %>% filter(mal.World.Bank.Region=="North America")

male_higher_drug_use_ECA<-male_higher_drug_use %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
male_higher_drug_use_SAS<-male_higher_drug_use %>% filter(mal.World.Bank.Region=="South Asia")
male_higher_drug_use_SSA<-male_higher_drug_use %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
male_higher_drug_use_MEN<-male_higher_drug_use %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
male_higher_drug_use_LAT<-male_higher_drug_use %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
male_higher_drug_use_EAP<-male_higher_drug_use %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
male_higher_drug_use_NOA<-male_higher_drug_use %>% filter(mal.World.Bank.Region=="North America")
female_higher_drug_use_ECA<-female_higher_drug_use %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
female_higher_drug_use_SAS<-female_higher_drug_use %>% filter(mal.World.Bank.Region=="South Asia")
female_higher_drug_use_SSA<-female_higher_drug_use %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
female_higher_drug_use_MEN<-female_higher_drug_use %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
female_higher_drug_use_LAT<-female_higher_drug_use %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
female_higher_drug_use_EAP<-female_higher_drug_use %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
female_higher_drug_use_NOA<-female_higher_drug_use %>% filter(mal.World.Bank.Region=="North America")

male_higher_unsafesex_ECA<-male_higher_unsafesex %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
male_higher_unsafesex_SAS<-male_higher_unsafesex %>% filter(mal.World.Bank.Region=="South Asia")
male_higher_unsafesex_SSA<-male_higher_unsafesex %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
male_higher_unsafesex_MEN<-male_higher_unsafesex %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
male_higher_unsafesex_LAT<-male_higher_unsafesex %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
male_higher_unsafesex_EAP<-male_higher_unsafesex %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
male_higher_unsafesex_NOA<-male_higher_unsafesex %>% filter(mal.World.Bank.Region=="North America")
female_higher_unsafesex_ECA<-female_higher_unsafesex %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
female_higher_unsafesex_SAS<-female_higher_unsafesex %>% filter(mal.World.Bank.Region=="South Asia")
female_higher_unsafesex_SSA<-female_higher_unsafesex %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
female_higher_unsafesex_MEN<-female_higher_unsafesex %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
female_higher_unsafesex_LAT<-female_higher_unsafesex %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
female_higher_unsafesex_EAP<-female_higher_unsafesex %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
female_higher_unsafesex_NOA<-female_higher_unsafesex %>% filter(mal.World.Bank.Region=="North America")

male_higher_hyp_prev_ECA<-male_higher_hyp_prev %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
male_higher_hyp_prev_SAS<-male_higher_hyp_prev %>% filter(mal.World.Bank.Region=="South Asia")
male_higher_hyp_prev_SSA<-male_higher_hyp_prev %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
male_higher_hyp_prev_MEN<-male_higher_hyp_prev %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
male_higher_hyp_prev_LAT<-male_higher_hyp_prev %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
male_higher_hyp_prev_EAP<-male_higher_hyp_prev %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
male_higher_hyp_prev_NOA<-male_higher_hyp_prev %>% filter(mal.World.Bank.Region=="North America")
female_higher_hyp_prev_ECA<-female_higher_hyp_prev %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
female_higher_hyp_prev_SAS<-female_higher_hyp_prev %>% filter(mal.World.Bank.Region=="South Asia")
female_higher_hyp_prev_SSA<-female_higher_hyp_prev %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
female_higher_hyp_prev_MEN<-female_higher_hyp_prev %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
female_higher_hyp_prev_LAT<-female_higher_hyp_prev %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
female_higher_hyp_prev_EAP<-female_higher_hyp_prev %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
female_higher_hyp_prev_NOA<-female_higher_hyp_prev %>% filter(mal.World.Bank.Region=="North America")

male_higher_diab_prev_ECA<-male_higher_diab_prev %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
male_higher_diab_prev_SAS<-male_higher_diab_prev %>% filter(mal.World.Bank.Region=="South Asia")
male_higher_diab_prev_SSA<-male_higher_diab_prev %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
male_higher_diab_prev_MEN<-male_higher_diab_prev %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
male_higher_diab_prev_LAT<-male_higher_diab_prev %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
male_higher_diab_prev_EAP<-male_higher_diab_prev %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
male_higher_diab_prev_NOA<-male_higher_diab_prev %>% filter(mal.World.Bank.Region=="North America")
female_higher_diab_prev_ECA<-female_higher_diab_prev %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
female_higher_diab_prev_SAS<-female_higher_diab_prev %>% filter(mal.World.Bank.Region=="South Asia")
female_higher_diab_prev_SSA<-female_higher_diab_prev %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
female_higher_diab_prev_MEN<-female_higher_diab_prev %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
female_higher_diab_prev_LAT<-female_higher_diab_prev %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
female_higher_diab_prev_EAP<-female_higher_diab_prev %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
female_higher_diab_prev_NOA<-female_higher_diab_prev %>% filter(mal.World.Bank.Region=="North America")

male_higher_HIV_AIDS_prev_ECA<-male_higher_HIV_AIDS_prev %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
male_higher_HIV_AIDS_prev_SAS<-male_higher_HIV_AIDS_prev %>% filter(mal.World.Bank.Region=="South Asia")
male_higher_HIV_AIDS_prev_SSA<-male_higher_HIV_AIDS_prev %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
male_higher_HIV_AIDS_prev_MEN<-male_higher_HIV_AIDS_prev %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
male_higher_HIV_AIDS_prev_LAT<-male_higher_HIV_AIDS_prev %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
male_higher_HIV_AIDS_prev_EAP<-male_higher_HIV_AIDS_prev %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
male_higher_HIV_AIDS_prev_NOA<-male_higher_HIV_AIDS_prev %>% filter(mal.World.Bank.Region=="North America")
female_higher_HIV_AIDS_prev_ECA<-female_higher_HIV_AIDS_prev %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
female_higher_HIV_AIDS_prev_SAS<-female_higher_HIV_AIDS_prev %>% filter(mal.World.Bank.Region=="South Asia")
female_higher_HIV_AIDS_prev_SSA<-female_higher_HIV_AIDS_prev %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
female_higher_HIV_AIDS_prev_MEN<-female_higher_HIV_AIDS_prev %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
female_higher_HIV_AIDS_prev_LAT<-female_higher_HIV_AIDS_prev %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
female_higher_HIV_AIDS_prev_EAP<-female_higher_HIV_AIDS_prev %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
female_higher_HIV_AIDS_prev_NOA<-female_higher_HIV_AIDS_prev %>% filter(mal.World.Bank.Region=="North America")

male_higher_hyp_mort_ECA<-male_higher_hyp_mort %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
male_higher_hyp_mort_SAS<-male_higher_hyp_mort %>% filter(mal.World.Bank.Region=="South Asia")
male_higher_hyp_mort_SSA<-male_higher_hyp_mort %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
male_higher_hyp_mort_MEN<-male_higher_hyp_mort %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
male_higher_hyp_mort_LAT<-male_higher_hyp_mort %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
male_higher_hyp_mort_EAP<-male_higher_hyp_mort %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
male_higher_hyp_mort_NOA<-male_higher_hyp_mort %>% filter(mal.World.Bank.Region=="North America")
female_higher_hyp_mort_ECA<-female_higher_hyp_mort %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
female_higher_hyp_mort_SAS<-female_higher_hyp_mort %>% filter(mal.World.Bank.Region=="South Asia")
female_higher_hyp_mort_SSA<-female_higher_hyp_mort %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
female_higher_hyp_mort_MEN<-female_higher_hyp_mort %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
female_higher_hyp_mort_LAT<-female_higher_hyp_mort %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
female_higher_hyp_mort_EAP<-female_higher_hyp_mort %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
female_higher_hyp_mort_NOA<-female_higher_hyp_mort %>% filter(mal.World.Bank.Region=="North America")

male_higher_diab_mort_ECA<-male_higher_diab_mort %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
male_higher_diab_mort_SAS<-male_higher_diab_mort %>% filter(mal.World.Bank.Region=="South Asia")
male_higher_diab_mort_SSA<-male_higher_diab_mort %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
male_higher_diab_mort_MEN<-male_higher_diab_mort %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
male_higher_diab_mort_LAT<-male_higher_diab_mort %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
male_higher_diab_mort_EAP<-male_higher_diab_mort %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
male_higher_diab_mort_NOA<-male_higher_diab_mort %>% filter(mal.World.Bank.Region=="North America")
female_higher_diab_mort_ECA<-female_higher_diab_mort %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
female_higher_diab_mort_SAS<-female_higher_diab_mort %>% filter(mal.World.Bank.Region=="South Asia")
female_higher_diab_mort_SSA<-female_higher_diab_mort %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
female_higher_diab_mort_MEN<-female_higher_diab_mort %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
female_higher_diab_mort_LAT<-female_higher_diab_mort %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
female_higher_diab_mort_EAP<-female_higher_diab_mort %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
female_higher_diab_mort_NOA<-female_higher_diab_mort %>% filter(mal.World.Bank.Region=="North America")

male_higher_HIV_AIDS_mort_ECA<-male_higher_HIV_AIDS_mort %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
male_higher_HIV_AIDS_mort_SAS<-male_higher_HIV_AIDS_mort %>% filter(mal.World.Bank.Region=="South Asia")
male_higher_HIV_AIDS_mort_SSA<-male_higher_HIV_AIDS_mort %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
male_higher_HIV_AIDS_mort_MEN<-male_higher_HIV_AIDS_mort %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
male_higher_HIV_AIDS_mort_LAT<-male_higher_HIV_AIDS_mort %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
male_higher_HIV_AIDS_mort_EAP<-male_higher_HIV_AIDS_mort %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
male_higher_HIV_AIDS_mort_NOA<-male_higher_HIV_AIDS_mort %>% filter(mal.World.Bank.Region=="North America")
female_higher_HIV_AIDS_mort_ECA<-female_higher_HIV_AIDS_mort %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
female_higher_HIV_AIDS_mort_SAS<-female_higher_HIV_AIDS_mort %>% filter(mal.World.Bank.Region=="South Asia")
female_higher_HIV_AIDS_mort_SSA<-female_higher_HIV_AIDS_mort %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
female_higher_HIV_AIDS_mort_MEN<-female_higher_HIV_AIDS_mort %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
female_higher_HIV_AIDS_mort_LAT<-female_higher_HIV_AIDS_mort %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
female_higher_HIV_AIDS_mort_EAP<-female_higher_HIV_AIDS_mort %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
female_higher_HIV_AIDS_mort_NOA<-female_higher_HIV_AIDS_mort %>% filter(mal.World.Bank.Region=="North America")

step1_male_higher_hyp_ECA<-step1_male_higher_hyp %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
step1_male_higher_hyp_SAS<-step1_male_higher_hyp %>% filter(mal.World.Bank.Region=="South Asia")
step1_male_higher_hyp_SSA<-step1_male_higher_hyp %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
step1_male_higher_hyp_MEN<-step1_male_higher_hyp %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
step1_male_higher_hyp_LAT<-step1_male_higher_hyp %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
step1_male_higher_hyp_EAP<-step1_male_higher_hyp %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
step1_male_higher_hyp_NOA<-step1_male_higher_hyp %>% filter(mal.World.Bank.Region=="North America")
step1_female_higher_hyp_ECA<-step1_female_higher_hyp %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
step1_female_higher_hyp_SAS<-step1_female_higher_hyp %>% filter(mal.World.Bank.Region=="South Asia")
step1_female_higher_hyp_SSA<-step1_female_higher_hyp %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
step1_female_higher_hyp_MEN<-step1_female_higher_hyp %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
step1_female_higher_hyp_LAT<-step1_female_higher_hyp %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
step1_female_higher_hyp_EAP<-step1_female_higher_hyp %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
step1_female_higher_hyp_NOA<-step1_female_higher_hyp %>% filter(mal.World.Bank.Region=="North America")

step2_male_higher_hyp_ECA<-step2_male_higher_hyp %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
step2_male_higher_hyp_SAS<-step2_male_higher_hyp %>% filter(mal.World.Bank.Region=="South Asia")
step2_male_higher_hyp_SSA<-step2_male_higher_hyp %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
step2_male_higher_hyp_MEN<-step2_male_higher_hyp %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
step2_male_higher_hyp_LAT<-step2_male_higher_hyp %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
step2_male_higher_hyp_EAP<-step2_male_higher_hyp %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
step2_male_higher_hyp_NOA<-step2_male_higher_hyp %>% filter(mal.World.Bank.Region=="North America")
step2_female_higher_hyp_ECA<-step2_female_higher_hyp %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
step2_female_higher_hyp_SAS<-step2_female_higher_hyp %>% filter(mal.World.Bank.Region=="South Asia")
step2_female_higher_hyp_SSA<-step2_female_higher_hyp %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
step2_female_higher_hyp_MEN<-step2_female_higher_hyp %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
step2_female_higher_hyp_LAT<-step2_female_higher_hyp %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
step2_female_higher_hyp_EAP<-step2_female_higher_hyp %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
step2_female_higher_hyp_NOA<-step2_female_higher_hyp %>% filter(mal.World.Bank.Region=="North America")

step3_male_higher_hyp_ECA<-step3_male_higher_hyp %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
step3_male_higher_hyp_SAS<-step3_male_higher_hyp %>% filter(mal.World.Bank.Region=="South Asia")
step3_male_higher_hyp_SSA<-step3_male_higher_hyp %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
step3_male_higher_hyp_MEN<-step3_male_higher_hyp %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
step3_male_higher_hyp_LAT<-step3_male_higher_hyp %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
step3_male_higher_hyp_EAP<-step3_male_higher_hyp %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
step3_male_higher_hyp_NOA<-step3_male_higher_hyp %>% filter(mal.World.Bank.Region=="North America")
step3_female_higher_hyp_ECA<-step3_female_higher_hyp %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
step3_female_higher_hyp_SAS<-step3_female_higher_hyp %>% filter(mal.World.Bank.Region=="South Asia")
step3_female_higher_hyp_SSA<-step3_female_higher_hyp %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
step3_female_higher_hyp_MEN<-step3_female_higher_hyp %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
step3_female_higher_hyp_LAT<-step3_female_higher_hyp %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
step3_female_higher_hyp_EAP<-step3_female_higher_hyp %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
step3_female_higher_hyp_NOA<-step3_female_higher_hyp %>% filter(mal.World.Bank.Region=="North America")

step1_male_higher_diab_ECA<-step1_male_higher_diab %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
step1_male_higher_diab_SAS<-step1_male_higher_diab %>% filter(mal.World.Bank.Region=="South Asia")
step1_male_higher_diab_SSA<-step1_male_higher_diab %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
step1_male_higher_diab_MEN<-step1_male_higher_diab %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
step1_male_higher_diab_LAT<-step1_male_higher_diab %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
step1_male_higher_diab_EAP<-step1_male_higher_diab %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
step1_male_higher_diab_NOA<-step1_male_higher_diab %>% filter(mal.World.Bank.Region=="North America")
step1_female_higher_diab_ECA<-step1_female_higher_diab %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
step1_female_higher_diab_SAS<-step1_female_higher_diab %>% filter(mal.World.Bank.Region=="South Asia")
step1_female_higher_diab_SSA<-step1_female_higher_diab %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
step1_female_higher_diab_MEN<-step1_female_higher_diab %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
step1_female_higher_diab_LAT<-step1_female_higher_diab %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
step1_female_higher_diab_EAP<-step1_female_higher_diab %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
step1_female_higher_diab_NOA<-step1_female_higher_diab %>% filter(mal.World.Bank.Region=="North America")

step2_male_higher_diab_ECA<-step2_male_higher_diab %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
step2_male_higher_diab_SAS<-step2_male_higher_diab %>% filter(mal.World.Bank.Region=="South Asia")
step2_male_higher_diab_SSA<-step2_male_higher_diab %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
step2_male_higher_diab_MEN<-step2_male_higher_diab %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
step2_male_higher_diab_LAT<-step2_male_higher_diab %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
step2_male_higher_diab_EAP<-step2_male_higher_diab %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
step2_male_higher_diab_NOA<-step2_male_higher_diab %>% filter(mal.World.Bank.Region=="North America")
step2_female_higher_diab_ECA<-step2_female_higher_diab %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
step2_female_higher_diab_SAS<-step2_female_higher_diab %>% filter(mal.World.Bank.Region=="South Asia")
step2_female_higher_diab_SSA<-step2_female_higher_diab %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
step2_female_higher_diab_MEN<-step2_female_higher_diab %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
step2_female_higher_diab_LAT<-step2_female_higher_diab %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
step2_female_higher_diab_EAP<-step2_female_higher_diab %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
step2_female_higher_diab_NOA<-step2_female_higher_diab %>% filter(mal.World.Bank.Region=="North America")

step3_male_higher_diab_ECA<-step3_male_higher_diab %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
step3_male_higher_diab_SAS<-step3_male_higher_diab %>% filter(mal.World.Bank.Region=="South Asia")
step3_male_higher_diab_SSA<-step3_male_higher_diab %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
step3_male_higher_diab_MEN<-step3_male_higher_diab %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
step3_male_higher_diab_LAT<-step3_male_higher_diab %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
step3_male_higher_diab_EAP<-step3_male_higher_diab %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
step3_male_higher_diab_NOA<-step3_male_higher_diab %>% filter(mal.World.Bank.Region=="North America")
step3_female_higher_diab_ECA<-step3_female_higher_diab %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
step3_female_higher_diab_SAS<-step3_female_higher_diab %>% filter(mal.World.Bank.Region=="South Asia")
step3_female_higher_diab_SSA<-step3_female_higher_diab %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
step3_female_higher_diab_MEN<-step3_female_higher_diab %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
step3_female_higher_diab_LAT<-step3_female_higher_diab %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
step3_female_higher_diab_EAP<-step3_female_higher_diab %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
step3_female_higher_diab_NOA<-step3_female_higher_diab %>% filter(mal.World.Bank.Region=="North America")

step1_male_higher_HIV_AIDS_ECA<-step1_male_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
step1_male_higher_HIV_AIDS_SAS<-step1_male_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="South Asia")
step1_male_higher_HIV_AIDS_SSA<-step1_male_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
step1_male_higher_HIV_AIDS_MEN<-step1_male_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
step1_male_higher_HIV_AIDS_LAT<-step1_male_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
step1_male_higher_HIV_AIDS_EAP<-step1_male_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
step1_male_higher_HIV_AIDS_NOA<-step1_male_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="North America")
step1_female_higher_HIV_AIDS_ECA<-step1_female_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
step1_female_higher_HIV_AIDS_SAS<-step1_female_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="South Asia")
step1_female_higher_HIV_AIDS_SSA<-step1_female_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
step1_female_higher_HIV_AIDS_MEN<-step1_female_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
step1_female_higher_HIV_AIDS_LAT<-step1_female_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
step1_female_higher_HIV_AIDS_EAP<-step1_female_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
step1_female_higher_HIV_AIDS_NOA<-step1_female_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="North America")

step2_male_higher_HIV_AIDS_ECA<-step2_male_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
step2_male_higher_HIV_AIDS_SAS<-step2_male_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="South Asia")
step2_male_higher_HIV_AIDS_SSA<-step2_male_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
step2_male_higher_HIV_AIDS_MEN<-step2_male_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
step2_male_higher_HIV_AIDS_LAT<-step2_male_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
step2_male_higher_HIV_AIDS_EAP<-step2_male_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
step2_male_higher_HIV_AIDS_NOA<-step2_male_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="North America")
step2_female_higher_HIV_AIDS_ECA<-step2_female_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
step2_female_higher_HIV_AIDS_SAS<-step2_female_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="South Asia")
step2_female_higher_HIV_AIDS_SSA<-step2_female_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
step2_female_higher_HIV_AIDS_MEN<-step2_female_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
step2_female_higher_HIV_AIDS_LAT<-step2_female_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
step2_female_higher_HIV_AIDS_EAP<-step2_female_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
step2_female_higher_HIV_AIDS_NOA<-step2_female_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="North America")

step3_male_higher_HIV_AIDS_ECA<-step3_male_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
step3_male_higher_HIV_AIDS_SAS<-step3_male_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="South Asia")
step3_male_higher_HIV_AIDS_SSA<-step3_male_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
step3_male_higher_HIV_AIDS_MEN<-step3_male_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
step3_male_higher_HIV_AIDS_LAT<-step3_male_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
step3_male_higher_HIV_AIDS_EAP<-step3_male_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
step3_male_higher_HIV_AIDS_NOA<-step3_male_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="North America")
step3_female_higher_HIV_AIDS_ECA<-step3_female_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Europe and Central Asia")
step3_female_higher_HIV_AIDS_SAS<-step3_female_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="South Asia")
step3_female_higher_HIV_AIDS_SSA<-step3_female_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Sub-Saharan Africa")
step3_female_higher_HIV_AIDS_MEN<-step3_female_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Middle East and North Africa")
step3_female_higher_HIV_AIDS_LAT<-step3_female_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="Latin America & the Caribbean")
step3_female_higher_HIV_AIDS_EAP<-step3_female_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="East Asia and Pacific")
step3_female_higher_HIV_AIDS_NOA<-step3_female_higher_HIV_AIDS %>% filter(mal.World.Bank.Region=="North America")

######################