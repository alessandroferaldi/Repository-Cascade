######## Package and functions ########
library(ggplot2)
library(reshape2)
require(tidyverse)
`%ni%` <- Negate(`%in%`)

######## Import Data and Pre processing ########

### Import ISO3 list ####
iso3<-read.csv2(file="~/Desktop/Data/ISO3.csv")

### Import World Bank Income groups 2023 ####
inc_grp<-read.csv(file="~/Desktop/Data/WB_income2023.csv")

# Set Upper-middle Income Group for Venezuela (as in 2022), since 2023 is missing
inc_grp$wbinc2023<-ifelse(inc_grp$ISO=="VEN","UM",inc_grp$wbinc2023)   

### Import World Bank Regions ####
wbreg<-read.csv(file="~/Desktop/Data/WB_region.csv")          

### Import Risk Factors and include ISO3, World Bank income group and region (Adjust location names) ####
risk<-read.csv2(file="~/Desktop/Data/Risk_factors_2019.csv") 
risk$Location<-ifelse(risk$Location=="Türkiye","Turkey",risk$Location)
risk$Location<-ifelse(risk$Location=="Côte d'Ivoire","Cte d'Ivoire",risk$Location)
risk<-merge(iso3,risk,by.x="location",by.y="Location",all.x = T)
risk<-merge(risk,inc_grp,by.x="iso",by.y="ISO",all.x = T)
risk<-merge(risk,wbreg,by.x="iso",by.y="ISO3",all.x = T)

### Import Prevalence and include ISO3, World Bank income group and region (Adjust location names) ####
prev<-read.csv(file="~/Desktop/Data/Cause_prevalence.csv")
prev$Location<-ifelse(prev$Location=="Türkiye","Turkey",prev$Location)
prev$Location<-ifelse(prev$Location=="Côte d'Ivoire","Cte d'Ivoire",prev$Location)
prev<-merge(iso3,prev,by.x="location",by.y="Location",all.y=T)
prev<-merge(prev,inc_grp,by.x="iso",by.y="ISO",all.x=T)
prev<-merge(prev,wbreg,by.x="iso",by.y="ISO3",all.x=T)

### Import Death Rates and include ISO3, World Bank income group and region (Adjust location names) ####
mort<-read.csv(file="~/Desktop/Data/Cause_mortality.csv") 
mort$Location<-ifelse(mort$Location=="Türkiye","Turkey",mort$Location)
mort$Location<-ifelse(mort$Location=="Côte d'Ivoire","Cte d'Ivoire",mort$Location)
mort<-merge(iso3,mort,by.x="location",by.y="Location",all.y=T)
mort<-merge(mort,inc_grp,by.x="iso",by.y="ISO",all.x=T)
mort<-merge(mort,wbreg,by.x="iso",by.y="ISO3",all.x=T)

### Hypertension Cascade (include ISO3, World Bank income group and region) ####
hyp<-read.csv2(file="~/Desktop/Data/Hypertension_cascade.csv")
hyp<-merge(hyp,inc_grp,by.x="ISO",by.y="ISO",all.x = T)
hyp<-merge(hyp,wbreg,by.x="ISO",by.y="ISO3",all.x = T)

### Importn Diabetes mellitus Cascade (include ISO3, World Bank income group and region) ####
dat_diab<-read.csv2(file="~/Desktop/Data/Diabetes_cascade.csv",sep="\t") ### with income groups

### Remove tested
dat_diab1<- dat_diab %>% filter(Step!="tested")

### Separate each step
dat_diab_prev<-dat_diab1 %>% filter(Step=="prevalence")
dat_diab_cont<-dat_diab1 %>% filter(Step=="controlled")
dat_diab_trea<-dat_diab1 %>% filter(Step=="treated")
dat_diab_diag<-dat_diab1 %>% filter(Step=="diagnosed")

### Merge data
dat_diab2<-merge(dat_diab_prev,dat_diab_diag[,c("Age","Sex","Country","n","denominator_unconditional","demoninator_conditional")],
                 by=c("Age","Sex","Country"),suffixes = c("", ".diag"),all=T)
dat_diab3<-merge(dat_diab2,dat_diab_trea[,c("Age","Sex","Country","n","denominator_unconditional","demoninator_conditional")],
                 by=c("Age","Sex","Country"),suffixes = c("", ".trea"),all=T)
dat_diab4<-merge(dat_diab3,dat_diab_cont[,c("Age","Sex","Country","n","denominator_unconditional","demoninator_conditional")],
                 by=c("Age","Sex","Country"),suffixes = c("", ".contr"),all=T)
dat_diab5<-dat_diab4 %>% filter(Age!="all ages")

### Adjust country names
dat_diab5[dat_diab5$Country=="Afghanistan ","wb_region"]<-"South Asia"
dat_diab5[dat_diab5$Country=="Ethopia","wb_region"]<-"Sub-Saharan Africa"
dat_diab5[dat_diab5$Country=="Morocco","wb_region"]<-"Middle East and North Africa"
dat_diab5[dat_diab5$Country=="Myanmmar","wb_region"]<-"East Asia and Pacific"
dat_diab5[dat_diab5$Country=="Solomon Islands ","wb_region"]<-"East Asia and Pacific"
dat_diab5[dat_diab5$Country=="Timor Leste","wb_region"]<-"East Asia and Pacific"
dat_diab5[dat_diab5$Country=="Turkemenistan","wb_region"]<-"Europe and Central Asia"
dat_diab5[dat_diab5$Country=="Vietnam","wb_region"]<-"East Asia and Pacific"

dat_diab_age<-dat_diab5

### Remove young ages
dat_diab_age<-dat_diab_age %>% filter(Age!="15-29")
diab<-dat_diab_age

### Import ISO3 and include World Bank income group and region
iso3_diab<-read.csv2(file="~/Desktop/Data/ISO3_diab.csv")
diab<-merge(iso3_diab,diab,by.x="location",by.y="Country")
diab<-merge(diab,inc_grp,by.x="iso",by.y="ISO",all.x = T)
diab<-merge(diab,wbreg,by.x="iso",by.y="ISO3",all.x = T)

### Calculate cascade
diab$step1<-diab$n.diag/diab$denominator_unconditional.diag
diab$step2<-diab$n.trea/diab$denominator_unconditional.trea
diab$step3<-diab$n.contr/diab$denominator_unconditional.contr
diab$step2c<-diab$n.trea/diab$n.diag
diab$step3c<-diab$n.contr/diab$n.contr

### Compute 95% binomial confidence intervals for cascade steps
for(i in 1:nrow(diab)){
  diab$step1_lower[i]<-prop.test(diab$n.diag[i],diab$denominator_unconditional.diag[i],conf.level=.95)$conf.int[1]
  diab$step2_lower[i]<-prop.test(diab$n.trea[i],diab$denominator_unconditional.trea[i],conf.level=.95)$conf.int[1]
  diab$step3_lower[i]<-prop.test(diab$n.contr[i],diab$denominator_unconditional.contr[i],conf.level=.95)$conf.int[1]
  diab$step1_upper[i]<-prop.test(diab$n.diag[i],diab$denominator_unconditional.diag[i],conf.level=.95)$conf.int[2]
  diab$step2_upper[i]<-prop.test(diab$n.trea[i],diab$denominator_unconditional.trea[i],conf.level=.95)$conf.int[2]
  diab$step3_upper[i]<-prop.test(diab$n.contr[i],diab$denominator_unconditional.contr[i],conf.level=.95)$conf.int[2]
  diab$step2c_lower[i]<-prop.test(diab$n.trea[i],diab$demoninator_conditional.trea[i]+1,conf.level=.95)$conf.int[1]
  diab$step2c_upper[i]<-prop.test(diab$n.trea[i],diab$demoninator_conditional.trea[i]+1,conf.level=.95)$conf.int[2]
  diab$step3c_lower[i]<-prop.test(diab$n.contr[i],diab$demoninator_conditional.contr[i]+1,conf.level=.95)$conf.int[1]
  diab$step3c_upper[i]<-prop.test(diab$n.contr[i],diab$demoninator_conditional.contr[i]+1,conf.level=.95)$conf.int[2]
}

### Duplicate iso variable
diab$iso3<-diab$iso

### Import HIV/AIDS Cascade and include ISO3, World Bank income group and region ##### 
HIV_AIDS<-read.csv2(file="~/Desktop/Data/HIVAIDS_cascade.csv")
HIV_AIDS$Country<-ifelse(HIV_AIDS$Country=="Cape Verde","Cabo Verde",HIV_AIDS$Country)
HIV_AIDS$Country<-ifelse(HIV_AIDS$Country=="Ivory Coast","Cte d'Ivoire",HIV_AIDS$Country)

HIV_AIDS<-merge(iso3,HIV_AIDS,by.x="location",by.y="Country",all.y = T)
HIV_AIDS<-merge(HIV_AIDS,inc_grp,by.x="iso",by.y="ISO")
HIV_AIDS<-merge(HIV_AIDS,wbreg,by.x="iso",by.y="ISO3")

