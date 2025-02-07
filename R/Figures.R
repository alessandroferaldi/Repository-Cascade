
######## Source Clean Data ##############
source(file="~/Desktop/Data Processing.R")

######## Source Analysis.R to get Results ##############
source(file="~/Desktop/Analysis.R")

######## Plot Health Pathways - Overall ############
##### Hypertension #####
#### Males 
hyp_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  sodi=cbind(table(male_higher_sodium$mal.Age.group.name)),
  gluc=0,
  over=cbind(table(male_higher_overweight$mal.Age.group.name)),
  obes=c(1,0,0,0,0,0,0,0,1,1),
  smok=cbind(table(male_higher_smoking$mal.Age.group.name)),
  prev=c(8,3,1,1,1,0,0,0,0,0),
  step1=0,
  step2=0,
  step3=0,
  mort=cbind(table(male_higher_hyp_mort$mal.Age.group.name)))

#### Females 
hyp_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  sodi=cbind(table(female_higher_sodium$mal.Age.group.name)),
  gluc=c(0,0,0,0,0,0,0,0,0,1),
  over=cbind(table(female_higher_overweight$mal.Age.group.name)),
  obes=cbind(table(female_higher_obesity$mal.Age.group.name)),
  smok=c(0,0,0,0,0,0,0,1,0,0),
  prev=c(0,0,0,0,0,0,0,0,1,1),
  step1=c(cbind(table(step1_female_higher_hyp$mal.Age)),0),
  step2=c(cbind(table(step2_female_higher_hyp$mal.Age)),0,0),
  step3=c(cbind(table(step3_female_higher_hyp$mal.Age)),0,0,0),
  mort=c(0,0,0,0,0,0,0,0,1,1))

hyp_path_male_OV<-hyp_path_male
hyp_path_female_OV<-hyp_path_female

#### Proportions
hyp_path_male_prop<-data.frame(age=row.names(hyp_path_male),
                               ceiling(hyp_path_male[,1:6]/204*100),
                               ceiling(hyp_path_male[,7:9]/200*100),
                               mort=ceiling(hyp_path_male[,10]/204*100))

hyp_path_female_prop<-data.frame(age=row.names(hyp_path_female),
                                 ceiling(hyp_path_female[,1:6]/204*100),
                                 ceiling(hyp_path_female[,7:9]/200*100),
                                 mort=ceiling(hyp_path_female[,10]/204*100))

#### Female-Male difference
hyp_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                          hyp_path_female-hyp_path_male);rownames(hyp_path_diff)<-NULL
hyp_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               ceiling(hyp_path_diff[,2:7]/204*100),
                               ceiling(hyp_path_diff[,8:10]/200*100),
                               mort=ceiling(hyp_path_diff[,11]/204*100))
rownames(hyp_path_diff_prop)<-NULL

#### Add missing %, needed for complete legend
# hyp_path_diff_prop[1:3,2]<-c(90,65,-90)

#### Set dataframe for plot
df <- hyp_path_diff_prop
colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "More Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot


##### Diabetes #####
#### Males 
diab_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  gluc=0,
  over=cbind(table(male_higher_overweight$mal.Age.group.name)),
  obes=c(1,0,0,0,0,0,0,0,1,1),
  smok=cbind(table(male_higher_smoking$mal.Age.group.name)),
  inact=cbind(table(male_higher_inactivity$mal.Age.group.name)),
  prev=cbind(table(male_higher_diab_prev$mal.Age.group.name)),
  step1=0,
  step2=0,
  step3=0,
  mort=cbind(table(male_higher_diab_mort$mal.Age.group.name)))

#### Females 
diab_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  gluc=c(0,0,0,0,0,0,0,0,0,1),
  over=cbind(table(female_higher_overweight$mal.Age.group.name)),
  obes=cbind(table(female_higher_obesity$mal.Age.group.name)),
  smok=c(0,0,0,0,0,0,0,1,0,0),
  inact=0,
  prev=cbind(table(female_higher_diab_prev$mal.Age)),
  step1=c(0,0,0,1,1,1,1,1,1,1),
  step2=c(0,0,0,1,1,1,1,1,1,1),
  step3=0,
  mort=cbind(table(female_higher_diab_mort$mal.Age)))

diab_path_male_OV<-diab_path_male
diab_path_female_OV<-diab_path_female

#### Female-Male difference
diab_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                           diab_path_female-diab_path_male);rownames(diab_path_diff)<-NULL
diab_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                ceiling(diab_path_diff[,2:7]/204*100),
                                ceiling(diab_path_diff[,8:10]/39*100),
                                mort=ceiling(diab_path_diff[,11]/204*100))
rownames(diab_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- diab_path_diff_prop
colnames(df)<-c("Age_Group","Glucose","Overweight","Obesity","Smoking","Phys. Inac.","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot


##### HIV/AIDS #####
#### Males 
HIV_AIDS_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  druguse=cbind(table(male_higher_drug_use$mal.Age.group.name)),
  unsafe=c(1,0,0,0,0,0,0,0,0,0),
  prev=cbind(table(male_higher_HIV_AIDS_prev$mal.Age.group.name)),
  step1=0,
  step2=1,
  step3=1,
  mort=cbind(table(male_higher_HIV_AIDS_mort$mal.Age.group.name)))

#### Females 
HIV_AIDS_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  druguse=c(0,0,0,0,0,1,1,1,1,5),
  unsafe=cbind(table(female_higher_unsafesex$mal.Age.group.name)),
  prev=cbind(table(female_higher_HIV_AIDS_prev$mal.Age.group.name)),
  step1=length(table(step1_female_higher_HIV_AIDS$mal.iso)),
  step2=length(table(step2_female_higher_HIV_AIDS$mal.iso)),
  step3=length(table(step3_female_higher_HIV_AIDS$mal.iso)),
  mort=cbind(table(female_higher_HIV_AIDS_mort$mal.Age.group.name)))

HIV_AIDS_path_male_OV<-HIV_AIDS_path_male
HIV_AIDS_path_female_OV<-HIV_AIDS_path_female

#### Female-Male difference
HIV_AIDS_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               HIV_AIDS_path_female-HIV_AIDS_path_male);rownames(HIV_AIDS_path_diff)<-NULL
HIV_AIDS_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                    ceiling(HIV_AIDS_path_diff[,2:4]/204*100),
                                    ceiling(HIV_AIDS_path_diff[,5:7]/76*100),
                                    mort=ceiling(HIV_AIDS_path_diff[,8]/204*100))
rownames(HIV_AIDS_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- HIV_AIDS_path_diff_prop
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = "",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot


######## Plot Health Pathways by Income ############
##### Hypertension - High #####
#### Males 
hyp_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  sodi=cbind(table(male_higher_sodium_HI$mal.Age.group.name)),
  gluc=0,
  over=c(cbind(table(male_higher_overweight_HI$mal.Age.group.name)),0),
  obes=0,
  smok=cbind(table(male_higher_smoking_HI$mal.Age.group.name)),
  prev=c(1,1,1,1,1,0,0,0,0,0),
  step1=0,
  step2=0,
  step3=0,
  mort=cbind(table(male_higher_hyp_mort_HI$mal.Age.group.name)))

#### Females 
hyp_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  sodi=0,
  gluc=0,
  over=c(0,0,0,cbind(table(female_higher_overweight_HI$mal.Age.group.name)),0,0),
  obes=cbind(table(female_higher_obesity_HI$mal.Age.group.name)),
  smok=0,
  prev=0,
  step1=0,
  step2=0,
  step3=0,
  mort=c(0,0,0,0,0,0,0,0,1,1))

hyp_path_male_HI<-hyp_path_male
hyp_path_female_HI<-hyp_path_female

#### Proportions
hyp_path_male_prop<-data.frame(age=row.names(hyp_path_male),
                               ceiling(hyp_path_male[,1:6]/67*100),
                               ceiling(hyp_path_male[,7:9]/63*100),
                               mort=ceiling(hyp_path_male[,10]/67*100))

hyp_path_female_prop<-data.frame(age=row.names(hyp_path_female),
                                 ceiling(hyp_path_female[,1:6]/67*100),
                                 ceiling(hyp_path_female[,7:9]/63*100),
                                 mort=ceiling(hyp_path_female[,10]/67*100))

#### Female-Male difference
hyp_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                          hyp_path_female-hyp_path_male);rownames(hyp_path_diff)<-NULL
hyp_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               ceiling(hyp_path_diff[,2:7]/67*100),
                               ceiling(hyp_path_diff[,8:10]/63*100),
                               mort=ceiling(hyp_path_diff[,11]/67*100))
rownames(hyp_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- hyp_path_diff_prop
colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "More Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot




##### Hypertension - Upper-Middle #####
#### Males 
hyp_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  sodi=c(0,0,0,0,1,0,0,0,1,0),
  gluc=0,
  over=c(4,2,0,0,0,0,0,0,0,0),
  obes=c(1,0,0,0,0,0,0,0,0,0),
  smok=cbind(table(male_higher_smoking_UM$mal.Age.group.name)),
  prev=c(4,1,0,0,0,0,0,0,0,0),
  step1=0,
  step2=0,
  step3=0,
  mort=cbind(table(male_higher_hyp_mort_UM$mal.Age.group.name)))
#### Females 
hyp_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  sodi=0,
  gluc=0,
  over=cbind(table(female_higher_overweight_UM$mal.Age.group.name)),
  obes=cbind(table(female_higher_obesity_UM$mal.Age.group.name)),
  smok=0,
  prev=0,
  step1=c(cbind(table(step1_female_higher_hyp_UM$mal.Age)),0,0),
  step2=c(cbind(table(step2_female_higher_hyp_UM$mal.Age)),0,0),
  step3=c(cbind(table(step3_female_higher_hyp_UM$mal.Age)),0,0,0),
  mort=0)

hyp_path_male_UM<-hyp_path_male
hyp_path_female_UM<-hyp_path_female

#### Proportions
hyp_path_male_prop<-data.frame(age=row.names(hyp_path_male),
                               ceiling(hyp_path_male[,1:6]/54*100),
                               ceiling(hyp_path_male[,7:9]/54*100),
                               mort=ceiling(hyp_path_male[,10]/54*100))

hyp_path_female_prop<-data.frame(age=row.names(hyp_path_female),
                                 ceiling(hyp_path_female[,1:6]/54*100),
                                 ceiling(hyp_path_female[,7:9]/54*100),
                                 mort=ceiling(hyp_path_female[,10]/54*100))

#### Female-Male difference
hyp_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                          hyp_path_female-hyp_path_male);rownames(hyp_path_diff)<-NULL
hyp_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               ceiling(hyp_path_diff[,2:7]/54*100),
                               ceiling(hyp_path_diff[,8:10]/54*100),
                               mort=ceiling(hyp_path_diff[,11]/54*100))
rownames(hyp_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- hyp_path_diff_prop
colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "More Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot




##### Hypertension - Lower-Middle #####
#### Males 
hyp_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  sodi=cbind(table(male_higher_sodium_LM$mal.Age.group.name)),
  gluc=0,
  over=0,
  obes=0,
  smok=cbind(table(male_higher_smoking_LM$mal.Age.group.name)),
  prev=c(2,1,0,0,0,0,0,0,0,0),
  step1=0,
  step2=0,
  step3=0,
  mort=c(cbind(table(male_higher_hyp_mort_LM$mal.Age.group.name)),0))

#### Females 
hyp_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  sodi=cbind(table(female_higher_sodium_LM$mal.Age.group.name)),
  gluc=c(0,0,0,0,0,0,0,0,0,1),
  over=cbind(table(female_higher_overweight_LM$mal.Age.group.name)),
  obes=cbind(table(female_higher_obesity_LM$mal.Age.group.name)),
  smok=c(0,0,0,0,0,0,0,1,0,0),
  prev=c(0,0,0,0,0,0,0,0,1,1),
  step1=c(cbind(table(step1_female_higher_hyp_LM$mal.Age)),0),
  step2=c(cbind(table(step2_female_higher_hyp_LM$mal.Age)),0,0),
  step3=c(2,2,0,0,0,0,0,0,0,0),
  mort=0)

hyp_path_male_LM<-hyp_path_male
hyp_path_female_LM<-hyp_path_female

#### Proportions
hyp_path_male_prop<-data.frame(age=row.names(hyp_path_male),
                               ceiling(hyp_path_male[,1:6]/54*100),
                               ceiling(hyp_path_male[,7:9]/54*100),
                               mort=ceiling(hyp_path_male[,10]/54*100))

hyp_path_female_prop<-data.frame(age=row.names(hyp_path_female),
                                 ceiling(hyp_path_female[,1:6]/54*100),
                                 ceiling(hyp_path_female[,7:9]/54*100),
                                 mort=ceiling(hyp_path_female[,10]/54*100))

#### Female-Male difference
hyp_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                          hyp_path_female-hyp_path_male);rownames(hyp_path_diff)<-NULL
hyp_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               ceiling(hyp_path_diff[,2:7]/54*100),
                               ceiling(hyp_path_diff[,8:10]/54*100),
                               mort=ceiling(hyp_path_diff[,11]/54*100))
rownames(hyp_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- hyp_path_diff_prop
colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "More Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot


##### Hypertension - Low #####
#### Males 
hyp_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  sodi=0,
  gluc=0,
  over=c(0,0,1,0,0,0,0,0,0,1),
  obes=c(0,0,0,0,0,0,0,0,1,1),
  smok=cbind(table(male_higher_smoking_LO$mal.Age.group.name)),
  prev=0,
  step1=0,
  step2=0,
  step3=0,
  mort=c(cbind(table(male_higher_hyp_mort_LO$mal.Age.group.name)),0,0,0))

#### Females 
hyp_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  sodi=c(0,0,0,0,4,0,0,0,0,0),
  gluc=0,
  over=cbind(table(female_higher_overweight_LO$mal.Age.group.name)),
  obes=cbind(table(female_higher_obesity_LO$mal.Age.group.name)),
  smok=0,
  prev=0,
  step1=0,
  step2=0,
  step3=0,
  mort=0)

hyp_path_male_LO<-hyp_path_male
hyp_path_female_LO<-hyp_path_female

#### Proportions
hyp_path_male_prop<-data.frame(age=row.names(hyp_path_male),
                               ceiling(hyp_path_male[,1:6]/26*100),
                               ceiling(hyp_path_male[,7:9]/26*100),
                               mort=ceiling(hyp_path_male[,10]/26*100))

hyp_path_female_prop<-data.frame(age=row.names(hyp_path_female),
                                 ceiling(hyp_path_female[,1:6]/26*100),
                                 ceiling(hyp_path_female[,7:9]/26*100),
                                 mort=ceiling(hyp_path_female[,10]/26*100))

#### Female-Male difference
hyp_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                          hyp_path_female-hyp_path_male);rownames(hyp_path_diff)<-NULL
hyp_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               ceiling(hyp_path_diff[,2:7]/26*100),
                               ceiling(hyp_path_diff[,8:10]/26*100),
                               mort=ceiling(hyp_path_diff[,11]/26*100))
rownames(hyp_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- hyp_path_diff_prop
colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "More Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot




############################################
##### Diabetes - High #####
#### Males 
diab_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  gluc=0,
  over=c(cbind(table(male_higher_overweight_HI$mal.Age.group.name)),0),
  obes=0,
  smok=cbind(table(male_higher_smoking_HI$mal.Age.group.name)),
  inact=c(1,0,0,0,0,0,0,0,0,0),
  prev=cbind(table(male_higher_diab_prev_HI$mal.Age.group.name)),
  step1=0,
  step2=0,
  step3=0,
  mort=cbind(table(male_higher_diab_mort_HI$mal.Age.group.name)))

#### Females 
diab_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  gluc=0,
  over=c(0,0,0,1,2,2,2,2,0,0),
  obes=cbind(table(female_higher_obesity_HI$mal.Age.group.name)),
  smok=0,
  inact=0,
  prev=c(1,0,0,0,0,0,1,1,1,1),
  step1=0,
  step2=0,
  step3=0,
  mort=c(0,0,0,0,0,0,0,0,2,2))

diab_path_male_HI<-diab_path_male
diab_path_female_HI<-diab_path_female

#### Female-Male difference
diab_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                           diab_path_female-diab_path_male);rownames(diab_path_diff)<-NULL
diab_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                ceiling(diab_path_diff[,2:7]/67*100),
                                ceiling(diab_path_diff[,8:10]/4*100),
                                mort=ceiling(diab_path_diff[,11]/67*100))
rownames(diab_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- diab_path_diff_prop
colnames(df)<-c("Age_Group","Glucose","Overweight","Obesity","Smoking","Phys. Inac.","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot



##### Diabetes - Upper-Middle #####
#### Males 
diab_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  gluc=0,
  over=c(4,2,0,0,0,0,0,0,0,0),
  obes=c(1,0,0,0,0,0,0,0,0,0),
  smok=cbind(table(male_higher_smoking_UM$mal.Age.group.name)),
  inact=c(1,0,0,1,1,0,0,0,0,0),
  prev=cbind(table(male_higher_diab_prev_UM$mal.Age.group.name)),
  step1=0,
  step2=0,
  step3=0,
  mort=cbind(table(male_higher_diab_mort_UM$mal.Age.group.name)))

#### Females 
diab_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  gluc=0,
  over=cbind(table(female_higher_overweight_UM$mal.Age.group.name)),
  obes=cbind(table(female_higher_obesity_UM$mal.Age.group.name)),
  smok=0,
  inact=0,
  prev=cbind(table(female_higher_diab_prev_UM$mal.Age.group.name)),
  step1=0,
  step2=0,
  step3=0,
  mort=c(1,0,0,0,2,1,1,1,2,4))

diab_path_male_UM<-diab_path_male
diab_path_female_UM<-diab_path_female

#### Female-Male difference
diab_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                           diab_path_female-diab_path_male);rownames(diab_path_diff)<-NULL
diab_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                ceiling(diab_path_diff[,2:7]/54*100),
                                ceiling(diab_path_diff[,8:10]/10*100),
                                mort=ceiling(diab_path_diff[,11]/54*100))
rownames(diab_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- diab_path_diff_prop
colnames(df)<-c("Age_Group","Glucose","Overweight","Obesity","Smoking","Phys. Inac.","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot




##### Diabetes - Lower-Middle #####
#### Males 
diab_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  gluc=0,
  over=0,
  obes=0,
  smok=cbind(table(male_higher_smoking_LM$mal.Age.group.name)),
  inact=c(1,0,1,0,0,0,1,0,0,0),
  prev=cbind(table(male_higher_diab_prev_LM$mal.Age.group.name)),
  step1=0,
  step2=0,
  step3=0,
  mort=c(9,5,5,3,5,1,0,1,1,0))

#### Females 
diab_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  gluc=0,
  over=cbind(table(female_higher_overweight_LM$mal.Age.group.name)),
  obes=cbind(table(female_higher_obesity_LM$mal.Age.group.name)),
  smok=0,
  inact=0,
  prev=cbind(table(female_higher_diab_prev_LM$mal.Age.group.name)),
  step1=c(0,0,0,1,1,1,1,1,1,1),
  step2=c(0,0,0,1,1,1,1,1,1,1),
  step3=0,
  mort=c(1,1,1,0,1,0,0,0,0,0))

diab_path_male_LM<-diab_path_male
diab_path_female_LM<-diab_path_female

#### Female-Male difference
diab_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                           diab_path_female-diab_path_male);rownames(diab_path_diff)<-NULL
diab_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                ceiling(diab_path_diff[,2:7]/54*100),
                                ceiling(diab_path_diff[,8:10]/20*100),
                                mort=ceiling(diab_path_diff[,11]/54*100))
rownames(diab_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- diab_path_diff_prop
colnames(df)<-c("Age_Group","Glucose","Overweight","Obesity","Smoking","Phys. Inac.","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot



##### Diabetes - Low #####
#### Males 
diab_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  gluc=0,
  over=c(0,0,1,0,0,0,0,0,0,1),
  obes=c(0,0,0,0,0,0,0,0,1,1),
  smok=cbind(table(male_higher_smoking_LO$mal.Age.group.name)),
  inact=0,
  prev=cbind(table(male_higher_diab_prev_LO$mal.Age.group.name)),
  step1=0,
  step2=0,
  step3=0,
  mort=c(6,0,1,2,3,0,0,0,0,0))

#### Females 
diab_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  gluc=0,
  over=cbind(table(female_higher_overweight_LO$mal.Age.group.name)),
  obes=cbind(table(female_higher_obesity_LO$mal.Age.group.name)),
  smok=0,
  inact=0,
  prev=c(1,0,0,0,0,0,1,1,1,1),
  step1=0,
  step2=0,
  step3=0,
  mort=c(0,0,0,0,0,0,0,0,2,2))

diab_path_male_LO<-diab_path_male
diab_path_female_LO<-diab_path_female

#### Female-Male difference
diab_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                           diab_path_female-diab_path_male);rownames(diab_path_diff)<-NULL
diab_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                ceiling(diab_path_diff[,2:7]/26*100),
                                ceiling(diab_path_diff[,8:10]/5*100),
                                mort=ceiling(diab_path_diff[,11]/26*100))
rownames(diab_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- diab_path_diff_prop
colnames(df)<-c("Age_Group","Glucose","Overweight","Obesity","Smoking","Phys. Inac.","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

####

############################################
##### HIV/AIDS - High #####
#### Males 
HIV_AIDS_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  druguse=cbind(table(male_higher_drug_use_HI$mal.Age.group.name)),
  unsafe=c(0,0,0,0,0,0,0,0,0,0),
  prev=cbind(table(male_higher_HIV_AIDS_prev_HI$mal.Age.group.name)),
  step1=length(table(step1_male_higher_HIV_AIDS_HI$mal.iso)),
  step2=length(table(step2_male_higher_HIV_AIDS_HI$mal.iso)),
  step3=length(table(step3_male_higher_HIV_AIDS_HI$mal.iso)),
  mort=cbind(table(male_higher_HIV_AIDS_mort_HI$mal.Age.group.name)))

#### Females 
HIV_AIDS_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  druguse=c(0,0,0,0,0,0,0,0,1,1),
  unsafe=cbind(table(female_higher_unsafesex_HI$mal.Age.group.name)),
  prev=c(0,0,1,1,0,0,0,0,0,0),
  step1=length(table(step1_female_higher_HIV_AIDS_HI$mal.iso)),
  step2=length(table(step2_female_higher_HIV_AIDS_HI$mal.iso)),
  step3=length(table(step3_female_higher_HIV_AIDS_HI$mal.iso)),
  mort=cbind(table(female_higher_HIV_AIDS_mort_HI$mal.Age.group.name)))

HIV_AIDS_path_male_HI<-HIV_AIDS_path_male
HIV_AIDS_path_female_HI<-HIV_AIDS_path_female

#### Female-Male difference
HIV_AIDS_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               HIV_AIDS_path_female-HIV_AIDS_path_male);rownames(HIV_AIDS_path_diff)<-NULL
HIV_AIDS_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                    ceiling(HIV_AIDS_path_diff[,2:4]/67*100),
                                    ceiling(HIV_AIDS_path_diff[,5:7]/16*100),
                                    mort=ceiling(HIV_AIDS_path_diff[,8]/67*100))
rownames(HIV_AIDS_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- HIV_AIDS_path_diff_prop
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = "",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot




##### HIV/AIDS - Upper-Middle #####
#### Males 
HIV_AIDS_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  druguse=cbind(table(male_higher_drug_use_UM$mal.Age.group.name)),
  unsafe=0,
  prev=cbind(table(male_higher_HIV_AIDS_prev_UM$mal.Age.group.name)),
  step1=length(table(step1_male_higher_HIV_AIDS_UM$mal.iso)),
  step2=length(table(step2_male_higher_HIV_AIDS_UM$mal.iso)),
  step3=length(table(step3_male_higher_HIV_AIDS_UM$mal.iso)),
  mort=cbind(table(male_higher_HIV_AIDS_mort_UM$mal.Age.group.name)))

#### Females 
HIV_AIDS_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  druguse=c(0,0,0,0,0,0,0,0,0,2),
  unsafe=cbind(table(female_higher_unsafesex_UM$mal.Age.group.name)),
  prev=cbind(table(female_higher_HIV_AIDS_prev_UM$mal.Age.group.name)),
  step1=length(table(step1_female_higher_HIV_AIDS_UM$mal.iso)),
  step2=length(table(step2_female_higher_HIV_AIDS_UM$mal.iso)),
  step3=length(table(step3_female_higher_HIV_AIDS_UM$mal.iso)),
  mort=c(6,2,1,1,0,0,0,0,0,0))

HIV_AIDS_path_male_UM<-HIV_AIDS_path_male
HIV_AIDS_path_female_UM<-HIV_AIDS_path_female

#### Female-Male difference
HIV_AIDS_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               HIV_AIDS_path_female-HIV_AIDS_path_male);rownames(HIV_AIDS_path_diff)<-NULL
HIV_AIDS_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                    ceiling(HIV_AIDS_path_diff[,2:4]/54*100),
                                    ceiling(HIV_AIDS_path_diff[,5:7]/25*100),
                                    mort=ceiling(HIV_AIDS_path_diff[,8]/54*100))
rownames(HIV_AIDS_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- HIV_AIDS_path_diff_prop
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = "",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot


##### HIV/AIDS - Lower-Middle #####
#### Males 
HIV_AIDS_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  druguse=cbind(table(male_higher_drug_use_LM$mal.Age.group.name)),
  unsafe=c(1,0,0,0,0,0,0,0,0,0),
  prev=cbind(table(male_higher_HIV_AIDS_prev_LM$mal.Age.group.name)),
  step1=length(table(step1_male_higher_HIV_AIDS_LM$mal.iso)),
  step2=length(table(step2_male_higher_HIV_AIDS_LM$mal.iso)),
  step3=length(table(step3_male_higher_HIV_AIDS_LM$mal.iso)),
  mort=cbind(table(male_higher_HIV_AIDS_mort_LM$mal.Age.group.name)))

#### Females 
HIV_AIDS_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  druguse=c(1,0,0,0,0,0,0,0,0,0),
  unsafe=cbind(table(female_higher_unsafesex_LM$mal.Age.group.name)),
  prev=cbind(table(female_higher_HIV_AIDS_prev_LM$mal.Age.group.name)),
  step1=length(table(step1_female_higher_HIV_AIDS_LM$mal.iso)),
  step2=length(table(step2_female_higher_HIV_AIDS_LM$mal.iso)),
  step3=length(table(step3_female_higher_HIV_AIDS_LM$mal.iso)),
  mort=cbind(table(female_higher_HIV_AIDS_mort_LM$mal.Age.group.name)))

HIV_AIDS_path_male_LM<-HIV_AIDS_path_male
HIV_AIDS_path_female_LM<-HIV_AIDS_path_female

#### Female-Male difference
HIV_AIDS_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               HIV_AIDS_path_female-HIV_AIDS_path_male);rownames(HIV_AIDS_path_diff)<-NULL
HIV_AIDS_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                    ceiling(HIV_AIDS_path_diff[,2:4]/54*100),
                                    ceiling(HIV_AIDS_path_diff[,5:7]/28*100),
                                    mort=ceiling(HIV_AIDS_path_diff[,8]/54*100))
rownames(HIV_AIDS_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- HIV_AIDS_path_diff_prop
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = "",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot


##### HIV/AIDS - Low #####
#### Males 
HIV_AIDS_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  druguse=c(3,4,3,1,0,0,3,13,20,20),
  unsafe=0,
  prev=cbind(table(male_higher_HIV_AIDS_prev_LO$mal.Age.group.name)),
  step1=length(table(step1_male_higher_HIV_AIDS_LO$mal.iso)),
  step2=length(table(step2_male_higher_HIV_AIDS_LO$mal.iso)),
  step3=length(table(step3_male_higher_HIV_AIDS_LO$mal.iso)),
  mort=c(0,cbind(table(male_higher_HIV_AIDS_mort_LO$mal.Age.group.name))))

#### Females 
HIV_AIDS_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  druguse=c(0,0,0,0,0,1,1,1,0,2),
  unsafe=cbind(table(female_higher_unsafesex_LO$mal.Age.group.name)),
  prev=c(8,7,6,3,1,0,0,0,0,0),
  step1=length(table(step1_female_higher_HIV_AIDS_LO$mal.iso)),
  step2=length(table(step2_female_higher_HIV_AIDS_LO$mal.iso)),
  step3=length(table(step3_female_higher_HIV_AIDS_LO$mal.iso)),
  mort=c(4,1,0,0,0,0,0,0,0,0))

HIV_AIDS_path_male_LO<-HIV_AIDS_path_male
HIV_AIDS_path_female_LO<-HIV_AIDS_path_female

#### Female-Male difference
HIV_AIDS_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               HIV_AIDS_path_female-HIV_AIDS_path_male);rownames(HIV_AIDS_path_diff)<-NULL
HIV_AIDS_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                    ceiling(HIV_AIDS_path_diff[,2:4]/26*100),
                                    ceiling(HIV_AIDS_path_diff[,5:7]/7*100),
                                    mort=ceiling(HIV_AIDS_path_diff[,8]/26*100))
rownames(HIV_AIDS_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- HIV_AIDS_path_diff_prop
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = "",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

############################################
######## Plot Health Pathways by Region ############
##### Hypertension - Europe and Central Asia #####
#### Males 
hyp_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  sodi=cbind(table(male_higher_sodium_ECA$mal.Age.group.name)),
  gluc=0,
  over=c(cbind(table(male_higher_overweight_ECA$mal.Age.group.name)),0,0),
  obes=0,
  smok=cbind(table(male_higher_smoking_ECA$mal.Age.group.name)),
  prev=c(3,1,1,1,0,0,0,0,0,0),
  step1=0,
  step2=0,
  step3=0,
  mort=cbind(table(male_higher_hyp_mort_ECA$mal.Age.group.name)))

#### Females 
hyp_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  sodi=0,
  gluc=0,
  over=c(0,0,0,0,cbind(table(female_higher_overweight_ECA$mal.Age.group.name))),
  obes=cbind(table(female_higher_obesity_ECA$mal.Age.group.name)),
  smok=0,
  prev=0,
  step1=0,
  step2=c(0,1,0,0,0,0,0,0,0,0),
  step3=c(1,1,0,0,0,0,0,0,0,0),
  mort=0)

hyp_path_male_ECA<-hyp_path_male
hyp_path_female_ECA<-hyp_path_female

#### Proportions
hyp_path_male_prop<-data.frame(age=row.names(hyp_path_male),
                               ceiling(hyp_path_male[,1:6]/52*100),
                               ceiling(hyp_path_male[,7:9]/50*100),
                               mort=ceiling(hyp_path_male[,10]/52*100))

hyp_path_female_prop<-data.frame(age=row.names(hyp_path_female),
                                 ceiling(hyp_path_female[,1:6]/52*100),
                                 ceiling(hyp_path_female[,7:9]/50*100),
                                 mort=ceiling(hyp_path_female[,10]/52*100))

#### Female-Male difference
hyp_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                          hyp_path_female-hyp_path_male);rownames(hyp_path_diff)<-NULL
hyp_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               ceiling(hyp_path_diff[,2:7]/52*100),
                               ceiling(hyp_path_diff[,8:10]/50*100),
                               mort=ceiling(hyp_path_diff[,11]/52*100))
rownames(hyp_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- hyp_path_diff_prop
colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "More Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot





##### Hypertension - South Asia #####
#### Males 
hyp_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  sodi=cbind(table(male_higher_sodium_SAS$mal.Age.group.name)),
  gluc=0,
  over=c(0,0,1,0,0,0,0,0,0,0),
  obes=0,
  smok=cbind(table(male_higher_smoking_SAS$mal.Age.group.name)),
  prev=c(2,1,0,0,0,0,0,0,0,0),
  step1=0,
  step2=0,
  step3=0,
  mort=c(cbind(table(male_higher_hyp_mort_SAS$mal.Age.group.name)),0,0,0,0))

#### Females 
hyp_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  sodi=0,
  gluc=0,
  over=c(cbind(table(female_higher_overweight_SAS$mal.Age.group.name)),0,0),
  obes=cbind(table(female_higher_obesity_SAS$mal.Age.group.name)),
  smok=c(0,0,0,0,0,0,0,1,0,0),
  prev=c(0,0,0,0,0,0,0,0,1,1),
  step1=0,
  step2=0,
  step3=0,
  mort=0)

hyp_path_male_SAS<-hyp_path_male
hyp_path_female_SAS<-hyp_path_female

#### Proportions
hyp_path_male_prop<-data.frame(age=row.names(hyp_path_male),
                               ceiling(hyp_path_male[,1:6]/8*100),
                               ceiling(hyp_path_male[,7:9]/8*100),
                               mort=ceiling(hyp_path_male[,10]/8*100))

hyp_path_female_prop<-data.frame(age=row.names(hyp_path_female),
                                 ceiling(hyp_path_female[,1:6]/8*100),
                                 ceiling(hyp_path_female[,7:9]/8*100),
                                 mort=ceiling(hyp_path_female[,10]/8*100))

#### Female-Male difference
hyp_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                          hyp_path_female-hyp_path_male);rownames(hyp_path_diff)<-NULL
hyp_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               ceiling(hyp_path_diff[,2:7]/8*100),
                               ceiling(hyp_path_diff[,8:10]/8*100),
                               mort=ceiling(hyp_path_diff[,11]/8*100))
rownames(hyp_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- hyp_path_diff_prop
colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "More Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot


##### Hypertension - Sub-Saharan Africa #####
#### Males 
hyp_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  sodi=0,
  gluc=0,
  over=c(0,0,0,0,0,0,0,0,0,1),
  obes=c(0,0,0,0,0,0,0,0,1,1),
  smok=cbind(table(male_higher_smoking_SSA$mal.Age.group.name)),
  prev=0,
  step1=0,
  step2=0,
  step3=0,
  mort=c(cbind(table(male_higher_hyp_mort_SSA$mal.Age.group.name)),0,0,0))

#### Females 
hyp_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  sodi=cbind(table(female_higher_sodium_SSA$mal.Age.group.name)),
  gluc=0,
  over=cbind(table(female_higher_overweight_SSA$mal.Age.group.name)),
  obes=cbind(table(female_higher_obesity_SSA$mal.Age.group.name)),
  smok=0,
  prev=0,
  step1=c(cbind(table(step1_female_higher_hyp_SSA$mal.Age)),0,0,0,0,0),
  step2=0,
  step3=0,
  mort=0)

hyp_path_male_SSA<-hyp_path_male
hyp_path_female_SSA<-hyp_path_female

#### Proportions
hyp_path_male_prop<-data.frame(age=row.names(hyp_path_male),
                               ceiling(hyp_path_male[,1:6]/44*100),
                               ceiling(hyp_path_male[,7:9]/44*100),
                               mort=ceiling(hyp_path_male[,10]/44*100))

hyp_path_female_prop<-data.frame(age=row.names(hyp_path_female),
                                 ceiling(hyp_path_female[,1:6]/44*100),
                                 ceiling(hyp_path_female[,7:9]/44*100),
                                 mort=ceiling(hyp_path_female[,10]/44*100))

#### Female-Male difference
hyp_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                          hyp_path_female-hyp_path_male);rownames(hyp_path_diff)<-NULL
hyp_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               ceiling(hyp_path_diff[,2:7]/44*100),
                               ceiling(hyp_path_diff[,8:10]/44*100),
                               mort=ceiling(hyp_path_diff[,11]/44*100))
rownames(hyp_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- hyp_path_diff_prop
colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "More Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot


##### Hypertension - Middle East and North Africa #####
#### Males 
hyp_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  sodi=0,
  gluc=0,
  over=0,
  obes=0,
  smok=cbind(table(male_higher_smoking_MEN$mal.Age.group.name)),
  prev=0,
  step1=0,
  step2=0,
  step3=0,
  mort=cbind(table(male_higher_hyp_mort_MEN$mal.Age.group.name)))

#### Females 
hyp_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  sodi=c(0,0,0,0,1,0,0,0,0,0),
  gluc=c(0,0,0,0,0,0,0,0,0,1),
  over=cbind(table(female_higher_overweight_MEN$mal.Age.group.name)),
  obes=cbind(table(female_higher_obesity_MEN$mal.Age.group.name)),
  smok=0,
  prev=0,
  step1=c(0,cbind(table(step1_female_higher_hyp_MEN$mal.Age)),0),
  step2=c(cbind(table(step1_female_higher_hyp_MEN$mal.Age)),0,0),
  step3=c(1,1,0,0,0,0,0,0,0,0),
  mort=c(0,0,0,0,0,0,0,0,1,1))

hyp_path_male_MEN<-hyp_path_male
hyp_path_female_MEN<-hyp_path_female

#### Proportions
hyp_path_male_prop<-data.frame(age=row.names(hyp_path_male),
                               ceiling(hyp_path_male[,1:6]/22*100),
                               ceiling(hyp_path_male[,7:9]/22*100),
                               mort=ceiling(hyp_path_male[,10]/22*100))

hyp_path_female_prop<-data.frame(age=row.names(hyp_path_female),
                                 ceiling(hyp_path_female[,1:6]/22*100),
                                 ceiling(hyp_path_female[,7:9]/22*100),
                                 mort=ceiling(hyp_path_female[,10]/22*100))

#### Female-Male difference
hyp_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                          hyp_path_female-hyp_path_male);rownames(hyp_path_diff)<-NULL
hyp_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               ceiling(hyp_path_diff[,2:7]/22*100),
                               ceiling(hyp_path_diff[,8:10]/22*100),
                               mort=ceiling(hyp_path_diff[,11]/22*100))
rownames(hyp_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- hyp_path_diff_prop
colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "More Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot


##### Hypertension - Latin America & the Caribbean #####
#### Males 
hyp_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  sodi=c(0,0,0,0,1,0,0,0,1,0),
  gluc=0,
  over=c(0,1,0,0,0,0,0,0,0,0),
  obes=0,
  smok=cbind(table(male_higher_smoking_LAT$mal.Age.group.name)),
  prev=c(1,1,0,0,0,0,0,0,0,0),
  step1=0,
  step2=0,
  step3=0,
  mort=cbind(table(male_higher_hyp_mort_LAT$mal.Age.group.name)))

#### Females 
hyp_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  sodi=0,
  gluc=0,
  over=cbind(table(female_higher_overweight_LAT$mal.Age.group.name)),
  obes=cbind(table(female_higher_obesity_LAT$mal.Age.group.name)),
  smok=0,
  prev=0,
  step1=c(cbind(table(step1_female_higher_hyp_LAT$mal.Age)),0,0),
  step2=c(cbind(table(step2_female_higher_hyp_LAT$mal.Age)),0,0),
  step3=c(cbind(table(step3_female_higher_hyp_LAT$mal.Age)),0,0,0),
  mort=0)

hyp_path_male_LAT<-hyp_path_male
hyp_path_female_LAT<-hyp_path_female

#### Proportions
hyp_path_male_prop<-data.frame(age=row.names(hyp_path_male),
                               ceiling(hyp_path_male[,1:6]/38*100),
                               ceiling(hyp_path_male[,7:9]/37*100),
                               mort=ceiling(hyp_path_male[,10]/38*100))

hyp_path_female_prop<-data.frame(age=row.names(hyp_path_female),
                                 ceiling(hyp_path_female[,1:6]/38*100),
                                 ceiling(hyp_path_female[,7:9]/37*100),
                                 mort=ceiling(hyp_path_female[,10]/38*100))

#### Female-Male difference
hyp_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                          hyp_path_female-hyp_path_male);rownames(hyp_path_diff)<-NULL
hyp_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               ceiling(hyp_path_diff[,2:7]/38*100),
                               ceiling(hyp_path_diff[,8:10]/37*100),
                               mort=ceiling(hyp_path_diff[,11]/38*100))
rownames(hyp_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- hyp_path_diff_prop
colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "More Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot



##### Hypertension - East Asia and Pacific #####
#### Males 
hyp_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  sodi=0,
  gluc=0,
  over=c(3,3,1,1,1,0,0,1,1,0),
  obes=c(1,0,0,0,0,0,0,0,0,0),
  smok=cbind(table(male_higher_smoking_EAP$mal.Age.group.name)),
  prev=c(1,0,0,0,0,0,0,0,0,0),
  step1=0,
  step2=0,
  step3=0,
  mort=cbind(table(male_higher_hyp_mort_EAP$mal.Age.group.name)))

#### Females 
hyp_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  sodi=0,
  gluc=0,
  over=cbind(table(female_higher_overweight_EAP$mal.Age.group.name)),
  obes=cbind(table(female_higher_obesity_EAP$mal.Age.group.name)),
  smok=0,
  prev=0,
  step1=0,
  step2=0,
  step3=0,
  mort=0)

hyp_path_male_EAP<-hyp_path_male
hyp_path_female_EAP<-hyp_path_female

#### Proportions
hyp_path_male_prop<-data.frame(age=row.names(hyp_path_male),
                               ceiling(hyp_path_male[,1:6]/34*100),
                               ceiling(hyp_path_male[,7:9]/33*100),
                               mort=ceiling(hyp_path_male[,10]/34*100))

hyp_path_female_prop<-data.frame(age=row.names(hyp_path_female),
                                 ceiling(hyp_path_female[,1:6]/34*100),
                                 ceiling(hyp_path_female[,7:9]/33*100),
                                 mort=ceiling(hyp_path_female[,10]/34*100))

#### Female-Male difference
hyp_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                          hyp_path_female-hyp_path_male);rownames(hyp_path_diff)<-NULL
hyp_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               ceiling(hyp_path_diff[,2:7]/34*100),
                               ceiling(hyp_path_diff[,8:10]/33*100),
                               mort=ceiling(hyp_path_diff[,11]/34*100))
rownames(hyp_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- hyp_path_diff_prop
colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "More Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot



##### Hypertension - North America #####
#### Males 
hyp_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  sodi=c(1,0,1,1,0,0,1,1,0,0),
  gluc=0,
  over=c(1,0,0,0,0,0,0,0,0,0),
  obes=0,
  smok=0,
  prev=0,
  step1=0,
  step2=0,
  step3=0,
  mort=c(cbind(table(male_higher_hyp_mort_NOA$mal.Age.group.name)),0,0))

#### Females 
hyp_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  sodi=c(0,0,0,0,0,0,0,0,0,0),
  gluc=0,
  over=0,
  obes=0,
  smok=0,
  prev=0,
  step1=0,
  step2=0,
  step3=0,
  mort=0)

hyp_path_male_NOA<-hyp_path_male
hyp_path_female_NOA<-hyp_path_female

#### Proportions
hyp_path_male_prop<-data.frame(age=row.names(hyp_path_male),
                               ceiling(hyp_path_male[,1:6]/8*100),
                               ceiling(hyp_path_male[,7:9]/8*100),
                               mort=ceiling(hyp_path_male[,10]/8*100))

hyp_path_female_prop<-data.frame(age=row.names(hyp_path_female),
                                 ceiling(hyp_path_female[,1:6]/8*100),
                                 ceiling(hyp_path_female[,7:9]/8*100),
                                 mort=ceiling(hyp_path_female[,10]/8*100))

#### Female-Male difference
hyp_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                          hyp_path_female-hyp_path_male);rownames(hyp_path_diff)<-NULL
hyp_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               ceiling(hyp_path_diff[,2:7]/8*100),
                               ceiling(hyp_path_diff[,8:10]/8*100),
                               mort=ceiling(hyp_path_diff[,11]/8*100))
rownames(hyp_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- hyp_path_diff_prop
colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "More Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot



######################
##### Diabetes - Europe and Central Asia #####
#### Males 
diab_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  gluc=0,
  over=c(cbind(table(male_higher_overweight_ECA$mal.Age.group.name)),0,0),
  obes=0,
  smok=cbind(table(male_higher_smoking_ECA$mal.Age.group.name)),
  inact=0,
  prev=cbind(table(male_higher_diab_prev_ECA$mal.Age.group.name)),
  step1=0,
  step2=0,
  step3=0,
  mort=cbind(table(male_higher_diab_mort_ECA$mal.Age.group.name)))

#### Females 
diab_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  gluc=0,
  over=c(0,0,0,0,2,4,6,5,2,2),
  obes=cbind(table(female_higher_obesity_ECA$mal.Age.group.name)),
  smok=0,
  inact=0,
  prev=0,
  step1=0,
  step2=0,
  step3=0,
  mort=c(0,0,0,0,0,0,0,0,1,2))

diab_path_male_ECA<-diab_path_male
diab_path_female_ECA<-diab_path_female

#### Female-Male difference
diab_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                           diab_path_female-diab_path_male);rownames(diab_path_diff)<-NULL
diab_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                ceiling(diab_path_diff[,2:7]/50*100),
                                ceiling(diab_path_diff[,8:10]/8*100),
                                mort=ceiling(diab_path_diff[,11]/50*100))
rownames(diab_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- diab_path_diff_prop
colnames(df)<-c("Age_Group","Glucose","Overweight","Obesity","Smoking","Phys. Inac.","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot



##### Diabetes - South Asia #####
#### Males 
diab_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  gluc=0,
  over=c(0,0,1,0,0,0,0,0,0,0),
  obes=0,
  smok=cbind(table(male_higher_smoking_SAS$mal.Age.group.name)),
  inact=0,
  prev=c(0,0,1,1,0,0,0,0,0,0),
  step1=0,
  step2=0,
  step3=0,
  mort=c(1,1,1,0,0,0,0,0,0,0))

#### Females 
diab_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  gluc=0,
  over=c(1,1,1,2,1,1,1,1,0,0),
  obes=cbind(table(female_higher_obesity_SAS$mal.Age.group.name)),
  smok=c(0,0,0,0,0,0,0,1,0,0),
  inact=0,
  prev=0,
  step1=0,
  step2=0,
  step3=0,
  mort=cbind(table(female_higher_diab_mort_SAS$mal.Age.group.name)))

diab_path_male_SAS<-diab_path_male
diab_path_female_SAS<-diab_path_female

#### Female-Male difference
diab_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                           diab_path_female-diab_path_male);rownames(diab_path_diff)<-NULL
diab_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                ceiling(diab_path_diff[,2:7]/8*100),
                                ceiling(diab_path_diff[,8:10]/4*100),
                                mort=ceiling(diab_path_diff[,11]/8*100))
rownames(diab_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- diab_path_diff_prop
colnames(df)<-c("Age_Group","Glucose","Overweight","Obesity","Smoking","Phys. Inac.","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot



##### Diabetes - Sub-Saharan Africa #####
#### Males 
diab_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  gluc=0,
  over=c(0,0,0,0,0,0,0,0,0,1),
  obes=c(0,0,0,0,0,0,0,0,1,1),
  smok=cbind(table(male_higher_smoking_SSA$mal.Age.group.name)),
  inact=0,
  prev=cbind(table(male_higher_diab_prev_SSA$mal.Age.group.name)),
  step1=0,
  step2=0,
  step3=0,
  mort=c(cbind(table(male_higher_diab_mort_SSA$mal.Age.group.name)),0))

#### Females 
diab_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  gluc=0,
  over=cbind(table(female_higher_overweight_SSA$mal.Age.group.name)),
  obes=cbind(table(female_higher_obesity_SSA$mal.Age.group.name)),
  smok=0,
  inact=0,
  prev=c(0,0,0,0,0,1,1,1,1,1),
  step1=c(0,0,0,1,1,1,1,1,1,1),
  step2=c(0,0,0,1,1,1,1,1,1,1),
  step3=0,
  mort=0)

diab_path_male_SSA<-diab_path_male
diab_path_female_SSA<-diab_path_female

#### Female-Male difference
diab_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                           diab_path_female-diab_path_male);rownames(diab_path_diff)<-NULL
diab_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                ceiling(diab_path_diff[,2:7]/44*100),
                                ceiling(diab_path_diff[,8:10]/11*100),
                                mort=ceiling(diab_path_diff[,11]/44*100))
rownames(diab_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- diab_path_diff_prop
colnames(df)<-c("Age_Group","Glucose","Overweight","Obesity","Smoking","Phys. Inac.","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot




##### Diabetes - Middle East and North Africa #####
#### Males 
diab_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  gluc=0,
  over=0,
  obes=0,
  smok=cbind(table(male_higher_smoking_MEN$mal.Age.group.name)),
  inact=0,
  prev=c(1,1,0,2,1,2,1,1,3,4),
  step1=0,
  step2=0,
  step3=0,
  mort=c(cbind(table(male_higher_diab_mort_MEN$mal.Age.group.name)),0))

#### Females 
diab_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  gluc=c(0,0,0,0,0,0,0,0,0,1),
  over=cbind(table(female_higher_overweight_MEN$mal.Age.group.name)),
  obes=cbind(table(female_higher_obesity_MEN$mal.Age.group.name)),
  smok=0,
  inact=0,
  prev=c(0,0,cbind(table(female_higher_diab_prev_MEN$mal.Age.group.name))),
  step1=0,
  step2=0,
  step3=0,
  mort=c(0,0,0,0,0,0,0,0,1,1))

diab_path_male_MEN<-diab_path_male
diab_path_female_MEN<-diab_path_female

#### Female-Male difference
diab_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                           diab_path_female-diab_path_male);rownames(diab_path_diff)<-NULL
diab_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                ceiling(diab_path_diff[,2:7]/22*100),
                                ceiling(diab_path_diff[,8:10]/6*100),
                                mort=ceiling(diab_path_diff[,11]/22*100))
rownames(diab_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- diab_path_diff_prop
colnames(df)<-c("Age_Group","Glucose","Overweight","Obesity","Smoking","Phys. Inac.","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot




##### Diabetes - Latin America & the Caribbean #####
#### Males 
diab_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  gluc=0,
  over=c(1,0,0,0,0,0,0,0,0,0),
  obes=0,
  smok=cbind(table(male_higher_smoking_LAT$mal.Age.group.name)),
  inact=c(2,0,0,1,1,0,0,0,0,0),
  prev=c(0,0,0,0,0,0,1,2,3,3),
  step1=0,
  step2=0,
  step3=0,
  mort=cbind(table(male_higher_diab_mort_LAT$mal.Age.group.name)))

#### Females 
diab_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  gluc=0,
  over=cbind(table(female_higher_overweight_LAT$mal.Age.group.name)),
  obes=cbind(table(female_higher_obesity_LAT$mal.Age.group.name)),
  smok=0,
  inact=0,
  prev=cbind(table(female_higher_diab_prev_LAT$mal.Age.group.name)),
  step1=0,
  step2=0,
  step3=0,
  mort=c(1,0,0,0,2,1,1,1,2,3))

diab_path_male_LAT<-diab_path_male
diab_path_female_LAT<-diab_path_female

#### Female-Male difference
diab_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                           diab_path_female-diab_path_male);rownames(diab_path_diff)<-NULL
diab_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                ceiling(diab_path_diff[,2:7]/38*100),
                                ceiling(diab_path_diff[,8:10]/2*100),
                                mort=ceiling(diab_path_diff[,11]/38*100))
rownames(diab_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- diab_path_diff_prop
colnames(df)<-c("Age_Group","Glucose","Overweight","Obesity","Smoking","Phys. Inac.","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot




##### Diabetes - East Asia and Pacific #####
#### Males 
diab_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  gluc=0,
  over=c(3,3,1,1,1,0,0,1,1,0),
  obes=c(1,0,0,0,0,0,0,0,0,0),
  smok=cbind(table(male_higher_smoking_EAP$mal.Age.group.name)),
  inact=c(1,0,1,0,0,0,1,0,0,0),
  prev=cbind(table(male_higher_diab_prev_EAP$mal.Age.group.name)),
  step1=0,
  step2=0,
  step3=0,
  mort=cbind(table(male_higher_diab_mort_EAP$mal.Age.group.name)))

#### Females 
diab_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  gluc=0,
  over=cbind(table(female_higher_overweight_EAP$mal.Age.group.name)),
  obes=cbind(table(female_higher_obesity_EAP$mal.Age.group.name)),
  smok=0,
  inact=0,
  prev=c(1,0,1,1,1,1,1,1,2,0),
  step1=0,
  step2=0,
  step3=0,
  mort=0)

diab_path_male_EAP<-diab_path_male
diab_path_female_EAP<-diab_path_female

#### Female-Male difference
diab_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                           diab_path_female-diab_path_male);rownames(diab_path_diff)<-NULL
diab_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                ceiling(diab_path_diff[,2:7]/34*100),
                                ceiling(diab_path_diff[,8:10]/8*100),
                                mort=ceiling(diab_path_diff[,11]/34*100))
rownames(diab_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- diab_path_diff_prop
colnames(df)<-c("Age_Group","Glucose","Overweight","Obesity","Smoking","Phys. Inac.","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot




##### Diabetes - North America #####
#### Males 
diab_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  gluc=0,
  over=c(1,0,0,0,0,0,0,0,0,0),
  obes=0,
  smok=0,
  inact=0,
  prev=c(0,0,2,3,3,3,3,3,3,3),
  step1=0,
  step2=0,
  step3=0,
  mort=cbind(table(male_higher_diab_mort_NOA$mal.Age.group.name)))

#### Females 
diab_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  gluc=c(0,0,0,0,0,0,0,0,0,0),
  over=0,
  obes=0,
  smok=0,
  inact=0,
  prev=0,
  step1=0,
  step2=0,
  step3=0,
  mort=0)

diab_path_male_NOA<-diab_path_male
diab_path_female_NOA<-diab_path_female

#### Female-Male difference
diab_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                           diab_path_female-diab_path_male);rownames(diab_path_diff)<-NULL
diab_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                ceiling(diab_path_diff[,2:7]/3*100),
                                ceiling(diab_path_diff[,8:10]/0*100),
                                mort=ceiling(diab_path_diff[,11]/3*100))
rownames(diab_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- diab_path_diff_prop
colnames(df)<-c("Age_Group","Glucose","Overweight","Obesity","Smoking","Phys. Inac.","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    na.value = "white"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = " ",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot


##################
##### HIV/AIDS - Europe and Central Asia #####
#### Males 
HIV_AIDS_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  druguse=cbind(table(male_higher_drug_use_ECA$mal.Age.group.name)),
  unsafe=0,
  prev=cbind(table(male_higher_HIV_AIDS_prev_ECA$mal.Age.group.name)),
  step1=length(table(step1_male_higher_HIV_AIDS_ECA$mal.iso)),
  step2=length(table(step2_male_higher_HIV_AIDS_ECA$mal.iso)),
  step3=length(table(step3_male_higher_HIV_AIDS_ECA$mal.iso)),
  mort=cbind(table(male_higher_HIV_AIDS_mort_ECA$mal.Age.group.name)))

#### Females 
HIV_AIDS_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  druguse=c(0,0,0,0,0,0,0,0,1,2),
  unsafe=cbind(table(female_higher_unsafesex_ECA$mal.Age.group.name)),
  prev=c(0,0,1,1,1,1,1,1,1,2),
  step1=length(table(step1_female_higher_HIV_AIDS_ECA$mal.iso)),
  step2=length(table(step2_female_higher_HIV_AIDS_ECA$mal.iso)),
  step3=length(table(step3_female_higher_HIV_AIDS_ECA$mal.iso)),
  mort=cbind(table(female_higher_HIV_AIDS_mort_ECA$mal.Age.group.name)))

HIV_AIDS_path_male_ECA<-HIV_AIDS_path_male
HIV_AIDS_path_female_ECA<-HIV_AIDS_path_female

#### Female-Male difference
HIV_AIDS_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               HIV_AIDS_path_female-HIV_AIDS_path_male);rownames(HIV_AIDS_path_diff)<-NULL
HIV_AIDS_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                    ceiling(HIV_AIDS_path_diff[,2:4]/50*100),
                                    ceiling(HIV_AIDS_path_diff[,5:7]/20*100),
                                    mort=ceiling(HIV_AIDS_path_diff[,8]/50*100))
rownames(HIV_AIDS_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- HIV_AIDS_path_diff_prop
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = "",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot


##### HIV/AIDS - South Asia #####
#### Males 
HIV_AIDS_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  druguse=c(0,0,1,1,1,1,0,0,0,0),
  unsafe=c(1,0,1,1,1,1,0,0,0,0),
  prev=cbind(table(male_higher_HIV_AIDS_prev_SAS$mal.Age.group.name)),
  step1=length(table(step1_male_higher_HIV_AIDS_SAS$mal.iso)),
  step2=length(table(step2_male_higher_HIV_AIDS_SAS$mal.iso)),
  step3=length(table(step3_male_higher_HIV_AIDS_SAS$mal.iso)),
  mort=cbind(table(male_higher_HIV_AIDS_mort_SAS$mal.Age.group.name)))

#### Females 
HIV_AIDS_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  druguse=c(0,0,0,0,0,1,1,1,0,1),
  unsafe=cbind(table(female_higher_unsafesex_SAS$mal.Age.group.name)),
  prev=0,
  step1=length(table(step1_female_higher_HIV_AIDS_SAS$mal.iso)),
  step2=length(table(step2_female_higher_HIV_AIDS_SAS$mal.iso)),
  step3=length(table(step3_female_higher_HIV_AIDS_SAS$mal.iso)),
  mort=0)

HIV_AIDS_path_male_SAS<-HIV_AIDS_path_male
HIV_AIDS_path_female_SAS<-HIV_AIDS_path_female

#### Female-Male difference
HIV_AIDS_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               HIV_AIDS_path_female-HIV_AIDS_path_male);rownames(HIV_AIDS_path_diff)<-NULL
HIV_AIDS_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                    ceiling(HIV_AIDS_path_diff[,2:4]/8*100),
                                    ceiling(HIV_AIDS_path_diff[,5:7]/3*100),
                                    mort=ceiling(HIV_AIDS_path_diff[,8]/8*100))
rownames(HIV_AIDS_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- HIV_AIDS_path_diff_prop
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = "",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot


##### HIV/AIDS - Sub-Saharan Africa #####
#### Males 
HIV_AIDS_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  druguse=cbind(table(male_higher_drug_use_SSA$mal.Age.group.name)),
  unsafe=0,
  prev=c(cbind(table(male_higher_HIV_AIDS_prev_SSA$mal.Age.group.name)),0,0),
  step1=length(table(step1_male_higher_HIV_AIDS_SSA$mal.iso)),
  step2=length(table(step2_male_higher_HIV_AIDS_SSA$mal.iso)),
  step3=length(table(step3_male_higher_HIV_AIDS_SSA$mal.iso)),
  mort=cbind(table(male_higher_HIV_AIDS_mort_SSA$mal.Age.group.name)))

#### Females 
HIV_AIDS_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  druguse=0,
  unsafe=cbind(table(female_higher_unsafesex_SSA$mal.Age.group.name)),
  prev=c(cbind(table(female_higher_HIV_AIDS_prev_SSA$mal.Age.group.name)),0),
  step1=length(table(step1_female_higher_HIV_AIDS_SSA$mal.iso)),
  step2=length(table(step2_female_higher_HIV_AIDS_SSA$mal.iso)),
  step3=length(table(step3_female_higher_HIV_AIDS_SSA$mal.iso)),
  mort=c(5,1,0,0,0,0,0,0,0,0))

HIV_AIDS_path_male_SSA<-HIV_AIDS_path_male
HIV_AIDS_path_female_SSA<-HIV_AIDS_path_female

#### Female-Male difference
HIV_AIDS_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               HIV_AIDS_path_female-HIV_AIDS_path_male);rownames(HIV_AIDS_path_diff)<-NULL
HIV_AIDS_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                    ceiling(HIV_AIDS_path_diff[,2:4]/44*100),
                                    ceiling(HIV_AIDS_path_diff[,5:7]/21*100),
                                    mort=ceiling(HIV_AIDS_path_diff[,8]/44*100))
rownames(HIV_AIDS_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- HIV_AIDS_path_diff_prop
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = "",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot


##### HIV/AIDS - Middle East and North Africa #####
#### Males 
HIV_AIDS_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  druguse=cbind(table(male_higher_drug_use_MEN$mal.Age.group.name)),
  unsafe=0,
  prev=cbind(table(male_higher_HIV_AIDS_prev_MEN$mal.Age.group.name)),
  step1=length(table(step1_male_higher_HIV_AIDS_MEN$mal.iso)),
  step2=length(table(step2_male_higher_HIV_AIDS_MEN$mal.iso)),
  step3=length(table(step3_male_higher_HIV_AIDS_MEN$mal.iso)),
  mort=cbind(table(male_higher_HIV_AIDS_mort_MEN$mal.Age.group.name)))

#### Females 
HIV_AIDS_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  druguse=c(0,0,0,0,0,0,0,0,0,1),
  unsafe=cbind(table(female_higher_unsafesex_MEN$mal.Age.group.name)),
  prev=c(0,0,1,1,0,0,0,0,0,0),
  step1=length(table(step1_female_higher_HIV_AIDS_MEN$mal.iso)),
  step2=length(table(step2_female_higher_HIV_AIDS_MEN$mal.iso)),
  step3=length(table(step3_female_higher_HIV_AIDS_MEN$mal.iso)),
  mort=cbind(table(female_higher_HIV_AIDS_mort_MEN$mal.Age.group.name)))

HIV_AIDS_path_male_MEN<-HIV_AIDS_path_male
HIV_AIDS_path_female_MEN<-HIV_AIDS_path_female

#### Female-Male difference
HIV_AIDS_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               HIV_AIDS_path_female-HIV_AIDS_path_male);rownames(HIV_AIDS_path_diff)<-NULL
HIV_AIDS_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                    ceiling(HIV_AIDS_path_diff[,2:4]/22*100),
                                    ceiling(HIV_AIDS_path_diff[,5:7]/8*100),
                                    mort=ceiling(HIV_AIDS_path_diff[,8]/22*100))
rownames(HIV_AIDS_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- HIV_AIDS_path_diff_prop
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = "",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot



##### HIV/AIDS - Latin America & the Caribbean #####
#### Males 
HIV_AIDS_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  druguse=cbind(table(male_higher_drug_use_LAT$mal.Age.group.name)),
  unsafe=0,
  prev=cbind(table(male_higher_HIV_AIDS_prev_LAT$mal.Age.group.name)),
  step1=length(table(step1_male_higher_HIV_AIDS_LAT$mal.iso)),
  step2=length(table(step2_male_higher_HIV_AIDS_LAT$mal.iso)),
  step3=length(table(step3_male_higher_HIV_AIDS_LAT$mal.iso)),
  mort=cbind(table(male_higher_HIV_AIDS_mort_LAT$mal.Age.group.name)))

#### Females 
HIV_AIDS_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  druguse=0,
  unsafe=c(0,0,1,1,1,3,20,18,18,18),
  prev=0,
  step1=length(table(step1_female_higher_HIV_AIDS_LAT$mal.iso)),
  step2=length(table(step2_female_higher_HIV_AIDS_LAT$mal.iso)),
  step3=length(table(step3_female_higher_HIV_AIDS_LAT$mal.iso)),
  mort=c(2,0,0,0,0,0,0,0,0,0))

HIV_AIDS_path_male_LAT<-HIV_AIDS_path_male
HIV_AIDS_path_female_LAT<-HIV_AIDS_path_female

#### Female-Male difference
HIV_AIDS_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               HIV_AIDS_path_female-HIV_AIDS_path_male);rownames(HIV_AIDS_path_diff)<-NULL
HIV_AIDS_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                    ceiling(HIV_AIDS_path_diff[,2:4]/37*100),
                                    ceiling(HIV_AIDS_path_diff[,5:7]/16*100),
                                    mort=ceiling(HIV_AIDS_path_diff[,8]/37*100))
rownames(HIV_AIDS_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- HIV_AIDS_path_diff_prop
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = "",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot


##### HIV/AIDS - East Asia and Pacific #####
#### Males 
HIV_AIDS_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  druguse=cbind(table(male_higher_drug_use_EAP$mal.Age.group.name)),
  unsafe=0,
  prev=cbind(table(male_higher_HIV_AIDS_prev_EAP$mal.Age.group.name)),
  step1=length(table(step1_male_higher_HIV_AIDS_EAP$mal.iso)),
  step2=length(table(step2_male_higher_HIV_AIDS_EAP$mal.iso)),
  step3=length(table(step3_male_higher_HIV_AIDS_EAP$mal.iso)),
  mort=cbind(table(male_higher_HIV_AIDS_mort_EAP$mal.Age.group.name)))

#### Females 
HIV_AIDS_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  druguse=c(0,0,0,0,0,0,0,0,0,1),
  unsafe=cbind(table(female_higher_unsafesex_EAP$mal.Age.group.name)),
  prev=0,
  step1=length(table(step1_female_higher_HIV_AIDS_EAP$mal.iso)),
  step2=length(table(step2_female_higher_HIV_AIDS_EAP$mal.iso)),
  step3=length(table(step3_female_higher_HIV_AIDS_EAP$mal.iso)),
  mort=c(cbind(table(female_higher_HIV_AIDS_mort_EAP$mal.Age.group.name)),0,0))

HIV_AIDS_path_male_EAP<-HIV_AIDS_path_male
HIV_AIDS_path_female_EAP<-HIV_AIDS_path_female

#### Female-Male difference
HIV_AIDS_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               HIV_AIDS_path_female-HIV_AIDS_path_male);rownames(HIV_AIDS_path_diff)<-NULL
HIV_AIDS_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                    ceiling(HIV_AIDS_path_diff[,2:4]/34*100),
                                    ceiling(HIV_AIDS_path_diff[,5:7]/8*100),
                                    mort=ceiling(HIV_AIDS_path_diff[,8]/34*100))
rownames(HIV_AIDS_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- HIV_AIDS_path_diff_prop
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = "",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### HIV/AIDS - North America #####
#### Males 
HIV_AIDS_path_male<-data.frame(
  #age=row.names(cbind(table(male_higher_sodium$mal.Age.group.name))),
  druguse=c(1,1,1,1,1,0,0,0,0,0),
  unsafe=0,
  prev=c(0,0,0,cbind(table(male_higher_HIV_AIDS_prev_NOA$mal.Age.group.name))),
  step1=length(table(step1_male_higher_HIV_AIDS_NOA$mal.iso)),
  step2=length(table(step2_male_higher_HIV_AIDS_NOA$mal.iso)),
  step3=length(table(step3_male_higher_HIV_AIDS_NOA$mal.iso)),
  mort=cbind(table(male_higher_HIV_AIDS_mort_NOA$mal.Age.group.name)))

#### Females 
HIV_AIDS_path_female<-data.frame(
  #age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
  druguse=0,
  unsafe=0,
  prev=0,
  step1=length(table(step1_female_higher_HIV_AIDS_NOA$mal.iso)),
  step2=length(table(step2_female_higher_HIV_AIDS_NOA$mal.iso)),
  step3=length(table(step3_female_higher_HIV_AIDS_NOA$mal.iso)),
  mort=c(1,0,0,0,1,1,1,1,1,1))

HIV_AIDS_path_male_NOA<-HIV_AIDS_path_male
HIV_AIDS_path_female_NOA<-HIV_AIDS_path_female

#### Female-Male difference
HIV_AIDS_path_diff<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                               HIV_AIDS_path_female-HIV_AIDS_path_male);rownames(HIV_AIDS_path_diff)<-NULL
HIV_AIDS_path_diff_prop<-data.frame(age=row.names(cbind(table(female_higher_sodium$mal.Age.group.name))),
                                    ceiling(HIV_AIDS_path_diff[,2:4]/3*100),
                                    ceiling(HIV_AIDS_path_diff[,5:7]/0*100),
                                    mort=ceiling(HIV_AIDS_path_diff[,8]/3*100))
rownames(HIV_AIDS_path_diff_prop)<-NULL

#### Set dataframe for plot
df <- HIV_AIDS_path_diff_prop
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-101,-80,-60,-40,-20,-.1,0, 20, 40, 60, 80, 100),
    labels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "-81-100%" = "#08306b",   # Dark Blue  
      "-61-80%" = "#08519c",    # Medium Dark Blue  
      "-41-60%" = "#3182bd",    # Medium Blue  
      "-21-40%" = "#6baed6",    # Light Blue  
      "-1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80",         # Neutral Grey  
      "1-20%" = "#fff7bc",      # Very Light Yellow  
      "21-40%" = "#fee391",     # Light Yellow  
      "41-60%" = "#fec44f",     # Medium Yellow  
      "61-80%" = "#fe9929",     # Dark Yellow-Orange  
      "81-100%" = "#d95f0e"     # Dark Orange  
    ),
    breaks = c("-81-100%", "-61-80%", "-41-60%", "-21-40%", "-1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("81-100%", "61-80%", "41-60%", "21-40%", "1-20%","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    na.value = "white",
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12), # Adjust legend text size
    legend.title = element_text(size = 13) # Adjust legend title size
  ) +
  labs(
    x = "",
    y = "Age group",
    title = " ",
    fill = "% Countries \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### Verify #######
hyp_path_male_OV-(hyp_path_male_HI+hyp_path_male_UM+hyp_path_male_LM+hyp_path_male_LO)
hyp_path_female_OV-(hyp_path_female_HI+hyp_path_female_UM+hyp_path_female_LM+hyp_path_female_LO)

diab_path_male_OV-(diab_path_male_HI+diab_path_male_UM+diab_path_male_LM+diab_path_male_LO)
diab_path_female_OV-(diab_path_female_HI+diab_path_female_UM+diab_path_female_LM+diab_path_female_LO)

HIV_AIDS_path_male_OV-(HIV_AIDS_path_male_HI+HIV_AIDS_path_male_UM+HIV_AIDS_path_male_LM+HIV_AIDS_path_male_LO)
HIV_AIDS_path_female_OV-(HIV_AIDS_path_female_HI+HIV_AIDS_path_female_UM+HIV_AIDS_path_female_LM+HIV_AIDS_path_female_LO)

hyp_path_male_OV-(hyp_path_male_ECA+hyp_path_male_SAS+hyp_path_male_SSA+hyp_path_male_MEN+hyp_path_male_LAT+hyp_path_male_EAP+hyp_path_male_NOA)
hyp_path_female_OV-(hyp_path_female_ECA+hyp_path_female_SAS+hyp_path_female_SSA+hyp_path_female_MEN+hyp_path_female_LAT+hyp_path_female_EAP+hyp_path_female_NOA)

diab_path_male_OV-(diab_path_male_ECA+diab_path_male_SAS+diab_path_male_SSA+diab_path_male_MEN+diab_path_male_LAT+diab_path_male_EAP+diab_path_male_NOA)
diab_path_female_OV-(diab_path_female_ECA+diab_path_female_SAS+diab_path_female_SSA+diab_path_female_MEN+diab_path_female_LAT+diab_path_female_EAP+diab_path_female_NOA)

HIV_AIDS_path_male_OV-(HIV_AIDS_path_male_ECA+HIV_AIDS_path_male_SAS+HIV_AIDS_path_male_SSA+HIV_AIDS_path_male_MEN+HIV_AIDS_path_male_LAT+HIV_AIDS_path_male_EAP+HIV_AIDS_path_male_NOA)
HIV_AIDS_path_female_OV-(HIV_AIDS_path_female_ECA+HIV_AIDS_path_female_SAS+HIV_AIDS_path_female_SSA+HIV_AIDS_path_female_MEN+HIV_AIDS_path_female_LAT+HIV_AIDS_path_female_EAP+HIV_AIDS_path_female_NOA)

