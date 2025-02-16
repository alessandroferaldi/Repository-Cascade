#######################
######## MALES ########
#######################

######## Source Clean Data ##############
source(file="~/Desktop/Data Processing.R")

######## Source Analysis.R to get Results ##############
source(file="~/Desktop/Analysis.R")

######## Source Figure.R to get Males Health Pathways ##############
source(file="~/Desktop/Figures.R")

######## Plot Health Pathways - Overall - Figure A7 ############
##### Hypertension - Panel A #####
#### Set dataframe for plot
df <- hyp_path_male_prop_OV

#### Add missing %, needed for complete legend
# df[1,2]<-c(90)

colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
      ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot



##### Diabetes  - Panel C #####
#### Set dataframe for plot
df <- diab_path_male_prop_OV
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
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot



##### HIV/AIDS - Panel E #####
#### Set dataframe for plot
df <- HIV_AIDS_path_male_prop_OV
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey  
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot



######## Plot Health Pathways - Overall - Figure A8 ############
##### Hypertension - High ############
#### Set dataframe for plot
df <- hyp_path_male_prop_HI

colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### Hypertension - Upper-Middle ############
#### Set dataframe for plot
df <- hyp_path_male_prop_UM

colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### Hypertension - Lower-Middle ############
#### Set dataframe for plot
df <- hyp_path_male_prop_LM

colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### Hypertension - Low ############
#### Set dataframe for plot
df <- hyp_path_male_prop_LO

colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

######## Plot Health Pathways - Overall - Figure A9 ############
##### Hypertension - Europe and Central Asia ############
#### Set dataframe for plot
df <- hyp_path_male_prop_ECA

colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### Hypertension - South Asia ############
#### Set dataframe for plot
df <- hyp_path_male_prop_SAS

colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### Hypertension - Sub-Saharan Africa ############
#### Set dataframe for plot
df <- hyp_path_male_prop_SSA

colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### Hypertension - Middle East and North Africa ############
#### Set dataframe for plot
df <- hyp_path_male_prop_MEN

colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### Hypertension - Latin America & the Caribbean ############
#### Set dataframe for plot
df <- hyp_path_male_prop_LAT

colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### Hypertension - East Asia and Pacific ############
#### Set dataframe for plot
df <- hyp_path_male_prop_EAP

colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### Hypertension - North America ############
#### Set dataframe for plot
df <- hyp_path_male_prop_NOA

colnames(df)<-c("Age_Group","Sodium","Glucose","Overweight","Obesity","Smoking","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

######## Plot Health Pathways - Overall - Figure A10 ############
##### Diabetes - High #####
#### Set dataframe for plot
df <- diab_path_male_prop_HI
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
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### Diabetes - Upper-Middle #####
#### Set dataframe for plot
df <- diab_path_male_prop_UM
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
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### Diabetes - Lower-Middle #####
#### Set dataframe for plot
df <- diab_path_male_prop_LM
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
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### Diabetes - Low #####
#### Set dataframe for plot
df <- diab_path_male_prop_LO
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
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot




######## Plot Health Pathways - Overall - Figure A11 ############
##### Diabetes - Europe and Central Asia #####
#### Set dataframe for plot
df <- diab_path_male_prop_ECA
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
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### Diabetes - South Asia #####
#### Set dataframe for plot
df <- diab_path_male_prop_SAS
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
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### Diabetes - Sub-Saharan Africa #####
#### Set dataframe for plot
df <- diab_path_male_prop_SSA
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
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### Diabetes - Middle East and North Africa #####
#### Set dataframe for plot
df <- diab_path_male_prop_MEN
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
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### Diabetes - Latin America & the Caribbean #####
#### Set dataframe for plot
df <- diab_path_male_prop_LAT
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
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### Diabetes - East Asia and Pacific #####
#### Set dataframe for plot
df <- diab_path_male_prop_EAP
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
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### Diabetes - North America #####
#### Set dataframe for plot
df <- diab_path_male_prop_NOA
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
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey     
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
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
    fill = "% Countries: \n\n Male > Female"
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 5.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_vline(xintercept = 9.5, size = 1.2) +
  geom_text(aes(x = 3, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 8, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

######## Plot Health Pathways - Overall - Figure A12 ############
##### HIV/AIDS - High #####
#### Set dataframe for plot
df <- HIV_AIDS_path_male_prop_HI
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey  
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### HIV/AIDS - Upper-Middle #####
#### Set dataframe for plot
df <- HIV_AIDS_path_male_prop_UM
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey  
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### HIV/AIDS - Lower-Middle #####
#### Set dataframe for plot
df <- HIV_AIDS_path_male_prop_LM
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey  
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### HIV/AIDS - Low #####
#### Set dataframe for plot
df <- HIV_AIDS_path_male_prop_LO
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey  
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

######## Plot Health Pathways - Overall - Figure A13 ############
##### HIV/AIDS - Europe and Central Asia #####
#### Set dataframe for plot
df <- HIV_AIDS_path_male_prop_ECA
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey  
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### HIV/AIDS - South Asia #####
#### Set dataframe for plot
df <- HIV_AIDS_path_male_prop_SAS
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey  
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### HIV/AIDS - Sub-Saharan Africa #####
#### Set dataframe for plot
df <- HIV_AIDS_path_male_prop_SSA
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey  
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### HIV/AIDS - Middle East and North Africa #####
#### Set dataframe for plot
df <- HIV_AIDS_path_male_prop_MEN
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey  
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### HIV/AIDS - Latin America & the Caribbean #####
#### Set dataframe for plot
df <- HIV_AIDS_path_male_prop_LAT
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey  
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### HIV/AIDS - East Asia and Pacific #####
#### Set dataframe for plot
df <- HIV_AIDS_path_male_prop_EAP
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey  
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
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
    fill = "% Countries: \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot

##### HIV/AIDS - North America #####
#### Set dataframe for plot
df <- HIV_AIDS_path_male_prop_NOA
colnames(df)<-c("Age_Group","Drug Use","Unsafe Sex","Prevalence","Diagnosed","Treated","Controlled","Death")
melted_df <- melt(df, id.vars="Age_Group")

#### Set breaks
melted_df$fill_category <- factor(
  cut(
    melted_df$value,
    breaks = c(-.1,0, 20, 40, 60, 80, 100),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    right = TRUE
  ),
  levels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%") # Include all levels explicitly
)

#### Plot
heatmap_plot <- ggplot(melted_df, aes(x = variable, y = Age_Group, fill = fill_category)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  scale_fill_manual(
    values = c(
      "81-100%" = "#08306b",   # Dark Blue  
      "61-80%" = "#08519c",    # Medium Dark Blue  
      "41-60%" = "#3182bd",    # Medium Blue  
      "21-40%" = "#6baed6",    # Light Blue  
      "1-20%" = "#bdd7e7",     # Very Light Blue  
      "0%" = "grey80"         # Neutral Grey  
    ),
    breaks = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
    labels = c("0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
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
    x = "",
    y = "Age group",
    title = " ",
    fill = "% Countries: \n\n Male > Female      "
  ) +
  scale_y_discrete(limits = c(df$Age_Group[-11], "")) +
  geom_vline(xintercept = 2.5, size = 1.2) +
  geom_vline(xintercept = 3.5, size = 1.2) +
  geom_vline(xintercept = 6.5, size = 1.2) +
  geom_text(aes(x = 1.5, y = 11, label = "Risk factors"), size = 4.5) +
  geom_text(aes(x = 5, y = 11, label = "Cascade"), size = 4.5);heatmap_plot
