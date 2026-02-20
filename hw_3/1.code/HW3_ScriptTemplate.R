################################################################################
#
# GPCO 454 - Quantitative Methods II - Winter 2025
# Homework 3 - Solutions
# Author: Lucas Iantorno Klotz
# last modified: 02/15/2026
# 
################################################################################

# In this script, I provide the data analysis for homework 3.

# I usually have a master file where I load the packages and define a few functions.
# I'm going to add them here.

# Dear TA, the plots' titles are displayed on the writeup. I commented the lines
# that set the title to the plot.

# You will also note that the variables' names are a bit distinct from the original
# raw data. That's because I use janitor::clean_name() which standardize the names.

# 0. Preamble --------

rm(list = ls())
gc()

options(scipen = 20)                                                            # Avoid scientific notation when looking at data.frames

pack <- c('tidyverse', 'stargazer', 'readxl', 'patchwork', 'knitr')

pacman::p_load(pack, install = F, character.only = T)                           # loading packages



th <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  # theme for ggplot figures. This is the theme I usually work with.
            plot.title =  element_text(size = 16, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 12, hjust = 0.5),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            axis.title =  element_text(size=16), legend.position="bottom", legend.title = element_blank(),
            legend.text = element_text(size=14),
            axis.text =   element_text(size=12),
            #axis.text.x = element_text(angle = 45, hjust = 1),
            plot.caption = element_text(hjust = 0, size=11),
            strip.text = element_text(size = 15),
            plot.caption.position =  "plot")

# Set working directory
setwd("/Users/liklotz/Library/CloudStorage/OneDrive-Pessoal/UCSD/winter_quarter_2026/QM_II/homeworks/hw_3") # the TA should change the working directory
getwd()
# |||||||||||||||||||||||||||||
## Section 3.1 - Preliminaries -----
# |||||||||||||||||||||||||||||

## 1. Load the dataset into R -----
justice_data<-read.table("raw_data/justice_results.tab",
                         header = T,
                         sep = "\t",
                         encoding = "ISO-8859-1")

# Pitch difference = vocal pitch in questions directed toward petitioner minus
# vocal pitc in questions directed toward respondents.


## 2. Explore the dataset -------
# Display the structure of the dataset

# 3. Summarize the key variables -----

# 4. Count unique observations in the 'docket' variable -------

length(unique(justice_data$docket))

# 1002 Supreme Court cases

# 5. Check for missing values ------

na_data<-justice_data |>
  summarise(across(everything(), ~sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "var", values_to = "n_NA") |>
  filter(n_NA > 0) %>%
  arrange(desc(n_NA))

# ---------------------------
# Section 3.2 - Getting to Know the Data and Descriptive Statistics
# ---------------------------

# 1. Examine Key Variables: Generate summary statistics

summary_stats <- justice_data |>
  summarise(across(c(petitioner_vote, pitch_diff, petitioner_harvard_pos), list(
    N    = ~sum(!is.na(.)),
    mean = ~mean(., na.rm=TRUE),
    sd   = ~sd(., na.rm=TRUE),
    min  = ~min(., na.rm=TRUE),
    p25  = ~quantile(., .25, na.rm=TRUE),
    med  = ~median(., na.rm=TRUE),
    p75  = ~quantile(., .75, na.rm=TRUE),
    max  = ~max(., na.rm=TRUE),
    n_NA = ~sum(is.na(.))
  ), .names = "{.col}__{.fn}")) |>
  pivot_longer(everything(), names_to = "name", values_to = "value") %>%
  separate(name, into = c("var", "stat"), sep = "__") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  select(var, N, mean, sd, min, p25, med, p75, max, n_NA)

kable(summary_stats)


# Pitch difference and number of positive words with some outliers.

histogram <- function(variable, x_label) {
  
  plot_data <- justice_data %>%
    filter(!is.na(.data[[variable]]))
  
  ggplot(plot_data, aes(x = .data[[variable]])) +
    geom_histogram(
      aes(y = after_stat(count / sum(count)) * 100),                              # after_stat() replaces ..variable..
      #binwidth = .5,
      fill = "#2e67be", 
      alpha = 0.5, 
      color = "#2e67be"
    ) +
    labs(#title = title,
         x = x_label,
         y = "Frequency (%)") +
    th
}

# Generate plots for each variable
plot1 <- histogram("pitch_diff", "Pitch difference")
plot2 <- histogram("petitioner_harvard_pos", "Positive words addressed to petitioner")

# Display plots
final_panel <- (plot1 | plot2) 
print(final_panel)

## 2. Create New Variables----

# Create the binary variable high_pitch_diff

# Create the categorical variable court_period based on chief justice tenure

# Convert court_period to a factor for categorical analysis
justice_data<-justice_data|>
  mutate(high_pitch_diff = ifelse(pitch_diff > mean(pitch_diff),1,0),           # No NAs no need for na.rm = T
         court_period = case_when(
           term >= 1969 & term <= 1985 ~ "Burger Court",
           term >= 1986 & term <= 2004 ~ "Rehnquist Court",
           term >= 2005 ~ "Roberts Court",
         ))           

# Verify the new variables
table(justice_data$high_pitch_diff)
table(justice_data$court_period)


# 3. Visualize Amicus Support and Voting Patterns ------

# Filter for the three Chief Justices

chief_data<-justice_data|>
  filter(justiceName %in% c("WEBurger","WHRehnquist","JGRoberts"))|>
  select(justiceName,petitioner_vote,sgpetac)|>
  #filter(across(everything(), ~ !is.na(.)))
  filter(if_all(everything(),~!is.na(.)))                                       # It looks like it does the same as line 164

# Calculate proportion of votes in favor of petitioner

summary_proportions<-chief_data|>
  group_by(justiceName, sgpetac)|> # we want the proportion by chief justice and amicus brief
  summarise(share_votes = mean(petitioner_vote),
            sgpetac = as.factor(sgpetac))
  
# Create bar plot

colors <- c("1" = "#2e67be", "0" = "#E58724")                                   # I like these colors, if that's okay

plot_bar<- function(justice, title) {
  
  plot_data <- summary_proportions %>%
    filter(justiceName == justice)
  
  ggplot(plot_data, aes(x = as.factor(sgpetac), y = round(share_votes,2), fill =  as.factor(sgpetac))) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    scale_fill_manual(values = colors,
                      guide = "none") + # removes the legend
    scale_x_discrete(labels = c(`0` = "No Amicus", `1` = "Amicus")) +
    scale_y_continuous(breaks = seq(0, 1, by = .2),  
                       limits = c(0, 1))+
    labs(title = title, x = "Amicus Brief Submission", y = "Share of Votes",
         fill = "Amicus Brief Submission") +
    th
}

plot1 <- plot_bar("WEBurger", "Warren E. Burguer")
plot2 <- plot_bar("WHRehnquist", "William Rehnquist")
plot3 <- plot_bar("JGRoberts", "John Roberts")


final_panel <- (plot1 | plot2) / (plot3)

print(final_panel)

# save plot
ggsave("2. output/figures/HW3_Fig1.png", plot = final_panel, width = 12, height = 10, dpi = 300)



# 4. Visualize Pitch Differential by Court Period

# Filter for relevant variables and remove missing values

# Create high_pitch_diff variable

# Create court_period variable

# Calculate proportion of votes by pitch differential and court period

# Create bar plot

# Save the plot


# ---------------------------
# Section 3.3 - Regression Analyses
# ---------------------------

# 1. Create a New Variable: Proportion of Positive Words

# 2. Estimate First Regression Model (Baseline Model)

# 3. Addressing Justice-Specific Effects

# Convert justiceName to a factor for inclusion in the regression

# Run the regression with justice fixed effects

# 4. Adding Term-Specific Indicators

# Create a regression table with all models

# 5. Court Period Interaction with Pitch Differential

# Create court period variable based on chief justice tenure

# Run models for overall and court-period interactions

# Visualize interaction effects

# Save the plot

# 6. Progressive Model Building

# Run multiple regressions with increasing complexity

# Create regression table

# 7. Interaction Effect Visualization
# Model: Pitch Differential x Court Period

# Plot the interaction effect

# Save the plot

# Interaction: Pitch Differential x Positive Words

# Plot the interaction effect

# Save the plot

# ---------------------------
# Section 4 - Outlier Analysis and Threats to Validity
# ---------------------------

# 1. Outlier Diagnostics on Final Regression Model

# Re-run the final regression model from Section 3

# Calculate outlier diagnostics

# Create a dataframe to store outlier diagnostics

# Define Critical Thresholds for Outliers

# Identify Outliers Based on Thresholds

# Identify Egregious Outliers (if all thresholds are exceeded)

# 2. Visualize Potential Outliers

# Save the plot

# 3. Regression Models with and without Outliers

# Filter out identified outliers

# Model 1: Full Dataset

# Model 2: Without Outliers

# Regression Table Comparison
