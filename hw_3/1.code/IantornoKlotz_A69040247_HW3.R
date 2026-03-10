################################################################################
#
# GPCO 454 - Quantitative Methods II - Winter 2025
# Homework 3 - Solutions
# Author: Lucas Iantorno Klotz
# last modified: 03/09/2026
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

pack <- c('tidyverse', 'stargazer', 'readxl', 'patchwork', 'knitr',
          'ggeffects', 'interactions')

pacman::p_load(pack, install = F, character.only = T)                           # loading packages



th <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  # theme for ggplot figures. This is the theme I usually work with.
            plot.title =  element_text(size = 16, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 12, hjust = 0.5),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            axis.title =  element_text(size=16), legend.position="bottom", legend.title = element_blank(),
            legend.text = element_text(size=16),
            legend.key.size = unit(1.5, "cm"),
            axis.text =   element_text(size=12),
            #axis.text.x = element_text(angle = 45, hjust = 1),
            plot.caption = element_text(hjust = 0, size=11),
            strip.text = element_text(size = 15),
            plot.caption.position =  "plot")

# Set working directory
#setwd("/Users/liklotz/Library/CloudStorage/OneDrive-Pessoal/UCSD/winter_quarter_2026/QM_II/homeworks/hw_3") # the TA should change the working directory
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

summary(justice_data)

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



# 4. Visualize Pitch Differential by Court Period ------


chief_data<-justice_data|>
  filter(justiceName %in% c("WEBurger","WHRehnquist","JGRoberts"))|>            # Filter for relevant variables and remove missing values
  select(justiceName,term,petitioner_vote,pitch_diff,high_pitch_diff,court_period)|>
  filter(if_all(everything(),~!is.na(.)))


# Calculate proportion of votes by pitch differential and court period

summary_proportions<-chief_data|>
  group_by(high_pitch_diff, court_period)|>                                     # we want the proportion by court period and pitch_difference average measure
  summarise(share_votes = mean(petitioner_vote))

# Create bar plot

plot_bar<- function(court, title) {
  
  plot_data <- summary_proportions %>%
    filter(court_period == court)
  
  ggplot(plot_data, aes(x = as.factor(high_pitch_diff), y = round(share_votes,2), fill =  as.factor(high_pitch_diff))) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    scale_fill_manual(values = colors,
                      guide = "none") + # removes the legend
    scale_x_discrete(labels = c(`0` = "Below Avg. Pitch Differential", `1` = "Above Avg. Pitch Differential")) +
    scale_y_continuous(breaks = seq(0, 1, by = .2),  
                       limits = c(0, 1))+
    labs(title = title, x = "Pitch Differential Measure", y = "Share of Votes",
         fill = "Pitch Differential Measure") +
    th
}

plot1 <- plot_bar("Burger Court", "Warren E. Burguer Court")
plot2 <- plot_bar("Rehnquist Court", "William Rehnquist Court")
plot3 <- plot_bar("Roberts Court", "John Roberts Court")


final_panel <- (plot1 | plot2) / (plot3)

print(final_panel)

# save plot
ggsave("2. output/figures/HW3_Fig2.png", plot = final_panel, width = 12, height = 10, dpi = 300)

# Save the plot


# |||||||||||||||||||||||||||||||||
# Section 3.3 - Regression Analyses ----
# ||||||||||||||||||||||||||||||||

## 1. Create a New Variable: Proportion of Positive Words ----

justice_data <- justice_data|>
  mutate(share_pos_petitioner = (petitioner_harvard_pos/petitioner_wc),         # share of positive words when addressing the petitioner
         share_pos_respondent = (respondent_harvard_pos/respondent_wc),         # share of positive words when addressing the respondent
         pr_petitioner_pos = share_pos_petitioner - share_pos_respondent)       # difference between each share

## 2. Estimate First Regression Model (Baseline Model) ----

m_3_1 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos, data = justice_data)

# Each standard deviation increase of pitch_diff is associated with a decrease
# of, on average, ~5.7% in the probability of the justice voting in favor of the
# petitioner.

## 3. Addressing Justice-Specific Effects ------

m_3_2 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos + as.factor(justiceName), data = justice_data)

summary(m_3_2)


## 4. Adding Term-Specific Indicators ----

m_3_3 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos + as.factor(justiceName) + as.factor(court_period), data = justice_data)

summary(m_3_3)

# Create a regression table with all models

all_models <- list(m_3_1, m_3_2, m_3_3)

stargazer(
  all_models,
  type = "latex",
  #title = TITLE DISPLAYED ON THE WRITEUP,
  dep.var.labels = "Favor Petitioner Vote",
  covariate.labels = c(
    "Pitch Difference",
    "Positive Words Difference",
    "Justice: AScalia",
    "Justice: BRWhite",
    "Justice: CThomas",
    "Justice: DHSouter",
    "Justice: EKagan",
    "Justice: HABlackmun",
    "Justice: JGRoberts",
    "Justice: JPStevens",
    "Justice: LFPowell",
    "Justice: RBGinsburg",
    "Justice: SAAlito",
    "Justice: SDOConnor",
    "Justice: SGBreyer",
    "Justice: SSotomayor",
    "Justice: TMarshall",
    "Justice: WEBurguer",
    "Justice: WHRehnquist",
    "Court Period: Rehnquist",
    "Court Period: Roberts"
  ),
  #column.labels = "Arts, Entertainment, and Recreation",
  align = TRUE,
  no.space = TRUE,
  digits = 3,
  out = "2. output/tables/HW3_Table1.tex"
)

## 5. Court Period Interaction with Pitch Differential ----

justice_data$court_period <- factor(justice_data$court_period,
                          levels = c("Burger Court", "Rehnquist Court", "Roberts Court")) 

# Run models for overall and court-period interactions

m_3_4 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos, data = justice_data)                # baseline model

m_3_5 <- lm(petitioner_vote ~ pr_petitioner_pos + court_period*pitch_diff,
            data = justice_data)                                              # interaction model

 
# Visualize interaction effects

fig_3_3_5 <- ggpredict(m_3_5, terms = c("pitch_diff", "court_period"))

fig_3<-ggplot(fig_3_3_5, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +  # Line plot for interaction effect
  geom_point(size = 3) +  # Points for means
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +  # Confidence interval shading
  scale_color_manual(values = c("#E58724","#2e67be","#2A9D8F"), 
                     labels = c("Burguer Court", "Rehnquist Court", "Roberts Court")) +
  labs(#title = DISPLAYED ON THE WRITEUP,
       x = "Pitch Difference",
       y = "Predicted Probability of Voting for Petitioner") +
  th

print(fig_3)

# Save the plot

ggsave("2. output/figures/HW3_Fig3.png", plot = fig_3, width = 12, height = 10, dpi = 300)

## 6. Progressive Model Building ----

# Run multiple regressions with increasing complexity

model_1 <- lm(petitioner_vote ~ pitch_diff, data = justice_data)

model_2 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos, data = justice_data)

model_3 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos + sgpetac, data = justice_data)
  
model_4 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos + sgpetac + court_period, data = justice_data)

model_5 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos + sgpetac + court_period +
                pitch_diff:court_period, data = justice_data)

model_6 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos + sgpetac + court_period +
                           pitch_diff:pr_petitioner_pos, data = justice_data)

# Create regression table

all_models <- list(model_1, model_2, model_3, model_4, model_5, model_6)

stargazer(
  all_models,
  type = "latex",
  #title = TITLE DISPLAYED ON THE WRITEUP,
  dep.var.labels = "Favor Petitioner Vote",
  covariate.labels = c(
    "Pitch Difference",
    "Positive Words Difference",
    "Amicus Brief",
    "Court Period: Rehnquist",
    "Court Period: Roberts",
    "Pitch Difference X Court Period: Rehnquist",
    "Pitch Difference X Court Period: Roberts",
    "Pitch Difference X Positive Words Difference"
  ),
  #column.labels = "Arts, Entertainment, and Recreation",
  align = TRUE,
  no.space = TRUE,
  digits = 3,
  out = "2. output/tables/HW3_Table2.tex"
)

## 7. Interaction Effect Visualization ------
# Model: Pitch Differential x Court Period
# Plot the interaction effect
fig_3_3_7 <- ggpredict(model_5, terms = c("pitch_diff", "court_period"))

fig_4<-ggplot(fig_3_3_7, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +  # Line plot for interaction effect
  geom_point(size = 3) +  # Points for means
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +  # Confidence interval shading
  scale_color_manual(values = c("#E58724","#2e67be","#2A9D8F"), 
                     labels = c("Burguer Court", "Rehnquist Court", "Roberts Court")) +
  labs(#title = DISPLAYED ON THE WRITEUP,
    x = "Pitch Difference",
    y = "Predicted Probability of Voting for Petitioner") +
  th

print(fig_4)


# Save the plot

ggsave("2. output/figures/HW3_Fig4.png", plot = fig_4, width = 12, height = 10, dpi = 300)

# Interaction Plot: Pitch Differential x Positive Words

# For this plot we used interact_plot function from interactions package. It automatically
# splits the continuous variable in groups: mean, and +-1 standard deviations.

fig_5<-interact_plot(model_6, pred = pitch_diff, modx = pr_petitioner_pos, 
                     interval = T, int.width = .95,
                     colors = c("#E58724", "#2e67be", "#2A9D8F")) +
  labs(#title = DISPLAYED ON THE WRITEUP,
    x = "Pitch Difference",
    y = "Predicted Probability of Voting for Petitioner") +
  th


print(fig_5)


# Save the plot

ggsave("2. output/figures/HW3_Fig5.png", plot = fig_5, width = 12, height = 10, dpi = 300)


# |||||||||||||||||||||||||||||||||||||||||||||||||||||
# Section 4 - Outlier Analysis and Threats to Validity -------
# |||||||||||||||||||||||||||||||||||||||||||||||||||||

## 1. Outlier Diagnostics on Final Regression Model ------

# Re-run the final regression model from Section 3

vars <- c("petitioner_vote", "pitch_diff", "pr_petitioner_pos", "sgpetac", "court_period")
justice_data_nna <- justice_data[complete.cases(justice_data[, vars]), ]            # The full dataset has NA rows that we need to disregard

final_model<- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos + sgpetac + court_period +
                   pitch_diff:pr_petitioner_pos, data = justice_data_nna)

# Calculate outlier diagnostics

### 1.1 Studentized Residuals -----
justice_data_nna$studentized_residuals <- rstudent(final_model)

# Identify potential outliers based on the rule: abs(residual) > 2
outliers_residuals <- justice_data_nna %>%
  filter(abs(studentized_residuals) > 2)

cat("\nObservations with Studentized Residuals > 2:\n")
print(outliers_residuals[, c("caseId", "petitioner_vote", "court_period", "studentized_residuals")])

# No outliers based on this metric/threshold.

### 1.2 Leverage ------

justice_data_nna$leverage <- hatvalues(final_model)

# Rule of thumb: Leverage > 2k+2)/n
k <- length(coef(final_model)) - 1  # Number of predictors
n <- nrow(justice_data_nna)
leverage_threshold <- (2*k + 2) / n

outliers_leverage <- justice_data_nna %>%
  filter(leverage > leverage_threshold)

cat("\nObservations with High Leverage (> (2k+2)/n):\n")
print(outliers_leverage[, c("caseId", "petitioner_vote", "court_period", "leverage")])

# 242 outliers based on this metric

### 1.3 Cook's Distance ---------
justice_data_nna$cooks_distance <- cooks.distance(final_model)

# Rule of thumb: Cook's Distance > 4/n
cooks_threshold <- 4 / n

outliers_cooks <- justice_data_nna %>%
  filter(cooks_distance > cooks_threshold)

cat("\nObservations with High Cook's Distance (> 4/n):\n")
print(outliers_cooks[, c("caseId", "petitioner_vote", "court_period", "cooks_distance")])

# 132 outliers based on this metric

### 1.4 Difference in Fitted Values (DFFITS) -----

justice_data_nna$dffits <- dffits(final_model)

# Rule of thumb: |DFFITS| > 2 * sqrt(k/n)
dffits_threshold <- 2 * sqrt(k / n)

outliers_dffits <- justice_data_nna %>%
  filter(abs(dffits) > dffits_threshold)

cat("\nObservations with High DFFITS (> 2√(k/n)):\n")
print(outliers_dffits[, c("caseId", "petitioner_vote", "court_period", "dffits")])

# 149 outliers based on this metric




### 1.5 Identify Outliers and Egregious Outliers Based on Thresholds ----

justice_data_nna <- justice_data_nna %>%
  mutate(
    outlier = as.integer(
      abs(studentized_residuals) > 2 |
        leverage > leverage_threshold |
        cooks_distance > cooks_threshold |
        abs(dffits) > dffits_threshold
    ),
    # Define egregious outlier: flagged by ALL metrics
    egregious_outlier = as.integer(
      (abs(studentized_residuals) > 2) +
        (leverage > leverage_threshold) +
        (cooks_distance > cooks_threshold) +
        (abs(dffits) > dffits_threshold) >= 4
    )
  )

cat("\nOutlier counts:\n")
cat("Outliers (1+ metrics):", sum(justice_data_nna$outlier), "\n")
cat("Egregious outliers (4 metrics):", sum(justice_data_nna$egregious_outlier), "\n")

outlier_summary <- justice_data_nna %>%
  mutate(n_metrics_flagged = 
           (abs(studentized_residuals) > 2) +
           (leverage > leverage_threshold) +
           (cooks_distance > cooks_threshold) +
           (abs(dffits) > dffits_threshold)
  ) %>%
  filter(n_metrics_flagged > 1) %>%
  select(caseId, term, pitch_diff, court_period, studentized_residuals, leverage, cooks_distance, dffits, n_metrics_flagged) %>%
  arrange(desc(n_metrics_flagged))

cat("Observations flagged by more than one metric:", nrow(outlier_summary), "\n")


## 2. Visualize Potential Outliers -----

fig_6<-ggplot(justice_data_nna, aes(x = abs(dffits), y = leverage)) +
  geom_point(aes(color = factor(outlier)), alpha = 0.5) +
  scale_color_manual(values = c("0" = "black", "1" = "red"),
                     labels = c("0" = "Non-outlier", "1" = "Outlier"),
                     ) +
  geom_hline(yintercept = leverage_threshold, color = "#2e67be", linetype = "dashed") +
  geom_vline(xintercept = dffits_threshold, color = "#E58724", linetype = "dashed") +
  labs( #title = DISPLAYED ON THE WRITEUP,
    x = "DFFITS",
    y = "Leverage") +
  th

print(fig_6)


# Save the plot

ggsave("2. output/figures/HW3_Fig6.png", plot = fig_6, width = 12, height = 10, dpi = 300)

## 3. Regression Models with and without Outliers -----

# Filter out identified outliers

# Model 1: Full Dataset

model_1<- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos + sgpetac + court_period +
                   pitch_diff:pr_petitioner_pos, data = justice_data_nna)

# Model 2: Without Outliers

clean_data<- justice_data_nna|>
  filter(outlier == 0)

model_2<- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos + sgpetac + court_period +
               pitch_diff:pr_petitioner_pos, data = clean_data)

# Regression Table Comparison

all_models <- list(model_1, model_2)

stargazer(
  all_models,
  type = "latex",
  #title = TITLE DISPLAYED ON THE WRITEUP,
  dep.var.labels = "Favor Petitioner Vote",
  covariate.labels = c(
    "Pitch Difference",
    "Positive Words Difference",
    "Amicus Brief",
    "Court Period: Rehnquist",
    "Court Period: Roberts",
    "Pitch Difference X Positive Words Difference"
  ),
  #column.labels = "Arts, Entertainment, and Recreation",
  align = TRUE,
  no.space = TRUE,
  digits = 3,
  out = "2. output/tables/HW3_Table3.tex"
)
