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

pack <- c('tidyverse', 'stargazer', 'readxl', 'patchwork')

pacman::p_load(pack, install = F, character.only = T)                           # loading packages



th <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  # theme for ggplot figures. This is the theme I usually work with.
            plot.title =  element_text(size = 16, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 12, hjust = 0.5),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            axis.title =  element_text(size=16), legend.position="bottom", legend.title = element_blank(),
            legend.text = element_text(size=16),
            axis.text =   element_text(size=10),
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
                         enconding = "ISO-8859-1")

## 2. Explore the dataset -------
# Display the structure of the dataset

# 3. Summarize the key variables -----

# 4. Count unique observations in the 'docket' variable -------

# 5. Check for missing values ------


# ---------------------------
# Section 3.2 - Getting to Know the Data and Descriptive Statistics
# ---------------------------

# 1. Examine Key Variables: Generate summary statistics

# 2. Create New Variables

# Create the binary variable high_pitch_diff

# Create the categorical variable court_period based on chief justice tenure

# Convert court_period to a factor for categorical analysis

# Verify the new variables

# 3. Visualize Amicus Support and Voting Patterns

# Filter for the three Chief Justices

# Calculate proportion of votes in favor of petitioner

# Create bar plot

# Save the plot


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
