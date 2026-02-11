################################################################################
#
# QM II: Homework 1
# Author: Lucas Iantorno Klotz
# last modified: 01/26/2026
# 
################################################################################

# In this script, I provide the data analysis for homework 1, which is related
# to wildfires and pro-environment policies support.

# I usually have a master file where I load the packages and define a few functions.
# I'm going to add them here.

# 0. Preamble --------

rm(list = ls())
gc()

options(scipen = 20)                                                            # Avoid scientific notation when looking at data.frames

pack <- c('tidyverse', 'stargazer')

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
#setwd("/Users/liklotz/Library/CloudStorage/OneDrive-Pessoal/UCSD/winter quarter 2026/QM II/homeworks/hw 1") # the TA should change the working directory
getwd()

# Loading data
data<-read.csv("raw_data/HW1_wildfire_exposure.csv", sep = ",")

# 1. Preliminaries ------

str(data)                                                                       # variables are either numeric or integer. That's good.
dim(data)                                                                       # dist_km is not in the codebook.


## Q1 - Q3 -------
# dim() shows the N of observations and the N of variables.
# 20990 observations and 16 variables.

## Q4 ------

sum(data[data == " "])                                                          # no missing values.

# 2. Data and descriptive stats ---------

## Q5 & Q6 ------

table(data$wildfire2yr5000)

# 66 BGs experienced a wildfire over 5000 acres in the last two years. 

# converting distance to km
data<-data|>                                                                    # using pipe operator.
  mutate(mindistkm = mindist5000/1000)|>                                        # km = m/1000
  select(1:4,17,5:16)                                                           # re-ordering the columns.

mean_envbi<-mean(data$envbi)
var_envbi<-var(data$envbi)
mean_mindistkm<-mean(data$mindistkm)
var_mindistkm<-var(data$mindistkm)
cov_envbi_mindistkm<-cov(data$envbi,data$mindistkm)

# 3. Visualizing data -------

## 3.1 DV histogram --------

bw <- diff(range(data$envbi, na.rm = TRUE)) / 20                                # defining the bins width

plot_envbi <- ggplot(data, aes(x = envbi)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count)) * 100),                              # after_stat() replaces ..variable..
    binwidth = bw,
    fill = "#2e67be", 
    alpha = 0.5, 
    color = "#2e67be"
  ) +
  geom_density(
    aes(y = after_stat(density * bw * 100)),
    color = "#E58724",
    linewidth = 1
  ) +
  labs(x = "Environmental Ballot Index",
       y = "Frequency (%)") +
  th


ggsave("2. output/figures/distribution_envbi.png", plot = plot_envbi, width = 8, height = 6) # save plot

## 3.2 IV histogram -------

bw <- diff(range(data$mindistkm, na.rm = TRUE)) / 20                            

plot_distkm <- ggplot(data, aes(x = mindistkm)) +
  geom_histogram(
    aes(y = after_stat(count/sum(count))*100),                                  
    bins = 20, 
    fill = "#2e67be", 
    alpha = 0.5, 
    color = "#2e67be"
  ) +
  geom_density(
    aes(y = after_stat(density * bw * 100)),                               
    color = "#E58724", 
    size = 1
  ) +
  scale_x_continuous(
    breaks = seq(0, 300, by = 50),  
    limits = c(0, 300)
  ) +
  labs(
    #title = "Distribution of Environmental Ballot Index",
    x = "Distance to wildfires (km)",
    y = "Frequency (%)"
  ) +
  th

# Two values removed because they are greater than 300km. The plot's overall interpretation
# is not affected.

ggsave("2. output/figures/distribution_distkm.png", plot = plot_distkm, width = 8, height = 6) # save plot

## 3.3 Scatter plot Environmental Index x Distance to wildfires -----

plot_scatter <- ggplot(data, aes(x = mindistkm, y = envbi)) +
  geom_point(color = "#2e67be", size = 2, alpha = 0.3) +
  geom_smooth(method = "lm", color = "#E58724", se = TRUE, size = 1) +
  scale_y_continuous(
    breaks = seq(0.2, 1, by = .2),  
    limits = c(0.2, 1)
  ) +
  labs(
    #title = "Scatter Plot of GPA vs. Confidence",
    x = "Distance to wildfires (km)",
    y = "Environmental Ballot Index"
  )  +
  th

# There's somewhat a downward trend, but a lot of variability at each distance
# point. Correlation is very weak.

ggsave("2. output/figures/scatter_index_distance.png", plot = plot_scatter, width = 8, height = 6) # save plot

# 4. Regression Analysis ------

## Q8 ------

# Regression model

m1<-lm(envbi ~ mindistkm, data = data)

summary(m1)

# Negative (and very significant) relationship. But not too strong: an 100km 
# increase on the distance from the wildfire, on average, is associated with
# a nearly .04 point reduction in the environmental index.

# Intercept (b0) shows that when the wildfires are at ground zero the index is, on average,
# app. 0.66.

# R2 very low (.025), meaning that only 2.5% of the variation of the DV is explained
# by the variation of the IV. So this model is not very good at explaining what
# is associated with voter support for pro-environment policies.

# Saving the table

stargazer(
  m1, 
  type = "latex", 
  title = "",
  dep.var.labels = "Dependent Variable: Envri. Index",
  covariate.labels = c("mindistkm", "Intercept"),
  out = "2. output/tables/model_summary_2.tex"
)

model_list <- list(
  "Envir. Index (1)" = m1
)


# Manually calculating \beta_0 and \beta_1

b1 <- cov_envbi_mindistkm / var_mindistkm
b0 <- mean_envbi - b1 * mean_mindistkm

# Just as lm() delivers

# Covariance determines the sign of the relationship.

