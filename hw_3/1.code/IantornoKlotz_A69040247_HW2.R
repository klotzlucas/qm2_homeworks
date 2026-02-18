################################################################################
#
# QM II: Homework 2
# Author: Lucas Iantorno Klotz
# last modified: 02/15/2026
# 
################################################################################

# In this script, I provide the data analysis for homework 2, which is related
# to Taylor Swift's (TS) Eras Tour and its impact on local economies.

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
#setwd("/Users/liklotz/Library/CloudStorage/OneDrive-Pessoal/UCSD/winter_quarter_2026/QM_II/homeworks/hw_2") # the TA should change the working directory
getwd()

# |||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Section 3.1 - Preliminaries ---------
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||

## 2. Load the datasets into R ------

cagdp_data <- read_excel("raw_data/HW2_CAGDP2_ALL_AREAS_2001_2023.xlsx")|>      # GDP data by county
  janitor::clean_names()                                                        # Standardize columns names (all lowercase and _ as separator)

eras_data <- read_excel("raw_data/HW2_The_Eras_Tour_US_Schedule.xlsx")|>        # Eras Tour data
  janitor::clean_names()            

controls_data <- read_excel("raw_data/HW2_County_Demographics.xlsx")|>          # Demographics data
  janitor::clean_names()                                                        


## 3. Merge cagdp_data and controls_data ------

merged_data<-cagdp_data|>
  left_join(controls_data,by = "geo_fips")

# merged_data has the same N of observations as cagdp. This is expected. We want
# every county-industry-level obs to have the same demographic information of
# the particular county.

## 4. Drop GeoName.y and rename GeoName.x to GeoName -------

merged_data<-merged_data|>
  select(1:30,32:39)|>                                                          # Selecting all variables except geoname.y
  rename(geo_name = geo_name.x)                                                 # Rename variable

# Check the structure of the merged dataset to ensure the merge was successful:
# Yes, very successful. Now the dataset has 38 variables.

## 5. Filter the dataset to include only cases where Rural_Urban_Continuum_Code_2023 == 1 ------

filtered_data <- merged_data|>
  filter(rural_urban_continuum_code_2023 == 1)|>                                # Let's consider only the most urban places
  select(-rural_urban_continuum_code_2023)                                      # This variable now becomes irrelevant as all rows are equal to 1

## 6. Drop Rural_Urban_Continuum_Code_2023 from filtered_data ----------

# Done above (row 81).


## 7. Merge eras_data into filtered_data. Name the resulting dataset working_data ------

working_data<-filtered_data|>
  left_join(eras_data, by = "geo_fips")

## 8. Check the dimension of working_data -----

dim(working_data)
# 14.416 obs and 42 variables

## 9. Count the unique number of observations in the GeoName variable ----

length(unique(working_data$geo_name))                                           # length() gives the size of an object in this case the unique values of geo_name

# 424 counties.

# |||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Section 3.2 - Data Cleaning: Dependent Variables ------
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||

## 1. Identify the columns corresponding to years 2001 to 2023 ----

year_columns <- c("x2001","x2002","x2003","x2004","x2005","x2006",              # Vector with all the year variables
                             "x2007","x2008","x2009","x2010","x2011","x2012",
                             "x2013","x2014","x2015","x2016","x2017","x2018",
                             "x2019","x2020","x2021","x2022","x2023")           # I had to do this because clean_names() adds an x for all variables whose name is a number

## 2. Replace "(D)" with NA and convert the columns to numeric -----

working_data <- working_data|>
  mutate(across(all_of(year_columns), ~ ifelse(.x == "(D)", NA, .x)))|>         # Replace "(D)" with NA
  mutate(across(all_of(year_columns), as.numeric))                              # Convert the columns to numeric format

# 3. Convert these variables to numeric data type after replacing (D) with NA.

# Done above (row 120)

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Section 3.3 - Variable Creation: Hosting an Eras Tour Concert (Binary Indicator) ----- 
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

## 1. Add a binary indicator for hosting an Eras Tour concert in 2023 -------

working_data <- working_data|>
  mutate(eras_tour_host = ifelse(is.na(hosted),0,1))                            # ifelse() makes more sense here than case_when()

# I'm assuming counties that have NA in the hosted variable did NOT host a TS concert.


# |||||||||||||||||||||||||||||||||||||||||||||||||||
# Section 3.4.4 - Transforming the Outcome Variables -------
# |||||||||||||||||||||||||||||||||||||||||||||||||||

## 1. Transform GDP variables to per capita values ------

working_data <- working_data |>
  mutate(across(all_of(year_columns), ~ round(.x / pop_estimate_2023, 2)))      # Round to two decimal places

# Here, I'm assuming a constant population estimate across 20 years.

## 2. Filter the dataset to include only rows for the Arts, Entertainment, and Recreation industry (NAICS 71) ----

working_data_71 <- working_data|>
  filter(industry_classification == 71)

## 3.Generate a histogram of the transformed variable, 2023 county-level GDP per capita in this industry, and save it as an image file -----

plot_gdp_pc_23 <- ggplot(working_data_71, aes(x = x2023)) +
  geom_histogram(
    aes(y = after_stat(count/sum(count)) * 100),                              # after_stat() replaces ..variable..
    binwidth = .5,
    fill = "#2e67be", 
    alpha = 0.5, 
    color = "#2e67be"
  ) +
  scale_x_continuous(
    breaks = seq(0, 57, by = 7),  
    limits = c(0, 57)
  ) +
  labs(# title = TITLE IS DISPLAYED ON THE WRITEUP ,
       x = "GDP per capita (in thousands of USD)",
       y = "Frequency (%)") +
  th

# Very skewed to the right, which is normal for gdp.

# Display the plot
print(plot_gdp_pc_23)

# Save the histogram as an image file

ggsave("2. output/figures/distribution_gdp_pc_naics_71_2023.png", plot = plot_gdp_pc_23, width = 8, height = 6) # save plot

## 4. Transform GDP variables to log of (1 + thousand per capita) values and generate the histogram -----
plot_gdp_pc_23_log <- ggplot(working_data_71, aes(x = log(1+x2023))) +
  geom_histogram(
    aes(y = after_stat(count / sum(count)) * 100),                              # after_stat() replaces ..variable..
    binwidth = .5,
    fill = "#2e67be", 
    alpha = 0.5, 
    color = "#2e67be"
  ) +
  labs(# title = TITLE IS DISPLAYED ON THE WRITEUP ,
       x = "Log GDP per capita",
       y = "Frequency (%)") +
  th


# Display the plot
print(plot_gdp_pc_23_log)


# Save the histogram as an image file
ggsave("2. output/figures/distribution_log_gdp_pc_naics_71_2023.png", plot = plot_gdp_pc_23_log, width = 8, height = 6) # save plot

# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Section 3.5 - Analyzing Time Trends in Arts, Entertainment, and Recreation Industry (NAICS 71) Annual Log GDP per Capita -------
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

## 1. Filter the data to focus on the Arts, Entertainment, and Recreation industry (NAICS 71) ---------

# See line 151

## 2. Transform the data to long format and calculate average log GDP per capita grouped by eras_tour_host and year -----

industry_summary <- working_data_71 |>
  pivot_longer(cols = all_of(year_columns), names_to = "year", values_to = "log_gdp_pc") |> # variable not yet as log
  select(1:3,21,22,4:20)|>
  mutate(log_gdp_pc = log(1+log_gdp_pc))|>                                      # log transformation
  group_by(year, eras_tour_host) %>%
  summarize(avg_log_gdp_per_capita = mean(log_gdp_pc, na.rm = TRUE), .groups = "drop")

## 3. Convert Year to a numeric variable for proper x-axis ordering -----

industry_summary <- industry_summary %>%
  mutate(year = as.integer(gsub("^x", "", as.character(year))),# I must remove x from the variable, and then turn into numeric.
         eras_tour_host = as.character(eras_tour_host))                 


## 4. Create the plot ------

colors <- c("1" = "#2e67be", "0" = "#E58724")                                   # set the color for each group


naics_71_log_gdp_plot <- ggplot(industry_summary, aes(x = year, y = avg_log_gdp_per_capita, group = eras_tour_host,
                                                      colour = eras_tour_host)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = colors,
                     labels = c("0" = "Did not host the Eras Tour",
                                "1" = "Hosted the Eras Tour"))+                   # label each group legend
  labs(# title = TITLE IS DISPLAYED ON THE WRITEUP,
    x = "Year",
    y = "Avg. Log GDP per capita") +
  th

# Display the plot
print(naics_71_log_gdp_plot)

# It looks like the counties that hosted the Eras Tour increased their GDP at
# a higher rate than the ones who did not host the tour. The counties present
# pre-existing differences.

## 5. Save the plot as an image file -----

ggsave("2. output/figures/naics_71_log_gdp_plot.png", plot = naics_71_log_gdp_plot, width = 8, height = 6) # save plot

# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Section 3.6 - Visualizing the Data: Demographic Differences Across Counties that Hosted the Eras Tour and Those That Did Not ----
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# 1. Install and Load Necessary Packages (Can be done at top of script)

## 2. Prepare population characteristics for visualization -------
regression_data <- working_data %>%
  mutate(less_than_HS_share = (less_than_a_high_school_diploma_2018_22/pop_estimate_2023) * 100,
         hs_only_share = (high_school_diploma_only_2018_22/pop_estimate_2023) * 100,
         some_college_share = (some_college_or_associates_degree_2018_22/pop_estimate_2023) * 100,
         bachelors_or_higher_share = (bachelors_degree_or_higher_2018_22/pop_estimate_2023)*100)
    

## 3. Summarize Data by Treatment Group ------

# Summarize data by eras_tour_host
summary_with_ci <- regression_data %>%
  group_by(eras_tour_host) %>%
  summarize(
    less_than_hs = mean(less_than_HS_share, na.rm = TRUE),
    less_than_hs_ci = ifelse(eras_tour_host == 1, 1.96 * sd(less_than_HS_share, na.rm = TRUE) / sqrt(n()), NA),
    hs_only = mean(hs_only_share, na.rm = TRUE),
    hs_only_ci = ifelse(eras_tour_host == 1, 1.96 * sd(hs_only_share, na.rm = TRUE) / sqrt(n()), NA),
    some_college = mean(some_college_share, na.rm = TRUE),
    some_college_ci = ifelse(eras_tour_host == 1, 1.96 * sd(some_college_share, na.rm = TRUE) / sqrt(n()), NA),
    bachelors_higher = mean(bachelors_or_higher_share, na.rm = TRUE),
    bachelors_higher_ci = ifelse(eras_tour_host == 1, 1.96 * sd(bachelors_or_higher_share, na.rm = TRUE) / sqrt(n()), NA),
    poverty = mean(pctpovall_2021, na.rm = TRUE),
    poverty_ci = ifelse(eras_tour_host == 1, 1.96 * sd(pctpovall_2021, na.rm = TRUE) / sqrt(n()), NA),
    net_migration = mean(net_mig_2023, na.rm = TRUE),
    net_migration_ci = ifelse(eras_tour_host == 1, 1.96 * sd(net_mig_2023, na.rm = TRUE) / sqrt(n()), NA),
    .groups = "drop"
  )

# Function to create bar plots with 95% CI
plot_bar_with_ci <- function(variable, ci_variable, title, y_label) {
  
  plot_data <- summary_with_ci %>%
    filter(!is.na(.data[[variable]]))
  
  ggplot(plot_data, aes(x = as.factor(eras_tour_host), y = .data[[variable]], fill = as.factor(eras_tour_host))) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    geom_errorbar(
      aes(ymin = .data[[variable]] - .data[[ci_variable]],
          ymax = .data[[variable]] + .data[[ci_variable]]),
      width = 0.2,
      data = filter(plot_data, eras_tour_host == 1)
    ) +
    scale_fill_manual(values = colors,
                       labels = c("0" = "No",
                                  "1" = "Yes"))+   
    labs(title = title, x = "Eras Tour Host", y = y_label,
         fill = "Eras Tour Host") +
    th
}

# Generate plots for each characteristic
plot1 <- plot_bar_with_ci("less_than_hs", "less_than_hs_ci",
                          "Less than High School Diploma", "Percentage (%)")
plot2 <- plot_bar_with_ci("hs_only", "hs_only_ci", "High School Diploma Only", "Percentage (%)")
plot3 <- plot_bar_with_ci("some_college", "some_college_ci",
                          "Some College or Associate Degree", "Percentage (%)")
plot4 <- plot_bar_with_ci("bachelors_higher", "bachelors_higher_ci",
                          "Bachelor’s Degree or Higher", "Percentage (%)")
plot5 <- plot_bar_with_ci("poverty", "poverty_ci", "Poverty Rate (2021)", "Percentage (%)")
plot6 <- plot_bar_with_ci("net_migration", "net_migration_ci", "Net Migration (2023)", "Net Migration Rate")



## 4. Generate the save the bar plots in a panel --------

final_panel <- (plot1 | plot2) / (plot3 | plot4) / (plot5 | plot6)

print(final_panel)

# Save the final plot as an image file
ggsave("2. output/figures/demographics_comparison_panel.png", plot = final_panel, width = 12, height = 10, dpi = 300)


# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Section 3.7.1 - Regression Analysis: Industries Most Likely to Be Impacted (NAICS 71) -----
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

## 1. Filter the dataset to only include NAICS 71 -----

# Done below

## 2. Prepare population characteristics and relevant variables -----

regression_data_71 <- regression_data |>
  filter(industry_classification == 71)|>
  mutate(less_than_HS_share = (less_than_a_high_school_diploma_2018_22/pop_estimate_2023),
         hs_only_share = (high_school_diploma_only_2018_22/pop_estimate_2023),
         some_college_share = (some_college_or_associates_degree_2018_22/pop_estimate_2023),
         bachelors_or_higher_share = (bachelors_degree_or_higher_2018_22/pop_estimate_2023),
         #net_mig_rate = (net_mig_2023/pop_estimate_2023),
         log_gdp_pc = log(1+x2023))|>
  select(1,48,29,32,37,43:47)

## 3. Define regression models with progressively added controls ---------

# Model 1: Log GDP Per Capita (2023) regressed on Eras Tour Host only

model_1 <- lm(log_gdp_pc ~ eras_tour_host, data = regression_data_71)

print(summary(model_1))

# Model 2: Add educational attainment controls

model_2 <- lm(log_gdp_pc ~ eras_tour_host + less_than_HS_share + hs_only_share +
                some_college_share + bachelors_or_higher_share, data = regression_data_71)

print(summary(model_2))

# Model 3: Add poverty control

model_3 <- lm(log_gdp_pc ~ eras_tour_host + less_than_HS_share + hs_only_share +
                some_college_share + bachelors_or_higher_share + pctpovall_2021, data = regression_data_71)

print(summary(model_3))

# Model 4: Add net migration control

model_4 <- lm(log_gdp_pc ~ eras_tour_host + less_than_HS_share + hs_only_share +
                some_college_share + bachelors_or_higher_share + pctpovall_2021 +
                net_mig_2023, data = regression_data_71)

print(summary(model_4))

# Model 5: Add GDP in 2022 as an additional control

model_5 <- lm(log_gdp_pc ~ eras_tour_host + less_than_HS_share + hs_only_share +
                some_college_share + bachelors_or_higher_share + pctpovall_2021 +
                net_mig_2023 + log(1+x2022), data = regression_data_71)

print(summary(model_5))

# 4. Generate the regression table with stargazer

all_models <- list(model_1,model_2,model_3,model_4,model_5)

stargazer(
  all_models,
  type = "latex",
  #title = TITLE DISPLAYED ON THE WRITEUP,
  dep.var.labels = "Log GDP Per Capita (2023)",
  covariate.labels = c(
    "Eras Tour Host",
    "Share with Less than High School",
    "Share with High School Only",
    "Share with Some College",
    "Share with Bachelor's Degree or Higher",
    "Poverty Rate (2021)",
    "Net Migration (2023)",
    "Log GDP Per Capita (2022)"
  ),
  #column.labels = "Arts, Entertainment, and Recreation",
  align = TRUE,
  no.space = TRUE,
  digits = 3,
  out = "2. output/tables/regression_results_71.tex"
)

# -------------------------------------------------------------------------------------
# Section 3.7.2 - Regression Analysis: Industries Most Likely to Be Impacted (NAICS 72) ----
# -------------------------------------------------------------------------------------

## 1. Filter the dataset to only include NAICS 72 -----

# Done below.

## 2. Prepare population characteristics and relevant variables -----

regression_data_72 <- regression_data |>
  filter(industry_classification == 72)|>
  mutate(less_than_HS_share = (less_than_a_high_school_diploma_2018_22/pop_estimate_2023),
         hs_only_share = (high_school_diploma_only_2018_22/pop_estimate_2023),
         some_college_share = (some_college_or_associates_degree_2018_22/pop_estimate_2023),
         bachelors_or_higher_share = (bachelors_degree_or_higher_2018_22/pop_estimate_2023),
         #net_mig_rate = (net_mig_2023/pop_estimate_2023),
         log_gdp_pc = log(1+x2023))|>
  select(1,48,29,32,37,43:47)

## 3. Define regression models with progressively added controls ---------

# Model 1: Log GDP Per Capita (2023) regressed on Eras Tour Host only

model_1 <- lm(log_gdp_pc ~ eras_tour_host, data = regression_data_72)

print(summary(model_1))

# Model 2: Add educational attainment controls

model_2 <- lm(log_gdp_pc ~ eras_tour_host + less_than_HS_share + hs_only_share +
                some_college_share + bachelors_or_higher_share, data = regression_data_72)

print(summary(model_2))

# Model 3: Add poverty control

model_3 <- lm(log_gdp_pc ~ eras_tour_host + less_than_HS_share + hs_only_share +
                some_college_share + bachelors_or_higher_share + pctpovall_2021, data = regression_data_72)

print(summary(model_3))

# Model 4: Add net migration control

model_4 <- lm(log_gdp_pc ~ eras_tour_host + less_than_HS_share + hs_only_share +
                some_college_share + bachelors_or_higher_share + pctpovall_2021 +
                net_mig_2023, data = regression_data_72)

print(summary(model_4))

# Model 5: Add GDP in 2022 as an additional control

model_5 <- lm(log_gdp_pc ~ eras_tour_host + less_than_HS_share + hs_only_share +
                some_college_share + bachelors_or_higher_share + pctpovall_2021 +
                net_mig_2023 + log(1+x2022), data = regression_data_72)

print(summary(model_5))

# 4. Generate the regression table with stargazer

all_models <- list(model_1,model_2,model_3,model_4,model_5)

stargazer(
  all_models,
  type = "latex",
  #title = "Regression Results: Effects of the Eras Tour on GDP (2023)",
  dep.var.labels = "Log GDP Per Capita (2023)",
  covariate.labels = c(
    "Eras Tour Host",
    "Share with Less than High School",
    "Share with High School Only",
    "Share with Some College",
    "Share with Bachelor's Degree or Higher",
    "Poverty Rate (2021)",
    "Net Migration (2023)",
    "Log GDP Per Capita (2022)"
  ),
  #column.labels = "Arts, Entertainment, and Recreation",
  align = TRUE,
  no.space = TRUE,
  digits = 3,
  out = "2. output/tables/regression_data_72.tex"
)

# -------------------------------------------------------------------------------------------
# Section 3.7.3 - Regression Analysis: Industries Moderately Likely to Be Impacted (NAICS 54) ------
# -------------------------------------------------------------------------------------------

## 1. Filter the dataset to only include NAICS 54 -------

# Done below.

## 2. Prepare population characteristics and relevant variables -----

regression_data_54 <- regression_data |>
  filter(industry_classification == 54)|>
  mutate(less_than_HS_share = (less_than_a_high_school_diploma_2018_22/pop_estimate_2023),
         hs_only_share = (high_school_diploma_only_2018_22/pop_estimate_2023),
         some_college_share = (some_college_or_associates_degree_2018_22/pop_estimate_2023),
         bachelors_or_higher_share = (bachelors_degree_or_higher_2018_22/pop_estimate_2023),
         #net_mig_rate = (net_mig_2023/pop_estimate_2023),
         log_gdp_pc = log(1+x2023))|>
  select(1,48,29,32,37,43:47)

## 3. Define regression models with progressively added controls ---------

# Model 1: Log GDP Per Capita (2023) regressed on Eras Tour Host only

model_1 <- lm(log_gdp_pc ~ eras_tour_host, data = regression_data_54)

print(summary(model_1))

# Model 2: Add educational attainment controls

model_2 <- lm(log_gdp_pc ~ eras_tour_host + less_than_HS_share + hs_only_share +
                some_college_share + bachelors_or_higher_share, data = regression_data_54)

print(summary(model_2))

# Model 3: Add poverty control

model_3 <- lm(log_gdp_pc ~ eras_tour_host + less_than_HS_share + hs_only_share +
                some_college_share + bachelors_or_higher_share + pctpovall_2021, data = regression_data_54)

print(summary(model_3))

# Model 4: Add net migration control

model_4 <- lm(log_gdp_pc ~ eras_tour_host + less_than_HS_share + hs_only_share +
                some_college_share + bachelors_or_higher_share + pctpovall_2021 +
                net_mig_2023, data = regression_data_54)

print(summary(model_4))

# Model 5: Add GDP in 2022 as an additional control

model_5 <- lm(log_gdp_pc ~ eras_tour_host + less_than_HS_share + hs_only_share +
                some_college_share + bachelors_or_higher_share + pctpovall_2021 +
                net_mig_2023 + log(1+x2022), data = regression_data_54)

print(summary(model_5))

# 4. Generate the regression table with stargazer

all_models <- list(model_1,model_2,model_3,model_4,model_5)

stargazer(
  all_models,
  type = "latex",
  #title = "Regression Results: Effects of the Eras Tour on GDP (2023)",
  dep.var.labels = "Log GDP Per Capita (2023)",
  covariate.labels = c(
    "Eras Tour Host",
    "Share with Less than High School",
    "Share with High School Only",
    "Share with Some College",
    "Share with Bachelor's Degree or Higher",
    "Poverty Rate (2021)",
    "Net Migration (2023)",
    "Log GDP Per Capita (2022)"
  ),
  #column.labels = "Arts, Entertainment, and Recreation",
  align = TRUE,
  no.space = TRUE,
  digits = 3,
  out = "2. output/tables/regression_data_54.tex"
)



# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Section 3.7.4 - Regression Analysis: Industries Least Likely to Be Impacted (NAICS 11) -------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

## 1. Filter the dataset to only include NAICS 11 -----

# Done below.

## 2. Prepare population characteristics and relevant variables -----

regression_data_11 <- regression_data |>
  filter(industry_classification == 11)|>
  mutate(less_than_HS_share = (less_than_a_high_school_diploma_2018_22/pop_estimate_2023),
         hs_only_share = (high_school_diploma_only_2018_22/pop_estimate_2023),
         some_college_share = (some_college_or_associates_degree_2018_22/pop_estimate_2023),
         bachelors_or_higher_share = (bachelors_degree_or_higher_2018_22/pop_estimate_2023),
         #net_mig_rate = (net_mig_2023/pop_estimate_2023),
         log_gdp_pc = log(1+x2023))|>
  select(1,48,29,32,37,43:47)

## 3. Define regression models with progressively added controls ---------

# Model 1: Log GDP Per Capita (2023) regressed on Eras Tour Host only

model_1 <- lm(log_gdp_pc ~ eras_tour_host, data = regression_data_11)

print(summary(model_1))

# Model 2: Add educational attainment controls

model_2 <- lm(log_gdp_pc ~ eras_tour_host + less_than_HS_share + hs_only_share +
                some_college_share + bachelors_or_higher_share, data = regression_data_11)

print(summary(model_2))

# Model 3: Add poverty control

model_3 <- lm(log_gdp_pc ~ eras_tour_host + less_than_HS_share + hs_only_share +
                some_college_share + bachelors_or_higher_share + pctpovall_2021, data = regression_data_11)

print(summary(model_3))

# Model 4: Add net migration control

model_4 <- lm(log_gdp_pc ~ eras_tour_host + less_than_HS_share + hs_only_share +
                some_college_share + bachelors_or_higher_share + pctpovall_2021 +
                net_mig_2023, data = regression_data_11)

print(summary(model_4))

# Model 5: Add GDP in 2022 as an additional control

model_5 <- lm(log_gdp_pc ~ eras_tour_host + less_than_HS_share + hs_only_share +
                some_college_share + bachelors_or_higher_share + pctpovall_2021 +
                net_mig_2023 + log(1+x2022), data = regression_data_11)

print(summary(model_5))

# 4. Generate the regression table with stargazer

all_models <- list(model_1,model_2,model_3,model_4,model_5)

stargazer(
  all_models,
  type = "latex",
  #title = "Regression Results: Effects of the Eras Tour on GDP (2023)",
  dep.var.labels = "Log GDP Per Capita (2023)",
  covariate.labels = c(
    "Eras Tour Host",
    "Share with Less than High School",
    "Share with High School Only",
    "Share with Some College",
    "Share with Bachelor's Degree or Higher",
    "Poverty Rate (2021)",
    "Net Migration (2023)",
    "Log GDP Per Capita (2022)"
  ),
  #column.labels = "Arts, Entertainment, and Recreation",
  align = TRUE,
  no.space = TRUE,
  digits = 3,
  out = "2. output/tables/regression_data_11.tex"
)