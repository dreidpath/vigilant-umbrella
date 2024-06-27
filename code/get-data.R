########## Import and wrangle the data

########## Load relevant libraries
library(tidyverse) # For data wrangling
library(readxl)  # Read data from excel spreadsheets


# Import the data
gffDF <- read_excel("./data/GFF and IDA to RMNCAH-N - Summarized data for FY2011-2023.xlsx", sheet = "Sum data")
# Create a flag marking the year of first "treatment" for Partner countries 
gffDF$treatment_flag <- ifelse(gffDF$gff_partnership_date <= gffDF$project_approval_fy & gffDF$gff_partner == 1, 1, 0)  # Treatment flag

# Figure out which year first "treatment" for Partner countries occurred and save it to a temporary dataframe
tmpDF <- gffDF %>%
  filter(treatment_flag == 1) %>%
  select(country, project_approval_fy) %>%
  group_by(country) %>%
  mutate(treatment_fy = min(project_approval_fy)) %>% 
  select(country, treatment_fy) %>%
  unique() 

# Add a "treatment_fy" variable (year of first treatment) to every row of the Partner countries
# and save it back into gffDF
gffDF <- gffDF %>%
  left_join(tmpDF, by = "country")
# Delete the temporary dataframe
rm(tmpDF)
# For Eligible countries, make 2016 the year of first treatment
gffDF$treatment_fy[gffDF$gff_partner==0] <- 2016
# Center the year on 2016
gffDF$year_c <- gffDF$project_approval_fy - 2016

# Calculate the proportion of RMNCHA-IDA
gffDF$ida_prop <- gffDF$rmnch_total_ida/gffDF$ida_commit  # Calculate the proportion
# Beta-regression can't have 1's make them 0.999
gffDF$ida_prop[gffDF$ida_prop==1] <- 0.999  
# Create a factor for the country random effect
gffDF$country <- as.factor(gffDF$country)  