setwd("~/ownCloud/SparkStreet Work/GFF Methods Paper")

# Install packages
library(tidyverse)
library(quantreg)

# Import the data
gffDF <- readxl::read_excel("GFF and IDA to RMNCAH-N - Summarized data for FY2011-2023.xlsx", sheet = "Sum data")
# Create a flag marking the year of first "treatment" for Partner countries 
gffDF$treatment_flag <- ifelse(gffDF$gff_partnership_date <= gffDF$project_approval_fy & gffDF$gff_partner == 1, 1, 0)  # Treatment flag

# Figure out which year first "treatment" for Partner countries occured and save it to a temporary dataframe
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

# Now start looking at total ida and rmnch_ida before and after "treatment" (ida_prop)
# Calculate within country RMNCH proportion before treatment (bt)
gff_bt <- gffDF %>%
  filter(project_approval_fy < treatment_fy) %>%
  group_by(country) %>%
  summarize(
    treatment = 0,
    gff_partner = min(gff_partner),
    sum_rmnch = sum(rmnch_total_ida),
    sum_ida = sum(ida_commit),
    ida_prop = sum_rmnch/sum_ida
  )

# Calculate within country RMNCH proportion after treatment (at)
gff_at <- gffDF %>%
  filter(project_approval_fy >= treatment_fy) %>%
  group_by(country) %>%
  summarize(
    treatment = 1,
    gff_partner = min(gff_partner),
    sum_rmnch = sum(rmnch_total_ida),
    sum_ida = sum(ida_commit),
    ida_prop = sum_rmnch/sum_ida
  )

# save the before and after data in a new dataframe
dDF <- rbind(gff_bt, gff_at) %>%
  mutate(gff_partner = factor(gff_partner, labels = c("Eligible", "Partner"))) 

# Calculate median values for plotting
median_dDF <- dDF %>%
group_by(treatment, gff_partner) %>%
  summarise(med_ida_prop = median(ida_prop, na.rm=T)) 


# # Create the boxplot of the within country ida_prop by gff_partner and treatment
dDF %>%
  ggplot(aes(x = as.factor(treatment), y = ida_prop, fill = as.factor(treatment))) +
  # Create the boxplot
  geom_boxplot(position=position_dodge(), width = 0.5) +
  # Add a line between the median values of pre- and post-treatment boxes
  geom_line(data = median_dDF, aes(x = treatment + 1, y = med_ida_prop, group = gff_partner), lwd = 1.1) +
  # Add a red dot on the median values of each boxplot 
  geom_point(data = median_dDF, aes(x = treatment + 1, y = med_ida_prop, group = gff_partner), size = 3, color = "red") +
  theme_bw() +
  labs(fill = "Period") +
  scale_fill_manual(
    values = c("orange", "purple"),
    guide = "none"
  ) + 
  scale_x_discrete(labels = c("Pre-GFF", "Post-GFF")) +
  xlab("Treatment Period") +
  ylab("Proportion RMNCHA-N")  +
  facet_grid(~ as.factor(gff_partner))
  
# Calculate the median prop_ida for the partner group pre- and post-treatment
dDF %>%
  filter(gff_partner == "Partner") %>%
  group_by(treatment) %>%
  summarise(median_propida = median(ida_prop, na.rm = T))

# Test the difference between the median values of ida_prop pre-post in Partners
# Create two vectors for the two groups
group_0 <- dDF$ida_prop[dDF$treatment == 0]
group_1 <- dDF$ida_prop[dDF$treatment == 1]

# Perform the Wilcoxon rank-sum test
wilcox.test(group_0, group_1)

