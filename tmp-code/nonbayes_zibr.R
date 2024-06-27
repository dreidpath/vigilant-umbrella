######################################################
# READ, PROCESS AND MODEL THE DATA USING A BAYESIAN  #
# ZERO-INFLATED BETA REGRESSION                      #
######################################################

########## Load the data
source("./code/get-data.R")


########## Load relevant libraries
library(glmmTMB)  # Non-Bayesian package for zibr models


# Create a centered year around the start of the GFF partnership date.
gffDF$year_cpd <-  gffDF$project_approval_fy - gffDF$gff_partnership_date 



# Fit a zero-inflated beta regression model
model1 <- glmmTMB(ida_prop ~ year_cpd + treatment_flag + year_c:treatment_flag + log(ida_commit) + (1 | country), 
                 ziformula = ~ year_c + treatment_flag + year_c:treatment_flag + log(ida_commit) + (1 | country),
                 data = gffDF[gffDF$gff_partner == 1,], 
                 family = beta_family(link = "logit"))

model2 <- glmmTMB(ida_prop ~ year_c + treatment_flag + log(ida_commit) + (1 | country), 
                  ziformula = ~ year_c + treatment_flag + log(ida_commit) + (1 | country),
                  data = gffDF[gffDF$gff_partner == 1,], 
                  family = beta_family(link = "logit"))




# Summary of the model
summary(model)


model <- glmmTMB(
  ida_prop ~ treatment_flag * year_c +  log(ida_commit) + (1 | country), 
  ziformula = ~ treatment_flag * year_c + log(ida_commit) + (1 | country),
  data = gffDF[gffDF$gff_partner == 1,], 
  family = beta_family(link = "logit")
)

model1 <- glmmTMB(
  ida_prop ~ treatment_flag + year_c +  log(ida_commit) + (1 | country), 
  ziformula = ~ treatment_flag + year_c + log(ida_commit) + (1 | country),
  data = gffDF[gffDF$gff_partner == 1,], 
  family = beta_family(link = "logit")
)


anova(model1, model)  # Model 1 is preferred

