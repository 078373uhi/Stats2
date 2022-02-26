# Load required packages
library(lme4)
library(sjPlot)
library(tidyverse)
library(glmmTMB)

# read in and check data
patients <- read.csv("~/Stats2/ST1.csv")

view(patients)
summary(patients)

# It is my intention to investigate if any of the patient variables (hospital, 
# sex, age, duration of stay) are linked to patient deaths.

# plots to investigate data
locsex <- ggplot(patients, aes(x = loc, fill = sex)) +
  geom_bar() +
  labs(y= "Number of patients", x = "Location") +
  ggtitle("Number of patients by hospital location")

locsex

dur <- ggplot(patients, aes(x = dur)) +
  geom_bar() +
  labs(y= "Number of patients", x = "Duration of stay") +
  ggtitle("Number of patients by duration of hospital stay")

dur

age <- ggplot(patients, aes(x = age)) +
  geom_bar() +
  labs(y= "Number of patients", x = "Patient age") +
  ggtitle("Number of patients by age")

age

ageloc <- ggplot(patients, aes(x = age, fill = loc)) +
  geom_bar() +
  labs(y= "Number of patients", x = "Patient age") +
  ggtitle("Number of patients by age and hospital location") +
  labs(fill = "Hospital location")

ageloc # I wondered if any of the hospitals were specialised, for example
# a paediatric hospital or one for elderly patients, which would show patients  
# of a particular age range but this does not seem to be the case.

# Analysis of plots: The plots and data show that both sex and death data are binary 
# variables.  Furthermore the duration is seen to be not normally distributed 
# as there are high numbers of short stays and few long stays in hospital.  
# It can be seen that patients are almost equally split between male and female.
# In terms of location there are an equal amount of patients from each hospital 
# (30) except one (Edi) which had double the number of patients than the rest.

# Since the data is both binary and not normally distributed ordinary linear 
# regression is not appropriate in this case.  Logistic regression will be used
# due to the binary response variable and independence of observations (no 
# patient appears twice on the list).

# Generalised Linear Regression
#convert yes/no to 0/1 in death column to enable models
patients$death<-ifelse(patients$death=="yes",1,0)

# create model 0 - deaths
m_glm0 = glm(death ~ 1, 
             data=patients, family=binomial)
summary(m_glm0)

#create model 1 death + age
m_glm1 = glm(death ~ age, 
             data=patients, family=binomial)
summary(m_glm1)

# create model 2 death + age/sex
m_glm2 = glm(death ~ age + sex, 
             data=patients, family=binomial)
summary(m_glm2)

# create model 3 death + age/sex/dur
m_glm3 = glm(death ~ age + sex + dur, 
             data=patients, family=binomial)
summary(m_glm3)

# create model 4 death + age/sex/dur/loc
m_glm4 = glm(death ~ age + sex + dur + loc, 
             data=patients, family=binomial)
summary(m_glm4)

# plot model 4 (all variables) and create table to visualise
plot_model(m_glm4, type='pred', grid = T)

tab_model(m_glm0, m_glm1, m_glm2, m_glm3, m_glm4)

# compare models with Chi2 test
anova(m_glm0, m_glm1, m_glm2, m_glm3, m_glm4, test = "Chisq")

# The residual deviance decreases with each model and model 4 (age, sex, 
# duration of stay and location) has the lowest residual deviance (262.86) so
# can be seen to have the best fit.  However, the p-value of model 4, 0.8627, is 
# not a significantly significant decrease on the previous model (model 3).  
# Model 3 has a p-value of 0.0054 which is a significantly significant decrease  
# on the previous model (model 2) and is therefore an improvement.

#convert to percentage
100*(exp(coef(m_glm3))-1)

# Conclusion
# Increased age has a  small impact on patient deaths while females were more likely 
# to die than males. The biggest impact on death of a patient was length of stay.
