# Load required packages
library(lme4)
library(sjPlot)
library(tidyverse)
library(glmmTMB)

# read in and check data
patients <- read.csv("~/Stats2/ST1.csv")

view(patients)

# It is my intention to investigate if any of the patient variables (hospital, 
# sex, age, duration of stay) are linked to whether the patient survives or dies.

# plots
locsex <- ggplot(patients, aes(x = loc, fill = sex)) +
  geom_bar()

locsex

dur <- ggplot(patients, aes(x = dur)) +
  geom_bar()

dur

age <- ggplot(patients, aes(x = age)) +
  geom_bar()

age

ageloc <- ggplot(patients, aes(x = age, fill = loc)) +
  geom_bar() 

ageloc # I wondered if any of the hospital were specialised, for example
# a paediatric hospital or one for elderly patients, which would show patients  
# of a particular age range but this does not seem to be the case.

# Analysis: The plots and data show that both sex and death data are binary 
# variables.  Furthermore the duration is seen to be not normally distributed 
# as there are high numbers of short stays and few long stays in hospital.  
# It can be seen that patients are almost equally split between male and female.
# In terms of location there are an equal amount of patients from each hospital 
# (30) except one (Edi) which had double the number of patients than the rest.

# Since the data is both binary and not normally distributed multiple linear 
# regression is not appropriate in this case.  Logistic regression will be used
# due to the binary response variable and independence of observations (no 
# patient appears twice on the list).

# Regression
#convert yes/no to 0/1 in death column
patients$death<-ifelse(patients$death=="yes",1,0)

#create model
m_glm = glm(death ~ age + sex + dur, 
             data=patients, family=binomial)

tab_model(m_glm)

plot_model(m_glm, type='pred', grid = T)
