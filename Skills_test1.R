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

#convert yes/no to 0/1 in death column
patients$death<-ifelse(patients$death=="yes",1,0)

#create model
m_glm = glm(death ~ loc + age + sex + dur, 
             data=patients, family=binomial)

tab_model(m_glm)

plot_model(m_glm, type='pred', grid = T)
