# Load required packages
library(lme4)
library(sjPlot)
library(tidyverse)
library(glmmTMB)

# read in and check data
patients <- read.csv("~/Stats2/ST1.csv")

view(patients)

# It is my intention to investigate if any of the patient variables (hospital, 
# sex, age, duration of stay) are linked to whether the patient dies.

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

# Analysis of data: The plots and data show that both sex and death data are binary 
# variables.  Furthermore the duration is seen to be not normally distributed 
# as there are high numbers of short stays and few long stays in hospital.  
# It can be seen that patients are almost equally split between male and female.
# In terms of location there are an equal amount of patients from each hospital 
# (30) except one (Edi) which had double the number of patients than the rest.

# Since the data is both binary and not normally distributed ordinary linear 
# regression is not appropriate in this case.  Logistic regression will be used
# due to the binary response variable and independence of observations (no 
# patient appears twice on the list).

# Regression
#convert yes/no to 0/1 in death column
patients$death<-ifelse(patients$death=="yes",1,0)

# create model 0
m_glm0 = glm(death ~ 1, 
             data=patients, family=binomial)

#create model 1 + age
m_glm1 = glm(death ~ age, 
             data=patients, family=binomial)

# create model 2 + age and sex
m_glm2 = glm(death ~ age + sex, 
             data=patients, family=binomial)

# create model 3 + age, sex, dur
m_glm3 = glm(death ~ age + sex + dur, 
             data=patients, family=binomial)

plot_model(m_glm3, type='pred', grid = T)

tab_model(m_glm0, m_glm1, m_glm2, m_glm3)

anova(m_glm0, m_glm1, m_glm2, m_glm3, test = "Chisq")

# Model 3 (age, sex and duration of stay) has the lowest residual deviance so 
# can be seen to have the best fit.  The p-value of 0.0054 shows that this is a 
# significantly significant decrease on the previous model and is therefore an 
# improvement.

# look at this part again because this is not a good result
pred = m_glm3$fitted.values
y = patients$death
pred_0 = mean(pred[y == 0])
pred_1 = mean(pred[y == 1])
pred_1 - pred_0

#location
m_glmL = glm(death ~ loc, 
             data=patients, family=binomial)

tab_model(m_glmL)

plot_model(m_glmL, type='pred')

anova(m_glmL, test = "Chisq")

