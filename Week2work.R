install.packages('sjPlot')
install.packages('car')
library(sjPlot)
library(car)

##Ordinary linear regression versus logistic regression

## generate data
set.seed(1)
x <- rnorm(200,5,4)        ## sample x from a normal distribution
mu <- 3 + 2*x                ## linear prediction
y <- rnorm(200, mu, 3)       ## generate y from prediction with a normal error distribution
## fit linear model 
m <- lm(y~x)
## plot predicted value
plot_model(m, type='pred', show.data=T, ci.lvl = NA)

## plot diagnostics
plot_model(m, type="diag")

##Fitting a linear model on binary data

## generate data
set.seed=1
x = rnorm(200, 5, 4)              ## sample x from a normal distribution
mu = -1 + 0.4*x                   ## linear predictor
prob = 1 / (1 + exp(-mu))         ## transform linear predictor with inverse of "logit" link
y = rbinom(200, 1, prob = prob)   ## sample y from probabilities with binomial error distribution

## fit linear model and plot
m_lm = lm(y ~ x)
plot_model(m_lm, type='pred', show.data=T, ci.lvl=NA)

library(ggfortify)
autoplot(m_lm)

## Fitting a logistic regression model on binary data

m_glm <- glm(y ~ x, family = binomial(link = 'logit'))
plot_model(m_glm, type='pred', show.data=T, terms='x [all]')

##Fitting and interpreting a GLM

set.seed(1)
## sample independent variables
d = data.frame(hours_studied = rpois(n=1000, lambda=12),
               selftest = rbinom(n=1000, size=1, prob=0.25),
               alcohol = rpois(n=1000, lambda=10))
## linear prediction 
mu = -1.5 + 0.4*d$hours_studied + 1.2*d$selftest + -0.2*d$alcohol
## transform with inverse of "logit" link
prob = 1 / (1 + exp(-mu))
## generate x from prediction with binomial error distribution
d$passed_test = y = rbinom(1000, 1, prob = prob)
head(d)

m_glm2 = glm(passed_test ~ hours_studied + selftest + alcohol, 
             data=d, family=binomial)

tab_model(m_glm2)
