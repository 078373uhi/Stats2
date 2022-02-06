install.packages('sjPlot')
install.packages('car')
library(sjPlot)
library(car)

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

## generate data
set.seed=1
x = rnorm(200, 5, 4)              ## sample x from a normal distribution
mu = -1 + 0.4*x                   ## linear predictor
prob = 1 / (1 + exp(-mu))         ## transform linear predictor with inverse of "logit" link
y = rbinom(200, 1, prob = prob)   ## sample y from probabilities with binomial error distribution

## fit linear model and plot
m_lm = lm(y ~ x)
plot_model(m_lm, type='pred', show.data=T, ci.lvl=NA)
