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

## When to use logistic regression

plot_model(m_glm2, type='pred', grid = T)

## Interpreting coefficients: odds ratios and log odds ratios

log(1.49)
exp(0.3987761)

prob_to_odds <- function(p) p / (1 - p)
odds_to_prob <- function(o) o / (1 + o)
odds <- prob_to_odds(0.6)
odds_with_test <- odds * 3.43
odds_to_prob(odds_with_test)

odds <- 0.1939 * 1.4904^10 * 3.4315^1 * 0.8445^7
odds / (1 + odds)

newdata <- data.frame(hours_studied = 10, selftest = 1, alcohol = 7)
predict(m_glm2, newdata=newdata, type = 'response')

## Comparing models
m_0 = glm(passed_test ~ 1, data=d, family= binomial)
m_1 = glm(passed_test ~ hours_studied + selftest, data=d, family= binomial)
m_2 = glm(passed_test ~ hours_studied + selftest + alcohol, data=d, family=binomial)

tab_model(m_0, m_1, m_2)
anova(m_0, m_1, m_2, test = "Chisq")

pchisq(38.253, df=1, lower.tail=F)












