library(readr)
library(tidyverse)
library(usmap)
library(lme4)
library(GGally)
library(sjPlot)
library(patchwork)
library(gridExtra)



books <- read_csv("~/Stats2/CS2.csv")

glimpse(books)

# what combination of book features are most likely to result in a successful
# challenge?

books %>%
  count(removed == 1) 
# Out of 800 books 190 were removed (successfully challenged)

ggcorr(books)
# We can see there is a strong correlation between Obama being president and the
# days after 2000 and also between income and college graduates which makes sense.


# STATE
# Plot by state
books %>%
  filter(removed == 1) %>%
  group_by(state) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = reorder(state,(-count)), y = count)) +
  geom_col() +
  labs(title = "Number of Removed Books per State", x = "State", 
       y = "Number of Books")


# POLITICAL LEANING
# add in a 'leaning' column
books <- books %>% 
  mutate(leaning = ifelse(pvi2 == 0, "neutral", ifelse(pvi2 < 0, "Republican", 
                                                      "Democratic")))

# Despite there being more challenges overall in Democratic states, more books
# were removed in Republican States
books %>%
  ggplot(aes(x = leaning, fill = as.factor(removed))) +
  geom_bar() +
  labs(title = "Book Challenges by Political Leaning of States", 
       x = "Political Leaning", y = "Number of Books", fill = "") +
  scale_fill_discrete(labels = c("Not Removed", "Removed"), )


# STATES
# Plot challenges on a map to see any obvious trends North/South etc
states1 <- books %>%
  count(state)

# Total Challenges - Pennsylvania and Oregon have the most
plot_usmap(data = states1, values = "n", labels = TRUE) + 
  labs(title = "Total Book Challenges by State", fill = "No of Challenges") +
  theme(legend.position = "right")

# % of Successful challenges - New Mexico, Mississippi and Idaho have the highest
states2 <- data.frame(unclass(prop.table(table(books$state, books$removed), 
                                         margin = 1)))
states2$state <- row.names(states2)

plot_usmap(data = states2,values = "X1", labels = TRUE) + 
  labs(title = "Percentage of Challenged Books Removed", fill = "Books Removed") +
  theme(legend.position = "right") +
  scale_fill_gradient(breaks = 0.25*0:4, labels = percent(0.25*0:4))

# Total removed books - Virgina, Idaho, Illinois, Pennsyvania and California have most
states3 <- books %>%
  filter(removed ==1) %>%
  count(state)

plot_usmap(data = states3,values = "n", labels = TRUE) + 
  labs(title = "Total Removed Books", fill = "No of Books \nRemoved") +
  theme(legend.position = "right") +
  scale_fill_continuous(low = "yellow", high = "red",  
                        breaks= c(2,4,6,8,10,12))

# Political Leaning
plot_usmap(data = books,values = "leaning", labels = TRUE) + 
  labs(title = "Political Leaning of State", fill = "Leaning") +
  theme(legend.position = "right") +
  scale_fill_manual(values = c("blue", "red"), 
                    labels = c("Democratic", "Republican", "No Data"))

                    
#days since 2000
books %>%
  group_by(days2000, removed) %>%
  count(removed) %>%
  ggplot(aes(x = days2000, y = n, colour = as.factor(removed), group = removed)) +
  geom_line() +
  facet_wrap(vars(removed), ncol = 1)


# The plots below show the relationship between political leaning with income and
# level of education.  Income and education levels are higher in Democratic 
# states.  Income is linked to education and therefore we may not need
# both variables in our final model.

l1 <- books %>%
  ggplot(aes(x = pvi2, y = cmedin)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Median State Income", subtitle = "Grand Median Centered", 
       x = "Political Leaning", y = 'Median Income ($)')

l2 <- books %>%
  ggplot(aes(x = pvi2, y = cperhs)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "% High School Grads", subtitle = "Grand Median Centered", 
       x = "Political Leaning", y = 'HIgh School Graduates (%)')

l3 <- books %>%
  ggplot(aes(x = pvi2, y = cperba)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "% College Grads", subtitle = "Grand Median Centered", 
       x = "Political Leaning", y = 'College Graduates (%)')

l1 + l2 + l3

books %>%
  ggplot(aes(x = cperba, y = cperhs)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "College Graduates vs Income", 
       x = "% College Grads (Grand Mean Centered)", 
       y = 'Median State Income (Grand Mean Centered)')

# DATE
# Obama became president 20/1/2009 - 3307 days after 1/1/2000
# We can see that there do seem to be fewer books removed over time but no definite
# relationship to Obama becoming president
books %>%
  filter(removed ==1) %>%
  group_by(days2000) %>%
  count(days2000) %>%
  ggplot(aes(x = days2000, y = n)) +
  geom_line() +
  labs(title = "Number of Removed Books Over Time", x = "Days After 1st January 2000",
       y = "Number of Books") +
  geom_vline(xintercept = 3307, colour = "blue") +
  geom_text(aes(x=2800, label="Inauguration of \nPresident Obama", y=10))

# BOOK LEVEL FEATURES

# ************ Michelle has these




# Several of the variables are a proxy for state i.e. state can be directly 
# transformed into some of the variables; income, high school grades, political 
# leaning etc and vice-versa

# The data has a hierarchical structure as the challenges are grouped within 
# different states.  Books being challenged in the same state are not 
# independent of each other because they will be subject to the same level/type of 
# scrutiny.  Therefore it seems like a model with state as a random effect 
# might be best suited.


# HYPOTHESES

# H0 - There is no significant difference in the probability of a book being banned 
# following a challenge in different states after accounting for other book-level
# variables and a generalised linear model will be the best fit for the data.

# H1 - There is a significant difference in the probability of a book being banned 
# following a challenge in different states after accounting for other book-level 
# variables and a generalised linear mixed model will be the best fit for the data.

# SPLIT DATA INTO TEST AND TRAIN

# rescale days & drop na
books <- books %>%
  mutate(scaledays = days2000/1000) %>%
  drop_na()


set.seed(10) # set seed so reproducible
# choose the sample 80%
books_ind <- sample(seq_len(nrow(books)), size = floor(0.8 * nrow(books)))

train <- books[books_ind, ]
test <- books[-books_ind, ]

# ASSESSING THE NEED FOR A MULTILEVEL MODEL

# RANDOM INTERCEPTS
# Fitting a baseline model with only the intercept
InterceptOnly <- glm(removed ~ 1, data = train, family = "binomial")
# Fitting a model where intercepts vary over the states
RandomInterceptOnly <- glmer(removed ~ 1 +  (1|state), data = train, 
                             family = "binomial")

summary(InterceptOnly)
summary(RandomInterceptOnly)
# Comparing the AIC and BIC of the models we can see that allowing the intercepts to 
# vary improves the fit of the model (AIC reduced from 697.13 to 651.51)

anova(RandomInterceptOnly, InterceptOnly)
# Here we can see that the BIC and LogLikelihoods are also reduced in the 
# multilevel model

ranef(RandomInterceptOnly)
# by extracting the random effects we can see that they do vary by state


# The evidence above suggests that there is a variance between the different 
# states, therefore we can reject the null hypothesis that there is no difference
# between the states and a GLMM will be the best fit for the data

# RANDOM SLOPE

# This model allows for the effect of time to vary across the states
RandomInterceptSlope <- glmer(removed ~ 1 + (scaledays|state), data = train, 
                              family = "binomial")

# Here we can see that there is a significant difference between the models
# (p<0.05) and adding random slopes has improved the model; AIC reduced from
# 652 to 637
anova(RandomInterceptOnly, RandomInterceptSlope)


# ADDING IN FIXED EFFECTS

# We will continue to use the AIC score as our decision rule


# Since several of the variables were directly derived from the 'state' variable
# we will begin with a model with all variables other than those related to state
m1 <-  glmer(removed ~  (scaledays|state) + obama + freqchal +  sexexp + 
                antifamily + occult + language + homosexuality + violence,
              data = train, family = "binomial")

# This model suggests that freqchal might be significant.  
summary(m1)
tab_model(m1)

# Here I am trying some combinations of the other variables with lowest P values
# (antifamily and language)
m2 <-  glmer(removed ~  (scaledays|state) + freqchal, data = train, 
             family = "binomial")

m3 <-  glmer(removed ~  (scaledays|state)  + freqchal + antifamily, data = train, 
             family = "binomial")

m4 <-  glmer(removed ~  (scaledays|state) + freqchal + antifamily + language, 
             data = train, family = "binomial")

m5 <-  glmer(removed ~  (scaledays|state) + freqchal + language, data = train, 
             family = "binomial")



# There are no significant differences between these models but AIC is lowest for
# m4 (as is BIC) suggesting it is the best fit for the training data
anova(m1, m2, m3, m4, m5)
tab_model(m1, m2, m3, m4, m5)

summary(m4)

# The chosen model (m4) has random intercepts for states and random slopes
# for the variable days2000 (date) along with fixed effects freqchal, antifamily
# and language




# WHAT FEATURES OF A STATE MAKE A BOOK MORE LIKELY TO BE BANNED?

# If something changed within a state e.g. if political leaning flipped, our 
# model (m4) would no longer be valid because it uses state as a level.  Our 
# model also couldn't be used for predictive purposes for the states where no 
# data was available (Nevada, Utah and Texas)


# A model with all of the variables to get a feel for what's most significant
statem1 <- glm(removed ~ cperhs + cmedin + cperba + scaledays + leaning + 
                 freqchal + language + sexexp + violence + antifamily, 
               data = train, family = "binomial")

summary(statem1)

statem2 <-  glm(removed ~ leaning*cperba + cperhs*cperba + scaledays + freqchal 
                + language + violence, data = train, 
                family = "binomial")

summary(statem2)

# Trying some combinations of variables
statem3 <- glm(removed ~ scaledays + cperhs*cperba + leaning + freqchal + 
                 language*violence, data = train, family = "binomial")

statem4 <- glm(removed ~ scaledays + cperhs*cperba + leaning + freqchal, 
               data = train, family = "binomial")

statem5 <- glm(removed ~ cperhs*cperba + leaning + freqchal, data = train, 
               family = "binomial")

statem6 <- glm(removed ~ cperhs*cperba + leaning, data = train, family = "binomial")

statem7 <- glm(removed ~ scaledays + cperhs * cperba + leaning + freqchal + 
                 language * violence, data = train, family = "binomial")


tab_model(statem2, statem3, statem4, statem5, statem6, statem7)
anova(m4, statem2, statem3, statem4, statem5, statem6, statem7)

# We can see that when using the actual variables rather than state model 7 is 
# the best fit using lowest AIC as our decision rule.  This is a model with
# days, leaning, frequently challenged, plus the interactions between high school 
# and college attendance and between language and violence.




# COMPARING THE TWO MODELS

# Brier score can be used to evaluate models with a binary outcome

# Brier of test data (models haven't seen this data before)
m4_test <- predict(m4, newdata = test, type = "response")
m7_test <- predict(statem7, newdata = test, type = "response")

error4_test <- test$removed - m4_test
error7_test <- test$removed - m7_test

Brier_test_m4 <- mean(error4_test^2)
Brier_test_m7 <- mean(error7_test^2)


# Brier of training data (data that the models were trained on)
m4_train <- predict(m4, type = "response")
m7_train <- predict(statem7, type = "response")

error4_train <- train$removed - m4_train
error7_train <- train$removed - m4_train

Brier_train_m4 <- mean(error4_train^2)
Brier_train_m7 <- mean(error7_train^2)




# Applying the models to the test data for AUC 
m4_test <-  glmer(removed ~  (scaledays|state) + freqchal + antifamily 
                  + language, data = test, family = "binomial")

m7_test <- glm(removed ~ scaledays + cperhs * cperba + leaning + freqchal + 
                 language * violence, data = test, family = "binomial")

ModelMetrics::auc(m4) 
ModelMetrics::auc(m4_test)
ModelMetrics::auc(statem7) 
ModelMetrics::auc(m7_test)


# Putting the results into a table
results <- tibble(
  " " = c("Brier Score Training Data", "Brier Score Test Data", 
          "AUC Training Data (%)", "AUC Test Data (%)"),
  "Model 4 (GLMM)" = round(c(Brier_train_m4, Brier_test_m4, 
                             100 * c(ModelMetrics::auc(m4), 
                                     ModelMetrics::auc(m4_test))),2),
  "Model 7 (GLM)" = round(c(Brier_train_m7, Brier_test_m7, 
                            100 * c(ModelMetrics::auc(statem7), 
                                    ModelMetrics::auc(m7_test))),2)
) 

grid.table(results, rows = NULL)
# Here we can see that the Brier Scores are roughly the same for both models, 
# Model 4 had a slightly lower (better) score for the test data which suggests 
# underfitting. The AUC tells use that for model 4 82.3% of the variation in the 
# training data and 88.4% of the variation in the test (unseen) data is explained 
# by the model. Model 4 is the (slightly) better model.



# REPORTING ON THE BEST MODEL

tab_model(m4)
# The odds ratios indicate that, when all other factors are constant, 
# a frequently challenged book is 1.99 times more likely to result in the book 
# being removed following a challenge. Books containing bad language are 1.44 
# times to be removed.  Books containing anti-family themes are actually 0.45 
# times LESS likely to be removed.  


#This is confirmed in the plot below; we can see that removed probability 
# increases with frequently challenged books, is higher in
# books with bad language but is lower in books with anti-family themes.

plot_model(m4, type = "pred", terms = c( "freqchal", "language" , "antifamily")) +
  xlab("Frequently challenged authors")




# PREDICTING FROM THE BEST MODEL

# In January 2022 the following books were banned in Missouri 
# Toni Morrison's The Bluest Eye and;
# Alison Bechdel's Fun Home
# Since these both appear in our dataset we can use their variables to see if our 
# model would predict this.

Morrison <- tibble(days2000 = 8036/1000,
                   state = "MO",
                   freqchal = 1,
                   antifamily = 0,
                   language = 0,
)

Bechdel <- tibble(days2000 = 8036/1000,
                  state = "MO",
                  freqchal = 0,
                  antifamily = 0,
                  language = 0,
)

predict(m4, newdata = Morrison, type = "response") # The model predicts a 68% probability of being removed
predict(m4, newdata = Bechdel, type = "response") # The model predicts a 51% probability of being removed

