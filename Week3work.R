library(lme4)
library(sjPlot)
library(tidyverse)
install.packages('glmmTMB')
library(glmmTMB)

# Multilevel modeling in R: a visual explanation
d <- tibble(Reaction = c(0,1,7,9,17,16,12,10,29,27,24,22,39,36,33,30,49,47,42,42),
            Days = c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4),
            Subject = c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5))
d$Subject<-factor(d$Subject)

ggplot(d, aes(x=Days, y=Reaction, colour=Subject))+
  geom_point(size=4)+scale_color_brewer(palette="Dark2")

# Regular linear model
m = lm(Reaction ~ Days, data=d)
tab_model(m)

ggplot(d, aes(x=Days, y=Reaction, colour=Subject))+
  geom_point(size=3)+
  geom_smooth(method=lm, na.rm = TRUE, fullrange= TRUE,
              aes(group=1),colour="black", se=FALSE) +
  scale_color_brewer(palette = "Dark2")

# Multilevel model with random intercepts
m_ri = lmer(Reaction ~ Days + (1 | Subject), data=d)
tab_model(m_ri)

coef(m_ri)$Subject[4,1]

d$pred_m_ri<-predict(m_ri)

ggplot(d, aes(x=Days, y=pred_m_ri, colour=Subject)) + 
  geom_point(aes(x=Days, y=Reaction),size=3) +  ## observations
  geom_smooth(method=lm, se=FALSE) + # predictions 
  scale_color_brewer(palette = "Dark2") 

# Multilevel model with random intercepts and random slopes
m_rs = lmer(Reaction ~ Days + (1 + Days | Subject), data=d)
tab_model(m_rs)

d$pred_m_rs<-predict(m_rs)

ggplot(d, aes(x=Days, y=pred_m_rs, colour=Subject)) + 
  geom_point(aes(x=Days, y=Reaction),size=3) +  ## observations
  geom_smooth(method=lm, se=FALSE) + # predictions 
  scale_color_brewer(palette = "Dark2") 

# Comparing multilevel models
m_base = lmer(Reaction ~ (1 | Subject), data=d)
m1 = lmer(Reaction ~ Days + (1 | Subject), data=d)
m2 = lmer(Reaction ~ Days + (1 + Days| Subject), data=d)
anova(m_base,m1,m2)

tab_model(m_base, m1, m2)

# Another example with more data

set.seed(1)
groups = 26
groupsize = 30
n = 26*30

level1 = tibble(class_id = rep(letters, each=groupsize),
                hours_studied = rpois(n, 12),
                wealth = rnorm(n, rep(1:groups, each=groupsize), 3))


level2 = tibble(class_id = letters,
                teacher_exp = rpois(groups, 1:groups/2))
level2$B0j = 2 + 0.3*level2$teacher_exp + rnorm(groups, 0, 0.4)
level2$B1j = 0.1 + rnorm(groups, 0, 0.15)

d = left_join(level1, level2, by='class_id')
d$exam_grade = d$B0j + d$B1j*d$hours_studied + rnorm(n, 0, 0.8)

# Analyzing the data with normal linear regression
m0 = lm(exam_grade ~ 1, data=d)
m1 = lm(exam_grade ~ 1 + hours_studied, data=d)
m2 = lm(exam_grade ~ 1 + hours_studied + wealth, data=d)

tab_model(m0, m1, m2)

m3 = lm(exam_grade ~ 1 + hours_studied + wealth + class_id, data=d)
tab_model(m3)
tab_model(m3, rm.terms = paste('class_id', letters, sep=''))

m3 = lm(exam_grade ~ 1 + hours_studied + wealth + class_id + teacher_exp, data=d)
m3$coefficients

#Analyzing the data with multilevel regression
m4 <- lmer(exam_grade ~ 1 + hours_studied + wealth + teacher_exp + (1 + hours_studied | class_id), data=d)
hist(d$hours_studied)

d = mutate(d, hours_studied = hours_studied - mean(hours_studied),
           wealth = wealth - mean(wealth))
m4 = lmer(exam_grade ~ 1 + hours_studied + wealth + teacher_exp + (1 + hours_studied | class_id), data=d)

m0 = lmer(exam_grade ~ 1 + (1 | class_id), data=d)
m1 = lmer(exam_grade ~ 1 + hours_studied + (1 | class_id), data=d)
m2 = lmer(exam_grade ~ 1 + hours_studied + wealth + (1 | class_id), data=d)
m3 = lmer(exam_grade ~ 1 + hours_studied + wealth + teacher_exp + (1 | class_id), data=d)
m4 = lmer(exam_grade ~ 1 + hours_studied + wealth + teacher_exp + (1 + hours_studied | class_id), data=d)

tab_model(m0, m1, m2, m3, m4, show.ci = F)

plot_model(m4,  type="re")
