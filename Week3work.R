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

