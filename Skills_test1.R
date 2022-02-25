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


