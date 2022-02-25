# Load required packages
library(lme4)
library(sjPlot)
library(tidyverse)
library(glmmTMB)

# read in and check data
patients <- read.csv("~/Stats2/ST1.csv")

view(patients)
