## Installing & Loading Packages
install.packages("tidyverse")
install.packages("psych")
install.packages("ggplot2")
install.packages("lavaan")
library(tidyverse)
library(psych)
library(ggplot2)
library(lavaan)

## reading data file
df <- read_csv("GenderMatchData.csv")
class(df$lc1)

## data cleaning
df <- df[ , c("tmkey", "tlkey", "gender", "age", "gender_l", "age_l", 
              "lc1", "lc2", "lc3", "lc4", "lc5", "lc6", "lc7", "lc8",
              "lmx1", "lmx2", "lmx3", "lmx4", "lmx5", "lmx6", "lmx7",
              "as1", "as2", "as3", "as4", "as5", "as6", "as7")]

df <- df %>% 
  mutate(mean_lc = (lc1 + lc2 + lc3 + lc4 + lc5 + lc6 + lc7 + lc8) / 8,
         mean_lmx = (lmx1 + lmx2 + lmx3 + lmx4 + lmx5 + lmx6 + lmx7) / 7,
         mean_as = (as1 + as2 + as3 + as4 + as5 + as6 + as7) / 7)

sum(is.na(df$mean_lmx))
sum(is.na(df$mean_as))
sum(is.na(df$mean_lc))

df <- df %>% 
  filter(mean_lc != "NA") %>% 
  filter(mean_lmx != "NA") %>% 
  filter(mean_as != "NA")

df <- df %>% 
  mutate(gender_match = case_when((gender == 3 & gender_l == 1) |
                                    (gender == 4 & gender_l == 2) ~ 1, TRUE ~ 0))

## Simple Modeling
model_1 <- lm(mean_as ~ mean_lc, data = df)
summary(model_1)

model_2 <- lm(mean_as ~ mean_lc + mean_lmx, data = df)
summary(model_2)

model_3 <- lm(mean_as ~ mean_lc + mean_lmx * gender_match, data = df)
summary(model_3)


## Multiple Group Path Model using LAVAAN
## recoding moderator variable to be categorical
df <- df %>% 
  mutate(gen_mat4 = case_when(
    gender == 3 & gender_l == 1 ~ "MM",
    gender == 4 & gender_l == 2 ~ "FF",
    gender == 3 & gender_l == 2 ~ "MF",
    gender == 4 & gender_l == 1 ~ "FM"
  ))
df$gender_match <- as.numeric(df$gender_match)

df$gen_mat4 <- factor(df$gen_mat4, ordered = TRUE, levels = c("MM", "FF", "MF", "FM"))

MGmodel1 <- '
  # Direct effects
  mean_lmx ~ a*mean_lc  
  mean_as ~ mean_lc + b*mean_lmx 
  
  # indirect effects
  indirect := a*b
'
fitMG1 <- sem(MGmodel1, data = df, group = "gen_mat4")
summary(fitMG1,  standardized = TRUE, rsq = TRUE)
