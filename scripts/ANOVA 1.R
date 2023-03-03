# SET UP ----

## packages ----

library(tidyverse)
library(here)
library(kableExtra)
library(GGally)
library(broom.helpers)
library(emmeans)
library(performance)
library(rstatix)
library(patchwork)

## import----

frog <- read_csv(here("data", "frogs_messy_data.csv")) # import of data

## tidy ----

glimpse(frog)
head(frog)

frog <- frog %>% 
  rename("13" = Temperature13,
         "18" = Temperature18,
         "25" = Temperature25,
         frogspawn_id = `Frogspawn sample id`) %>% 
  pivot_longer(`13`:`25`, names_to="temperature", values_to="days") %>% 
  drop_na(days)

# LINEAR MODEL ----

## mean ----

lmfrog1 <- lm(formula = days ~ 1, data = frog)

broom::tidy(lmfrog1)

frog %>% 
  group_by(temperature) %>% 
  summarise(mean=mean(days))                 # revealing means

lmfrog2 <- lm(days ~ temperature, data=frog) # differences between means

## assumptions ----

### is the residual variance (approx) normally distributed? ----

performance::check_model(lmfrog2, check=c("normality","qq"))

plot(lsmodel1, which=c(2,2))

### is residual variance (approx) equal between groups? ----

performance::check_model(lsmodel1, check="homogeneity")

plot(lsmodel1, which=c(1,3))

### are there any significant outliers? ----

performance::check_model(lsmodel1, check="outliers")

plot(lsmodel1, which=c(4,4))





























