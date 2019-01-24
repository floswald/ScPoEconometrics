library(tidyverse)
library(car)
library(magrittr)
library(stargazer)
library(broom)
library(lmtest)
library(AER)
library(estimatr)

df <- as_tibble(mtcars)
est <- lm(mpg ~ gear + I(gear^2), data=df)
broom::tidy(est)
coeftest(est,vcov=hccm)
est.robust <- lm_robust(mpg ~ gear + I(gear^2), data=df)
estimatr::tidy(est.robust)

stargazer(est,se=starprep(est.robust),type="text")

# iv regression
est.iv <- ivreg(mpg ~ drat + qsec + gear | wt + qsec + gear, data=df)
est.ivr <- iv_robust(mpg ~ drat + qsec + gear | wt + qsec + gear, data=df)
