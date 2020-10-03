library(readxl)
library(dplyr)
library(lattice)
library(broom)

credit <- read_excel("credit.xlsx")
View(credit)

default <-credit$default
default <- recode(default, "yes" = 1, "no" = 0)
check <- credit$check
savings <- credit$savings

# Building the first model
first_m=glm(as.factor(default) ~ as.factor(check) + as.factor(savings) + duration, data=credit, family="binomial"(link="logit"))
summary(first_m)
confint(first_m)
# R squared:
with(summary(first_m), 1 - deviance/null.deviance)


# second model try
second_m=glm(as.factor(default) ~ ., data=credit, family="binomial"(link="logit"))
summary(second_m)
confint(second_m)
with(summary(second_m), 1 - deviance/null.deviance)
