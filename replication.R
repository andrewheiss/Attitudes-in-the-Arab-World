# All files required for full replication are available at https://github.com/andrewheiss/Attitudes-in-the-Arab-World

#--------------------------
# Load data and libraries
#--------------------------
library(car)
library(ggplot2)
library(reshape2)
library(plyr)
library(xtable)

setwd("~/Documents/Duke 2012-2013/Spring 2013/Polsci 733/Attitudes in the Arab World Replication")
load("Barometer.RData")

set.seed(1234)


#---------------
# Build models
#---------------
# Save the formulas so they can be reused more easily
form.logit <- formula(autocracy.bad ~ quran + prime.minister + 
                        citizen.influence + maintain.order + 
                        education + age + family.econ)
form.ologit <- formula(autocracy.bad.ordinal ~ quran + prime.minister + 
                         citizen.influence + maintain.order + 
                         education + age + family.econ)

# Corresponding human-readable names for nicer output
nice.names <- c("Read Qur'an", "Prime minister", "Citizen influence", 
                "Maintain order", "Education", "Age", "Family economic status")

# logit models for all countries combined
all.countries.logit <- glm(form.logit, data=barometer, 
                           subset=(dem.best==1), family=binomial(link="logit"))
original4.logit <- glm(form.logit, data=barometer.original4, 
                       subset=(dem.best==1), family=binomial(link="logit"))

# ologit models for all countries combined
all.countries <- polr(form.ologit, data=barometer,
                      subset=(dem.best==1), method="logistic", Hess=TRUE)
original4 <- polr(form.ologit, data=barometer.original4,
                  subset=(dem.best==1), method="logistic", Hess=TRUE)


# Function to create a list of individual country models
# bquote() + .() passes the actual names of the arguments into the model call; eval() actually runs the model
run.country.models.logit <- function(country, form) {
  model <- bquote(glm(.(form), data=barometer,
                      subset=(dem.best==1 & country.name==.(country)),
                      family=binomial(link="logit")))
  eval(model)           
}

run.country.models <- function(country, form) {
  model <- bquote(polr(.(form), data=barometer,
                       subset=(dem.best==1 & country.name==.(country)),
                       method="logistic", Hess=TRUE))
  eval(model)             
}

# Create a list of models for all countries, given a model formula
country.models.logit <- lapply(levels(barometer$country.name), FUN=run.country.models.logit, form=form.logit)
names(country.models.logit) <- levels(barometer$country.name)  # Name the list for convenience

country.models <- lapply(levels(barometer$country.name), FUN=run.country.models, form=form.ologit)
names(country.models) <- levels(barometer$country.name)  # Name the list for convenience


#-----------------------
# Summarize all models
#-----------------------
summary(all.countries.logit)
summary(original4.logit)
summary(all.countries)
summary(original4)
lapply(country.models.logit, summary)
lapply(country.models, summary)