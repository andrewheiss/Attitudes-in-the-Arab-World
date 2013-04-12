###################################
# Replications of original tables
# www.journalofdemocracy.org/articles/gratis/TesslerGraphics-19-1.pdf
###################################

# Set up
library(car)
library(ROCR)
library(Hmisc)
library(ggplot2)
library(plyr)
library(reshape)
library(xtable)
library(MIfuns)
source('separationplot.R')

set.seed(1234)


#-------------------------------------------------
# Table 1
# Personal Religiosity and Support for Democracy
#-------------------------------------------------
all.countries.table <- xtabs(~ dem.best + quran, data=barometer)
round(prop.table(all.countries.table, 2), 2)

individual.countries.table <- xtabs(~ dem.best + quran + country.name, data=barometer)
round(prop.table(individual.countries.table, c(3,2)), 2)

# Alternative method for making table
results <- ftable(round(prop.table(xtabs(~ country.name + dem.best + quran, data=barometer), c(3,1)), 2))

print(xtable(ftable2data.frame(results), caption="Frequency table created from ICPSR data", label="tab:table2", digits=3), size="\\small", include.rownames=FALSE)

# Table 3
ftable(round(prop.table(xtabs(~ dem.best + sec.dem + country.name, data=barometer.original4), c(3, 1)), 2))




#-----------------------------------------------------------
# Table 2
# Binary Logistic Regression Models Estimating Support for
# Secularism among Respondents Who Support Democracy
#-----------------------------------------------------------

# Dependent variable
# 0 = Favors Islamic democracy; 1 = Favors secular democracy
# Footnote 13 explains why q4013 is the best measure

all.countries <- glm(sec.dem ~ I(-quran) + I(-prime.minister) + 
                       I(-citizen.influence) + I(-maintain.order) + 
                       education + age + family.econ, 
                     data=barometer.original4, subset=(dem.best==1), 
                     family=binomial(link="logit")) 
summary(all.countries)

separation.plot(all.countries$fitted.values, all.countries$y, line=TRUE)

# ROC Curve
auc <- somers2(all.countries$fitted.values, all.countries$y)[1]
ROCRpred <- prediction(all.countries$fitted.values, all.countries$y)

plot(performance(ROCRpred,'tpr','fpr'), main="ROC plot", lwd=3, colorize=TRUE)
abline(0, 1, col="grey", lty=3, lwd=3)
text(1, .1, paste("AUC =", round(auc, 3)), pos=2)


# Individual countries
run.country.models <- function(x) {
  model <- glm(sec.dem ~ I(-quran) + I(-prime.minister) + 
                 I(-citizen.influence) + I(-maintain.order) + 
                 education + age + family.econ, 
               data=barometer, subset=(dem.best==1 & country.name==x), 
               family=binomial(link="logit")) 
}

country.models <- sapply(levels(barometer$country.name), FUN=run.country.models)

# View everything
all.models <- cbind("All Countries"=coefficients(all.countries), as.data.frame(country.models[1,]))
all.models
xtable(all.models)

summary(country.models[1,1])
summary(all.countries)


#---------------------------------------------------
# Table 3
# Binary Logistic Regression Models Estimating 
# Opposition to a Strong Undemocratic Leader among 
# Respondents Who Support Democracy
#---------------------------------------------------

# Do this once Table 2 is accurate

all.countries <- glm(autocracy.bad ~ I(-quran) + I(-prime.minister) + 
                       I(-citizen.influence) + I(-maintain.order) + 
                       education + age + family.econ, 
                     data=barometer, subset=(dem.best==1), 
                     family=binomial(link="logit")) 
summary(all.countries)

# Individual countries
run.country.models <- function(x) {
  model <- glm(autocracy.bad ~ I(-quran) + I(-prime.minister) + 
                 I(-citizen.influence) + I(-maintain.order) + 
                 education + age + family.econ, 
               data=barometer, subset=(dem.best==1 & country.name==x), 
               family=binomial(link="logit")) 
}

country.models <- sapply(levels(barometer$country.name), FUN=run.country.models)

# View everything
all.models <- cbind("All Countries"=coefficients(all.countries), as.data.frame(country.models[1,]))
all.models