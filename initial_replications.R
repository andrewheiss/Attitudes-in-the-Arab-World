###################################
# Replications of original tables
# www.journalofdemocracy.org/articles/gratis/TesslerGraphics-19-1.pdf
###################################

# Set up
library(car)
library(ROCR)
library(Hmisc)
library(ggplot2)
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
                     data=barometer, subset=(dem.best==1), 
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
xtable(all.models)


# Simulate  ologit results for Jordan
model.ologit <- polr(sec.dem.ordinal ~ I(-quran) + I(-prime.minister) + 
                       I(-citizen.influence) + I(-maintain.order) + 
                       education + age + family.econ, data=barometer, 
                     subset=(dem.best==1 & country.name=="Jordan"), 
                     method="logistic")


# Predictions
# Look at effect of education
quran.range <- min(barometer$quran, na.rm=T):max(barometer$quran, na.rm=T)

# Extract stuff from model
beta.ologit <- coef(model.ologit)
tau.ologit <- model.ologit$zeta

my.mode <- function(x){
  which.max(table(x))
}

X <- cbind(quran.range, my.mode(barometer$prime.minister), my.mode(barometer$citizen.influence), my.mode(barometer$maintain.order), my.mode(barometer$education), my.mode(barometer$age), my.mode(barometer$family.econ))

# Simulation
tau.beta <- function(x) {
  beta <- x[1:7]
  tau <- x[8:10]
  p1 <- plogis(tau[1] - X %*% beta)
  p2 <- plogis(tau[2] - X %*% beta) - plogis(tau[1] - X %*% beta)
  p3 <- plogis(tau[3] - X %*% beta) - plogis(tau[2] - X %*% beta)
  p4 <- 1 - plogis(tau[3] - X %*% beta)
  return(data.frame(p1, p2, p3, p4))
}

runs <- 1000
draw <- mvrnorm(runs, c(beta.ologit, tau.ologit), vcov(model.ologit))
beta.sim <- draw[,1:7]
tau.sim <- draw[,8:10]

simulation.list <- apply(draw, 1, FUN=tau.beta)
plot.data <- cbind(simulation=rep(1:runs, each=length(quran.range)), quran=rep(quran.range, runs), ldply(simulation.list))
plot.data <- melt(plot.data, id.vars=c("simulation", "quran"), variable_name="category")
plot.data$category <- factor(plot.data$category, labels=c("Strongly agree", "Agree", "Disagree", "Strongly dissagree"))

ologit.spaghetti <- ggplot()
ologit.spaghetti + geom_line(aes(x=quran, y=value, group=interaction(simulation, category), colour=category), alpha=0.05, data=plot.data, size=1) + 
  scale_colour_brewer(palette="Set1") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size=2))) + 
  labs(y="Probability of answering\n", x="\nFrequency of Qur'an reading", colour="Secular democracy")


#---------------------------------------------------
# Table 3
# Binary Logistic Regression Models Estimating 
# Opposition to a Strong Undemocratic Leader among 
# Respondents Who Support Democracy
#---------------------------------------------------

# Do this once Table 2 is accurate
