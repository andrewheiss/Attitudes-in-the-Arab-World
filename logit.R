#-------------------------
# Load initial libraries
#-------------------------
library(car)
library(ggplot2)
library(reshape)
library(plyr)

set.seed(1234)

#-------
# Model
#-------
model.logit <- glm(sec.dem ~ quran + prime.minister + 
                       citizen.influence + maintain.order + 
                       education + age + family.econ,# + association + trust + vote + rally, 
                       data=barometer.original4, 
#                      subset=(dem.best==1 & country.name=="Jordan"), 
                     subset=(dem.best==1), 
                    family=binomial(link="logit"))
summary(model.logit)

# separation.plot(model.logit$fitted.values, model.logit$y, line=TRUE)

# Predictions
# y.plot.label <- "Probability of supporting secular democracy\n"
y.plot.label <- "Probability of thinking autocracy is bad\n"

# Look at effect of Quran reading
# sample.range <- min(barometer$quran, na.rm=T):max(barometer$quran, na.rm=T)
# x.labels <- c("Everyday", "Several times a week", "Sometimes", "Rarely", "I don't read it")
# x.plot.label <- "\nFrequency of Qur'an reading"

# sample.range <- min(barometer$citizen.influence, na.rm=T):max(barometer$citizen.influence, na.rm=T)
# x.labels <- c("Strongly agree", "Agree", "Disagree", "Strongly disagree")
# x.plot.label <- "\nCitizens have power to influence government policies"

sample.range <- min(barometer$maintain.order, na.rm=T):max(barometer$maintain.order, na.rm=T)
x.labels <- c("Strongly agree", "Agree", "Disagree", "Strongly disagree")
x.plot.label <- "\nDemocracies are not good at maintaining order."

# Scenarios
# Everything at the mode
# X <- cbind(1, sample.range, my.mode(barometer$prime.minister), my.mode(barometer$citizen.influence), my.mode(barometer$maintain.order), my.mode(barometer$education), my.mode(barometer$age), my.mode(barometer$family.econ))

# Everything at the mode
# X <- cbind(1, my.mode(barometer$quran), my.mode(barometer$prime.minister), sample.range, my.mode(barometer$maintain.order), my.mode(barometer$education), my.mode(barometer$age), my.mode(barometer$family.econ))

# Everything at the mode
# X <- cbind(1, my.mode(barometer$quran), my.mode(barometer$prime.minister), my.mode(barometer$citizen.influence), sample.range, my.mode(barometer$education), my.mode(barometer$age), my.mode(barometer$family.econ))

X <- cbind(1, my.mode(barometer$quran), my.mode(barometer$prime.minister), my.mode(barometer$citizen.influence), sample.range, my.mode(barometer$education), my.mode(barometer$age), my.mode(barometer$family.econ))


#------------
# Functions
#------------
# Calculate the mode
my.mode <- function(x){
  which.max(table(x))
}

# Get predicted probabilities for given coefficients
predict.probs <- function(beta) {
  probs.raw <- X %*% beta
  probs <- 1 / (1 + exp(-probs.raw))
  return(probs)
}

# Predicted probabilities for actual coefficients
single <- predict.probs(coef(model.logit))
single.plot <- data.frame(value=single)
single.plot$xvar <- sample.range


# Set up simulation
runs <- 250
draw <- mvrnorm(runs, coef(model.logit), vcov(model.logit))

# Predicted probabilities for simulated set of coefficients
simulation.list <- apply(draw, 1, FUN=predict.probs)
plot.data <- melt(simulation.list, varnames=c("xvar", "simulation"))


# Spaghetti plot of predicted probabilities
p <- ggplot()
p + geom_line(aes(x=xvar, y=value), data=single.plot, size=2, colour="red4") + 
  geom_line(aes(x=xvar, y=value, group=simulation), data=plot.data, colour="red4", size=1, alpha=0.05) + 
  ylim(0, 1) + 
  labs(x=x.plot.label, y=y.plot.label) + 
  scale_x_continuous(labels=x.labels) 
