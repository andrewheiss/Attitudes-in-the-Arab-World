#-------------------------
# Load initial libraries
#-------------------------
library(car)
library(ggplot2)
library(reshape)
library(plyr)
library(scales)

set.seed(1234)

#-------
# Model
#-------
model.ologit <- polr(autocracy.bad.ordinal ~ quran + prime.minister + 
                       citizen.influence + maintain.order + 
                       education + age + family.econ,# + association + trust, 
                     data=barometer.original4, 
#                      data=barometer, 
                     subset=(dem.best==1 & country.name=="Jordan"), 
#                      subset=(dem.best==1), 
                     method="logistic")
summary(model.ologit)


#---------------------
# Set up simulations
#---------------------

#-----
# Dependent variable details
#-----
# Secular democracy (sec.dem.ordinal)
# y.plot.label <- "Probability of answering\n"
# legend.title <- "Religious men decide"
# cat.labels <- c("Strongly agree", "Agree", "Disagree", "Strongly disagree")

# Autocracy bad (autocracy.bad.ordinal)
y.plot.label <- "Probability of answering\n"
legend.title <- "Autocracy bad"
cat.labels <- c("Very good", "Good", "Bad", "Very bad")


#-----
# Varied independent variables and scenarios
#-----
# Qur'an reading
# sample.range <- min(barometer$quran, na.rm=T):max(barometer$quran, na.rm=T)
# x.labels <- c("Everyday", "Several times a week", "Sometimes", "Rarely", "I don't read it")
# x.plot.label <- "\nFrequency of Qur'an reading"
 
# Qur'an scenarios
# Everything at the mode
# X <- cbind(sample.range, my.mode(barometer$prime.minister), my.mode(barometer$citizen.influence), my.mode(barometer$maintain.order), my.mode(barometer$education), my.mode(barometer$age), my.mode(barometer$family.econ))

# Education
# X <- cbind(sample.range, my.mode(barometer$prime.minister), my.mode(barometer$citizen.influence), my.mode(barometer$maintain.order), 7, my.mode(barometer$age), my.mode(barometer$family.econ))

# Political evaluation excellent
# X <- cbind(sample.range, 4, 4, 1, my.mode(barometer$education), my.mode(barometer$age), my.mode(barometer$family.econ))

# Political evaluation bad
# X <- cbind(sample.range, 1, 1, 4, my.mode(barometer$education), my.mode(barometer$age), my.mode(barometer$family.econ))


# Maintaining order
# sample.range <- min(barometer$maintain.order, na.rm=T):max(barometer$maintain.order, na.rm=T)
# x.labels <- c("Strongly agree", "Agree", "Disagree", "Strongly disagree")
# x.plot.label <- "\nDemocracies are not good at maintaining order"

# Maintaining order scenarios
# X <- cbind(my.mode(barometer$quran), my.mode(barometer$prime.minister), my.mode(barometer$citizen.influence), sample.range, my.mode(barometer$education), my.mode(barometer$age), my.mode(barometer$family.econ))
# X <- cbind(my.mode(barometer$quran), my.mode(barometer$prime.minister), my.mode(barometer$citizen.influence), sample.range, 7, my.mode(barometer$age), my.mode(barometer$family.econ))


# Citizen influence
sample.range <- min(barometer$citizen.influence, na.rm=T):max(barometer$citizen.influence, na.rm=T)
x.labels <- c("Strongly agree", "Agree", "Disagree", "Strongly disagree")
x.plot.label <- "\nCitizens have power to influence government policies"

# Citizen influence scenarios
X <- cbind(my.mode(barometer$quran), my.mode(barometer$prime.minister), sample.range, my.mode(barometer$maintain.order), 1, my.mode(barometer$age), my.mode(barometer$family.econ))


#------------
# Functions
#------------
# Calculate the mode
my.mode <- function(x) {
  which.max(table(x))
}

# Get predicted probabilities for given taus and betas
tau.beta <- function(x) {
  beta <- x[1:num.betas]
  tau <- x[(num.betas + 1):(num.betas + num.taus)]
  p1 <- plogis(tau[1] - X %*% beta)
  p2 <- plogis(tau[2] - X %*% beta) - plogis(tau[1] - X %*% beta)
  p3 <- plogis(tau[3] - X %*% beta) - plogis(tau[2] - X %*% beta)
  p4 <- 1 - plogis(tau[3] - X %*% beta)
  return(data.frame(p1, p2, p3, p4))
}

num.betas <- length(coef(model.ologit))
num.taus <- length(model.ologit$zeta)


#--------------------------
# Predicted probabilities
#--------------------------
# Extract betas and taus from model
beta.ologit <- coef(model.ologit)
tau.ologit <- model.ologit$zeta

# Predicted probabilities for actual betas and taus
single <- tau.beta(c(beta.ologit, tau.ologit))

# Reshape data for ggplot
single.plot <- cbind(xvar=sample.range, single)
single.plot <- melt(single.plot, id.vars="xvar", variable_name="category")
single.plot$category <- factor(single.plot$category, labels=cat.labels)
single.plot$xvar <- factor(single.plot$xvar, labels=x.labels)


# Set up simulation and extract betas and taus
runs <- 250
draw <- mvrnorm(runs, c(beta.ologit, tau.ologit), vcov(model.ologit))
beta.sim <- draw[,1:num.betas]
tau.sim <- draw[,(num.betas + 1):(num.betas + num.taus)]

# Predicted probabilities for simulated set of betas and taus
simulation.list <- apply(draw, 1, FUN=tau.beta)

# Reshape data for ggplot
plot.data <- cbind(simulation=rep(1:runs, each=length(sample.range)), xvar=rep(sample.range, runs), ldply(simulation.list))
plot.data <- melt(plot.data, id.vars=c("simulation", "xvar"), variable_name="category")
plot.data$category <- factor(plot.data$category, labels=cat.labels)
plot.data$xvar <- factor(plot.data$xvar, labels=x.labels)


# Spaghetti plot of predicted probabilities
ologit.spaghetti <- ggplot()
ologit.spaghetti <- ologit.spaghetti + geom_line(aes(x=xvar, y=value, group=category, colour=category), data=single.plot, size=2) + 
  geom_line(aes(x=xvar, y=value, group=interaction(simulation, category), colour=category), alpha=0.05, data=plot.data, size=1) + 
  # scale_colour_brewer(palette="Set1") +
  scale_colour_manual(values = c("#3182BD", "#9ECAE1", "#FC9272", "#DE2D26")) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1, size=2))) + 
  scale_y_continuous(limits=c(0, 1), labels=percent) + 
  labs(y=y.plot.label, x=x.plot.label, colour=legend.title) + 
  theme(text=element_text(family="Source Sans Pro"))
ologit.spaghetti