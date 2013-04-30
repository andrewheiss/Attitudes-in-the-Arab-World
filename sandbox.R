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
library(reshape2)
library(xtable)
library(MIfuns)
source('../separationplot.R')

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



#-----------------------------------------------------------------
# Save ggplot plots with Cairo device and transparent background
#-----------------------------------------------------------------
ggsave(filename, p, cairo_pdf, height=4.5, width=7, bg="transparent")  # logit graphs
ggsave(filename, p, cairo_pdf, height=5.5, width=7, bg="transparent")  # ologit graphs



#-------------------------
# Multinomial regression
#-------------------------
# Set up
library(car)
library(ggplot2)
library(reshape)
library(plyr)
library(nnet)

set.seed(1234)

model.mnl <- multinom(autocracy.bad.ordinal ~ quran + prime.minister + 
                        citizen.influence + maintain.order.ordinal + 
                        education + age + family.econ + association + trust, 
                      data=barometer.original4, 
                      subset=(dem.best==1), Hess=TRUE, model=TRUE)
summary(model.mnl)

sample.range <- c("Strongly agree", "Agree", "Disagree", "Strongly disagree")
autocracy.levels <- c("Very good", "Good", "Bad", "Very bad")

X <- data.frame(my.mode(barometer$quran), my.mode(barometer$prime.minister), my.mode(barometer$citizen.influence), sample.range, 1, my.mode(barometer$age), my.mode(barometer$family.econ), 0, 0)

# X <- cbind(my.mode(barometer$quran), my.mode(barometer$prime.minister), my.mode(barometer$citizen.influence), sample.range, 1, my.mode(barometer$age), my.mode(barometer$family.econ), 0, 0)


colnames(X) <- names(model.mnl$model)[-1]

single.mnl <- predict(model.mnl, newdata=X, type="probs")

rownames(single.mnl) <- sample.range
(plot.data <- melt(single.mnl))
plot.data$X1 <- factor(plot.data$X1, levels=sample.range, ordered=TRUE)
plot.data$X2 <- factor(plot.data$X2, levels=autocracy.levels, ordered=TRUE)

mnl.spaghetti <- ggplot()
mnl.spaghetti + geom_line(aes(x=X1, y=value, group=X2, colour=X2), data=plot.data, size=2) + 
  scale_colour_brewer(palette="Set1") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size=2))) + 
  ylim(0, 1) + 
  labs(y="Probability of answering\n", x=x.plot.label, colour="Autocracy")

