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
