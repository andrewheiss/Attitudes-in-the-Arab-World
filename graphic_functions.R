#------------
# Functions
#------------
library(ggplot2)
library(reshape2)
library(plyr)
library(scales)
library(grid)

coef.plot.multiple <- function(coef.plot.data, colors) {
  multiplier <- qnorm(1 - 0.05 / 2)
  p <- ggplot(aes(x=IV, y=Estimate), data=coef.plot.data)
  p <- p + geom_pointrange(aes(ymin=Estimate - qnorm(1 - 0.05 / 2) * StandardError, ymax=Estimate + qnorm(1 - 0.05 / 2) * StandardError, group=model, colour=model), size=.75, position=position_dodge(width=.50)) + 
    geom_hline(yintercept=0, colour="red4", alpha=0.6, size=1) + coord_flip() + labs(x=NULL, y=NULL, title=NULL) + 
    scale_colour_manual(values=colors) + 
    labs(y="\nLog odds", x=NULL, title=NULL, colour="Models: ") + 
    theme_bw() + 
#     theme(text=element_text(family="Source Sans Pro", size=13)) + 
    theme(plot.background=element_rect(fill=NA, colour=NA), legend.position="top", legend.key=element_blank(), legend.background=element_rect(fill=NA, colour=NA), legend.margin=unit(0,"cm"), legend.key.height=unit(0.5,"lines"))
  p
}

logit.spaghetti <- function(model, X, y.range, y.plot.label, x.labels, x.plot.label, runs=500) {
  # Get predicted probabilities for given coefficients
  predict.probs <- function(beta) {
    probs.raw <- X %*% beta
    probs <- 1 / (1 + exp(-probs.raw))
    return(probs)
  }
  
  # Predicted probabilities for actual coefficients
  single <- predict.probs(coef(model))
  single.plot <- data.frame(value=single)
  single.plot$xvar <- y.range
  
  
  # Set up simulation
  draw <- mvrnorm(runs, coef(model), vcov(model))
  
  # Predicted probabilities for simulated set of coefficients
  simulation.list <- apply(draw, 1, FUN=predict.probs)
  plot.data <- melt(simulation.list, varnames=c("xvar", "simulation"))
  
  
  # Spaghetti plot of predicted probabilities
  p <- ggplot()
  p <- p + geom_line(aes(x=xvar, y=value), data=single.plot, size=2, colour="#D7191C") + 
    geom_line(aes(x=xvar, y=value, group=simulation), data=plot.data, colour="#D7191C", size=1, alpha=0.05) + 
    scale_y_continuous(limits=c(0.5, 1), labels=percent) + 
    labs(title=NULL, y=y.plot.label, x=x.plot.label) + 
    scale_x_continuous(labels=x.labels) +
    theme_bw() + 
#     theme(text=element_text(family="Source Sans Pro", size=13)) + 
    theme(plot.background=element_rect(fill=NA, colour=NA))
  p
}

ologit.spaghetti <- function(model, X, y.range, y.plot.label, legend.title, cat.labels, x.labels, x.plot.label, runs=500, y.limit=c(0, 0.75)) {
  library(ggplot2)
  library(reshape2)
  library(plyr)
  library(scales)
  
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
  
  num.betas <- length(coef(model))
  num.taus <- length(model$zeta)
  
  
  #--------------------------
  # Predicted probabilities
  #--------------------------
  # Extract betas and taus from model
  beta.ologit <- coef(model)
  tau.ologit <- model$zeta
  
  # Predicted probabilities for actual betas and taus
  single <- tau.beta(c(beta.ologit, tau.ologit))
    
  # Reshape data for ggplot
  single.plot <- cbind(xvar=y.range, single)
  single.plot <- melt(single.plot, id.vars="xvar", variable.name="category")
  single.plot$category <- factor(single.plot$category, labels=cat.labels)
  single.plot$xvar <- factor(single.plot$xvar, labels=x.labels)
  
  # Set up simulation and extract betas and taus
  draw <- mvrnorm(runs, c(beta.ologit, tau.ologit), vcov(model))
  beta.sim <- draw[,1:num.betas]
  tau.sim <- draw[,(num.betas + 1):(num.betas + num.taus)]
  
  # Predicted probabilities for simulated set of betas and taus
  simulation.list <- apply(draw, 1, FUN=tau.beta)
  
  # Reshape data for ggplot
  plot.data <- cbind(simulation=rep(1:runs, each=length(y.range)), xvar=rep(y.range, runs), ldply(simulation.list))
  plot.data <- melt(plot.data, id.vars=c("simulation", "xvar"), variable.name="category")
  plot.data$category <- factor(plot.data$category, labels=cat.labels)
  plot.data$xvar <- factor(plot.data$xvar, labels=x.labels)
  
  head(plot.data)
  # Spaghetti plot of predicted probabilities
  p <- ggplot()
  p <- p + geom_line(aes(x=xvar, y=value, group=category, colour=category), data=single.plot, size=2) + 
    geom_line(aes(x=xvar, y=value, group=interaction(simulation, category), colour=category), alpha=0.05, data=plot.data, size=1) + 
    # scale_colour_brewer(palette="Set1") +
    scale_colour_manual(values = c("#2C7BB6", "#ABD9E9", "#FDAE61", "#D7191C")) + 
    guides(colour = guide_legend(override.aes = list(alpha = 1, size=2))) + 
    scale_y_continuous(labels=percent) + 
    coord_cartesian(ylim=y.limit) + 
    labs(y=y.plot.label, x=x.plot.label, colour=legend.title) + 
    theme_bw() + 
#     theme(text=element_text(family="Source Sans Pro", size=13)) + 
    theme(plot.background=element_rect(fill=NA, colour=NA), legend.position="top", legend.key=element_blank(), legend.background=element_rect(fill=NA, colour=NA))
  p
}

hist.spark <- function(var) {
  qplot(var) + 
    theme(panel.background=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          plot.margin=unit(c(0,0,0,0), "lines")) +
    labs(title=NULL, y=NULL, x=NULL)
}