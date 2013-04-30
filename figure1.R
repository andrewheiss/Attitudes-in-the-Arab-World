#-----------------------------
# Figure 1: Coefficient plot
#-----------------------------
original.coefs <- c(.017, .089, .227, .406, .240, .037, -.124)
original.ses <- c(.040, .046, .056, .058, .048, .040, .064)

coef.plot.original <- data.frame(IV=nice.names, Estimate=original.coefs, StandardError=original.ses, model="Published")
coef.plot.original4 <- data.frame(IV=nice.names, Estimate=original4.logit$coefficients[-1], StandardError=summary(original4.logit)$coefficients[-1, 2], model="Original four")
coef.plot.all <- data.frame(IV=nice.names, Estimate=all.countries.logit$coefficients[-1], StandardError=summary(all.countries.logit)$coefficients[-1, 2], model="All in barometer")

rownames(coef.plot.original) <- NULL
rownames(coef.plot.original4) <- NULL
rownames(coef.plot.all) <- NULL

coef.logit.combined <- rbind(coef.plot.original, coef.plot.original4, coef.plot.all)
coef.logit.combined$model <- factor(coef.logit.combined$model, levels=c("Published", "Original four", "All in barometer"))
fig1 <- coef.plot.multiple(coef.logit.combined, c("#1B9E77", "#D95F02", "#7570B3"))
fig1