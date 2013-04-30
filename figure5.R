#----------------------------------------
# Figure 5: Coefficient plot for ologit
#----------------------------------------
coef.plot.logit <- data.frame(IV=nice.names, Esitmate=all.countries.logit$coefficients[-1], StandardError=summary(all.countries.logit)$coefficients[-1,2], model="Logit")
coef.plot.ologit <- data.frame(IV=nice.names, Estimate=all.countries$coefficients[1:7], StandardError=summary(all.countries)$coefficients[,2][1:7], model="Ordered logit")

rownames(coef.plot.logit) <- NULL
rownames(coef.plot.ologit) <- NULL
names(coef.plot.logit) <- names(coef.plot.ologit)

coef.plot.combined <- rbind(coef.plot.logit, coef.plot.ologit)
coef.plot.combined$model <- factor(coef.plot.combined$model, levels=c("Logit", "Ordered logit"))

fig5 <- coef.plot.multiple(coef.plot.combined, c("#1B9E77", "#D95F02"))
fig5