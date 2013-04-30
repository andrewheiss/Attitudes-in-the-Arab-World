#-------------------------------------------------
# Figure 11: Coefficient plot for expanded model
#-------------------------------------------------
coef.plot.expanded <- data.frame(IV=nice.names.expanded, Esitmate=all.countries.expanded$coefficients[1:10], StandardError=summary(all.countries.expanded)$coefficients[,2][1:10], model="Expanded model")
coef.plot.ologit <- data.frame(IV=nice.names, Estimate=all.countries$coefficients[1:7], StandardError=summary(all.countries)$coefficients[,2][1:7], model="Original ordered logit")

rownames(coef.plot.expanded) <- NULL
rownames(coef.plot.ologit) <- NULL
names(coef.plot.expanded) <- names(coef.plot.ologit)

coef.plot.combined.expanded <- rbind(coef.plot.expanded, coef.plot.ologit)
coef.plot.combined.expanded$model <- factor(coef.plot.combined.expanded$model, levels=c("Expanded model", "Original ordered logit"))
coef.plot.combined.expanded$IV <- factor(coef.plot.combined.expanded$IV, levels=nice.names.expanded)

fig11 <- coef.plot.multiple(coef.plot.combined.expanded, c("#1B9E77", "#D95F02"))
fig11