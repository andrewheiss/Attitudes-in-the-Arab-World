#-----------------------------------------------------------
# Figure 12: Grid plot for coefficient t-values (expanded)
#-----------------------------------------------------------
# Create dataframe of t-values for combined model and all individual models
values <- data.frame(rbind(summary(all.countries.expanded)$coefficients[, "t value"],
                           t(sapply(seq(country.models.expanded), 
                                    FUN=function (x) summary(country.models.expanded[[x]])$coefficients[, "t value"]))
))

# Clean up dataframe
num.coefs <- length(attr(terms(form.expanded), "term.labels"))  # number of lhs coefficients
values <- values[, seq(num.coefs)]  # Remove taus; only include actual coefficients
values$country <- c("All countries", levels(barometer$country.name))  # Add column with country names

# Build dataframe for plotting
plot.data <- melt(values, id="country")  # Convert to long
levels(plot.data$variable) <- nice.names.expanded  # Make coefficient names pretty
plot.data$p.value <- pnorm(abs(plot.data$value), lower.tail=FALSE) * 2  # Calculate p-values for t-values
plot.data$stars <- cut(plot.data$p.value, breaks=c(-Inf, 0.001, 0.01, 0.05, Inf), label=c("***", "**", "*", ""))  # Create column of significance labels
plot.data$variable <- factor(plot.data$variable, levels=levels(with(plot.data[plot.data$country=="All countries",], reorder(variable, -value))))  # Sort coefficients by value
plot.data$country <- factor(plot.data$country, levels=levels(with(plot.data[plot.data$variable==levels(plot.data$variable)[1],], reorder(country, -value))))  # Sort country by highest by most significant coefficient

# Plot everything
p <- ggplot(aes(x=country, y=variable, fill=value), data=plot.data)
fig12 <- p + geom_tile() + scale_fill_gradient2(low="#D7191C", mid="white", high="#2C7BB6") + 
  #   geom_text(aes(label=stars, color=value), size=8) + scale_colour_gradient(low="grey30", high="white", guide="none") +
  geom_text(aes(label=stars), color="black", size=5) + 
  labs(y=NULL, x=NULL, fill="t-value") + geom_vline(xintercept=1.5, size=1.5, color="grey50") + 
  theme_bw() + theme(axis.text.x=element_text(angle = -45, hjust = 0))
fig12