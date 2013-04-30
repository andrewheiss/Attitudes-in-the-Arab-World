#------------------------------------------------------------------
# Figure 8: Predicted probabilities for maintaining order, ologit
#------------------------------------------------------------------
# Build range for varied parameter
sample.range <- min(barometer$maintain.order, na.rm=T):max(barometer$maintain.order, na.rm=T)

# Build new data for prediction
X <- cbind(my.mode(barometer$quran), my.mode(barometer$prime.minister), my.mode(barometer$citizen.influence), sample.range, my.mode(barometer$education), my.mode(barometer$age), my.mode(barometer$family.econ))

# Plot simulated probabilities
fig8 <- ologit.spaghetti(all.countries, X, y.range=sample.range,
                         y.plot.label="Probability of answering\n",
                         legend.title="Opinion of a strong non-democratic leader: ",
                         cat.labels=c("Very good", "Good", "Bad", "Very bad"),
                         x.plot.label="\nDemocracies are not good at maintaining order",
                         x.labels=c("Strongly agree", "Agree", "Disagree", "Strongly disagree"))
fig8