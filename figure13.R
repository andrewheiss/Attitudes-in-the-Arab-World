#--------------------------------------------------------
# Figure 13: Predicted probabilities for gradual change
#--------------------------------------------------------
# Build range for varied parameter
sample.range <- min(barometer$gradual.change, na.rm=T):max(barometer$gradual.change, na.rm=T)

# Build new data for prediction
X <- cbind(my.mode(barometer$quran), my.mode(barometer$prime.minister), my.mode(barometer$citizen.influence), my.mode(barometer$maintain.order), my.mode(barometer$education), my.mode(barometer$age), my.mode(barometer$family.econ), 1, 0, sample.range)

# Plot simulated probabilities
fig13 <- ologit.spaghetti(all.countries.expanded, X, y.range=sample.range,
                 y.plot.label="Probability of answering\n",
                 legend.title="Opinion of a strong non-democratic leader: ",
                 cat.labels=c("Very good", "Good", "Bad", "Very bad"),
                 x.labels=c("Strongly agree", "Agree", "Disagree", "Strongly disagree"),
                 x.plot.label="\nPolitical reform should be introduced gradually")
fig13