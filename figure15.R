#-------------------------------------------------------
# Figure 15: Predicted probabilities for high extremes
#-------------------------------------------------------
# Build range for varied parameter
sample.range <- min(barometer$maintain.order, na.rm=T):max(barometer$maintain.order, na.rm=T)

# Build new data for prediction
X <- cbind(my.mode(barometer$quran), min(barometer$prime.minister, na.rm=T), min(barometer$citizen.influence, na.rm=T), sample.range, max(barometer$education, na.rm=T), my.mode(barometer$age), my.mode(barometer$family.econ), 0, 1, min(barometer$gradual.change, na.rm=T))

# Plot simulated probabilities
fig15 <- ologit.spaghetti(all.countries.expanded, X, y.range=sample.range,
                 y.plot.label="Probability of answering\n",
                 legend.title="Opinion of a strong non-democratic leader: ",
                 cat.labels=c("Very good", "Good", "Bad", "Very bad"),
                 x.labels=c("Strongly agree", "Agree", "Disagree", "Strongly disagree"),
                 x.plot.label="\nDemocracies are not good at maintaining order",
                 y.limit=(c(0,1)))
fig15