#---------------------------------------------------------------
# Figure 7: Predicted probabilities for Qur'an reading, ologit
#---------------------------------------------------------------
# Build range for varied parameter
sample.range <- min(barometer$quran, na.rm=T):max(barometer$quran, na.rm=T)

# Build new data for prediction
X <- cbind(sample.range, my.mode(barometer$prime.minister), my.mode(barometer$citizen.influence), my.mode(barometer$maintain.order), my.mode(barometer$education), my.mode(barometer$age), my.mode(barometer$family.econ))

# Plot simulated probabilities
fig7 <- ologit.spaghetti(all.countries, X, y.range=sample.range,
                 y.plot.label="Probability of answering\n",
                 legend.title="Opinion of a strong non-democratic leader: ",
                 cat.labels=c("Very good", "Good", "Bad", "Very bad"),
                 x.labels=c("Everyday", "Several times a week", "Sometimes", "Rarely", "I don't read it"),
                 x.plot.label="\nFrequency of Qur'an reading")
fig7