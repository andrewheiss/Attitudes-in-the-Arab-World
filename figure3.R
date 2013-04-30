#-------------------------------------------------------
# Figure 3: Predicted probabilities for Qur'an reading
#-------------------------------------------------------
# Build range for varied parameter
sample.range <- min(barometer$quran, na.rm=T):max(barometer$quran, na.rm=T)

# Build new data for prediction
X <- cbind(sample.range, my.mode(barometer$prime.minister), my.mode(barometer$citizen.influence), my.mode(barometer$maintain.order), my.mode(barometer$education), my.mode(barometer$age), my.mode(barometer$family.econ))

# Plot simulated probabilities
fig3 <- logit.spaghetti(all.countries.logit, cbind(1, X), y.range=sample.range, 
                y.plot.label="Probability of thinking autocracy is bad\n", 
                x.plot.label="\nFrequency of Qur'an reading",
                x.labels=c("Everyday", "Several times a week", "Sometimes", "Rarely", "I don't read it"),
                runs=250)
fig3