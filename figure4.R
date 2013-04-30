#----------------------------------------------------------
# Figure 4: Predicted probabilities for maintaining order
#----------------------------------------------------------
# Build range for varied parameter
sample.range <- min(barometer$maintain.order, na.rm=T):max(barometer$maintain.order, na.rm=T)

# Build new data for prediction
X <- cbind(my.mode(barometer$quran), my.mode(barometer$prime.minister), my.mode(barometer$citizen.influence), sample.range, my.mode(barometer$education), my.mode(barometer$age), my.mode(barometer$family.econ))

# Plot simulated probabilities
fig4 <- logit.spaghetti(all.countries.logit, cbind(1, X), y.range=sample.range, 
                y.plot.label="Probability of thinking autocracy is bad\n", 
                x.plot.label="\nDemocracies are not good at maintaining order",
                x.labels=c("Strongly agree", "Agree", "Disagree", "Strongly disagree"),
                runs=250)
fig4