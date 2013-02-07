# Create a more useable subset of the original tab-delimited Arab Barometer file
library(car)

#------------------------
# Import and clean data
#------------------------
barometer.raw <- read.table("ICPSR_26581/DS0001/26581-0001-Data.tsv", sep="\t", header=TRUE)

# 97, 98, and 99 excluded
#is.na(barometer.raw) <- barometer.raw >= 97 & barometer.raw <= 99
barometer.clean <- recode(barometer.raw, '97:99=NA')

# Set working dataset---temporary thing?
barometer <- barometer.clean


#----------------
# New variables
#----------------
barometer$dem.best <- as.factor(recode(barometer$q2324, "1:2=1; 3:4=0"))
barometer$sec.dem <- recode(barometer$q4013, "1:2=0; 3:4=1")
barometer$country.name <- (factor(barometer$country, levels=c(1, 2, 3, 4, 6, 7), labels=c("Jordan", "Palestine", "Algeria", "Morocco", "Lebanon", "Yemen")))

# Rename variables for sake of simplicity
barometer$quran <- barometer$q712
barometer$prime.minister <- barometer$q2011
barometer$citizen.influence <- barometer$q2511
barometer$maintain.order <- barometer$q2323
barometer$education <- barometer$q703
barometer$age <- barometer$q701agecategories
barometer$family.econ <- barometer$q103


# Convenience data frame
barometer.dem.best <- barometer[barometer$dem.best == 1,]
