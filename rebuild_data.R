#-------------------------
# Load initial libraries
#-------------------------
library(car)


#------------------------
# Import and clean data
#------------------------
setwd("~/Documents/Duke 2012-2013/Spring 2013/Polsci 733/Attitudes in the Arab World Replication")
barometer.raw <- read.table("ICPSR_26581/DS0001/26581-0001-Data.tsv", sep="\t", header=TRUE)

# 97, 98, and 99 excluded
#is.na(barometer.raw) <- barometer.raw >= 97 & barometer.raw <= 99
barometer <- recode(barometer.raw, '97:99=NA')


#----------------
# New variables
#----------------
# Dependent variables - binary
barometer$dem.best <- as.factor(recode(barometer$q2324, "1:2=1; 3:4=0"))  # 4 levels, agree -> disagree
barometer$sec.dem <- recode(barometer$q4013, "1:2=0; 3:4=1")  # 4 levels, agree -> disagree
barometer$autocracy.bad <- as.factor(recode(barometer$q2452, "1:2=0; 3:4=1"))  # 4, good -> bad

# Dependent varialbes - ordinal
barometer$dem.best.ordinal <- factor(barometer$q2324, labels=c("Strongly agree", "Agree", "Disagree", "Strongly disagree"))
barometer$sec.dem.ordinal <- factor(barometer$q4013, labels=c("Strongly agree", "Agree", "Disagree", "Strongly disagree"))
barometer$autocracy.bad.ordinal <- factor(barometer$q2452, labels=c("Very good", "Good", "Bad", "Very bad"))

# Independent variables
barometer$quran <- barometer$q712  # 5 levels, everyday -> never
barometer$prime.minister <- barometer$q2011  # 4 levels, lots -> none
barometer$citizen.influence <- barometer$q2511  # 4 levels, agree -> disagree
barometer$maintain.order <- barometer$q2323  # NEGATIVE: 4 levels, agree -> disagree
barometer$education <- barometer$q703  # 7 levels, illiterate -> masters
barometer$age <- barometer$q701agecategories  # 7 levels, 18-24 -> 75+
barometer$family.econ <- barometer$q103  # 4 levels, very good -> very bad

# Variable to test multinomial regression
barometer$maintain.order.ordinal <- factor(barometer$q2323, labels=c("Strongly agree", "Agree", "Disagree", "Strongly disagree"))

# Variables for extension
barometer$association <- recode(barometer$q202, "1=1; 2=0")
barometer$trust <- recode(barometer$q204, "1=1; 2=0")
barometer$vote <- recode(barometer$q207, "1=1; 2=0")
barometer$rally <- recode(barometer$q210, "1=1; 2=0")


#------------------------
# Create two dataframes
#------------------------
# Subset data to only include the countries in the original article
barometer.original4 <- barometer[(barometer$country==1 | barometer$country==2 | barometer$country==3 | barometer$country==4),]

# Add country names
barometer$country.name <- (factor(barometer$country, levels=c(1, 2, 3, 4, 6, 7), labels=c("Jordan", "Palestine", "Algeria", "Morocco", "Lebanon", "Yemen")))
barometer.original4$country.name <- (factor(barometer.original4$country, levels=c(1, 2, 3, 4), labels=c("Jordan", "Palestine", "Algeria", "Morocco")))