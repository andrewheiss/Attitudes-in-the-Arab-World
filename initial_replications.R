###################################
# Replications of original tables
# www.journalofdemocracy.org/articles/gratis/TesslerGraphics-19-1.pdf
###################################

#-------------------------------------------------
# Table 1
# Personal Religiosity and Support for Democracy
#-------------------------------------------------
all.countries.table <- xtabs(~dem.best+q712, data=barometer)
round(prop.table(all.countries.table, 2), 2)

individual.countries.table <- xtabs(~dem.best+q712+country.name, data=barometer)
round(prop.table(individual.countries.table, c(3,2)), 2)

# Alternative
ftable(round(prop.table(xtabs(~country.name+dem.best+q712, data=barometer), c(3,1)), 2))


#-----------------------------------------------------------
# Table 2
# Binary Logistic Regression Models Estimating Support for
# Secularism among Respondents Who Support Democracy
#-----------------------------------------------------------



#---------------------------------------------------
# Table 3
# Binary Logistic Regression Models Estimating 
# Opposition to a Strong Undemocratic Leader among 
# Respondents Who Support Democracy
#---------------------------------------------------




# Question 2324: Democracy better than any other form of government
# Question 2511: Citizens have power to influence government
# Question 2513: People are free to criticize government without fear
# Question 2514: People can join political organizations without fear
# Question 214:  Present political situation in country
# Question 2451: Democratic political system
# Question 2452: Strong non-democratic leader

# Q712: How often do you read the Quran?