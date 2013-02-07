###################################
# Replications of original tables
# www.journalofdemocracy.org/articles/gratis/TesslerGraphics-19-1.pdf
###################################

#-------------------------------------------------
# Table 1
# Personal Religiosity and Support for Democracy
#-------------------------------------------------
all.countries.table <- xtabs(~ dem.best + quran, data=barometer)
round(prop.table(all.countries.table, 2), 2)

individual.countries.table <- xtabs(~ dem.best + quran + country.name, data=barometer)
round(prop.table(individual.countries.table, c(3,2)), 2)

# Alternative
ftable(round(prop.table(xtabs(~ country.name + dem.best + quran, data=barometer), c(3,1)), 2))


#-----------------------------------------------------------
# Table 2
# Binary Logistic Regression Models Estimating Support for
# Secularism among Respondents Who Support Democracy
#-----------------------------------------------------------
# Data = limited to dem.best?

# Dependent variable
# 0 = Favors Islamic democracy; 1 = Favors secular democracy
# Footnote 13 explains why q4013 is the best measure
model <- glm(sec.dem ~ I(-quran) + I(-prime.minister) + 
               I(-citizen.influence) + I(-maintain.order) + 
               education + age + family.econ, 
             data=barometer, subset=(dem.best==1 & country==1), 
             family=binomial(link="logit")) 
summary(model)



#---------------------------------------------------
# Table 3
# Binary Logistic Regression Models Estimating 
# Opposition to a Strong Undemocratic Leader among 
# Respondents Who Support Democracy
#---------------------------------------------------


