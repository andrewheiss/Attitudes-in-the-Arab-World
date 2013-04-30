#------------------
# Expanded models
#------------------
form.expanded <- update(form.ologit, ~ . + trust + association + gradual.change)
all.countries.expanded <- polr(form.expanded, data=barometer,
                               subset=(dem.best==1), method="logistic", Hess=TRUE)
country.models.expanded <- lapply(levels(barometer$country.name), FUN=run.country.models, form=form.expanded)
names(country.models.expanded) <- levels(barometer$country.name)
nice.names.expanded <- c(nice.names, "Trust", "Member of association", "Reform should be gradual")
