######
# create tidfitmean object from chldat using modfitmean

library(devtools)

load_all()

data(chldat)

tidfitmean <- modfitmean(chldat)

save(tidfitmean, file = 'data/tidfitmean.RData')
