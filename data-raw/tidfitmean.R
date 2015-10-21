######
# create tidfitmean object from chldat using modfitmean

library(devtools)

load_all()

data(chldat)

tidfitmean <- modfit(chldat, resp_type = 'mean')

save(tidfitmean, file = 'data/tidfitmean.RData', compress = 'xz')
