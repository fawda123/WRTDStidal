######
# create tidobjmean object from chldat after running wrtdsmean function

library(devtools)

load_all()

data(chldat)

tidobjmean <- wrtdsmean(chldat)

save(tidobjmean, file = 'data/tidobjmean.RData')
