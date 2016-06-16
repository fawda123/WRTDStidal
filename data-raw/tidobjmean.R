######
# create tidobjmean object from chldat after running wrtdsmean function

library(devtools)

load_all()

data(chldat)
chldat <- tidalmean(chldat)

tidobjmean <- wrtds(chldat)

save(tidobjmean, file = 'data/tidobjmean.RData', compress = 'xz')
