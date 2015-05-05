######
# create tidobj object from chldat after running wrtds function

library(devtools)

load_all()

data(chldat)
chldat <- tidal(chldat)

tidobj <- wrtds(chldat, tau = c(0.1, 0.5, 0.9))

save(tidobj, file = 'data/tidobj.RData')
