######
# create tidfit object from chldat using modfit

library(devtools)

load_all()

data(chldat)

tidfit <- modfit(chldat, tau = c(0.1, 0.5, 0.9))

save(tidfit, file = 'data/tidfit.RData', compress = 'xz')
