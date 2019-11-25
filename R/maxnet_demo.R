library(maxnet)
library(glmnet)
data(bradypus)
p <- bradypus$presence
data <- bradypus[,-1]
mod <- maxnet(p, data)
plot(mod, type="cloglog")
mod <- maxnet(p, data, maxnet.formula(p, data, classes="lq"))
plot(mod, "tmp6190_ann")
