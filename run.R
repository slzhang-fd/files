
library(foreign)
dynamic.data <- read.dta(file = "dynamic_ukhls.dta")

# Relabel: Mentioned = 1, Not mentioned = 0
where_resp <- grep("y[a-z]p[0-9]", colnames(dynamic.data))
for (i in where_resp) {
  dynamic.data[, i] <- as.numeric(dynamic.data[, i] == "Mentioned")
}
dynamic.data$count <- ave(dynamic.data$occ, dynamic.data[, c("pidp")],
                          FUN = length)

# Individual id with consecutive codes (this is the index j=1,...,N)
dyn.data$ind <- data.table::rleid(dyn.data$pidp)
head(dyn.data$ind, 20)
# Within-person observation counter (this is the index t=1,...,T)
dyn.data$indob <- sequence(tabulate(dyn.data$ind))
head(dyn.data$indob, 20)
last <- by(dyn.data, dyn.data$pidp, tail, n = 1)
lastd <- do.call("rbind", as.list(last))
T.vec <- lastd$indob

jags_data_joint <- function() {
  y_tp <- dyn.data[, grep("ytp", colnames(dyn.data))][, 1:8]
  y_fp <- dyn.data[, grep("yfp", colnames(dyn.data))][, 1:8]
  x <- dyn.data[, c("age40", "female", "partner")]
  x <- cbind(x, model.matrix(~ -1 + jbs4, data = dyn.data))
  
  list(
    y_tp = as.matrix(y_tp),  # item responses aid to parents
    y_fp = as.matrix(y_fp),  # item responses aid from parents
    NN = nrow(y_tp),
    x = as.matrix(x),
    p = 8,
    q = ncol(x),
    N = max(dyn.data$ind),
    ind = dyn.data$ind,  # index of individuals
    tind = dyn.data$indob,  # index of time
    T.vec = T.vec  # T_1,...,T_N  # max number of records per individual
  )
}
setwd("~/Dropbox/Projs/longitudinal_sem_irini")
source("01-data.R")
library(runjags)

## mod_1.2rev3 original
source("mod1_2_v3.R")
mod_1.2rev3 <- run.jags(mod1_2_v3.jags, n.chains = 8, sample = 10000 / 8, thin = 1,
                        burnin = 8000, adapt = 2000, data = jags_data_joint(),
                        method = "parallel")
## mod_1.2rev4 original
source("mod1_2_v4.R")
mod_1.2rev4 <- run.jags(mod1_2_v4.jags, n.chains = 8, sample = 10000 / 8, thin = 1,
                        burnin = 8000, adapt = 2000, data = jags_data_joint(),
                        method = "parallel")
## mod_1.2rev4_1 fix_1
source("mod1_2_v4_1.R")
mod_1.2rev4_1 <- run.jags(mod1_2_v4_1.jags, n.chains = 8, sample = 10000 / 8, thin = 1,
                        burnin = 8000, adapt = 2000, data = jags_data_joint(),
                        method = "parallel")


summary(mod_1.2rev4)

