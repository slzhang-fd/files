#install.packages("foreign")
library(foreign)  # to read .dta files from STATA
library(data.table)
library(reshape2)
# setwd("E:/My Computer/Research-Projects/Fiona_ESRC/Fiona/Haziq's files")
setwd("~/Dropbox/Projs/longitudinal_sem_irini")
# Data cleanup -----------------------------------------------------------------
dynamic.data <- read.dta(file = "dynamic_ukhls.dta")
# > dim(dynamic.data)
# [1] 43737    29

# Inspect responses
where_resp <- grep("y[a-z]p[0-9]", colnames(dynamic.data))
for (i in where_resp) {
  print(table(dynamic.data[, i]))
}

# Relabel: Mentioned = 1, Not mentioned = 0
for (i in where_resp) {
  dynamic.data[, i] <- as.numeric(dynamic.data[, i] == "Mentioned")
}

# Delete individuals with only 1 record
dynamic.data$count <- ave(dynamic.data$occ, dynamic.data[, c("pidp")],
                          FUN = length)
dyn.data <- subset(dynamic.data, count > 1)
# > dim(dyn.data)
# [1] 43737   32

# (OPTIONAL) Select a small sub-sample to run the models faster
#set.seed(123)
#pidp.sub <- sample(unique(dyn.data$pidp), size = 500)
#dyn.data <- dyn.data[dyn.data$pidp %in% pidp.sub, ]

# Individual id with consecutive codes (this is the index j=1,...,N)
dyn.data$ind <- data.table::rleid(dyn.data$pidp)

# Within-person observation counter (this is the index t=1,...,T)
dyn.data$indob <- sequence(tabulate(dyn.data$ind))

# Since the number of waves for each respondent is different, we keep track of
# this in a vector
last <- by(dyn.data, dyn.data$pidp, tail, n = 1)
lastd <- do.call("rbind", as.list(last))
T.vec <- lastd$indob
rm(last, lastd)

# Function to create data set for HMM ------------------------------------------
jags_data <- function(response = c("ytp", "yfp")) {
  response <- match.arg(response, c("ytp", "yfp"))
  y <- dyn.data[, grep(response, colnames(dyn.data))][, 1:8]
  y[y == "Mentioned"] <- 1
  # y[y == "Not mentioned"] <- 0
  # y <- apply(y, 2, as.numeric)
  x <- dyn.data[, c("age40", "female", "partner")]
  x <- cbind(x, model.matrix(~ jbs4, data = dyn.data)[, -1])

  list(
    y = as.matrix(y),  # item responses
    NN = nrow(y),
    x = as.matrix(x),
    p = 8,
    q = ncol(x),
    N = max(dyn.data$ind),
    ind = dyn.data$ind,  # index of individuals
    tind = dyn.data$indob,  # index of time
    T.vec = T.vec  # T_1,...,T_N  # max number of records per individual
  )
}

jags_data_joint <- function() {
  y_tp <- dyn.data[, grep("ytp", colnames(dyn.data))][, 1:8]
  y_tp[y_tp == "Mentioned"] <- 1
  y_tp[y_tp == "Not mentioned"] <- 0
  y_tp <- apply(y_tp, 2, as.numeric)
  y_fp <- dyn.data[, grep("yfp", colnames(dyn.data))][, 1:8]
  y_fp[y_fp == "Mentioned"] <- 1
  y_fp[y_fp == "Not mentioned"] <- 0
  y_fp <- apply(y_fp, 2, as.numeric)
  x <- dyn.data[, c("age40", "female", "partner")]
  x <- cbind(x, model.matrix(~ jbs4, data = dyn.data)[, -1])

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
    T.vec = T.vec,  # T_1,...,T_N  # max number of records per individual
    ident_mat_2 = diag(2)
  )
}
