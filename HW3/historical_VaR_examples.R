##########################
####### LECTURE 2 ########
##########################

library(data.table)

# load data

data <- fread("indices.csv")

# compute returns (P_t - P_{t-1}) / P_{t-1} )
data[,sp:= (SP500 -shift(SP500, 1L))/shift(SP500, 1L)]
data[,nas:= (NASDAQ -shift(NASDAQ, 1L))/shift(NASDAQ, 1L)]
data[,dj:= (DJIA -shift(DJIA, 1L))/shift(DJIA, 1L)]

data = na.omit(data)

# create portfolio gains and losses (in million dollars)
data[, gain := 4 * sp + 5 * nas + 1 * dj]

# compute VaR
data_sorted = data[order(data$gain),]

c = 0.99
rank_worst = ceiling((1-c) * nrow(data_sorted))

worst_case = data_sorted$gain[rank_worst]
VaR = -worst_case

worst_case_bis = quantile(data$gain, c(1-c), type = 1) # alternative


# compute Expected Shortfall
ES = - mean(data_sorted$gain[1:rank_worst])


#----------------
# Exponentially-weighted VaR
#----------------
lambda = 0.995

n_obs = length(data$gain)
weight = lambda ^ (n_obs - (1:n_obs))  # exponential decay
weight = weights / sum(weights)        # probabilities add up to 1

# weight = 1 / n_obs # if you want regular historical VaR


data[, weights := weight]

# construct the cdf
data_sorted = data[order(data$gain),]
data_sorted[, cdf := cumsum(weights)]

# cdf cross 1%:
worst_case_index = which(data_sorted$cdf > 1-c)[1]
worst_case_ew = data_sorted$gain[worst_case_index]
VaR_ew = - worst_case_ew
