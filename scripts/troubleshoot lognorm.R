## Troubleshoot cumulative income shares

## Ben Mazzotta
## Using inc2010


##    Setup
setwd("data")
load("modeled saving of finexcl.Rda")

##    Libraries
require(data.table); require(plyr); require(ggplot2)

#     Throw out messy data; keep those where we at least have a Gini measure.
income <- subset(inc2010, !is.na(gini))

##      Calculate means and variances
income[,mu:=inccap]
income[,sigma:=exp(sqrt(2)*qnorm((gini/100+1)/2))]


summary(income)


## Now replicate the income calculations

## Income cutoffs:
##      The inverse lognormal distribution using .20, mu and sigma.
income[,inc.p20 := qlnorm(.2, meanlog=log(mu), sdlog=log(sigma))]
income[,inc.psav := qlnorm(psav, meanlog=log(mu), sdlog=log(sigma))]

head(income)

## Income shares
##      

## Candidate from Wikipedia

# cumulative income share at percentile B = h(B) 
# h <-  exp(mu + sigma^2/2) * \Phi (log(B) - (mu+sigma^2)/sigma)

income[,inc.sh20 := exp(mu + sigma^2/2) * qnorm((log(.20)-(mu+sigma^2))/sigma)]
income[,inc.shsav := exp(mu + sigma^2/2) * qnorm((log(psav)-(mu+sigma^2))/sigma)]

head(income)
summary(income)

### Simulate conditional means and sums.

##      A lognormal distribution with mean 8000 and sd 2.2
sim.income <- data.table(rlnorm(1000, log(8000), log(2.2)))
setnames(sim.income, "V1","inc.i")

##      Mean and SD
sim.uncond <- mean(sim.income$inc.i)
sim.sd <- sd(sim.income$inc.i)

##      Define quintile cutoffs
sim.quintiles <- qlnorm(c(.2, .4, .6, .8, .999), log(8000), log(2.2))

##      Estimate conditional means of income beneath quintile cutoffs
sim.condq1 <- sim.income[inc.i < sim.quintiles[1], mean(inc.i)]
sim.condq2 <- sim.income[inc.i < sim.quintiles[2], mean(inc.i)]
sim.condq3 <- sim.income[inc.i < sim.quintiles[3], mean(inc.i)]
sim.condq4 <- sim.income[inc.i < sim.quintiles[4], mean(inc.i)]
sim.condq5 <- sim.income[inc.i < sim.quintiles[5], mean(inc.i)]

cat("The average income in the bottom quintile is ",sim.condq1,".")
cat("The average income in the bottom two quintiles is ",sim.condq2,".")
cat("The average income in the bottom three quintiles is ",sim.condq3,".")
cat("The average income in the bottom four quintiles is ",sim.condq4,".")
cat("The average income in the bottom five quintiles is ",sim.condq5,"...")
cat("with three digit accuracy and simulated population N=1000, compared to the unconditional mean",sim.uncond,".")
summary(sim.income)
