## Ben Mazzotta
## Gini Coefficients
## Import WDI data
## 10/15/2014

# #####
# 0.  Preliminaries
require(WDI); require(ggplot2); require(Hmisc); require(plyr)


# ##### 
# 1.  Gather GNI and Gini data

v.income <- WDIsearch(string="GNI")[26] ## Income, national, current US$
v.inccap <- WDIsearch(string="GNI")[30] ## Income per capita
v.savingrate <- WDIsearch(string="Gross savings")[4] ## Savings as a percent of income
v.saving <- WDIsearch(string="Gross savings")[2] ## Savings, current US$
v.gini <- WDIsearch("GINI index") ## Gini index, a pure number bounded on (0,1)
v.savers <- WDIsearch("Saved at a financial institution in the past year")[1] ## Savers as a share of population

vars.wdi <- c(v.income, v.inccap, v.savingrate, v.saving, v.gini, v.savers)

income <- WDI(indicator = vars.wdi,start = 2000, end=2012, extra=TRUE)
income <- subset(income, region !="Aggregates")
table(income$region)

str(income)
summary(income)

##      Goal: get a single estimate of income, income per capita, and Gini coefficient by year.

require(data.table)
inc.dt <- data.table(income)

##      Take the averages by country over all 13 years 2000-2012; this is just to test.
inc.dt[, income:= mean(get(v.income), na.rm=T), by="country"]
inc.dt[, inccap:= mean(get(v.inccap), na.rm=T), by="country"]
inc.dt[, gini:= mean(get(v.gini), na.rm=T), by="country"]
inc.dt[, saving:= mean(get(v.saving), na.rm=T), by="country"]
inc.dt[, savrate:= mean(get(v.savingrate), na.rm=T), by="country"]
inc.dt[, savers:= mean(get(v.savers), na.rm=T), by="country"]

##      Make a single year in cross-section, using the scrubbed averages.
inc2010 <- inc.dt[year==2010,.SD, .SDcols=c("iso2c","country","year","income","inccap","gini","saving","savrate", "savers")]
str(inc2010); summary(inc2010)

inc2010[!is.na(gini), country]

# #####
# Save and clean up 

save(inc.dt, file="./data/income_firstcut.Rda")
save.image("./data/current.Rdata")
save.image("./data/working.Rdata")
save.image("./data/archive 20141015.Rdata")
save.image(".Rdata")
