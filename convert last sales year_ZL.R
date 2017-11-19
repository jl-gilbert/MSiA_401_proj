sales_org <- read.csv("catalog sales data.csv")
sales <- read.csv("catalog sales data.csv")


# convert to date
sales$datead6 <- as.Date(sales$datead6,format = "%m/%d/%Y")
sales$datelp6 <- as.Date(sales$datelp6,format = "%m/%d/%Y")


# convert lpuryear to full year
sales$lpuryear[sales$lpuryear == 3] = 2003
sales$lpuryear[sales$lpuryear == 4] = 2004
sales$lpuryear[sales$lpuryear == 5] = 2005
sales$lpuryear[sales$lpuryear == 6] = 2006
sales$lpuryear[sales$lpuryear == 7] = 2007
sales$lpuryear[sales$lpuryear == 8] = 2008
sales$lpuryear[sales$lpuryear == 9] = 2009
sales$lpuryear[sales$lpuryear == 0] = 2010
sales$lpuryear[sales$lpuryear == 1] = 2011
sales$lpuryear[sales$lpuryear == 2] = 2012


# order year
sales$lordyear[sales$ordtyr > 0] = 2012
sales$lordyear[sales$ordtyr == 0 & sales$ordlyr > 0] = 2011
sales$lordyear[sales$ordtyr == 0 & sales$ordlyr == 0 & sales$ord2ago > 0] = 2010
sales$lordyear[sales$ordtyr == 0 & sales$ordlyr == 0 & sales$ord2ago == 0 & sales$ord3ago > 0] = 2009



# Covnert datelp6
library(lubridate)
for(i in 1:101532){
  if(as.numeric(format(sales$datelp6[i],"%m")) > 6){
    sales$lpyear[i] = as.numeric(format(sales$datelp6[i],"%Y")) + 1
  }
  else{
    sales$lpyear[i] = as.numeric(format(sales$datelp6[i],"%Y"))
  }
}


# Final year 
# lpuryear == lordyear == lpyear
sales$year = c(NA)
loc <- which(sales$lpuryear == sales$lordyear & sales$lpuryear == sales$lpyear & !is.na(sales$lpuryear) & !is.na(sales$lordyear))
sales$year[loc] <- sales$lordyear[loc]

# lordyear == lpyear | lordyear == lpuryear, then lordyear
loc2 <- which(sales$lpuryear == sales$lordyear | sales$lordyear == sales$lpyear & !is.na(sales$lordyear))
diff <- loc2[!loc2 %in% loc] #exclude
sales$year[diff] <- sales$lordyear[diff]


# choose the most recent purchase year from lpuear and lordyear
loc3 <- which(sales$lordyear != sales$lpyear & !is.na(sales$lpyear) & !is.na(sales$lordyear))
loc4 <- which(sales$lordyear > sales$lpyear)
diff1 <- loc4[(loc4 %in% loc3) & !(loc4 %in% loc2)]

sum(diff1 %in% loc2) #double check exclusion
sum(diff1 %in% loc) #double check exclusion

sales$year[diff1] <- sales$lordyear[diff1]


loc5 <- which(sales$lordyear < sales$lpyear)
diff2 <- loc5[(loc5 %in% loc3) & !(loc5 %in% loc2)]

sum(diff2 %in% loc2) #double check exclusion
sum(diff2 %in% loc) #double check exclusion

sales$year[diff2] <- sales$lpyear[diff2]


# check if lordyear == na for all the missing year
ordyear.na <- which(is.na(sales$lordyear))
year.na <- which(is.na(sales$year))
sum(!ordyear.na %in% year.na)
sum(!year.na %in% ordyear.na)







# recode the rest
loc6 <- which(as.numeric(format(sales$datelp6,"%Y")) == sales$lpuryear)
loc6 <- loc6[loc6 %in% ordyear.na]
sum(!is.na(sales$year[loc6])) # check exclusion
sales$year[loc6] <- sales$lpuryear[loc6]


loc7 <- which(as.numeric(format(sales$datelp6,"%Y")) > sales$lpuryear)
year.na <- which(is.na(sales$year))
loc7 <- loc7[loc7 %in% year.na]
sum(!is.na(sales$year[loc7]))  # check exclusion
sales$year[loc7] <- as.numeric(format(sales$datelp6[loc7],"%Y"))

loc8 <- which(as.numeric(format(sales$datelp6,"%Y")) < sales$lpuryear)
year.na <- which(is.na(sales$year))
loc8 <- loc8[loc8 %in% year.na]
# no matching cases


year.na <- which(is.na(sales$year))
sales$year[year.na] <- as.numeric(format(sales$datelp6[year.na],"%Y"))

sum(is.na(sales$year)) # year value is clean


temp<-sales[is.na(sales$year),]
View(temp)

View(sales)
