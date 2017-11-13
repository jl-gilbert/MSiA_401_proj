sales_org <- read.csv("catalog sales data.csv")
sales <- read.csv("catalog sales data.csv")

head(sales)

dol <- sales[sales$targdol>0,]

library(stringr)
sales$year<-str_sub(sales$datelp6,-4,-1)
head(sales)


# check if the order matches with sales
this <- ((sales$ordtyr > 0 & sales$slstyr > 0) | (sales$ordtyr == 0 & sales$slstyr == 0))
sum(!this)

vio_this <- sales[which(!this),]
sum(vio_this$slstyr)


lst <- ((sales$ordlyr > 0 & sales$slslyr > 0) | (sales$ordlyr == 0 & sales$slslyr == 0))
sum(!lst)
sum((sales[which(!lst),])$slslyr)

ago2 <- ((sales$ord2ago > 0 & sales$sls2ago > 0) | (sales$ord2ago == 0 & sales$sls2ago == 0))
sum(!ago2)
sum((sales[which(!ago2),])$sls2ago)

ago3 <- ((sales$ord3ago > 0 & sales$sls3ago > 0) | (sales$ord3ago == 0 & sales$sls3ago == 0))
sum(!ago3)
sum((sales[which(!ago3),])$sls3ago)


# check if last purchase agrees with order year
ord_this <- ((sales$year == 2012 & sales$ordtyr > 0) | (sales$year < 2012 & sales$ordtyr == 0))
sum(!ord_this)
sales[(!ord_this),]

# check if falord+sprord = ordhist
sales$ord_sum <- sales$falord+sales$sprord
sum(sales$ord_sum != sales$ordhist)
sum(sales$ord_sum < sales$ordhist)
sales[(sales$ord_sum != sales$ordhist),]
