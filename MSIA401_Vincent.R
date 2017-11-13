##
raw_data<-read.csv("catalog sales data.csv",header = T)
summary(raw_data)
head(raw_data)

##split train and test dataset
train<-subset(raw_data, raw_data$train==1)
test<-subset(raw_data,raw_data$train==0)

#train dataset EDA
summary(train)
head(train)
dim(train)

#unique length
colnames(train)
length(unique(train$datead6)) #3038

#table_unqiue_date_year
table_unique_length<-matrix(c(length(unique(train$datead6)),length(unique(train$datelp6)),
                              length(unique(train$lpuryear))),ncol=3, byrow=TRUE)
colnames(table_unique_length) <- c("datead6","datelp6","lpuryear")
table_unique_length <- as.table(table_unique_length)

unique(train$lpuryear)
#2003- 2012

# check how many NAs in each feature
length(which(is.na((train$lpuryear))))
num.NA <- sort(colSums(sapply(train, is.na)))
num.NA
#only lpuryear has NAs. So we are lucky.

##for NA, the first strategy is to elimiate all NAs
train_cleaned<-train[!is.na(train$lpuryear),]
summary(train_cleaned)

# Let's check the date and year
table(train_cleaned$datead6)
table(train_cleaned$datelp6)
table(train_cleaned$lpuryear)


#check the type of date and year
class(train_cleaned$datead6)
train_cleaned$datead6<-as.Date(train_cleaned$datead6,"%m/%d/%Y")
train_cleaned$datelp6<-as.Date(train_cleaned$datelp6,"%m/%d/%Y")

#date plot
barplot(table(train_cleaned$datead6))
barplot(table(train_cleaned$datelp6))
barplot(table(train_cleaned$lpuryear))

barplot(table(train_cleaned[train_cleaned$targdol>0,]$datead6))
barplot(table(train_cleaned[train_cleaned$targdol>0,]$datelp6))
barplot(table(train_cleaned[train_cleaned$targdol>0,]$lpuryear))

#change the name of year
train_cleaned$lpuryear[train_cleaned$lpuryear==3]<-2003
train_cleaned$lpuryear[train_cleaned$lpuryear==4]<-2004
train_cleaned$lpuryear[train_cleaned$lpuryear==5]<-2005
train_cleaned$lpuryear[train_cleaned$lpuryear==6]<-2006
train_cleaned$lpuryear[train_cleaned$lpuryear==7]<-2007
train_cleaned$lpuryear[train_cleaned$lpuryear==8]<-2008
train_cleaned$lpuryear[train_cleaned$lpuryear==9]<-2009
train_cleaned$lpuryear[train_cleaned$lpuryear==0]<-2010
train_cleaned$lpuryear[train_cleaned$lpuryear==1]<-2011
train_cleaned$lpuryear[train_cleaned$lpuryear==2]<-2012
barplot(table(train_cleaned$lpuryear))

#relation between date and life to date orders and sales

with(train_cleaned, plot(datelp6, slshist))
with(train_cleaned, plot(datelp6, ordhist))

#correlations among orders and salarys
library(corrplot)
correlation<-cor(train_cleaned[,c("slstyr","slslyr","sls2ago","sls3ago", 
                                  "slshist","ordtyr","ordlyr","ord2ago","ord3ago","ordhist","falord","sprord","train" )])
corrplot(correlation, method = "square", tl.cex = 1)
corrplot(correlation, method = "square", tl.cex = 1,type = "upper")

correlation_2<-cor(train_cleaned[train_cleaned$targdol>0,c("slstyr","slslyr","sls2ago","sls3ago", 
                                  "slshist","ordtyr","ordlyr","ord2ago","ord3ago","ordhist","falord","sprord","train" )])
corrplot(correlation_2, method = "square", tl.cex = 1)
#some variables have strong correlation



library(tabplot)
tableplot(train_cleaned,select =c("targdol","datead6","datelp6","slstyr","slslyr","sls2ago","sls3ago", 
                                  "slshist","ordtyr","ordlyr","ord2ago","ord3ago","ordhist","falord","sprord","train" ) )

#sanity check
with(train_cleaned, plot(falord + sprord, ordhist))
abline(a=1,b=1,col = "red")
#find out the falord + sprord >80
abnorml_data<-train_cleaned[(train_cleaned$falord+train_cleaned$sprord)>80,]

#lpuryear and datelp6, the year is different
train_cleaned$datelp6<-as.character(train_cleaned$datelp6)
train_cleaned$datelp6_year<-sapply(strsplit(train_cleaned$datelp6,'-'),'[[',1)
train_cleaned$datelp6_year<-as.integer(train_cleaned$datelp6_year)
train_cleaned$lpuryear<-as.integer(train_cleaned$lpuryear)

plot(train_cleaned$lpuryear,train_cleaned$datelp6_year)
abline(a=1,b=1,col = "red")

unique(train_cleaned$datelp6_year)
###It seems that lots of year which is not matched.


