#creating philippine's consumption dataframe
coffee = read.csv("Coffee_domestic_consumption.csv")
coffee
coffee_P <- subset(coffee, coffee$Country == 'Philippines')
colnames(coffee_P) <- gsub('X', "", colnames(coffee_P))
coffee_P <- as.data.frame(t(coffee_P))
coffee_P
nrow(coffee_P)
ncol(coffee_P)
colnames(coffee_P) <- c('consumption')
coffee_P
coffee_P$year <- rownames(coffee_P)
rownames(coffee_P) <- NULL
coffee_P <- coffee_P[,c(2,1)]
coffee_P$year <- as.integer(coffee_P$year)
coffee_P <- na.omit(coffee_P)
ncol(coffee_P)
coffee_P <- coffee_P %>% mutate(consumption_in_millions_kg = consumption/1000000)
head(coffee_P)

#creating brazil's consumption dataframe
coffee = read.csv("Coffee_domestic_consumption.csv")
coffee
coffee_A <- subset(coffee, coffee$Country == 'Brazil')
coffee_A
colnames(coffee_A) <- gsub('X', "", colnames(coffee_A))
coffee_A
coffee_A <- as.data.frame(t(coffee_A))
coffee_A
nrow(coffee_A)
ncol(coffee_P)
colnames(coffee_A) <- c('consumption')
coffee_A$year <- rownames(coffee_A)
rownames(coffee_A) <- NULL
coffee_A <- coffee_A[,c(2,1)]
coffee_A$year <- as.integer(coffee_A$year)
coffee_A <- na.omit(coffee_A)
ncol(coffee_A)
coffee_A <- coffee_A %>% mutate(consumption_in_millions_kg = consumption/1000000)
head(coffee_A)

#combining data frames and taking svd
coffee <- as.data.frame(c(coffee_P, coffee_A))
coffee
coffee = coffee[,c(1:2,4)]
colnames(coffee) = c('Year', 'P Consumption', 'B Consumption')
coffee
is.na(coffee)
class(coffee)
coffee
coffee = as.data.frame(sapply(coffee, as.numeric))
coffee = coffee[,2:3]
svd(coffee)

#brazil population data frame
bpop = read.csv("brazil_population.csv")
bpop = bpop[1:10,1:2]
bpop
yrs = as.data.frame(bpop[order(nrow(bpop):1),1])
nums = as.data.frame(bpop[order(nrow(bpop):1),2])
bpop = as.data.frame(c(yrs,nums))
colnames(bpop) = c('years', 'population')
bpop

#philippine population data frame
ppop = read.csv("population.csv")
ppop = ppop[1:29, 1:2]
ppop
ppop = ppop[c(1,5,10,15,20,25,26,27,28,29),]
nrow(ppop)

#combining population data frames
pop = as.data.frame(c(ppop, bpop))
pop = pop[,c(1,2,4)]
colnames(pop) = c('year', 'philippines pop', 'brazil pop')
pop = as.data.frame(pop)
years = c('1990', '1995', '2000', '2005', '2010', '2015', '2016', '2017', '2018', '2019')
ppop = (ppop$population)/10000000
bpop = (bpop$population)/10000000
length(ppop)
length(bpop)
#graphing population v year
require(zoo)
set.seed(200)                                             
df <- data.frame(x = years,
                 y1 = ppop,
                 y2 = bpop)
plot(pop$year, df$y1, type = "o", col = 1, ylim = c(0, 30),
     xlab = 'Year',
     ylab = 'Population in 10 million kgs',
     main = 'Population vs Years') 
lines(pop$year, df$y2, type = "o", col = 2)
legend("topright", legend = c("Philippines", "Brazil"),
       col = c(1, 2), lty = 1, cex = 0.8)

#export charts
exb = read.csv('Coffee_export.csv')
colnames(exb) <- gsub('X', "", colnames(exb))
exb = subset(exb, exb$Country == 'Brazil')
as.data.frame(t(exb))
rownames(exb) <- c('Brazil_Export')
nrow(exb)
class(exb)
exb = t(exb[,2:32])
exb = as.data.frame(exb)

exp = subset(exb, exb$Country == 'Philippines')
exp
rownames(exb) <- gsub('X', "", rownames(exb))
as.data.frame(t(exp))
rownames(exp) <- c('Philippines_Export')
nrow(exp)
exp = t(exp[,2:32])
exp = as.data.frame(exp)
exp

#barplot
barplot(exb$Brazil_Export, xlab = "Years", ylab = "Exports", main = "Brazil Exports")
barplot(exp$Philippines_Export, xlab = "Years", ylab = "Exports", main = "Philippine Exports")


#Brazil imports
imb = read.csv('Coffee_Time Series.csv')
ncol(imb)
nrow(imb)
imb = ts(imb, start = c(1990,1), frequency = 12)
imb = as.data.frame(imb[1:372,2])
imb
imb = rowsum(imb, rep(1:31, each = 12))
imb
colnames(imb) = c('Brazil Imports')
rownames(imb) = c(1990:2020)
imb

git config --global user.email "isabellemckesson@gmail.com"
git config --global user.name "Isabelle Mckesson"

