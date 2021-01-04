#Load library
library(data.table)
library(ggplot2)
#Load data
data <- ChickWeight 
#Check of assumptions
#Normality
setDT(data)[, sapply(.SD, function(x) shapiro.test(x)$p.value), .SDcols = 'weight' , by = Diet] 
#Var equality
bartlett.test(weight ~ Diet, data = data)
#Test wilcox.test for 2 diets
wilcox.test(weight ~ Diet , data[Diet %in% c('1','2')])

#Bootstraping  approach
set.seed(1234) 

#Create new dataset
dataset <- data[Diet %in% c('1','2')]

Number <- 15000 #the number of bootstrap samples
p.value <- vector()

Boots_trap <- function(x) {
  one_vec <- sample(dataset$weight[dataset$Diet == '1'] , length(dataset$weight[dataset$Diet == '1']), replace = TRUE)
  two_vec <- sample(dataset$weight[dataset$Diet == '2'] , length(dataset$weight[dataset$Diet == '2']), replace = TRUE)
  p.value <<- c(p.value , wilcox.test(one_vec , two_vec)$p.value
)
}

#Activate Bootstrap 
sapply(1:Number , function(x) Boots_trap(x))
#Median of p.value
median(p.value)
#Plot Resuts
ggplot() +
  geom_density(aes(p.value))
