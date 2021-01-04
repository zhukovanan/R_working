#Download necessary libraries
library(DBI)
library(data.table)
library(odbc)
library(lubridate)
library(ggplot2)
library(scales)
library(mgcv)
library(magrittr)

#Create connection to database
con <- dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "####", 
                 Database = "####", Trusted_Connection = "True", encoding = "1251")
#Load sales data
#Check field of tables
dbListFields(con, "sales")

sales <- dbGetQuery(con, "SELECT sum(sales.SaleSum) as Revenue , sum(RewardSumAll) as Profit , orders.PhoneNumber, Number, orders.Date as Date
                          FROM sales left join orders 
                          ON sales.OrderUID = orders.OrderUID
                          WHERE Unit = 'КрасоткаПроТрейд' 
                          group by orders.PhoneNumber , Number, orders.Date")

#Remove all mess and form the correct table
sales <- setDT(sales)[Revenue > 0  & Profit >0 & PhoneNumber != "" ][, Date := as.Date(Date)][, list(Revenue = sum(Revenue), Profit = sum(Profit)), by = list(PhoneNumber,Date)]

#Define data of first customer transaction and frame period clusters
sales[, First_time := floor_date(min(Date),"weeks", week_start = 1), by = PhoneNumber]
sales[, Period := floor(as.double(difftime(Date, First_time , units = "weeks")))][order(-PhoneNumber, Period)] #Period - number of weeks after the first order

#Calculate size of historical cohort
cohort_value <- sales[, list(Size = uniqueN(PhoneNumber)), by = First_time]

#Calculate cohort value regarding retention, revenue and orders
cohort_profit <- sales[, list(User_retention = uniqueN(PhoneNumber), Profit = sum(Profit), Orders = .N), by = list(First_time, Period)]
cohort_profit <- merge(cohort_profit , cohort_value, by = 'First_time')

#Get statistics AGMPU, Retention , Orders for user in every cohort
cohort_profit[, `:=`(Retention = User_retention/Size, AGMPU = Profit/User_retention, Order = ceiling(Orders/User_retention ))]

#Average statistics calculation for historical cohort
cohort_stat <- cohort_profit[, list(Average_AGMPU = median(AGMPU),
                                    Average_Retention = median(Retention),
                                    Average_Order = median(Order)), by = Period]


#Visualization 
#Average Retention Plot
ggplot(cohort_stat , aes(Period, Average_Retention)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = mean(cohort_stat$Average_Retention) , alpha = 0.3, linetype = "dashed")+
  xlim(1,max(cohort_stat$Period)) +
  scale_y_continuous(name = "Average Retention",labels = percent, breaks = seq(0, 0.1, by = 0.015) ,limits = c(0,0.10)) + 
  ggtitle("Average Retention for historical cohorts") +
  theme_minimal()

#Average AGMPU Plot
ggplot(cohort_stat , aes(Period, Average_AGMPU)) +
  geom_line(color = "green") +
  geom_hline(yintercept = mean(cohort_stat$Average_AGMPU) , alpha = 0.5, linetype = "dashed")+
  xlim(1,max(cohort_stat$Period)) +
  scale_y_continuous(name = "Average AGMPU") +
  ggtitle("Average AGMPU for historical cohorts") +
  theme_minimal()

#Use Generalized additive models for AGMPU and Retention approximation

fit_retention <- gam(Average_Retention ~ s(Period) , data = cohort_stat)
fit_AGMPU <- gam(Average_AGMPU ~ s(Period) , data = cohort_stat)
cohort_stat[, `:=` (Retention_forecast = predict(fit_retention, Period = cohort_stat$Period),
                   AGMPU_forecast = predict(fit_AGMPU, Period = cohort_stat$Period))]

#Plot approxamation
ggplot(cohort_stat , aes(Period,Average_Retention)) +
  geom_line(color = "green") +
  geom_line(aes(y = Retention_forecast), col = "red")+
  xlim(1,max(cohort_stat$Period)) +
  scale_y_continuous(name = "Average Retention",labels = percent, breaks = seq(0, 0.1, by = 0.015) ,limits = c(0,0.10)) + 
  ggtitle("Retention approximation") +
  theme_minimal()

ggplot(cohort_stat , aes(Period,Average_AGMPU)) +
  geom_line(color = "green") +
  geom_line(aes(y =  AGMPU_forecast), col = "purple")+
  xlim(1,max(cohort_stat$Period)) +
  ggtitle("AGMPU Approximation") +
  theme_minimal()

#Ltv Prediction  for 52 weeks
prediction <- data.table(Period = 0:52)
prediction[,  `:=` (AGMPU_pred = predict(fit_AGMPU ,newdata  = prediction ),
                    Retention_pred = predict(fit_retention ,newdata  = prediction))]

#Final Ltv
Ltv <- prediction[, .(Ltv = sum(AGMPU_pred*abs(Retention_pred)))] %>% dplyr::pull()
