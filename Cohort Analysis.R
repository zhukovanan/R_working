#Download necessary libraries
library(DBI)
library(data.table)
library(odbc)
library(lubridate)
library(ggplot2)
library(scales)
library(magrittr)

#Function for months difference
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
                      lt$year*12 + lt$mon } 
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }

#Create cpnnection to database
con <- dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "SQL002\\MSSQL002", 
                 Database = "report_kr", Trusted_Connection = "True", encoding = "1251")

#DownLoad data
sales <- dbGetQuery(con, "SELECT  orders.PhoneNumber, Number, orders.Date as Date
                          FROM sales left join orders 
                          ON sales.OrderUID = orders.OrderUID
                          WHERE Unit = 'КрасоткаПроТрейд' and Cast(orders.Date as Date) >= '20200101'
                          group by orders.PhoneNumber , Number, orders.Date")

#Try to attain tidy format
sales <- setDT(sales)[ PhoneNumber != "" ][, Date := as.Date(Date)]

#Define data of first customer transaction and frame period clusters based on weeks , months 
sales[, `:=` ( First_time_week = floor_date(min(Date),"weeks", week_start = 1), First_time_month = floor_date(min(Date),"month")), by = PhoneNumber]
sales[,  `:=` (Period_week =  ceiling(as.double(difftime(Date, First_time_week , units = "weeks"))),
               Period_month = mondf(First_time_month, Date))][order(-PhoneNumber)] #Period - number of weeks after the first order

#Form more appropriate cohort for next charts
x <- c("Январь", "Февраль", "Март", "Апрель", "Май", "Июнь", "Июль", "Август", "Сентябрь", "Октябрь", "Ноябрь" , "Декабрь")
sales[, `:=` (Cohort_month = factor(format(First_time_month, "%B"), levels = x), Cohort_week = week(Date))][order(Cohort_month)]

#From wide format data
x <- c("Январь", "Февраль", "Март", "Апрель", "МаЙ", "Июнь", "Июль", "Август", "Сентябрь", "Октябрь", "Ноябрь" , "Декабрь")
MAU.month.grid <- tidyr::pivot_wider(data = sales[order(Period_month, Cohort_month )], Cohort_month , names_from = Period_month, values_from = PhoneNumber, values_fn = uniqueN, values_fill = 0)
#Change column names
colnames(MAU.month.grid)[2] <- c("First")
colnames(MAU.month.grid)[3:13] <- paste("Period", colnames(MAU.month.grid)[3:13], sep ="_")

#Calculate Retention by month cohort
setDT(MAU.month.grid)[, c(3:13) := lapply(.SD, function(x) round(x/First,3)),.SD = c(3:13) ]



