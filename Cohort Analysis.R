#Download necessary libraries
library(DBI)
library(data.table)
library(odbc)
library(lubridate)
library(ggplot2)
library(scales)
library(magrittr)
library(DT)

#Function for months difference
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
                      lt$year*12 + lt$mon } 
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }

#Create connection to database
con <- dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "SQL002\\MSSQL002", 
                 Database = "report_kr", Trusted_Connection = "True", encoding = "1251")
#Download data
sales <- dbGetQuery(con, "SELECT  orders.PhoneNumber, Number, orders.Date as Date
                          FROM sales left join orders 
                          ON sales.OrderUID = orders.OrderUID
                          WHERE Unit = 'КрасоткаПроТрейд' and Cast(orders.Date as Date) >= '20200101'
                          group by orders.PhoneNumber , Number, orders.Date")

#Try to attain tidy format
sales <- setDT(sales)[ PhoneNumber != "" ][, Date := as.Date(Date)]
#Define data of first customer transaction and frame period clusters based on weeks , months 
sales[, `:=` ( First_time_week = ifelse(min(Date) < as.Date("2020-01-06"), as.Date("2020-01-01"), floor_date(min(Date),"weeks", week_start = 1)), First_time_month = floor_date(min(Date),"month")), by = PhoneNumber]
sales[,  `:=` (Period_week =  isoweek(Date)-isoweek( First_time_week), #Period - number of weeks and months after the first order
               Period_month = mondf(First_time_month, Date))][order(-PhoneNumber)]



#Form more appropriate cohort for next charts
x <- c("Январь", "Февраль", "Март", "Апрель", "Май", "Июнь", "Июль", "Август", "Сентябрь", "Октябрь", "Ноябрь" , "Декабрь")
sales[, `:=` (Cohort_month = factor(format(First_time_month, "%B"), levels = x), Cohort_week = isoweek(First_time_week))][order(Cohort_month)]

#From wide format data
MAU.month.grid <- tidyr::pivot_wider(data = sales[order(Period_month, Cohort_month )], Cohort_month , names_from = Period_month, values_from = PhoneNumber, values_fn = uniqueN)
#Change column names
colnames(MAU.month.grid)[2] <- c("First")
colnames(MAU.month.grid)[3:13] <- paste("Period", colnames(MAU.month.grid)[3:13], sep ="_")

#Calculate Retention by month cohort
setDT(MAU.month.grid)[, c(3:13) := lapply(.SD, function(x) round(x/First,3)),.SD = c(3:13) ]

#Frame mean by columns an bind this result with retention
Retention <- MAU.month.grid
avgs.ret <- round(apply(Retention[,-1],2,mean, na.rm=TRUE),4)
Retention <- dplyr::bind_rows(Retention , avgs.ret )
#Define color graph based on quantile division
breaks <- quantile(Retention [,3:13], probs = seq(.05, .95, .025), na.rm = TRUE)
colors <- sapply(round(seq(1, 0, length.out = length(breaks) + 1), 6),
                 function(x){ rgb(x,x,0.85) } )
#Make Retention table
datatable(Retention ,
              class = 'cell-border stripe',
              rownames = FALSE,
              options = list(
                ordering=F,
                dom = 't',
                pageLength = 13) ) %>%
  formatStyle("Cohort_month",
              backgroundColor = 'lightgrey',
              fontWeight = 'bold') %>%
  formatPercentage(c(3:13),2) %>% 
  formatStyle("First", fontWeight = 'bold') %>%
  formatStyle(colnames(Retention )[3:13],color = 'lightblack',fontWeight = 'bold', backgroundColor = styleInterval(breaks,colors))

#Make Churn Rate graph
Churn <- MAU.month.grid
setDT(Churn)[, c(3:13) := lapply(.SD, function(x) round(abs(x-1),3)),.SD = c(3:13) ]
#Calculation mean
avg.churn <- round(apply(Churn[,-1],2,mean, na.rm=TRUE),4)
Churn <- dplyr::bind_rows(Churn , avg.churn )

#Makes colors 
breaks_churn <- quantile(Churn[,3:13], probs = seq(.05, .95, .05), na.rm = TRUE)
colors_churn <- sapply(round(seq(255, 30, length.out = length(breaks_churn) + 1), 0),
                  function(x){ rgb(255,x,x, maxColorValue = 255) } )

datatable(Churn,
          class = 'cell-border stripe',
          rownames = FALSE,
          options = list(
            ordering=F,
            dom = 't',
            pageLength = 13) ) %>%
  formatStyle("Cohort_month",
              backgroundColor = 'lightgrey',
              fontWeight = 'bold') %>%
  formatPercentage(c(3:13),2) %>% # We don't want column 0 in %
  formatStyle("First", fontWeight = 'bold') %>%
  formatStyle(colnames(Churn )[3:13],color = 'lightblack',fontWeight = 'bold', backgroundColor = styleInterval(breaks_churn,colors_churn))


#Make the sames based on week cohort
MAU.week.grid <- tidyr::pivot_wider(data = sales[order(Period_week, Cohort_week )], Cohort_week , names_from = Period_week, values_from = PhoneNumber, values_fn = uniqueN)

#Change week column names
colnames(MAU.week.grid)[2] <- c("First")
colnames(MAU.week.grid)[3:51] <- paste("Period", colnames(MAU.week.grid)[3:51], sep ="_")

#Calculate Retention by week cohort
setDT(MAU.week.grid)[, c(3:51) := lapply(.SD, function(x) round(x/First,3)),.SD = c(3:51) ]

#Frame mean by columns an bind this result with retention
Retention.week <- MAU.week.grid
avgs.week.ret <- round(apply(Retention.week[,-1],2,mean, na.rm=TRUE),4)
Retention.week <- dplyr::bind_rows(Retention.week , avgs.week.ret )



#Makes colors 
breaks.week.retention <- quantile(Retention.week[,3:51], probs = seq(.05, .95, .05), na.rm = TRUE)
colors.week.retention <- sapply(round(seq(255, 30, length.out = length(breaks.week.retention) + 1), 0),
                       function(x){ rgb(x,255,x, maxColorValue = 255) } )

datatable(Retention.week,
          class = 'cell-border stripe',
          rownames = FALSE,
          options = list(
            ordering=F,
            dom = 't',
            pageLength = 50) ) %>%
  formatStyle("Cohort_week",
              backgroundColor = 'lightgrey',
              fontWeight = 'bold') %>%
  formatPercentage(c(3:51),2) %>% # We don't want column 0 in %
  formatStyle("First", fontWeight = 'bold') %>%
  formatStyle(colnames(Retention.week )[3:51],color = 'lightblack',fontWeight = 'bold', backgroundColor = styleInterval(breaks.week.retention,colors.week.retention))

