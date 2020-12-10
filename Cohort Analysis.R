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
MAU.month.grid <- tidyr::pivot_wider(data = sales[order(Period_month, Cohort_month )], Cohort_month , names_from = Period_month, values_from = PhoneNumber, values_fn = uniqueN, values_fill = 0)
#Change column names
colnames(MAU.month.grid)[2] <- c("First")
colnames(MAU.month.grid)[3:13] <- paste("Period", colnames(MAU.month.grid)[3:13], sep ="_")

#Calculate Retention by month cohort
setDT(MAU.month.grid)[, c(3:13) := lapply(.SD, function(x) round(x/First,3)),.SD = c(3:13) ]

longformat <- tidyr::pivot_longer(MAU.month.grid, -c(1,2), names_to = "Period_month", values_to = "percent") 
setDT(longformat)[,  Period_month := as.numeric(gsub("Period_", "",Period_month )),
                           ][order(Cohort_month,Period_month) ]

  ggplot(longformat, aes(Period_month, reorder(Cohort_month, dplyr::desc(Cohort_month))))+
  geom_raster(aes(fill = percent)) +
    scale_fill_continuous(guide = FALSE) + # no legend
    xlab("cohort age") + ylab("cohort") 
  
avgs.ret <- round(apply(cw.retention[,-1],2,mean, na.rm=TRUE),4)
cw.retention <- MAU.month.grid
rbind(cw.retention,avgs.ret)
cw.retention <- dplyr::bind_rows(cw.retention, avgs.ret )

breaks <- quantile(cw.retention[,3:13], probs = seq(.05, .95, .025), na.rm = TRUE)
colors <- sapply(round(seq(155, 80, length.out = length(breaks) + 1), 0),
                 function(x){ rgb(x,x,155, maxColorValue = 155) } )
library(DT)
 datatable(cw.retention,
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
  formatStyle(names(cw.retention[c(3)]),color = 'white',fontWeight = 'bold', backgroundColor = styleInterval(breaks,colors))

