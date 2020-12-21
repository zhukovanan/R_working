#Download library
library(reticulate)
library(DBI)
library(odbc)
library(data.table)
library(jsonlite)
library(httr)

#Form named api vector 
api.par <- c("ГЕМ" = "NDY4Zjk0NjQtNTBiOC00MWM5LWE4MDQtOWI4M2Y5ZTQ1MTAz" , "КТ" = "Y2ViOTVkNTgtZjUyOS00ZmY4LWI3YjYtZjhjYjM3NTQzNWRk"
, "КСВ" = "MmVkOGZkZTgtYjVmNi00ZTk4LWJhYzMtMWI5ZWU5MDVmZTNi", "МОДЖИО" = "ZGYzY2VkYmUtMDI5MC00ZTU2LWI4YmUtMDNlZTA5MGIzYjgy",
"МЕВ" = "NDJjZTM3OWMtZDQ1Yi00ZDY1LTkwMGItODA3MjQ4ODY1MzA4" , "МАС" = "ODRlOTNiMTEtNTg1OC00ZDg1LTlkZDYtZmI3ODU2OTFiMmVm" ,
 "УРК" = "MGI0NzhkYjMtYjg1Yy00Nzg2LWI2MTUtMDQxMWYzMmI3Nzli")

#Make functions for different api tables
#Sales
sales.api <- function(x) {
  res <- GET("https://suppliers-stats.wildberries.ru/api/v1/supplier/sales", query = list(dateFrom = "2020-01-01" ,flag = 0, key = x))
  resp_char <- rawToChar(res$content)
  Encoding(resp_char) <- "UTF-8"
  df <-  as.data.frame(fromJSON(resp_char))
  tryCatch(df$Vendor <- names(api.par[api.par == x]), error=function(e) NULL)
  if (nrow(df) == 0) {
    return(NULL) 
  } else {
    return(df)
  }
}
#Orders
orders.api <- function(x) {
  res <- GET("https://suppliers-stats.wildberries.ru/api/v1/supplier/orders", query = list(dateFrom = "2020-01-01" ,flag = 0, key = x))
  resp_char <- rawToChar(res$content)
  Encoding(resp_char) <- "UTF-8"
  df <-  as.data.frame(fromJSON(resp_char))
  tryCatch(df$Vendor <- names(api.par[api.par == x]), error=function(e) NULL)
  if (nrow(df) == 0) {
    return(NULL) 
  } else {
    return(df)
  }
  }
#Report by daily 
Realisation.api <- function(x) {
  res <- GET("https://suppliers-stats.wildberries.ru/api/v1/supplier/reportDetailByPeriod", query = list(dateFrom = "2020-09-01" , key = x, dateto = "2020-12-21"))
  resp_char <- rawToChar(res$content)
  Encoding(resp_char) <- "UTF-8"
  df <-  as.data.frame(fromJSON(resp_char))
  tryCatch(df$Vendor <- names(api.par[api.par == x]), error=function(e) NULL)
  if (nrow(df) == 0) {
    return(NULL) 
  } else {
    return(df)
  }
}
#Wayrhouse_cost
Warehouse.api <- function(x) {
  res <- GET("https://suppliers-stats.wildberries.ru/api/v1/supplier/stochrancost", query = list(dateFrom = "2020-09-01" , key = x, dateto = "2020-12-21"))
  resp_char <- rawToChar(res$content)
  Encoding(resp_char) <- "UTF-8"
  df <-  as.data.frame(fromJSON(resp_char))
  tryCatch(df$Vendor <- names(api.par[api.par == x]), error=function(e) NULL)
  if (nrow(df) == 0) {
    return(NULL) 
  } else {
    return(df)
  }
}
#Stock
stock.api <- function(x) {
  res <- GET("https://suppliers-stats.wildberries.ru/api/v1/supplier/stocks", query = list(dateFrom = "2017-03-25T21:00:00.000Z" , key = x))
  resp_char <- rawToChar(res$content)
  Encoding(resp_char) <- "UTF-8"
  df <-  as.data.frame(fromJSON(resp_char))
  tryCatch(df$Vendor <- names(api.par[api.par == x]), error=function(e) NULL)
  if (nrow(df) == 0) {
    return(NULL) 
  } else {
    return(df)
  }
}
#Incomes
incomes.api <- function(x) {
  res <- GET("https://suppliers-stats.wildberries.ru/api/v1/supplier/incomes", query = list(dateFrom = "2017-03-25T21:00:00.000Z" , key = x))
  resp_char <- rawToChar(res$content)
  Encoding(resp_char) <- "UTF-8"
  df <-  as.data.frame(fromJSON(resp_char))
  tryCatch(df$Vendor <- names(api.par[api.par == x]), error=function(e) NULL)
  if (nrow(df) == 0) {
    return(NULL) 
  } else {
    return(df)
  }
}

#Based on api data form necessary data frames
Orders_WB <- do.call(dplyr::bind_rows, lapply(api.par, orders.api)) 
Sales_WB <- do.call(dplyr::bind_rows, lapply(api.par, sales.api))
Realisation_WB <- do.call(dplyr::bind_rows, lapply(api.par, Realisation.api))
Warehouse_cost_WB <- do.call(dplyr::bind_rows, lapply(api.par, Warehouse.api))
Stock_WB <- do.call(dplyr::bind_rows, lapply(api.par, stock.api))
Incomes_WB <- do.call(dplyr::bind_rows, lapply(api.par, incomes.api)) 

#Connect to SQL Server

con <- dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "SQL002\\MSSQL002", 
                 Database = "report_kr", UID = "report", PWD = rstudioapi::askForPassword("Database password"), encoding = "1251)

#Try convert to appropriate data type 
df$date <- as.Date.character(df$date)

dbCreateTable(con,"WB_supply", df[,1:13])
