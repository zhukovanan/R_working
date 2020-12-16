#Download library
library(reticulate)
library(DBI)
library(odbc)

#Activate python packages
requests <- import("requests")
json <- import("jason")
pd <- import("pandas")

#Form data frame from api url 
df <- pd$read_json("https://suppliers-stats.wildberries.ru/api/v1/supplier/incomes?dateFrom=2020-03-25&key=token")

#Connect to SQL Server

con <- dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "SQL002\\MSSQL002", 
                 Database = "report_kr", UID = "report", PWD = rstudioapi::askForPassword("Database password"), encoding = "1251)

#Try convert to appropriate data type 
df$date <- as.Date.character(df$date)

dbCreateTable(con,"WB_supply", df[,1:13])
