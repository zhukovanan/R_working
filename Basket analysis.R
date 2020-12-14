#Load all libraries
library(DBI)
library(data.table)
library(odbc)
library(lubridate)
library(ggplot2)
library(magrittr)


#Create connection to database
con <- dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "SQL002\\MSSQL002", 
                 Database = "report_kr", Trusted_Connection = "True", encoding = "1251")

order_stat <- dbGetQuery(con, "with t as (select  DISTINCT m.*, brands.name as brand
	                        from (SELECT t1.name AS name,t1.code,t1.uid,t1.brand_uid,t1.date_add,t1.Artikul, t2.name as Подкатегория,t2.code as Code_PDK, t3.name as Категория,t3.code as Code_Cat, t4.name as Группа,t4.code as Code_Group
                                FROM goods AS t1
                                LEFT JOIN goods AS t2 ON t1.uid_Parents = t2.uid
                                LEFT JOIN goods AS t3 ON t2.uid_Parents = t3.uid
                                left join goods as t4 on t3.uid_Parents = t4.uid) as m left join brands on m.brand_uid = brands.uid), 
                          k as(select name, code, uid, brand, date_add,Artikul, Подкатегория,Code_PDK, coalesce(Категория,Подкатегория) as Категория,coalesce(Code_Cat,Code_PDK) as Code_Cat  , coalesce(Группа,Категория,Подкатегория) as Группа, coalesce(Code_Group,Code_Cat ,Code_PDK) as Code_Group
                               from t), 
                          l as (select  name, code, uid, brand, date_add,Artikul, Подкатегория,Code_PDK, CASE WHEN Группа = Категория then Подкатегория else Категория  end as Категория , Code_PDK as Code_Cat  ,Группа, Code_Group
                                from k) , 
                                
                          n as (select l.Code , l.name, l.uid, l.Brand, Подкатегория
                               from l left join goods k3 on l.code = k3.code
                               where l.uid not in (select uid
                                                   from goods
                                                   where it_group = 'Да'))
                                                   
                          SELECT n.Подкатегория , Cast(orders.Date as Date) as Date, Number
                          FROM sales left join orders  ON sales.OrderUID = orders.OrderUID
                                     inner join n on sales.GoodUID = n.uid
                          WHERE Unit = 'КрасоткаПроТрейд' and orders.Date >= '20201210'
                          group by n.Подкатегория , Number,  cast(orders.Date as Date)")

#Form matrix of occurence
Matrix_occurence <- crossprod(table(order_stat[,c(3,1)])) #First part is index of order number column , the second - category column
diag(Matrix_occurence) <- 0

#Transform matrix in more useful type
Occurence <- as.data.frame(as.table(Matrix_occurence)) %>% 
             dplyr::rename(Category = Подкатегория, Friend_category =  Подкатегория.1 )
#Define Category goes with friend category always 
lone_wolf <- Occurence %>% dplyr::filter(Freq == 0) %>% .$Category %>% unique()
Occurence %>% dplyr::filter(Freq =!0 & Category  )
