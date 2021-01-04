#Load all libraries
library(DBI)
library(data.table)
library(odbc)
library(lubridate)
library(ggplot2)
library(magrittr)


#Create connection to database
con <- dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "####", 
                 Database = "####", Trusted_Connection = "True", encoding = "1251")

order_stat <- dbGetQuery(con, "with t as (select  DISTINCT m.*, brands.name as brand
	                        from (select t1.name AS name,t1.code,t1.uid,t1.brand_uid,t1.date_add,t1.Artikul, t2.name as Подкатегория,t2.code as Code_PDK, t3.name as Категория,t3.code as Code_Cat, t4.name as Группа,t4.code as Code_Group
                                from goods AS t1
                                left join goods AS t2 ON t1.uid_Parents = t2.uid
                                left join goods AS t3 ON t2.uid_Parents = t3.uid
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

#Count of occurrences of 3 subcategory in order and separate for next joins
new <- setDT(order_stat)[, if (.N >= 3L) .(triplet =  
                                      sapply(combn(Подкатегория, 3L, simplify = FALSE),paste, collapse = "|")),
                  by = .(
                    Number,Date) ][, c("one", "two", "three") := tstrsplit(triplet, "|", fixed=TRUE)]
#Set keys
setkey(new, Number,one, two, three)
setkey(order_stat, Number, Подкатегория)

#Join sales
new <- new[, one_sale := order_stat[new, on = c(Number = "Number", Подкатегория = "one" ), 
         x.Sale] ][, two_sale := order_stat[new, on = c(Number = "Number", Подкатегория = "two" ), 
         x.Sale]][, three_sale := order_stat[new, on = c(Number = "Number", Подкатегория = "three" ), 
                                           x.Sale]]
#Join Reward
new <- new[, one_profit := order_stat[new, on = c(Number = "Number", Подкатегория = "one" ), 
                                    x.Reward] ][, two_profit := order_stat[new, on = c(Number = "Number", Подкатегория = "two" ), 
                                    x.Reward]][, three_profit := order_stat[new, on = c(Number = "Number", Подкатегория = "three" ), 
                                    x.Reward]]
#Get total sum for sales and reward in terms of combinations
new <- new[, Sale := Reduce(`+`, .SD), .SDcol = 7:9][,Reward := Reduce(`+`, .SD), .SDcol = 10:12][,c(4:12) := NULL]
#Get mean statistics 
new <- new[ , .(Sale = sum(Sale), Reward = sum(Reward), Q= .N) ,by = .(triplet, month(Date))][, .(Sale = 
                                                                                      mean(Sale, na.rm =  T), Reward = mean(Reward, na.rm = T), Q = round(mean(Q))) , by = triplet ][order(-Q)]
