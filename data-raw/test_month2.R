month_update <- function(conn=tsda::conn_rds('jlrds'),Fyear =2020,Fmonth =7) {

  #删除数据
  month_DelBal(conn=conn,Fyear = Fyear,Fmonth = Fmonth)
  month_DelRpt(conn=conn,Fyear = Fyear,Fmonth = Fmonth)
  month_DelStat(conn=conn,Fyear = Fyear,Fmonth = Fmonth)

  #执行更新
  month_deal(conn=conn,year = Fyear,month = Fmonth)
  month_stat(conn=conn,year = Fyear,month = Fmonth)


}


library(jlrdspkg)
conn=tsda::conn_rds('jlrds')
Fyear=2021
Fmonth = 3
month_DelBal(conn=conn,Fyear = Fyear,Fmonth = Fmonth)
month_DelRpt(conn=conn,Fyear = Fyear,Fmonth = Fmonth)
month_DelStat(conn=conn,Fyear = Fyear,Fmonth = Fmonth)

#执行更新
month_deal(conn=conn,year = Fyear,month = Fmonth)
month_stat(conn=conn,year = Fyear,month = Fmonth)


conn=tsda::conn_rds('jlrds')
year=2021
month = 3

#处理当期数据

res_current <-month_selectDb(conn=conn,year = year,month = month)
print(res_current)
ncount_current <-!is.na(res_current)
#处理上期数据
last_info <- month_getLastMonth(conn=conn,year = year,month = month)
year_lastMonth = last_info$Fyear
month_lastMonth = last_info$Fmonth
res_lastMonth <-month_selectDb(conn=conn,year=year_lastMonth,month = month_lastMonth)
print(res_lastMonth)
ncount_lastMonth <-!is.na(res_lastMonth)
#处理去年同期数据
lastYear = year -1
res_lastYear <- month_selectDb(conn=conn,year = lastYear,month = month)
print(res_lastYear)
ncount_lastYear <-!is.na(res_lastYear)
#针对数据进行处理
if(ncount_current){
  #针对有数据情况
  res <-res_current
  names(res) <-c('FRptItemNo','FRptItemName','FCurrentAmt')
  res$FCurrentAmt <- tsdo::na_replace(res$FCurrentAmt,0)
  res$FCurrentAmt <- round(res$FCurrentAmt,2)
  #合并上期数据
  if(ncount_lastMonth ){
    print('s1')
    names(res_lastMonth) <-c('FRptItemNo','FRptItemName','FLastMonthAmt')
    res_lastMonth <- res_lastMonth[,c('FRptItemNo','FLastMonthAmt')]
    res <- dplyr::left_join(res,res_lastMonth,by='FRptItemNo')
    res$FLastMonthAmt <- tsdo::na_replace(round(res$FLastMonthAmt,2),0)
    res$FLastMonthVariance = round(res$FCurrentAmt -res$FLastMonthAmt,2)
    print('s1B')
    print(res)

    res$FLastMonthPercent =rpt_percent(res$FCurrentAmt ,res$FLastMonthAmt,4)

    print('s1 end')


  }else{
    print('s2')
    res$FLastMonthAmt <- as.numeric(NA)
    res$FLastMonthVariance <-as.numeric(NA)
    res$FLastMonthPercent =as.numeric(NA)

  }

  #合并去年数据
  print(ncount_lastYear)

  if(ncount_lastYear){
    print('s3')
    names(res_lastYear) <- c('FRptItemNo','FRptItemName','FLastYearAmt')
    res_lastYear <- res_lastYear[,c('FRptItemNo','FLastYearAmt')]
    res <- dplyr::left_join(res,res_lastYear,by='FRptItemNo')
    res$FLastYearAmt <- round(res$FLastYearAmt,2)
    res$FLastYearVariance <- round(res$FCurrentAmt -res$FLastYearAmt,2)

    res$FLastYearPercent = rpt_percent(res$FCurrentAmt, res$FLastYearAmt,4)

  }else{
    print('s4')
    res$FLastYearAmt <-as.numeric(NA)
    res$FLastYearVariance <-as.numeric(NA)
    res$FLastYearPercent <- as.numeric(NA)

  }
  #增加相关信息
  res$Fyear <-year
  res$Fmonth <- month
  #写入数据库
  #print(head(res))
  for (i in 1:nrow(res)) {

    print(i)
    tsda::db_writeTable(conn = conn,table_name = 't_zjrb_monthStat',r_object = res[i,],append = T)

  }
  #tsda::db_writeTable(conn = conn,table_name = 't_zjrb_weekStat',r_object = res,append = T)


}else{
  #针对没有数据情况
  res <- NA
}
return(res)





