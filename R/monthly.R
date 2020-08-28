#' 按月读取余额数
#'
#' @param conn 连接
#' @param month 月份
#' @param year 年份
#'
#' @return 返回值
#' @export
#'
#' @examples
#' month_getEndBal()
month_getEndBal <- function(conn=tsda::conn_rds('jlrds'),year=2020,month=3) {
  options(digits=15)
  sql <- paste0("select FendBal  from t_zjrb_monthBal
where fyear=",year," and fmonth=",month)
  res <- tsda::sql_select(conn,sql)
  #print(res)
  ncount <- nrow(res)
  if(ncount >0){
    info <- res$FendBal
  }else{
    info <-NA
  }
  return(info)

}

#' 获取上一年份与月份事情
#'
#' @param conn 连接
#' @param month 月份
#' @param year 年份
#'
#' @return 返回值
#' @export
#'
#' @examples
#' month_getLastMonth()
month_getLastMonth <- function(conn=tsda::conn_rds('jlrds'),year=2020,month=3){
  sql<-paste0("select Fyear,Fmonth  from t_md_period where FmonthId in
(select  FmonthId-1 as FmonthId from t_md_period  where Fyear=",year," and Fmonth = ",month,")")
  res <- tsda::sql_select(conn,sql)
  ncount <- nrow(res)
  if(ncount >0){
    Fyear = res$Fyear
    Fmonth = res$Fmonth
    info <- list(Fyear=Fyear,Fmonth=Fmonth)

  }else{
    info <- NA
  }
  return(info)
}




#' 获取每个月的期初数
#'
#' @param conn 连接
#' @param month 月份
#' @param year 年份
#'
#' @return 返回值
#' @export
#'
#' @examples
#' month_getBeginBal()
month_getBeginBal <-function(conn=tsda::conn_rds('jlrds'),year=2020,month=3){
  lastMonthInfo <- month_getLastMonth(conn=conn,year = year,month = month)
  Fyear = lastMonthInfo$Fyear
  Fmonth = lastMonthInfo$Fmonth
  res <- month_getEndBal(conn=conn,year = Fyear,month = Fmonth)
  return(res)

}





#' 获取一周的开始日期与结束日志
#'
#' @param conn 连接
#' @param month 月份
#' @param year 年份
#'
#' @return 返回值
#' @export
#'
#' @examples
#' month_getDates()
month_getDates <- function(conn=tsda::conn_rds('jlrds'),year=2020,month=3) {

  sql <- paste0("select FPeriodStartDate,FPeriodEndDate  from t_md_period where Fyear = ",year," and Fmonth = ",month)
  res <- tsda::sql_select(conn,sql)
  ncount <- nrow(res)
  if(ncount >0){
    startDate = as.character(res$FPeriodStartDate)
    endDate = as.character(res$FPeriodEndDate)
    res <- list(startDate=startDate,endDate =endDate)

  }else{
    res <-NA
  }
  return(res)
}

#' 更新月余额数据
#'
#' @param conn 连接
#' @param year 年份
#' @param month 月份
#' @param value 数值
#'
#' @return 返回值
#' @export
#'
#' @examples
#' month_writeMonthBal()
month_writeMonthBal <- function(conn=tsda::conn_rds('jlrds'),year=2020,month=3,value){
  sql <-paste0("insert into t_zjrb_monthBal (Fyear,Fmonth,FendBal,FisDo)
values(",year,",",month,",",value,",1)")
  #print(sql)
  try(tsda::sql_update(conn,sql))

}


#' 按月处理数据
#'
#' @param conn 连接
#' @param month 月份
#' @param year 年份
#'
#' @return 返回值
#' @export
#'
#' @examples
#' month_deal()
month_deal <- function(conn=tsda::conn_rds('jlrds'),year=2020,month=3){
  #计取期初数据
  FbeginBal <- month_getBeginBal(conn=conn,year = year,month = month)
  #读取期间汇报数据
  #print('1')

  dates <- month_getDates(conn=conn,year = year,month = month)
  startDate = dates$startDate
  endDate = dates$endDate
  sql <-paste0("select   FRptItemNo,FRptItemName,sum(FAmount)  as FAmount ,FLevel from
  t_zjrb_dailyRpt where FDate >='",startDate,"' and FDate <='",endDate,"'
group by FRptItemNo,FRptItemName,FLevel")
  res <- tsda::sql_select(conn,sql)
  #print(res)
  ncount <- nrow(res)
  if(ncount >0){
    #针对期初及期末数据进行处理
    res[res$FRptItemNo =='A','FAmount'] <- FbeginBal
    FreceAmt <- res[res$FRptItemNo =='C','FAmount']
    FpayAmt <- res[res$FRptItemNo =='D','FAmount']
    FEndAmt = FbeginBal+FreceAmt-FpayAmt

    res[res$FRptItemNo =='E','FAmount'] <- FEndAmt
    res[res$FRptItemNo =='B','FAmount'] <- FreceAmt-FpayAmt
    res$Fyear <- year
    res$Fmonth <- month

    #print(res)
    #View(res)
    #将期末数写入月余额表
    month_writeMonthBal(conn=conn,year=year,month = month,value = FEndAmt)
    #将数据写入月报
    tsda::db_writeTable(conn=conn,table_name = 't_zjrb_monthRpt',r_object = res,append = T)
  }else{
    res <-NA
  }

  return(res)




}

#' 从数据库中读取月报数据
#'
#' @param conn 连接
#' @param month 月份
#' @param year 年份
#'
#' @return 返回值
#' @export
#'
#' @examples
#' month_selectDb()
month_selectDb <- function(conn=tsda::conn_rds('jlrds'),year=2020,month=3){
  #处理当期数据
  sql_current <- paste0("select FRptItemNo,FRptItemName,FAmount from t_zjrb_monthRpt
where Fyear = ",year," and Fmonth =" ,month,"
order by FRptItemNo")
  res_current <-tsda::sql_select(conn,sql_current)
  ncount_current <-nrow(res_current)
  if(ncount_current >0){
    res <- res_current
  }else{
    res <-NA
  }
  return(res)

}



#' 处理周报统计信息
#'
#' @param conn 连接
#' @param year 年份
#' @param weekNo 周号
#' @param type 类型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' week_stat()
month_stat <- function(conn=tsda::conn_rds('jlrds'),year=2020,month=3){
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





}




#' 读取月报数据
#'
#' @param conn 连接
#' @param year 年份
#' @param AmtType 金额类型
#' @param FLevel 级次
#' @param amtUnit 金额单位默认万元
#'
#' @return 返回值
#' @export
#'
#' @examples
#' monthRpt_selectDB()
monthRpt_selectDB <- function(conn=tsda::conn_rds('jlrds'),year=2020,
                             AmtType=c('本月发生额','较上月变动额','较上月变动%'),
                             FLevel = 0,
                             amtUnit='wan') {
  #将向量转化为字符串
  AmtType = vect_to_string(vect = AmtType)
  #进行SQL处理
  if(FLevel ==0){
    if(amtUnit =='wan'){
      #处理万元
      sql = paste0("select  FRptItemNo,FRptItemName,FAmount,b.FAmtType,Fperiod from vw_monthRpt_wan_all_ByRows a
inner join t_zjrb_amtType b
on a.FAmtType = b.FId
where  b.FAmtType in(",AmtType,")
and Fyear =",year,"
order by Fyear,Fmonth,FRptItemNo,b.FId
")
    }else{
      #处理元
      sql = paste0("select  FRptItemNo,FRptItemName,FAmount,b.FAmtType,Fperiod from vw_monthRpt_all_ByRows a
inner join t_zjrb_amtType b
on a.FAmtType = b.FId
where  b.FAmtType in(",AmtType,")
and Fyear =",year,"
order by Fyear,Fmonth,FRptItemNo,b.FId
")
    }

  }else{
    if( amtUnit =='wan'){
      #处理万元
      sql = paste0("select  FRptItemNo,FRptItemName,FAmount,b.FAmtType,Fperiod from vw_monthRpt_wan_all_ByRows a
inner join t_zjrb_amtType b
on a.FAmtType = b.FId
where  b.FAmtType in(",AmtType,")
and Fyear =",year,"  and FLevel =",FLevel,"
order by Fyear,Fmonth,FRptItemNo,b.FId
")

    }else{
      #处理元
      sql = paste0("select  FRptItemNo,FRptItemName,FAmount,b.FAmtType,Fperiod from vw_monthRpt_all_ByRows a
inner join t_zjrb_amtType b
on a.FAmtType = b.FId
where  b.FAmtType in(",AmtType,")
and Fyear =",year,"  and FLevel =",FLevel,"
order by Fyear,Fmonth,FRptItemNo,b.FId
")

    }

  }

  res <- tsda::sql_select(conn,sql)
  #转化为文本类型
  res$Fperiod <- as.character(res$Fperiod)
  #调整了顺序
  res2 <- reshape2::dcast(data = res,formula =FRptItemNo+FRptItemName+FAmtType~Fperiod,fun.aggregate = sum,value.var = 'FAmount' )
  res2[res2$FRptItemNo =='A','FRptItemName'] <-'期初余额'
  res2[res2$FRptItemNo =='B','FRptItemName'] <-'本月发生额'
  return(res2)
}




#' 获取月报的资金类型
#'
#' @param conn 连接
#'
#' @return 返回
#' @export
#'
#' @examples
#' month_getRptType()
month_getRptType <- function(conn=tsda::conn_rds('jlrds')) {
  sql <- paste0("select FAmtType from t_zjrb_AmtType where FRptType='month'
order  by FId")
  r <- tsda::sql_select(conn,sql)
  res <- tsdo::vect_as_list(r$FAmtType)
  return(res)

}



#' 删除月报余额
#'
#' @param conn 连接
#' @param Fyear 年份
#' @param Fmonth 月份
#'
#' @return 返回值
#' @export
#'
#' @examples
#' month_DelBal()
month_DelBal <- function(conn=tsda::conn_rds('jlrds'),Fyear =2020,Fmonth =7) {
  sql_sel <- paste0("select 1 from t_zjrb_monthBal where Fyear =",Fyear," and Fmonth =",Fmonth)
   r <- tsda::sql_select(conn,sql_sel)
   ncount <- nrow(r)
   if(ncount >0){
     sql_del <-paste0("delete from t_zjrb_monthBal where Fyear =",Fyear," and Fmonth =",Fmonth)
     tsda::sql_update(conn,sql_del)
   }

}

#' 删除月报数据
#'
#' @param conn 连接
#' @param Fyear 年份
#' @param Fmonth 月份
#'
#' @return 返回值
#' @export
#'
#' @examples
#' month_DelRpt()
month_DelRpt <- function(conn=tsda::conn_rds('jlrds'),Fyear =2020,Fmonth =7) {
  sql_sel <- paste0("select 1 from t_zjrb_monthRpt where Fyear =",Fyear," and Fmonth =",Fmonth)
  r <- tsda::sql_select(conn,sql_sel)
  ncount <- nrow(r)
  if(ncount >0){
    sql_del <-paste0("delete from t_zjrb_monthRpt where Fyear =",Fyear," and Fmonth =",Fmonth)
    tsda::sql_update(conn,sql_del)
  }

}


#' 删除月报统计数据
#'
#' @param conn 连接
#' @param Fyear 年份
#' @param Fmonth 月份
#'
#' @return 返回值
#' @export
#'
#' @examples
#' month_DelStat()
month_DelStat <- function(conn=tsda::conn_rds('jlrds'),Fyear =2020,Fmonth =7) {
  sql_sel <- paste0("select 1 from t_zjrb_monthStat where Fyear =",Fyear," and Fmonth =",Fmonth)
  r <- tsda::sql_select(conn,sql_sel)
  ncount <- nrow(r)
  if(ncount >0){
    sql_del <-paste0("delete from t_zjrb_monthStat where Fyear =",Fyear," and Fmonth =",Fmonth)
    tsda::sql_update(conn,sql_del)
  }

}


#' 月报更新
#'
#' @param conn 连接
#' @param Fyear 月份
#' @param Fmonth 月份
#'
#' @return 返回值
#' @export
#'
#' @examples
#' month_update()
month_update <- function(conn=tsda::conn_rds('jlrds'),Fyear =2020,Fmonth =7) {

  #删除数据
  month_DelBal(conn=conn,Fyear = Fyear,Fmonth = Fmonth)
  month_DelRpt(conn=conn,Fyear = Fyear,Fmonth = Fmonth)
  month_DelStat(conn=conn,Fyear = Fyear,Fmonth = Fmonth)

  #执行更新
  month_deal(conn=conn,year = Fyear,month = Fmonth)
  month_stat(conn=conn,year = Fyear,month = Fmonth)


}






