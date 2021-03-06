#' 按周读取余额数
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
#' week_getEndBal()
week_getEndBal <- function(conn=tsda::conn_rds('jlrds'),year=2020,weekNo=8,type='jala') {
  options(digits=15)
  sql <- paste0("select FendBal  from t_zjrb_weekBal
where fyear=",year," and fweekNo=",weekNo,"  and ftype='",type,"'")
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

#' 获取上一周年份与周事情
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
#' week_getLastWeek()
week_getLastWeek <- function(conn=tsda::conn_rds('jlrds'),year=2020,weekNo=9,type='jala'){
  sql<-paste0("select Fyear,FweekNo from t_md_week where fweekid in
(select  fweekid-1 as fweekid  from t_md_week where fyear=",year," and fweekno=",weekNo," and ftype='",type,"')
and ftype='",type,"'")
  print(sql)
  res <- tsda::sql_select(conn,sql)
  ncount <- nrow(res)
  if(ncount >0){
    Fyear = res$Fyear
    FweekNo = res$FweekNo
    info <- list(Fyear=Fyear,FweekNo=FweekNo)

  }else{
    info <- NA
  }
  return(info)
}




#' 获取每个月的期初数
#'
#' @param conn 连接
#' @param year 年份
#' @param weekNo 周次
#' @param type 类型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' week_getBeginBal()
week_getBeginBal <-function(conn=tsda::conn_rds('jlrds'),year=2020,weekNo=9,type='jala'){
  lastWeekInfo <- week_getLastWeek(conn=conn,year = year,weekNo = weekNo,type=type)
  Fyear = lastWeekInfo$Fyear
  FweekNo = lastWeekInfo$FweekNo
  res <- week_getEndBal(conn=conn,year = Fyear,weekNo = FweekNo,type = type)
  return(res)

}





#' 获取一周的开始日期与结束日志
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
#' week_getDates()
week_getDates <- function(conn=tsda::conn_rds('jlrds'),year=2020,weekNo=9,type='jala') {

  sql <- paste0("select fweekstartdate,fweekenddate  from t_md_week
                where fyear=",year," and fweekno =",weekNo," and ftype='",type,"'")
  res <- tsda::sql_select(conn,sql)
  ncount <- nrow(res)
  if(ncount >0){
    startDate = as.character(res$fweekstartdate)
    endDate = as.character(res$fweekenddate)
    res <- list(startDate=startDate,endDate =endDate)

  }else{
    res <-NA
  }
  return(res)
}

#' 更新周余额数据
#'
#' @param conn 连接
#' @param year 年份
#' @param weekNo 周号
#' @param value 数值
#' @param type 类型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' week_writeWeekBal()
week_writeWeekBal <- function(conn=tsda::conn_rds('jlrds'),year=2020,weekNo=9,value,type='jala'){
  sql <-paste0("insert into t_zjrb_weekBal (Fyear,FweekNo,FendBal,Ftype,FisDo)
values(",year,",",weekNo,",",value,",'",type,"',1)")
  #print(sql)
  try(tsda::sql_update(conn,sql))

}


#' 按周处理数据
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
#' week_deal()
week_deal <- function(conn=tsda::conn_rds('jlrds'),year=2020,weekNo=9,type='jala'){
  print('step04-week_deal')
  #计取期初数据
  print('step04.01')
  FbeginBal <- week_getBeginBal(conn=conn,year=year,weekNo = weekNo,type=type)
  print(FbeginBal)
  #读取期间汇报数据
  #print('1')
  print('step04.02')
  dates <- week_getDates(conn=conn,year = year,weekNo = weekNo,type = type)
  startDate = dates$startDate
  endDate = dates$endDate
  print('step04.03')
  sql <-paste0("select   FRptItemNo,FRptItemName,sum(FAmount)  as FAmount ,FLevel from
  t_zjrb_dailyRpt where FDate >='",startDate,"' and FDate <='",endDate,"'
group by FRptItemNo,FRptItemName,FLevel")
  print(sql)
  res <- tsda::sql_select(conn,sql)
  print('step04.04')
  #print(res)
  ncount <- nrow(res)
  if(ncount >0){
    #针对期初及期末数据进行处理
    print('step04.05A')
    print(1)
    res[res$FRptItemNo =='A','FAmount'] <- FbeginBal
    print(2)
    FreceAmt <- res[res$FRptItemNo =='C','FAmount']
    print(3)
    FpayAmt <- res[res$FRptItemNo =='D','FAmount']
    print(4)
    FEndAmt = FbeginBal+FreceAmt-FpayAmt
    print(FEndAmt)
    print(5)
    res[res$FRptItemNo =='E','FAmount'] <- FEndAmt
    print(6)
    res[res$FRptItemNo =='B','FAmount'] <- FreceAmt-FpayAmt
    print(7)
    res$Fyear <- year
    res$FweekNo <-weekNo
    res$Ftype = type
    print(8)
    print(res)
    #View(res)
    #将期末数写入周余额表
    print(9)
    week_writeWeekBal(conn=conn,year=year,weekNo = weekNo,value = FEndAmt,type=type)
    #将数据写入周报
    print(10)
    tsda::db_writeTable(conn=conn,table_name = 't_zjrb_weekRpt',r_object = res,append = T)
  }else{
    print('step04.05B')
    res <-NA
  }

  return(res)




}

#' 从数据库中读取周报数据
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
#' week_selectDb()
week_selectDb <- function(conn=tsda::conn_rds('jlrds'),year=2020,weekNo=10,type='jala'){
  #处理当期数据
  sql_current <- paste0("select FRptItemNo,FRptItemName,FAmount from t_zjrb_weekRpt
    where Fyear =",year," and FweekNo =",weekNo," and Ftype ='",type,"'
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
week_stat <- function(conn=tsda::conn_rds('jlrds'),year=2020,weekNo=10,type='jala'){
  #处理当期数据

  res_current <-week_selectDb(conn = conn,year = year,weekNo = weekNo,type = type)
  ncount_current <-!is.na(res_current)
  #处理上期数据
  last_info <- week_getLastWeek(conn=conn,year=year,weekNo = weekNo,type=type)
  year_lastWeek = last_info$Fyear
  weekNo_lastWeek = last_info$FweekNo
  res_lastWeek <-week_selectDb(conn=conn,year=year_lastWeek,weekNo = weekNo_lastWeek,type=type)
  ncount_lastWeek <-!is.na(res_lastWeek)
  #处理去年同期数据
  lastYear = year -1
  res_lastYear <-week_selectDb(conn = conn,year = lastYear,weekNo = weekNo,type = type)
  ncount_lastYear <-!is.na(res_lastYear)
  #针对数据进行处理
  if(ncount_current){
    #针对有数据情况
    res <-res_current
    names(res) <-c('FRptItemNo','FRptItemName','FCurrentAmt')
    res$FCurrentAmt <- round(res$FCurrentAmt,2)
    #合并上期数据
    if(ncount_lastWeek ){

      names(res_lastWeek) <-c('FRptItemNo','FRptItemName','FLastWeekAmt')
      res_lastWeek <- res_lastWeek[,c('FRptItemNo','FLastWeekAmt')]
      res <- dplyr::left_join(res,res_lastWeek,by='FRptItemNo')
      res$FLastWeekAmt <- round(res$FLastWeekAmt,2)
      res$FLastWeekVariance = round(res$FCurrentAmt -res$FLastWeekAmt,2)

        res$FLastWeekPercent =rpt_percent(res$FCurrentAmt ,res$FLastWeekAmt,4)


    }else{
      res$FLastWeekAmt <- as.numeric(NA)
      res$FLastWeekVariance <-as.numeric(NA)
      res$FLastWeekPercent =as.numeric(NA)

    }

    #合并去年数据
    if(ncount_lastYear){
      names(res_lastYear) <-c('FRptItemNo','FRptItemName','FLastYearAmt')
      res_lastYear <- res_lastYear[,c('FRptItemNo','FLastYearAmt')]
      res <- dplyr::left_join(res,res_lastYear,by='FRptItemNo')
      res$FLastYearAmt <- round(res$FLastYearAmt,2)
      res$FLastYearVariance <- round(res$FCurrentAmt -res$FLastYearAmt,2)

     res$FLastYearPercent = rpt_percent(res$FCurrentAmt, res$FLastYearAmt,4)

    }else{
      res$FLastYearAmt <-as.numeric(NA)
      res$FLastYearVariance <-as.numeric(NA)
      res$FLastYearPercent <- as.numeric(NA)

    }
    #增加相关信息
    res$Fyear <-year
    res$FweekNo = weekNo
    res$Ftype = type
    #写入数据库
    #print(head(res))
    for (i in 1:nrow(res)) {
      print(i)
      tsda::db_writeTable(conn = conn,table_name = 't_zjrb_weekStat',r_object = res[i,],append = T)

    }
    #tsda::db_writeTable(conn = conn,table_name = 't_zjrb_weekStat',r_object = res,append = T)


  }else{
    #针对没有数据情况
    res <- NA
  }
  return(res)





}








#' 查询周报数据
#'
#' @param conn 连接
#' @param year 年份
#' @param startWeekNo 开始周号
#' @param endWeekNo 结束周号
#' @param AmtType 字段类型向量
#' @param FType 类型
#' @param FLevel 级次
#' @param Amt_Unit 金额单位
#'
#' @return 返回值
#' @export
#'
#' @examples
#' weekRpt_selectDB()
weekRpt_selectDB <- function(conn=tsda::conn_rds('jlrds'),year=2020,startWeekNo=9,endWeekNo=12,
                             AmtType=c('本周发生额','较上周变动额','较上周变动%'),
                             FLevel = 0,
                             FType ='jala',
                             Amt_Unit='wan') {
  #将向量转化为字符串
  AmtType = vect_to_string(vect = AmtType)
  #进行SQL处理
  if(FLevel ==0){

    if(Amt_Unit =='wan'){
      #万元处理
      sql = paste0("select  FRptItemNo,FRptItemName,FAmount,b.FAmtType,FweekName from vw_weekRpt_wan_all_ByRows a
inner join t_zjrb_amtType b
on a.FAmtType = b.FId
where Ftype ='",FType,"' and b.FAmtType in(",AmtType,")
and Fyear =",year," and FweekNo >=",startWeekNo," and FweekNo <=",endWeekNo,"
order by Fyear,FweekNo,FRptItemNo,b.FId
")
    }else{
      #元处理
      sql = paste0("select  FRptItemNo,FRptItemName,FAmount,b.FAmtType,FweekName from vw_weekRpt_all_ByRows a
inner join t_zjrb_amtType b
on a.FAmtType = b.FId
where Ftype ='",FType,"' and b.FAmtType in(",AmtType,")
and Fyear =",year," and FweekNo >=",startWeekNo," and FweekNo <=",endWeekNo,"
order by Fyear,FweekNo,FRptItemNo,b.FId
")
    }

  }else{

    if(Amt_Unit =='wan'){
      #万元处理
      sql = paste0("select  FRptItemNo,FRptItemName,FAmount,b.FAmtType,FweekName from vw_weekRpt_wan_all_ByRows a
inner join t_zjrb_amtType b
on a.FAmtType = b.FId
where Ftype ='",FType,"' and b.FAmtType in(",AmtType,")
and Fyear =",year," and FweekNo >=",startWeekNo," and FweekNo <=",endWeekNo,"
  and FLevel =",FLevel,"
order by Fyear,FweekNo,FRptItemNo,b.FId
")

    }else{
      #元处理
      sql = paste0("select  FRptItemNo,FRptItemName,FAmount,b.FAmtType,FweekName from vw_weekRpt_all_ByRows a
inner join t_zjrb_amtType b
on a.FAmtType = b.FId
where Ftype ='",FType,"' and b.FAmtType in(",AmtType,")
and Fyear =",year," and FweekNo >=",startWeekNo," and FweekNo <=",endWeekNo,"
  and FLevel =",FLevel,"
order by Fyear,FweekNo,FRptItemNo,b.FId
")
    }

  }

  res <- tsda::sql_select(conn,sql)
  #调整了顺序
  res2 <- reshape2::dcast(data = res,formula =FRptItemNo+FRptItemName+FAmtType~FweekName,fun.aggregate = sum,value.var = 'FAmount' )
  res2[res2$FRptItemNo =='A','FRptItemName'] <-'期初余额'
  res2[res2$FRptItemNo =='B','FRptItemName'] <-'本周发生额'
  return(res2)
}




#' 获取周号信息
#'
#' @param conn 连接
#' @param year 年份
#' @param Ftype 类型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' week_getDateList()
week_getDateList <- function(conn=tsda::conn_rds('jlrds'),year=2020,Ftype='jala') {
  sql <- paste0("select FweekNo,FweekName  from t_md_week
where Fyear =",year," and Ftype ='",Ftype,"'")
  r <- tsda::sql_select(conn,sql)
  r$FweekNo <- as.integer(r$FweekNo)
  key <- tsdo::vect_as_list(r$FweekNo)
  # value <- tsdo::vect_as_list(r$FweekName)
  # res <- list(key=key,value=value)
  names(key) <- r$FweekName
  return(key)
}


#week_info <- jlrdspkg::week_getDateList(conn=conn,year = week_year,Ftype = week_Ftype)



#' 返回周报的报表类型
#'
#' @param conn 连接
#'
#' @return 返回
#' @export
#'
#' @examples
#' week_getRptType()
week_getRptType <- function(conn=tsda::conn_rds('jlrds')) {
  sql <- paste0("select FAmtType from t_zjrb_AmtType where FRptType='week'
order  by FId")
  r <- tsda::sql_select(conn,sql)
  res <- tsdo::vect_as_list(r$FAmtType)
  return(res)

}


#' 针对周的余额数据执行删除处理
#'
#' @param conn 连接
#' @param Fyear 年份
#' @param FweekNo 周次
#' @param Ftype 类型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' week_DelBal()
week_DelBal <- function(conn=tsda::conn_rds('jlrds'),Fyear =2020,FweekNo =30,Ftype ='nature') {
  sql_sel <-paste0("select 1 from t_zjrb_weekBal where Fyear =",Fyear," and FweekNo = ",FweekNo," and Ftype ='",Ftype,"'")
  print('step1-删除余额')
  print('week_DelBal')
  print(sql_sel)
  r <- tsda::sql_select(conn,sql_sel)
  ncount <-nrow(r)
  print(ncount)
  print('t_zjrb_weekBal')
  sql_del <-paste0("delete from t_zjrb_weekBal where Fyear =",Fyear," and FweekNo = ",FweekNo," and Ftype ='",Ftype,"'")
  print(sql_del)
  if(ncount >0){
    #sql_del <-paste0("delete from t_zjrb_weekBal where Fyear =",Fyear," and FweekNo = ",FweekNo," and Ftype ='",Ftype,"'")
    tsda::sql_update(conn,sql_del)
    print('delete data')
  }


}




#' 删除周报数据
#'
#' @param conn 连接
#' @param Fyear 年份
#' @param FweekNo 周次
#' @param Ftype 类型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' week_DelRpt()
week_DelRpt <- function(conn=tsda::conn_rds('jlrds'),Fyear =2020,FweekNo =30,Ftype ='nature') {
  print('step02-week_DelRpt')
  sql_sel <-paste0("select 1 from t_zjrb_weekRpt where Fyear =",Fyear," and FweekNo = ",FweekNo," and Ftype ='",Ftype,"'")
  print(sql_sel)
  r <- tsda::sql_select(conn,sql_sel)
  sql_del <-paste0("delete from t_zjrb_weekRpt where Fyear =",Fyear," and FweekNo = ",FweekNo," and Ftype ='",Ftype,"'")
  print(sql_del)
  ncount <-nrow(r)
  if(ncount >0){
    #sql_del <-paste0("delete from t_zjrb_weekRpt where Fyear =",Fyear," and FweekNo = ",FweekNo," and Ftype ='",Ftype,"'")
    tsda::sql_update(conn,sql_del)
  }

}




#' 删除统计信息
#'
#' @param conn 连接
#' @param Fyear 年份
#' @param FweekNo 周次
#' @param Ftype 类型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' week_DelStat()
week_DelStat <- function(conn=tsda::conn_rds('jlrds'),Fyear =2020,FweekNo =30,Ftype ='nature') {
  print('step03-week_DelStat')
  sql_sel <-paste0("select 1 from t_zjrb_weekStat where Fyear =",Fyear," and FweekNo = ",FweekNo," and Ftype ='",Ftype,"'")
  print(sql_sel)
  r <- tsda::sql_select(conn,sql_sel)
  sql_del <-paste0("delete from t_zjrb_weekStat where Fyear =",Fyear," and FweekNo = ",FweekNo," and Ftype ='",Ftype,"'")
  ncount <-nrow(r)
  print(sql_del)
  if(ncount >0){
    #sql_del <-paste0("delete from t_zjrb_weekStat where Fyear =",Fyear," and FweekNo = ",FweekNo," and Ftype ='",Ftype,"'")
    tsda::sql_update(conn,sql_del)
  }

}


#' 完善后的周报更新逻辑
#'
#' @param conn 连接
#' @param Fyear 周年份
#' @param FweekNo 周次
#' @param Ftype 类型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' week_update()
week_update <- function(conn=tsda::conn_rds('jlrds'),Fyear =2020,FweekNo =30,Ftype ='nature') {
  #删除余额
  week_DelBal(conn = conn,Fyear = Fyear,FweekNo = FweekNo,Ftype = Ftype)
  #删除周报
  week_DelRpt(conn=conn,Fyear = Fyear,FweekNo = FweekNo,Ftype = Ftype)
  #删除周报统计信息
  week_DelStat(conn=conn,Fyear = Fyear,FweekNo = FweekNo,Ftype = Ftype)
  #更新周报
  week_deal(conn = conn,year = Fyear,weekNo = FweekNo,type = Ftype)
  #更新更报统计信息
  week_stat(conn=conn,year = Fyear,weekNo = FweekNo,type = Ftype)

}


