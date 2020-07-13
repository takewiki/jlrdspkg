

#' 读取日报数据
#'
#' @param file 文件
#' @param sheet 页答
#'
#' @return 返回值
#' @export
#'
#' @examples
#' rpt_daily_readExcel()
rpt_daily_readExcel <- function(file="data-raw/jala_rpt.xlsx",sheet = "daily") {
  jala_rpt <- readxl::read_excel("data-raw/jala_rpt.xlsx",
                         sheet = "daily")
  allColNames <- names(jala_rpt)
  fixedColNames=c('序号','项目')
  varColNames = allColNames[!allColNames %in% fixedColNames]
  #View(jala_rpt)
  data_melt <- reshape2::melt(data=jala_rpt,id.vars = fixedColNames,measure.vars = varColNames,variable.name = '日期',value.name = '金额',
                    na.rm = T,

                    factorsAsStrings=FALSE)
  names(data_melt) <-c('FRptItemNo','FRptItemName','FDate','FAmount')
  data_melt$FDate <- as.integer(as.character(data_melt$FDate))
  #print(data_melt$FDate)
  data_melt$FDate <- as.character(as.Date(data_melt$FDate,origin='1899-12-30'))
  data_melt$FLevel  <- (nchar(data_melt$FRptItemNo)+1) %/% 2
  return(data_melt)

}

#' 资金日报写入数据库
#'
#' @param file 文件
#' @param sheet 页答
#' @param conn 连接信息
#'
#' @return 返回值
#' @export
#'
#' @examples
#' rpt_daily_writeDb()
rpt_daily_writeDb <- function(file="data-raw/jala_rpt.xlsx",sheet = "daily",conn=tsda::conn_rds('jlrds')){
    data <- rpt_daily_readExcel(file=file,sheet=sheet)
    tsda::db_writeTable(conn = conn,table_name = 't_zjrb_dailyInput',r_object = data,append = T)

}


#' 同步数据
#'
#' @param conn 连接信息
#'
#' @return 返回值
#' @export
#'
#' @examples
#' rpt_daily_sync()
rpt_daily_sync <- function(conn=tsda::conn_rds('jlrds')) {
  #backup the data

  sql1 <- paste0("insert into t_zjrb_dailyRptDel (FRptItemNo,FRptItemName,FDate,FAmount,FLevel)
select *  from t_zjrb_dailyRpt
where FDate in
(select FDate from t_zjrb_dailyInput)")
  tsda::sql_update(conn,sql1)
  # delete the data
  sql2 <- paste0("delete   from t_zjrb_dailyRpt
where FDate in
(select FDate from t_zjrb_dailyInput)")
  tsda::sql_update(conn,sql2)
  #insert into data
  sql3 <- paste0("insert into  t_zjrb_dailyRpt
select *  from t_zjrb_dailyInput")
  tsda::sql_update(conn,sql3)
  #truncate the input
  sql4 <- paste0("truncate table  t_zjrb_dailyInput")
  tsda::sql_update(conn,sql4)


}


#' 查询日志数据
#'
#' @param conn 连接
#' @param FStartDate 开始日期
#' @param FEndDate 结束日期
#' @param FLevel 级次
#' @param format 格式
#'
#' @return 返回值
#' @export
#'
#' @examples
#' rpt_daily_selectDb()
rpt_daily_selectDb <- function(conn=tsda::conn_rds('jlrds'),FStartDate='2020-06-01',FEndDate='2020-06-30',FLevel=0,format='rpt') {

  if(FLevel ==0){
    sql <- paste0("select  FRptItemNo,FRptItemName,FDate,FAmount  from t_zjrb_dailyRpt where FDate >= '",FStartDate,"' and FDate <='",FEndDate,"'")
  }else{
    sql <- paste0("select  FRptItemNo,FRptItemName,FDate,FAmount  from t_zjrb_dailyRpt where FDate >= '",FStartDate,"' and FDate <='",FEndDate,"'
and FLevel = ",FLevel," ")

  }
  data = tsda::sql_select(conn,sql)
  if(format != 'list'){
    data <- reshape2::dcast(data,FRptItemNo+FRptItemName~FDate,fun.aggregate = sum)
  }
  return(data)


}
