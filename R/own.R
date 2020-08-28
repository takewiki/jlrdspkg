#' 查询自有资金模板表
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' own_getRptTpl()
own_getRptTpl <- function(conn=tsda::conn_rds('jlrds')) {
sql <- paste0("select FRptItemNo,FRptItemName  from t_md_rptTpl
where FRptName ='自有资金月报'")
res <- tsda::sql_select(conn,sql)
return(res)

}

#' 获取资产负债表中的往来数据
#'
#' @param conn 连接
#' @param FYear 年
#' @param FMonth 月
#' @param FUnit 金额单位
#' @param digit 保留小数
#'
#' @return 返回值
#' @export
#'
#' @examples
#' own_getBalanceSheet()
own_getBalanceSheet <- function(conn=tsda::conn_rds('jlrds'),FYear=2020,FMonth=7,FUnit='yuan',digit=2){
  options(scipen = 30,digits = 12)
   if (FUnit == 'yuan'){
     var_unit = '元'
     var_coef = 1.0
   }
  if (FUnit == 'wan'){
    var_unit = '元'
    var_coef = 10000.0
  }else{
    var_unit = '元'
    var_coef = 1.0
  }
  sql <- paste0("select FRptItemNo,FCompany,FAmount from t_zjrb_OwnRptAll where FYear = ",FYear," and FMonth = ",FMonth," and FUnit = '",var_unit,"'")
  res <- tsda::sql_select(conn,sql)
  print(res)
  ncount <-nrow(res)
  if(ncount >0){
    res$FAmount <- round(res$FAmount/var_coef,digit)
    print(res)

  }else{
    res <- NA
  }
  return(res)
}

#' 获取月份数据
#'
#' @param conn 连接
#' @param FYear 年份
#' @param FMonth 月份
#' @param FUnit 金额单位
#' @param digit 小数位数
#'
#' @return 返回值
#' @export
#'
#' @examples
#' own_getMonthRpt()
own_getMonthRpt <- function(conn=tsda::conn_rds('jlrds'),FYear=2020,FMonth=7,FUnit='yuan',digit=2){
  options(scipen = 30,digits = 12)
  if (FUnit =='yuan'){

    var_coef = 1.0
  }
  if (FUnit =='wan'){

    var_coef = 10000.0
  }else{

    var_coef = 1.0
  }
  sql <- paste0("select FRptItemNo,FAmount from t_zjrb_ownFromMonth where FYear = ",FYear," and Fmonth = ",FMonth)
  res <- tsda::sql_select(conn,sql)
  ncount <-nrow(res)
  if(ncount >0){
    res$FAmount <- round(res$FAmount/var_coef,digit)

  }else{
    res <- NA
  }
  return(res)
}

#' 按固定格式处理数据
#'
#' @param conn 连接
#' @param FYear 年份
#' @param FMonth 月份
#' @param FUnit 金额单位
#' @param digit 保留小数
#'
#' @return 返回值
#' @export
#'
#' @examples
#' own_deal()
own_deal <- function(conn=tsda::conn_rds('jlrds'),FYear=2020,FMonth=7,FUnit='yuan',digit=2){
   #获取模板数据
   data <- own_getRptTpl(conn=conn)
   #针对数据进行处理
   #增加金额数据
   #获取月份数据
   data_monthRpt <- own_getMonthRpt(conn=conn,FYear = FYear ,FMonth = FMonth,FUnit = FUnit,digit = digit)
   #获取资产负债表数据
   data_bs <- own_getBalanceSheet(conn=conn,FYear=FYear,FMonth = FMonth,FUnit = FUnit,digit=digit)
   data$FAmount <-0
   data[data$FRptItemNo =='1','FAmount'] <- as.numeric(NA)
   #处理月份数据
   logi_month <- !is.na(data_monthRpt)
   if(logi_month){
     #月报有数据
     data[data$FRptItemNo =='2','FAmount'] <- data_monthRpt[data_monthRpt$FRptItemNo =='A','FAmount']
     data[data$FRptItemNo =='3','FAmount'] <- data_monthRpt[data_monthRpt$FRptItemNo =='C','FAmount']
     data[data$FRptItemNo =='4','FAmount'] <- data_monthRpt[data_monthRpt$FRptItemNo =='D','FAmount']
     data[data$FRptItemNo =='5','FAmount'] <- data_monthRpt[data_monthRpt$FRptItemNo =='E','FAmount']
   }else{
     data[data$FRptItemNo =='2','FAmount'] <- as.numeric(NA)
     data[data$FRptItemNo =='3','FAmount'] <- as.numeric(NA)
     data[data$FRptItemNo =='4','FAmount'] <- as.numeric(NA)
     data[data$FRptItemNo =='5','FAmount'] <- as.numeric(NA)
   }
   #处理资产负债表数据
   logi_bs <- !is.na(data_bs)
   if(logi_bs){
     #有数据
     #处理应收
     data[data$FRptItemNo =='7','FAmount'] <- data_bs[data_bs$FRptItemNo =='A1' &data_bs$FCompany=='小计','FAmount']
     data[data$FRptItemNo =='8','FAmount'] <- data_bs[data_bs$FRptItemNo =='A1' &data_bs$FCompany=='美妆','FAmount']
     data[data$FRptItemNo =='9','FAmount'] <- data_bs[data_bs$FRptItemNo =='A1' &data_bs$FCompany=='集团','FAmount']
     #其他应收
     data[data$FRptItemNo =='10','FAmount'] <- data_bs[data_bs$FRptItemNo =='A2' &data_bs$FCompany=='小计','FAmount']
     data[data$FRptItemNo =='11','FAmount'] <- data_bs[data_bs$FRptItemNo =='A2' &data_bs$FCompany=='美妆','FAmount']
     data[data$FRptItemNo =='12','FAmount'] <- data_bs[data_bs$FRptItemNo =='A2' &data_bs$FCompany=='集团','FAmount']
     #预收
     data[data$FRptItemNo =='13','FAmount'] <- data_bs[data_bs$FRptItemNo =='A3' &data_bs$FCompany=='小计','FAmount']
     data[data$FRptItemNo =='14','FAmount'] <- data_bs[data_bs$FRptItemNo =='A3' &data_bs$FCompany=='美妆','FAmount']
     data[data$FRptItemNo =='15','FAmount'] <- data_bs[data_bs$FRptItemNo =='A3' &data_bs$FCompany=='集团','FAmount']
     #处理应收未收合计数
     value21 <-data[data$FRptItemNo =='7','FAmount']
     value22 <-data[data$FRptItemNo =='10','FAmount']
     value23 <-data[data$FRptItemNo =='13','FAmount']
     data[data$FRptItemNo =='6','FAmount'] = value21 + value22 - value23

     #处理应付
     data[data$FRptItemNo =='17','FAmount'] <- data_bs[data_bs$FRptItemNo =='B1' &data_bs$FCompany=='小计','FAmount']
     data[data$FRptItemNo =='18','FAmount'] <- data_bs[data_bs$FRptItemNo =='B1' &data_bs$FCompany=='美妆','FAmount']
     data[data$FRptItemNo =='19','FAmount'] <- data_bs[data_bs$FRptItemNo =='B1' &data_bs$FCompany=='集团','FAmount']
     #处理其他应付
     data[data$FRptItemNo =='20','FAmount'] <- data_bs[data_bs$FRptItemNo =='B2' &data_bs$FCompany=='小计','FAmount']
     data[data$FRptItemNo =='21','FAmount'] <- data_bs[data_bs$FRptItemNo =='B2' &data_bs$FCompany=='美妆','FAmount']
     data[data$FRptItemNo =='22','FAmount'] <- data_bs[data_bs$FRptItemNo =='B2' &data_bs$FCompany=='集团','FAmount']
     #处理预付
     data[data$FRptItemNo =='23','FAmount'] <- data_bs[data_bs$FRptItemNo =='B3' &data_bs$FCompany=='小计','FAmount']
     data[data$FRptItemNo =='24','FAmount'] <- data_bs[data_bs$FRptItemNo =='B3' &data_bs$FCompany=='美妆','FAmount']
     data[data$FRptItemNo =='25','FAmount'] <- data_bs[data_bs$FRptItemNo =='B3' &data_bs$FCompany=='集团','FAmount']
     value31 <-data[data$FRptItemNo =='17','FAmount']
     value32 <-data[data$FRptItemNo =='20','FAmount']
     value33 <-data[data$FRptItemNo =='23','FAmount']
     data[data$FRptItemNo =='16','FAmount'] = value31 + value32 - value33






   }else{
     #没有数据
     #处理应收
     data[data$FRptItemNo =='7','FAmount'] <- as.numeric(NA)
     data[data$FRptItemNo =='8','FAmount'] <- as.numeric(NA)
     data[data$FRptItemNo =='9','FAmount'] <- as.numeric(NA)
     #其他应收
     data[data$FRptItemNo =='10','FAmount'] <- as.numeric(NA)
     data[data$FRptItemNo =='11','FAmount'] <-as.numeric(NA)
     data[data$FRptItemNo =='12','FAmount'] <- as.numeric(NA)
     #预收
     data[data$FRptItemNo =='13','FAmount'] <- as.numeric(NA)
     data[data$FRptItemNo =='14','FAmount'] <- as.numeric(NA)
     data[data$FRptItemNo =='15','FAmount'] <- as.numeric(NA)
     #处理应收未收合计数

     data[data$FRptItemNo =='6','FAmount'] = as.numeric(NA)

     #处理应付
     data[data$FRptItemNo =='17','FAmount'] <- as.numeric(NA)
     data[data$FRptItemNo =='18','FAmount'] <- as.numeric(NA)
     data[data$FRptItemNo =='19','FAmount'] <- as.numeric(NA)
     #处理其他应付
     data[data$FRptItemNo =='20','FAmount'] <- as.numeric(NA)
     data[data$FRptItemNo =='21','FAmount'] <- as.numeric(NA)
     data[data$FRptItemNo =='22','FAmount'] <- as.numeric(NA)
     #处理预付
     data[data$FRptItemNo =='23','FAmount'] <- as.numeric(NA)
     data[data$FRptItemNo =='24','FAmount'] <- as.numeric(NA)
     data[data$FRptItemNo =='25','FAmount'] <- as.numeric(NA)

     data[data$FRptItemNo =='16','FAmount'] = as.numeric(NA)
   }

   #处理最后一次数据
   value41 <-data[data$FRptItemNo =='5','FAmount']
   value42 <-data[data$FRptItemNo =='6','FAmount']
   value43 <-data[data$FRptItemNo =='16','FAmount']

   data[data$FRptItemNo =='26','FAmount'] = value41+value42-value43
   #data$FYear <- FYear
   #data$FMonth <- FMonth
   names(data) <- c('序号','项目','余额')
   return(data)













}
