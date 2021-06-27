#' 写入成本项目数据
#'
#' @param file 文件
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_budget_readData()
mrpt_budget_readData <- function(file="data-raw/mrpt04/BudgetData/预算整合数据.xlsx",

                                      conn=tsda::conn_rds('jlrds')) {

  #library(readxl)
  options(digits=15,scipen = 12)
  res3 <- readxl::read_excel(file
  )
  names(res3) <-c('FBrand',	'FChannel',	'FRptItemNumber',	'FRptItemName',   'FBudgetAmt','FYear','FPeriod')
  tsdo::na_values( res3$FBudgetAmt,0)
  res3$FBudgetAmt[is.na(res3$FBudgetAmt)] <- 0
  res3$FBudgetAmt <-round(res3$FBudgetAmt*10000,2)
  #res3$FBudgetAmt

  #res3$FYear = FYear
  #res3$FPeriod =FPeriod
  ncount =nrow(res3)
  if(ncount >0){
    tsda::db_writeTable(conn = conn,table_name = 't_mrpt_data_budget',r_object = res3,append = T)
  }

  return(res3)



}


#' 按事业部及预算格式读取预算数据
#'
#' @param file 文件
#' @param conn 连接
#' @param sheet 页答
#' @param FBrand 品牌
#' @param FChannel 渠道
#' @param FYear 年份
#' @param FSubChannel  子渠道
#' @param FPeriod  月份数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_budget_readData_ByDivision()
mrpt_budget_readData_ByDivision_cumPeriod <- function(file="data-raw/budget/执行预算_RDS_珀_OK.xlsx",
                                            conn=tsda::conn_rds('jlrds'),
                                            sheet = "电商",
                                            FBrand='珀芙研',
                                            FChannel='电商',
                                            FYear=2021,
                                            FPeriod =5,
                                            FSubChannel=NA) {
  library(readxl)
  data <- read_excel(file,
                     sheet = sheet, col_types = c("text",
                                                 "numeric", "numeric", "numeric",
                                                 "numeric", "numeric", "numeric",
                                                 "numeric", "numeric", "numeric",
                                                 "numeric", "numeric", "numeric",
                                                 "skip"), skip = 1)
  #print(data)
  #print(1)
  col_names <- names(data)
  col_count <- length(col_names)
  lapply(2:col_count, function(i){
    data[i] <<- round(data[i],2)
  })
  data[is.na(data)] <- 0
  data$`报表项目代码` <- mrpt_rptItem_getNumber(conn = conn)
  data2 <- reshape2::melt(data = data,id.vars=c('报表项目代码','报表项目'),variable.name='FPeriod',value.name='FAmt')
  data2$FPeriod <- as.integer(  stringr::str_replace(data2$FPeriod,'月',''))
  data2$FYear <- FYear
  data2$FBrand <-FBrand
  data2$FChannel <-FChannel
  data2$FSubChannel <- FSubChannel
  # print(data2)
  data2 <- data2[,c('FBrand','FChannel','FSubChannel','FYear','FPeriod','报表项目代码','报表项目','FAmt')]
  names(data2) <- c('FBrand','FChannel','FSubChannel','FYear','FPeriod','FRptItemNumber','FRptItemName','FAmt')
  #小于当前月份，不提供更多数据
  data2 <- data2[data2$FPeriod <= FPeriod, ]
  ncount2 <- nrow(data2)
  if(ncount2 >0){
    tsda::db_writeTable(conn = conn,table_name = 't_mrpt_budget',r_object = data2,append = T)
  }
  return(data2)
}


#' 获取当前期间的预算数
#'
#' @param file 文件
#' @param conn 连接
#' @param sheet 页答
#' @param FBrand 品牌
#' @param FChannel 渠道
#' @param FYear 年
#' @param FPeriod 月
#' @param FSubChannel  子渠道
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_budget_readData_ByDivision_currentPeriod()
mrpt_budget_readData_ByDivision_currentPeriod <- function(file="data-raw/budget/执行预算_RDS_珀_OK.xlsx",
                                                      conn=tsda::conn_rds('jlrds'),
                                                      sheet = "电商",
                                                      FBrand='珀芙研',
                                                      FChannel='电商',
                                                      FYear=2021,
                                                      FPeriod =5,
                                                      FSubChannel=NA) {
  library(readxl)
  data <- read_excel(file,
                     sheet = sheet, col_types = c("text",
                                                  "numeric", "numeric", "numeric",
                                                  "numeric", "numeric", "numeric",
                                                  "numeric", "numeric", "numeric",
                                                  "numeric", "numeric", "numeric",
                                                  "skip"), skip = 1)
  #print(data)
  #print(1)
  col_names <- names(data)
  col_count <- length(col_names)
  lapply(2:col_count, function(i){
    data[i] <<- round(data[i],2)
  })
  data[is.na(data)] <- 0
  data$`报表项目代码` <- mrpt_rptItem_getNumber(conn = conn)
  data2 <- reshape2::melt(data = data,id.vars=c('报表项目代码','报表项目'),variable.name='FPeriod',value.name='FAmt')
  data2$FPeriod <- as.integer(  stringr::str_replace(data2$FPeriod,'月',''))
  data2$FYear <- FYear
  data2$FBrand <-FBrand
  data2$FChannel <-FChannel
  data2$FSubChannel <- FSubChannel
  # print(data2)
  data2 <- data2[,c('FBrand','FChannel','FSubChannel','FYear','FPeriod','报表项目代码','报表项目','FAmt')]
  names(data2) <- c('FBrand','FChannel','FSubChannel','FYear','FPeriod','FRptItemNumber','FRptItemName','FAmt')
  #小于当前月份，不提供更多数据
  data2 <- data2[data2$FPeriod == FPeriod, ]
  ncount2 <- nrow(data2)
  if(ncount2 >0){
    tsda::db_writeTable(conn = conn,table_name = 't_mrpt_budget',r_object = data2,append = T)
  }
  return(data2)
}


