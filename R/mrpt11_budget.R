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
