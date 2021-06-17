#
# create table t_mrpt_adj(
#   FCostItem varchar(50), FRptAmt money, FCostCenter varchar(50),
#   FYear int, FPeriod int, FVchNumber varchar(50), FVchCompany varchar(80), FVchTxt varchar(100),
#   FText  varchar(200), FSource  varchar(50), FBrand varchar(50), FChannel  varchar(50),
#   FRptItemNumber varchar(50), FRptItemName varchar(100)
# )
#
# select * from t_mrpt_adj where FBrand ='美素'



#' adj
#'
#' @param file  ile
#' @param conn conn
#'
#' @return return
#' @export
#'
#' @examples
#' adj_readData()
adj_readData <- function(file="data-raw/adj/手工调整数据模板--自然堂事业部1-5月.xlsx",conn=tsda::conn_rds('jlrds')) {

  library(readxl)
  data <- read_excel(file,  col_types = c("text", "numeric", "text",
                                          "numeric", "numeric", "text", "text",
                                          "text", "text", "text", "text", "text",
                                          "text", "text", "numeric", "numeric",
                                          "text"))
  col_sel <- c("成本要素" ,     "报表货币值",   "成本中心" ,    "会计年度" ,    "起始期间"   ,  "参考凭证编号",
           "参考公司代码", "凭证抬头文本" ,"名称"     ,    "数据来源"  ,  "品牌" ,        "渠道"  ,
            "报表项目代码", "报表项目名称" )

  res <- data[ ,col_sel]
  names(res) <- c("FCostItem" ,     "FRptAmt",   "FCostCenter" ,    "FYear" ,    "FPeriod"   ,
                  "FVchNumber",
                  "FVchCompany", "FVchTxt" ,"FText"     ,    "FSource"  ,  "FBrand" ,        "FChannel"  ,
                  "FRptItemNumber", "FRptItemName" )
  res <- res[!is.na(res$FCostItem),]
  ncount <- nrow(res)
  print(str(res))

 # res$FRptAmt <- as.numeric(res$FRptAmt*1.0)
  print(res$FRptAmt)
  View(res)
  if(ncount >0){
    tsda::db_writeTable(conn = conn,table_name = 't_mrpt_adj',r_object = res,append = T )
  }
  return(res)


}
