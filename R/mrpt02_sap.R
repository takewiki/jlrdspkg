#' 针对SAP成本中心进行标准化处理
#'
#' @param file 文件名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' sap_costCenter_standardize()
sap_costCenter_standardize <- function(file="data-raw/rpt_mngr/成本中心划分总表.xlsx") {
  #library(readxl)
  costCenterRatio <- readxl::read_excel(file,
                                col_types = c("text", "text", "numeric",
                                              "numeric", "numeric", "numeric",
                                              "numeric", "numeric", "numeric",
                                              "numeric", "numeric", "numeric",
                                              "numeric", "numeric", "numeric",
                                              "numeric", "numeric", "numeric",
                                              "numeric", "numeric", "numeric",
                                              "numeric", "numeric", "numeric",
                                              "numeric", "numeric"))
  mydata <- reshape2::melt(costCenterRatio,id=c('成本中心','渠道/市场'))
  names(mydata) <- c('FcostCenter','FType','FChannel','FValue')
  mydata$FValue <-  round(mydata$FValue /100,2)
  res <- mydata[!is.na(mydata$FValue), ]
  return(res)
  #View(mydata)
  # openxlsx::write.xlsx(mydata,'sapRatio.xlsx')
}


