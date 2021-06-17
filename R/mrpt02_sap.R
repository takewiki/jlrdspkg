#' 根据渠道类型返回名称
#'
#' @param conn 连接
#' @param FChannelName 渠道描述
#'
#' @return 返回值
#' @export
#'
#' @examples
#' sap_getBrandChannel_byName()
sap_getBrandChannel_byName <- function(conn=tsda::conn_rds('jlrds'),FChannelName='COMO市场') {
  sql <- paste0("select  FBrand,FChannel from t_mrpt_division
where FDescription1 ='",FChannelName,"'")
  r <- tsda::sql_select(conn,sql)
  ncount <- nrow(r)
  if(ncount >0){
    res <- r
    return(res)
  }

}

#' 针对SAP成本中心进行标准化处理
#'
#' @param file 文件名
#' @param FYear 年份
#' @param FPeriod 月份
#' @param conn 连接信息
#'
#' @return 返回值
#' @export
#'
#' @examples
#' sap_costCenter_standardize()
sap_costCenter_standardize <- function(file="data-raw/rpt_mngr/成本中心划分总表 - V3.xlsx",
                                       FYear =2021,
                                       FPeriod =4,
                                       conn=tsda::conn_rds('jlrds')
                                       ) {
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
  print(nrow(res))
  #针对数据进行处理
  bc =as.character(res$FChannel)
  nlen = length(bc)
  # bc_split = strsplit(bc,"_")
  #bc_res <- lapply(bc_split, function(item){
    bc_res <- lapply(1:nlen, function(i){
      item = bc[i]
      print(paste0(i,"of",nlen))
      print(item)
    data <- sap_getBrandChannel_byName(conn=conn,FChannelName = item)
    print(data)
    FBrand2 = data$FBrand[1]
    FChannel2 =data$FChannel[1]
    res <- data.frame(FBrand2,FChannel2,stringsAsFactors = F)
    print(res)
    return(res)
  })
  bc_res2 = do.call('rbind',bc_res)
  print(nrow(bc_res2))
  print(nrow(res))
  res3 = cbind(res,bc_res2)
  res3$FYear = FYear
  res3$FPeriod =FPeriod
  ncount =nrow(res3)
  print(res3)
  if(ncount >0){
    tsda::db_writeTable(conn = conn,table_name = 't_mrpt_costCenterRatio_sap',r_object = res3,append = T)
  }

  return(res3)
  #View(mydata)
  # openxlsx::write.xlsx(mydata,'sapRatio.xlsx')
}




#' SAP报表读取数据
#'
#' @param file 文件
#' @param FYear 年
#' @param FPeriod 月
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' sap_rpt_readData()
sap_rpt_readData <- function(file="data-raw/mrpt04/SAPdata/SAP_04_2021V2.XLSX",
                             FYear =2021,
                             FPeriod =4,
                             conn=tsda::conn_rds('jlrds')) {

  #library(readxl)
  res3 <- readxl::read_excel(file,
                            col_types = c("date", "date", "text",
                                          "text", "text", "text", "numeric",
                                          "text", "text"))
  names(res3) <-c('FVchDate',	'FPostDate',	'FCostCenterNo',	'FCostObjName',	'FCostItemNumber',
                 'FCostItemName',
                 'FRptAmt',	'FVchNote',	'FVchNo')
  res3$FYear = FYear
  res3$FPeriod =FPeriod
  ncount =nrow(res3)
  if(ncount >0){
    tsda::db_writeTable(conn = conn,table_name = 't_mrpt_data_sap',r_object = res3,append = T)
  }



}


#' 写入成本项目数据
#'
#' @param file 文件
#' @param FYear 年
#' @param FPeriod 月
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' sap_rpt_costItem_readData()
sap_rpt_costItem_readData <- function(file="data-raw/rpt_mngr/SAP成本要素对应关系表.xlsx",
                             FYear =2021,
                             FPeriod =4,
                             conn=tsda::conn_rds('jlrds')) {

  #library(readxl)
  res3 <- readxl::read_excel(file
                             )
  names(res3) <-c('FCostItemName','FRptItemName')
  res3$FYear = FYear
  res3$FPeriod =FPeriod
  ncount =nrow(res3)
  if(ncount >0){
    tsda::db_writeTable(conn = conn,table_name = 't_mrpt_costItem_sap',r_object = res3,append = T)
  }



}






