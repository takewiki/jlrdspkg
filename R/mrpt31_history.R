#' 读取报表项目
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_rptItem_getNumber()
mrpt_rptItem_getNumber <- function(conn=tsda::conn_rds('jlrds')) {
  sql <- paste0("select FRptItemNumber from t_mrpt_rptItem")
  res <- tsda::sql_select(conn,sql)
  ncount <- nrow(res)
  if(ncount >0){
    res <- res$FRptItemNumber
  }else{
    res<-paste0('I',1:44)
  }
  return(res)

}

#' 读取历史数据真实数据
#'
#' @param conn 连接
#' @param file_name 文件名
#' @param sheet_name 页签名
#' @param FBrand 品牌
#' @param FChannel 渠道
#' @param FSubChannel 子渠道
#' @param FYear 年份
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_HistoryData_ReadOne()
mrpt_HistoryData_ReadOne <- function(conn=tsda::conn_rds('jlrds'),
                                 file_name="data-raw/hist/01-美素大客户事业部2019历史管报数据_reviewed.xlsx",
                                 sheet_name="美素美妆",
                                 FBrand='美素',
                                 FChannel='美妆',
                                 FYear=2019,
                                 FSubChannel=NA) {


data <- readxl::read_excel(file_name,
                                              sheet = sheet_name,
                   col_types = c("text",
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
ncount2 <- nrow(data2)
if(ncount2 >0){
  #写入数据库
  tsda::db_writeTable(conn = conn,table_name = 't_mrpt_actual',r_object = data2,append = T)
}
return(data2)

}


#' 读取历史数据
#'
#' @param conn 连接
#' @param toc_name 目录文件名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_HistoryData_ReadAll()
mrpt_HistoryData_ReadAll  <- function(conn=tsda::conn_rds('jlrds'),
                                                                 toc_name="data-raw/hist/hist_toc.xlsx"
){


  #library(readxl)
  #非常不错的处理方法
  hist_toc <- readxl::read_excel(toc_name,
                         sheet = "toc")
  #View(hist_toc)
  ncount <- nrow(hist_toc)
  if(ncount >0){
    lapply(1:ncount, function(i){
      print(paste0(i,'of',ncount))
      file_name= hist_toc$file_name[i]
      print(file_name)
      sheet_name=hist_toc$sheet_name[i]
      print(sheet_name)
      FBrand= hist_toc$FBrand[i]
      FChannel= hist_toc$FChannel[i]
      FYear= hist_toc$FYear[i]
      FSubChannel= hist_toc$FSubChannel[i]

      mrpt_HistoryData_ReadOne(conn = conn,file_name = file_name,sheet_name = sheet_name,FBrand = FBrand,FChannel = FChannel,FYear = FYear,FSubChannel = FSubChannel)


    })
  }

}

