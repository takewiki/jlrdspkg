
#' 读取历史数据
#'
#' @param conn 连接
#' @param FYear 年
#' @param FPeriod 期间
#' @param FBrand 品牌
#' @param FChannel 渠道
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_actualData_read()
mrpt_actualData_read <- function(conn=tsda::conn_rds('jlrds'),FYear= 2020,FPeriod =1,FBrand ='美素',FChannel='大客户') {

  sql <- paste0("select  FBrand,FChannel,FSubChannel,FRptItemNumber,FRptItemName,FAmt , FYear,FPeriod   from t_mrpt_actual
where FYear= ",FYear," and FPeriod =  ",FPeriod," and FBrand ='",FBrand,"' and FChannel='",FChannel,"'")
  res <- tsda::sql_select(conn,sql)
  ncount <- nrow(res)
  if(ncount >0){
    names(res) <- c('品牌','渠道','子渠道','报表项目代码','报表项目名称','报表金额','年','月')



  }

  return(res)

}
