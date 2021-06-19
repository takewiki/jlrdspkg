
#' 查看BW报表表头
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_bw_ui_getHeadingName()
mrpt_bw_ui_getHeadingName <- function(conn=tsda::conn_rds('jlrds')) {
  sql <- paste0("select FInterId,FName_show,FName_sql from t_mrpt_valueType
where FDataSource='bw'
order by FIndex")
  res <- tsda::sql_select(conn,sql)
  ncount <- nrow(res)
  if(ncount >0){
    names(res) <- c('序号','显示名称','存储名称')
  }
  return(res)
}


#' BW报表的维度名称
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_bw_ui_getDimName()
mrpt_bw_ui_getDimName <- function(conn=tsda::conn_rds('jlrds')) {
  sql <- paste0(" select   FSulotionNumber,FIndex,FName_show,FName_sql   from t_mrpt_dimType")
  res <- tsda::sql_select(conn,sql)
  ncount <- nrow(res)
  if(ncount >0){
    names(res) <- c('方案号','序号','显示名称','存储名称')
  }
  return(res)
}




