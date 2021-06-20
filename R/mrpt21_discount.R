#' 读取管报的供货折扣，用于计算公司零售额
#'
#' @param conn 连接
#' @param FBrand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_rule_discount()
mrpt_rule_discount <- function(conn=tsda::conn_rds('jlrds'),FBrand ='珀芙研') {
  sql <- paste0("select FBrand,FChannel,FCustomerName,FSaleDiscount from t_mrpt_discount
where FBrand ='",FBrand,"'")
  res <- tsda::sql_select(conn,sql)
  ncount <- nrow(res)
  if (ncount >0){
    names(res) <- c('品牌','渠道','客户名称','供货折扣')
  }
  return(res)
}
