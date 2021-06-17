
#' 找到成本中心及渠道费用分配标准
#'
#' @param conn 连接
#' @param FYear 年份
#' @param FPeriod 月
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_md_ui_costCenter()
mrpt_md_ui_costCenter <- function(conn=tsda::conn_rds('jlrds'),FYear =2021,FPeriod =5) {

sql <- paste0("select * from t_mrpt_costCenterRatio_sap
where FYear =  ",FYear," and FPeriod =  ",FPeriod)
r <- tsda::sql_select(conn,sql)
ncount <- nrow(r)
if (ncount >0){
  names(r) <- c('成本中心','类型','品牌渠道','分配值','品牌','渠道','年','月')
}
return(r)

}


#' 事业部定义表
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_md_ui_division()
mrpt_md_ui_division <- function(conn=tsda::conn_rds('jlrds')) {

  sql <- paste0("select * from t_mrpt_division")
  r <- tsda::sql_select(conn,sql)
  ncount <- nrow(r)
  if (ncount >0){
    names(r) <- c('品牌','渠道','渠道类型','事业部类型','过滤方案','描述1','描述2','成本中心类型')
  }
  return(r)

}
