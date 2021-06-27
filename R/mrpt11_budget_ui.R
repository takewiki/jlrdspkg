#' 读取预算数据
#'
#' @param conn 连接
#' @param FBrand 品牌
#' @param FChannel 渠道
#' @param FYear 年
#' @param FPeriod 月
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_budget_readFromDB_ByBrandChannel()
mrpt_budget_readFromDB_ByBrandChannel <- function(conn=tsda::conn_rds('jlrds'),
                                   FBrand ='珀芙研',
                                   FChannel ='药房',
                                   FYear =2021,
                                   FPeriod =5
                                   ) {

  sql <- paste0("select *  from t_mrpt_budget
where  FBrand ='",FBrand,"' and FChannel ='",FChannel,"'  and FYear =  ",FYear," and FPeriod = ",FPeriod,"
order by FRptItemNumber")
  #print(sql)
  res <- tsda::sql_select(conn,sql)
  ncount <- nrow(res)
  if(ncount >0){
    names(res) <-c('品牌','渠道','子渠道','年','月','报表项目代码','报表项目名称','报表金额')
  }
  return(res)
}
