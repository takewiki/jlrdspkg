
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


#' BW报表的业务处理规则
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_bw_ui_businessRule()
mrpt_bw_ui_businessRule <- function(conn=tsda::conn_rds('jlrds')) {
  sql <- paste0("   select  FInterId,FSolutionNumber,FValueName,FBrand_o,FChannel_o,FRptItem_o,FRptItemNumber    from t_mrpt_rule_bw")
  res <- tsda::sql_select(conn,sql)
  ncount <- nrow(res)
  if(ncount >0){
    names(res) <- c('序号','方案号','指标名称','品牌','渠道','报表项目名称','报表项目代码')
  }
  return(res)
}



#' 获取BW数据源
#'
#' @param conn 连接
#' @param FYear 年份
#' @param FPeriod 期间
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_bw_ds_data()
mrpt_bw_ds_data <- function(conn=tsda::conn_rds('jlrds'),FYear =2021,FPeriod =5) {
  sql <- paste0("    select FBrand,FChannel,FValueName,FSolutionNumber,FValue,FYear,FPeriod from t_mrpt_data_bw
 where FYear =  ",FYear," and FPeriod =   ",FPeriod,"
 order by FBrand,FChannel,  FValueName,FSolutionNumber")
  res <- tsda::sql_select(conn,sql)
  ncount <- nrow(res)
  if(ncount >0){
    names(res) <- c('品牌','渠道','指标名称','方案号','金额','年','月')
  }
  return(res)
}







#' 读取BW处理中间表
#'
#' @param conn 连接
#' @param FBrand 品牌
#' @param FChannel 渠道
#' @param FYear 年
#' @param FPeriod 月
#'
#' @return 返回
#' @export
#'
#' @examples
#' bw_res_ui_fromDB()
bw_res_ui_fromDB <-function(conn=tsda::conn_rds('jlrds'),
                             FBrand ='自然堂',
                             FChannel='商超',
                             FYear=2021,
                             FPeriod=5){
  sql <- paste0("select  * from vw_mrpt_data_bw
where FBrand ='",FBrand,"' and FChannel ='",FChannel,"' and FYear = ",FYear ," and FPeriod =   ",FPeriod)
  res <- tsda::sql_select(conn,sql)
  ncount =nrow(res)
  if(ncount >0){
    names(res) <- c('方案号','品牌','渠道','BW指标','报表金额','年','月','报表项目名称',
                    '报表项目代码')
  }
  return(res)


}



