
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



#' 读取SAP数据源信息
#'
#' @param conn 连接
#' @param FYear 年份
#' @param FPeriod 月份
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_ds_ui_sapData()
mrpt_ds_ui_sapData <- function(conn=tsda::conn_rds('jlrds'),FYear =2021,FPeriod =5) {

  sql <- paste0("select * from t_mrpt_data_sap
where FYear =  ",FYear," and FPeriod =  ",FPeriod)
  r <- tsda::sql_select(conn,sql)
  ncount <- nrow(r)
  if (ncount >0){
    names(r) <- c('凭证日期','过账日期','成本中心代码','成本中心名称','成本要素代码','成本要素名称','金额','摘要','凭证号','年','月')
  }
  return(r)

}



#' 成本项目及报表项目对照表
#'
#' @param conn 连接
#' @param FYear 年份
#' @param FPeriod 月份
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_ui_itemMapping_costRpt()
mrpt_ui_itemMapping_costRpt <- function(conn=tsda::conn_rds('jlrds'),FYear =2021,FPeriod =5) {

  sql <- paste0("select * from t_mrpt_costItem_sap
where FYear =  ",FYear," and FPeriod =  ",FPeriod)
  r <- tsda::sql_select(conn,sql)
  ncount <- nrow(r)
  if (ncount >0){
    names(r) <- c('成本要素','统一费用名称','年','月')
  }
  return(r)

}






#' 报表项目表
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_md_ui_rptItem()
mrpt_md_ui_rptItem <- function(conn=tsda::conn_rds('jlrds')) {

  sql <- paste0("select * from t_mrpt_rptItem")
  r <- tsda::sql_select(conn,sql)
  ncount <- nrow(r)
  if (ncount >0){
    names(r) <- c('报表项目序号','报表项目名称','计算公式','渠道类型','费用统一名称')
  }
  return(r)

}





#处理表--------
#' SAP处理结果表
#'
#' @param conn 连接
#' @param FBrand 品牌
#' @param FChannel 渠道
#' @param FYear 年份
#' @param FPeriod 月份
#'
#' @return 返回值
#' @export
#'
#' @examples
#' sap_res_ui_fromDB()
sap_res_ui_fromDB <-function(conn=tsda::conn_rds('jlrds'),
                             FBrand ='自然堂',
                             FChannel='商超',
                             FYear=2021,
                             FPeriod=5){
  sql <- paste0("select * from vw_mrpt_data_sap
where FBrand2 ='",FBrand,"' and FChannel2='",FChannel,"' and FYear= ",FYear," and FPeriod= ",FPeriod)
  res <- tsda::sql_select(conn,sql)
  ncount =nrow(res)
  if(ncount >0){
    names(res) <- c('年','月','成本中心代码','成本要素名称','凭证金额','报表项目名称','报表项目代码','成本中心类型',
                    '品牌','渠道','渠道费用比率','分配金额','凭证号')
  }
  return(res)


}




