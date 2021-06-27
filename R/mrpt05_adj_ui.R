#' 读取调整数据
#'
#' @param conn 连接
#' @param FYear 年份
#' @param FPeriod 期间
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_adj_readData_ui()
mrpt_adj_readData_ui <- function(conn=tsda::conn_rds('jlrds'),FYear =2021,FPeriod =1) {
  sql <- paste0("select  *  from t_mrpt_adj
where FYear =  ",FYear,"  and FPeriod =  ",FPeriod)
  res <- tsda::sql_select(conn,sql)
  ncount <- nrow(res)
  if(ncount >0){
    names(res) <-c("成本要素" ,     "报表货币值",   "成本中心" ,    "会计年度" ,    "起始期间"   ,  "参考凭证编号",
                   "参考公司代码", "凭证抬头文本" ,"名称"     ,    "数据来源"  ,  "品牌" ,        "渠道"  ,
                   "报表项目代码", "报表项目名称" )

  }
  return(res)
}



#' 读取adj处理中间表
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
#' adj_res_ui_fromDB()
adj_res_ui_fromDB <-function(conn=tsda::conn_rds('jlrds'),
                            FBrand ='自然堂',
                            FChannel='商超',
                            FYear=2021,
                            FPeriod=5){
  sql <- paste0("select  * from vw_mrpt_adj
where FBrand ='",FBrand,"' and FChannel ='",FChannel,"' and FYear = ",FYear ," and FPeriod =   ",FPeriod)
  res <- tsda::sql_select(conn,sql)
  ncount =nrow(res)
  if(ncount >0){
    names(res) <- c('成本要素名称','报表金额','成本中心','年','月','凭证号','凭证归属公司','凭证摘要',
                    '凭证描述','来源','品牌','渠道','报表项目代码','报表项目名称','成本中心类型','品牌2','渠道2')
  }
  return(res)


}
