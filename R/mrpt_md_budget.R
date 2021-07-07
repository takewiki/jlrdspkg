#' 获取品牌信息
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_md_brand()
mrpt_md_brand<- function(conn=tsda::conn_rds('jlrds')) {
  sql <- paste0("select  distinct FBrand  from t_mrpt_brand")
  r <- tsda::sql_select(conn = conn,sql)
  ncount <- nrow(r)
  if(ncount>0){
    res <-tsdo::vect_as_list(r$FBrand)
  }
  return(res)

}

#' 获取渠道信息
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_md_channel()
mrpt_md_channel<- function(conn=tsda::conn_rds('jlrds')) {
  sql <- paste0("select  distinct  FChannel
from t_mrpt_channel
where FChannel <> '市场'")
  r <- tsda::sql_select(conn = conn,sql)
  ncount <- nrow(r)
  if(ncount>0){
    res <-tsdo::vect_as_list(r$FChannel)
  }
  return(res)

}

#' 获取渠道信息并具有子渠道
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_md_channel2()
mrpt_md_channel2<- function(conn=tsda::conn_rds('jlrds')) {
  sql <- paste0("select  distinct  FChannel
from t_mrpt_channel2 ")
  r <- tsda::sql_select(conn = conn,sql)
  ncount <- nrow(r)
  if(ncount>0){
    res <-tsdo::vect_as_list(r$FChannel)
  }
  return(res)

}

#' 获取子渠道信息
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_md_channel()
mrpt_md_subChannel<- function(conn=tsda::conn_rds('jlrds')) {
  sql <- paste0("select FName  from t_mrpt_subChannel")
  r <- tsda::sql_select(conn = conn,sql)
  ncount <- nrow(r)
  if(ncount>0){
    res <-tsdo::vect_as_list(r$FName)
  }
  return(res)

}


