#1.0同步数据---------
#' 同步BW2版本的纵表数据
#'
#' @param conn 连接
#' @param FYear 年
#' @param FPeriod 月
#'
#' @return 返回值
#' @export
#'
#' @examples
#'
bw2_sync_data  <- function(conn=tsda::conn_rds('jlrds'),FYear =2021 ,FPeriod =6) {
 #删除已有数据
sql_del <- paste0("delete  from  rds_t_mrpt_ds_bw_rpa_v
where FYear =  ",FYear," and FPeriod =  ",FPeriod)
tsda::sql_update(conn,sql_del)
#插入新的数据
sql_ins <- paste0("insert into rds_t_mrpt_ds_bw_rpa_v
select   *  from vw_mrpt_ds_bw_rpa_v
where FYear = ",FYear," and FPeriod =  ",FPeriod)
tsda::sql_update(conn,sql_ins)

}

#' 维度筛选13A物料组包含
#'
#' @param conn 连接
#' @param FSolutionNumber 方案号
#' @param FSubNumber 子方案号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' bw2_Filter_F13_in()
bw2_Filter_F13_in <- function(conn=tsda::conn_rds('jlrds'),FSolutionNumber = 'S001', FSubNumber =1 ){

  sql <- paste0("select F13_itemGroupName from  t_mrpt_ds_bw_F13ItemGroupNumber_in
 where FSolutionNumber = '",FSolutionNumber,"' and FSubNumber =  ",FSubNumber," ")
  # print(sql)
  r <- tsda::sql_select(conn,sql)
  ncount <- nrow(r)
  if(ncount >0){
    res <- paste0(" F13_itemGroupName in ( ",sql," )  and")
  }else{
    res <-" "
  }
  return(res)

}


#' 维度筛选13B物料组排除
#'
#' @param conn 连接
#' @param FSolutionNumber 方案号
#' @param FSubNumber 子方案号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' bw2_Filter_F13_Notin()
bw2_Filter_F13_Notin <- function(conn=tsda::conn_rds('jlrds'),FSolutionNumber = 'S001', FSubNumber =1 ){

  sql <- paste0("select F13_itemGroupName from  t_mrpt_ds_bw_F13ItemGroupNumber_Notin
 where FSolutionNumber = '",FSolutionNumber,"' and FSubNumber =  ",FSubNumber," ")
  # print(sql)
  r <- tsda::sql_select(conn,sql)
  ncount <- nrow(r)
  if(ncount >0){
    res <- paste0(" F13_itemGroupName not in ( ",sql," )  and")
  }else{
    res <-" "
  }
  return(res)

}


#' 维度筛选14品牌
#'
#' @param conn 连接
#' @param FSolutionNumber 方案号
#' @param FSubNumber 子方案号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' bw2_Filter_F13_in()
bw2_Filter_F14_in <- function(conn=tsda::conn_rds('jlrds'),FSolutionNumber = 'S001', FSubNumber =1 ){

  sql <- paste0("select F14_brandName from  t_mrpt_ds_bw_F14_brandName_in
 where FSolutionNumber = '",FSolutionNumber,"' and FSubNumber =  ",FSubNumber," ")
  # print(sql)
  r <- tsda::sql_select(conn,sql)
  ncount <- nrow(r)
  if(ncount >0){
    res <- paste0(" F14_brandName  in  ( ",sql," )  and")
  }else{
    res <-" "
  }
  return(res)

}

#' 维度筛选33子渠道
#'
#' @param conn 连接
#' @param FSolutionNumber 方案号
#' @param FSubNumber 子方案号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' bw2_Filter_F13_in()
bw2_Filter_F33_in <- function(conn=tsda::conn_rds('jlrds'),FSolutionNumber = 'S001', FSubNumber =1 ){

  sql <- paste0("select F33_subChannelName from  t_mrpt_ds_bw_F33_subChannelName_in
 where FSolutionNumber = '",FSolutionNumber,"' and FSubNumber =  ",FSubNumber," ")
  # print(sql)
  r <- tsda::sql_select(conn,sql)
  ncount <- nrow(r)
  if(ncount >0){
    res <- paste0(" F33_subChannelName in   ( ",sql," )  and")
  }else{
    res <-" "
  }
  return(res)

}


#' 维度筛选41渠道
#'
#' @param conn 连接
#' @param FSolutionNumber 方案号
#' @param FSubNumber 子方案号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' bw2_Filter_F13_in()
bw2_Filter_F41_in <- function(conn=tsda::conn_rds('jlrds'),FSolutionNumber = 'S001', FSubNumber =1 ){

  sql <- paste0("select F41_channelName from t_mrpt_ds_bw_F41_channelName_in
 where FSolutionNumber = '",FSolutionNumber,"' and FSubNumber =  ",FSubNumber," ")
  # print(sql)
  r <- tsda::sql_select(conn,sql)
  ncount <- nrow(r)
  if(ncount >0){
    res <- paste0(" F41_channelName in   ( ",sql," )  and")
  }else{
    res <-" "
  }
  return(res)

}


#' 维度筛选30A客户包含
#'
#' @param conn 连接
#' @param FSolutionNumber 方案号
#' @param FSubNumber 子方案号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' bw2_Filter_F13_in()
bw2_Filter_F30_in <- function(conn=tsda::conn_rds('jlrds'),FSolutionNumber = 'S001', FSubNumber =1 ){

  sql <- paste0("select F30_customerName from t_mrpt_ds_bw_F30_customerName_in
 where FSolutionNumber = '",FSolutionNumber,"' and FSubNumber =  ",FSubNumber," ")
  # print(sql)
  r <- tsda::sql_select(conn,sql)
  ncount <- nrow(r)
  if(ncount >0){
    res <- paste0(" F30_customerNumber  in   ( ",sql," )  and")
  }else{
    res <-" "
  }
  return(res)

}

#' 维度筛选30B客户排除
#'
#' @param conn 连接
#' @param FSolutionNumber 方案号
#' @param FSubNumber 子方案号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' bw2_Filter_F13_in()
bw2_Filter_F30_Notin <- function(conn=tsda::conn_rds('jlrds'),FSolutionNumber = 'S001', FSubNumber =1 ){

  sql <- paste0("select F30_customerName from t_mrpt_ds_bw_F30_customerName_Notin
 where FSolutionNumber = '",FSolutionNumber,"' and FSubNumber =  ",FSubNumber," ")
  # print(sql)
  r <- tsda::sql_select(conn,sql)
  ncount <- nrow(r)
  if(ncount >0){
    res <- paste0(" F30_customerNumber   not in  ( ",sql," )  and")
  }else{
    res <-" "
  }
  return(res)

}



#' 维度筛选37地区销售部
#'
#' @param conn 连接
#' @param FSolutionNumber 方案号
#' @param FSubNumber 子方案号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' bw2_Filter_F13_in()
bw2_Filter_F37_in <- function(conn=tsda::conn_rds('jlrds'),FSolutionNumber = 'S001', FSubNumber =1 ){

  sql <- paste0("select F37_disctrictSaleDeptName  from t_mrpt_ds_bw_F37_disctrictSaleDeptName_in
 where FSolutionNumber = '",FSolutionNumber,"' and FSubNumber =  ",FSubNumber," ")
  # print(sql)
  r <- tsda::sql_select(conn,sql)
  ncount <- nrow(r)
  if(ncount >0){
    res <- paste0(" F37_disctrictSaleDeptName in   ( ",sql," )  and")
  }else{
    res <-" "
  }
  return(res)

}


#' 维度筛选61成本中心控制
#'
#' @param conn 连接
#' @param FSolutionNumber 方案号
#' @param FSubNumber 子方案号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' bw2_Filter_F61_in()
bw2_Filter_F61_in <- function(conn=tsda::conn_rds('jlrds'),FSolutionNumber = 'S001', FSubNumber =1 ){

  sql <- paste0("select F61_costCenterControlNumber  from t_mrpt_ds_bw_F61_costCenterControlNumber_in
 where FSolutionNumber = '",FSolutionNumber,"' and FSubNumber =  ",FSubNumber," ")
  # print(sql)
  r <- tsda::sql_select(conn,sql)
  ncount <- nrow(r)
  if(ncount >0){
    res <- paste0(" F61_costCenterControlNumber  in   ( ",sql," )  and")
  }else{
    res <-" "
  }
  return(res)

}


#' 维度筛选指标数据,一定是最后使用，不需要and
#'
#' @param conn 连接
#' @param FSolutionNumber 方案号
#' @param FSubNumber 子方案号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' bw2_Filter_F13_in()
bw2_Filter_valueType <- function(conn=tsda::conn_rds('jlrds'),FSolutionNumber = 'S001', FSubNumber =1 ){

  sql <- paste0("select FValueType  from t_mrpt_ds_bw_formula
 where FSolutionNumber = '",FSolutionNumber,"' and FSubNumber =  ",FSubNumber," ")
  # print(sql)
  r <- tsda::sql_select(conn,sql)
  ncount <- nrow(r)
  if(ncount >0){
    res <- paste0(" FValueType in   ( ",sql," )  ")
  }else{
    res <-" "
  }
  return(res)

}



#' 维度筛选指标数据,一定是最后使用，不需要and
#'
#' @param conn 连接
#' @param FSolutionNumber 方案号
#' @param FSubNumber 子方案号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' bw2_Filter_F13_in()
bw2_formula <- function(conn=tsda::conn_rds('jlrds'),FSolutionNumber = 'S001', FSubNumber =1 ){

  sql <- paste0("select  FBrand,FChannel,FRptItemNumber,FRptItemName from t_mrpt_ds_bw_formula
 where FSolutionNumber = '",FSolutionNumber,"' and FSubNumber =  ",FSubNumber," ")
  # print(sql)
  r <- tsda::sql_select(conn,sql)
  ncount <- nrow(r)
  if(ncount >0){
    res <- r
  }else{
    res <- NULL
  }
  return(res)

}

#' 整合查询
#'
#' @param conn 连接
#' @param FSolutionNumber 方案号
#' @param FSubNumber 子序号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' bw2_selectData
bw2_selectData <- function(conn=tsda::conn_rds('jlrds'),
                           FSolutionNumber = 'S001',
                           FSubNumber =1,
                           FYear =2021,
                           FPeriod =6 ){
  sql_heading = paste0("select *  from rds_t_mrpt_ds_bw_rpa_v where  FSolutionNumber = '",FSolutionNumber,"' and   FYear = ",FYear," and  FPeriod =  ",FPeriod,"  and  ")
  sql_all <- paste0(sql_heading,
                    bw2_Filter_F13_in(conn = conn,FSolutionNumber = FSolutionNumber,FSubNumber = FSubNumber),
                    bw2_Filter_F13_Notin(conn = conn,FSolutionNumber = FSolutionNumber,FSubNumber = FSubNumber),
                    bw2_Filter_F14_in(conn = conn,FSolutionNumber = FSolutionNumber,FSubNumber = FSubNumber),
                    bw2_Filter_F30_in(conn = conn,FSolutionNumber = FSolutionNumber,FSubNumber = FSubNumber),
                    bw2_Filter_F30_Notin(conn = conn,FSolutionNumber = FSolutionNumber,FSubNumber = FSubNumber),
                    bw2_Filter_F33_in(conn = conn,FSolutionNumber = FSolutionNumber,FSubNumber = FSubNumber),
                    bw2_Filter_F37_in(conn = conn,FSolutionNumber = FSolutionNumber,FSubNumber = FSubNumber),
                    bw2_Filter_F41_in(conn = conn,FSolutionNumber = FSolutionNumber,FSubNumber = FSubNumber),
                    bw2_Filter_F61_in(conn = conn,FSolutionNumber = FSolutionNumber,FSubNumber = FSubNumber),
                    bw2_Filter_valueType(conn = conn,FSolutionNumber = FSolutionNumber,FSubNumber = FSubNumber)

                    )
  # cat(sql_all)
  r <- tsda::sql_select(conn,sql_all)
  #查询数据
  info = bw2_formula(conn = conn,FSolutionNumber = FSolutionNumber,FSubNumber = FSubNumber)

  ncount <- nrow(r)
  if(ncount >0){
    #有数据的情况下
    r$FSubNumber <- FSubNumber
    r$FBrand_o <- info$FBrand
    r$FChannel_o <- info$FChannel
    r$FRptItemNumber_o <- info$FRptItemNumber
    r$FRptItemName_o <- info$FRptItemName
    #删除已有的数据
    sql_del <- paste0("delete from rds_t_mrpt_ds_bw_rpa_ruled
where FSolutionNumber ='",FSolutionNumber,"' and FSubNumber = ",FSubNumber," and FYear = ",FYear," and FPeriod = ",FPeriod,"")
    tsda::sql_update(conn,sql_del)
    #上传数据
    tsda::db_writeTable(conn = conn,table_name = 'rds_t_mrpt_ds_bw_rpa_ruled',r_object = r,append = T)



  }

  return(r)

}




#' 获取链接
#'
#' @param FYear 年份
#' @param FPeriod  月份
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' bw2_deal_list
bw2_deal_list <- function(conn=tsda::conn_rds('jlrds'),
                           FYear =2021,
                           FPeriod =6 ){
 sql <- paste0("select  FSolutionNumber,FSubNumber  from t_mrpt_ds_bw_formula")
 r <- tsda::sql_select(conn,sql)
 #r$FYear <- FYear
 #r$FPeriod <- FPeriod
 ncount <- nrow(r)
 #处理数据
 lapply(1:ncount, function(i){
   FSolutionNumber = r$FSolutionNumber[i]
   FSubNumber =  r$FSubNumber[i]
   bw2_selectData(conn = conn,FSolutionNumber = FSolutionNumber,FSubNumber = FSubNumber,FYear = FYear,FPeriod = FPeriod)
   print(paste0(FSolutionNumber,'-',FSubNumber))


 })

  return(r)

}





















