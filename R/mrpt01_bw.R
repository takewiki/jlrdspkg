#' 返回SAP报表的固定表头
#'
#' @param conn 连接信息从数据库读取
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_bw_value()
mrpt_bw_value <- function(conn=tsda::conn_rds('jlrds')) {
  # res <-c('零售原价','销售收入','销售成本','正品销售成本','其他销售成本(总计)','促销费-折扣折让费用','促销费-折扣折让费用-产成品-商品','促销费-物料配赠费用-外购赠品','促销费-物料配赠费用-产成品-非商品非试','促销费-物料配赠费用-销售物料','促销费-物料配赠费用-产成品-试用装','促销费-物料配赠费用-道具','促销费-物料配赠费用-销售员工物料',
  #         '促销费-物料配赠费用-69999','促销费-物料配赠费用-柜台')

  sql <- paste0("select FName_show from t_mrpt_valueType
where FDataSource='bw'
order by FIndex")
  res <- tsda::sql_select(conn,sql)
  #print(res)
  res <- res$FName_show
  return(res)
}



#' 获取BW报表表头
#'
#' @param conn 连接
#' @param solutionNumber 方案号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_bw_value_bySolutionNumber_checked()
mrpt_bw_value_bySolutionNumber_checked <- function(conn=tsda::conn_rds('jlrds'),solutionNumber='B001') {


  sql <- paste0("select FValueName from t_mrpt_rule_bw
where FSolutionNumber='",solutionNumber,"'")
  r <- tsda::sql_select(conn,sql)
  ncount <- nrow(r)
  if ( ncount >0 ){

    res <- r$FValueName


  }else{

    res <- NULL
  }

  return(res)

}




#' 获取表头信息
#'
#' @param conn 连接
#' @param solutionNumber  方案号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_bw_heading_bySolutionNumber_unChecked()
mrpt_bw_value_bySolutionNumber_unChecked <- function(conn=tsda::conn_rds('jlrds'),solutionNumber='B001') {

  value_checked <-mrpt_bw_value_bySolutionNumber_checked(conn=conn,solutionNumber = solutionNumber)
  value_all <- mrpt_bw_value(conn=conn)
  if (is.null(value_checked)){
    res <-value_all
  }else{
    flag_unchecked <- !value_all %in% value_checked
    res <- value_all[flag_unchecked]
  }
  return(res)


}



#' 返回BW报表的维度信息
#'
#' @param conn 连接
#' @param solutionNumber 方案号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_bw_dim_bySolutionNumber_checked
mrpt_bw_dim_bySolutionNumber_checked <- function(conn=tsda::conn_rds('jlrds'),solutionNumber='B001') {


  sql <- paste0("select FName_sql from t_mrpt_dimType
where FSulotionNumber ='",solutionNumber,"'")
  r <- tsda::sql_select(conn,sql)
  ncount <- nrow(r)
  if ( ncount >0 ){

    res <- r$FName_sql


  }else{

    res <- NULL
  }

  return(res)

}


#' 获取规则表的品牌及渠道
#'
#' @param conn 连接
#' @param solutionNumber 方案号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_bw_rule_brandChannel()
mrpt_bw_rule_brandChannel <- function(conn=tsda::conn_rds('jlrds'),solutionNumber='B001') {

  sql <- paste0("select  distinct  FBrand_o,FChannel_o   from t_mrpt_rule_bw
where FSolutionNumber='",solutionNumber,"'")
  r <- tsda::sql_select(conn,sql)
  ncount <- nrow(r)
  if ( ncount >0 ){

    res <- r


  }else{

    res <- NULL
  }

  return(res)

}


#' 根据文件获取方案号
#'
#' @param file_name 文件名
#'
#' @return返回值
#' @export
#'
#' @examples
#' mrpt_bw_getSolutionByFile()
mrpt_bw_getSolutionByFile <- function(file_name = "data-raw/mrpt04/mrpt01_bw/B001_04_data.xlsx") {
  #bb = "data-raw/mrpt04/mrpt01_bw/B001_04_data.xlsx"
  res = strsplit(file_name,'/')
  ncount =length(res[[1]])
  res2 =res[[1]][ncount]
  res3 = strsplit(res2,'_')
  res5 = res3[[1]][1]
  return(res5)


}

#' 获取方案对应的指标变更
#'
#' @param conn 连接
#' @param solutionNumber 方案号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_getValue_selected()
mrpt_getValue_selected  <- function(conn=tsda::conn_rds('jlrds'),solutionNumber='B001') {

  sql <- paste0("select FValueName  from t_mrpt_rule_bw
where FSolutionNumber = '",solutionNumber,"'")
  r <- tsda::sql_select(conn,sql)
  ncount <- nrow(r)
  if (ncount >0){
    res <- r$FValueName
  }else{
    res <- NULL
  }
  return(res)

}
#核心处理函数*************——————————————————————————————————
#' BW报表处理规则
#'
#' @param file 文件名
#' @param conn 连接
#' @param skip 忽略行
#' @param FYear 年份
#' @param FPeriod 月份
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_bw_deal()
mrpt_bw_deal <- function(file="data-raw/mrpt04/BWdata/B001_04.xlsx",
                         FYear =2021,
                         FPeriod =4,
                         conn=tsda::conn_rds('jlrds'),

                         skip = 1) {
  #library(readxl)
  res <- readxl::read_excel(file,skip = skip)
  #print('bug2')
  solutionNumber= mrpt_bw_getSolutionByFile(file_name = file)
  #
  heading_dim <- mrpt_bw_dim_bySolutionNumber_checked(conn = conn,solutionNumber = solutionNumber)
  heading_value <- mrpt_bw_value()

  heading_total <-c(heading_dim,heading_value)
  #print('bug3')
  names(res) <- heading_total
  #print('bug4')
  #print(names(res))
  #print(heading_value)

  res <- res[ ,heading_value]
  #print('bug1')
  #names()
  res$`方案` <- solutionNumber
  # 处理品牌信息
   bc = mrpt_bw_rule_brandChannel(conn = conn,solutionNumber = solutionNumber)
   print(bc)
  res$`品牌` <-bc$FBrand_o
  res$`渠道` <-bc$FChannel_o
  #针对数据进行标准化
  res2 = reshape2::melt(res,id.vars=c('方案', '品牌', '渠道'))
  #获取指定数据
  value_selected = mrpt_getValue_selected(conn = conn,solutionNumber = solutionNumber)
  #print('bug')
  #print(value_selected)
  #print(res2$variable)
  flag =  res2$variable %in% value_selected
  #print(flag)
  res3 = res2[flag, ]
  #针对数据进一下处理，将0数据处理掉
  res3$FYear = FYear
  res3$FPeriod = FPeriod
  names(res3) <-c('FSolutionNumber','FBrand','FChannel','FValueName','FValue','FYear','FPeriod')
  #上传数据库
  ncount2 = nrow(res3)
  if(ncount2 >0){
    #写入数据库
    tsda::db_writeTable(conn = conn,table_name = 't_mrpt_data_bw',r_object = res3,append = T)
  }



  return(res3)
}

#' 针对数据进行批量处理
#'
#' @param files 文件名
#' @param FYear 年份
#' @param FPeriod 月份
#' @param conn 连接
#' @param skip 跳出行
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_bw_deal_batch()
mrpt_bw_deal_batch <- function(files="data-raw/mrpt04/BWdata",
                               FYear =2021,
                               FPeriod =4,
                               conn=tsda::conn_rds('jlrds'),

                               skip = 1) {
  file_names = dir(files,full.names = TRUE)



  try({
    lapply(file_names, function(file){
      print(file)
      mrpt_bw_deal(file = file,FYear = FYear,FPeriod = FPeriod,conn = conn,skip = skip)

    })
  })





}




dir("data-raw/mrpt04/BWdata",full.names = TRUE)










#' BW001报表的处理
#'
#' @param file 文件名
#' @param skip  忽略第一行
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_bw_b001()
mrpt_bw_b001 <- function(file="data-raw/mrpt04/mrpt01_bw/B001_04_data.xlsx",skip = 1) {
  #library(readxl)
  res <- readxl::read_excel(file,skip = skip)
  #
  heading_dim <- c('13物料组(物料主数据)','13物料组(物料主数据)-名称','33子渠道（SAP客户组）(客户主数据)','33子渠道（SAP客户组）(客户主数据)-名称','41渠道(分析用)')
  heading_value <- mrpt_bw_heading()

  heading_total <-c(heading_dim,heading_value)
  names(res) <- heading_total
  res <- res[ ,heading_value]
  res$`方案` <- 'B001'
  res$`品牌` <-'自然堂'
  res$`渠道` <-'美妆'

  return(res)

}


#' BW001报表的处理
#'
#' @param file 文件名
#' @param skip  忽略第一行
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_bw_b002()
mrpt_bw_b002 <- function(file="data-raw/mrpt04/mrpt01_bw/B002_04_data.xlsx",skip = 1) {
  #library(readxl)
  res <- readxl::read_excel(file,skip = skip)
  #
  heading_dim <- c('13物料组(物料主数据)','13物料组(物料主数据)-名称','33子渠道（SAP客户组）(客户主数据)','33子渠道（SAP客户组）(客户主数据)-名称','41渠道(分析用)')
  heading_value <- mrpt_bw_heading()

  heading_total <-c(heading_dim,heading_value)
  names(res) <- heading_total
  res <- res[ ,heading_value]
  res$`方案` <- 'B002'
  res$`品牌` <-'自然堂'
  res$`渠道` <-'美妆'

  return(res)

}








