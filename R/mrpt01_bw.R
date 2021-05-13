#' 返回SAP报表的固定表头
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_bw_heading()
mrpt_bw_heading <- function() {
  res <-c('零售原价','销售收入','销售成本','正品销售成本','其他销售成本(总计)','促销费-折扣折让费用','促销费-折扣折让费用-产成品-商品','促销费-物料配赠费用-外购赠品','促销费-物料配赠费用-产成品-非商品非试','促销费-物料配赠费用-销售物料','促销费-物料配赠费用-产成品-试用装','促销费-物料配赠费用-道具','促销费-物料配赠费用-销售员工物料',
          '促销费-物料配赠费用-69999','促销费-物料配赠费用-柜台')
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








