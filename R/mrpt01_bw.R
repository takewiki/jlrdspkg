#' 返回SAP报表的固定表头
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_bw_heading()
mrpt_bw_heading <- function() {
  res <-c('零售原价','销售收入','销售成本','正品销售成本','其他销售成本(总计)','促销费-折扣折让费用','促销费-折扣折让费用-产成品-商品','促销费-物料配赠费用-外购赠品','促销费-物料配赠费用-产成品-非商品非试','促销费-物料配赠费用-销售物料','促销费-物料配赠费用-产成品-试用装','促销费-物料配赠费用-道具','促销费-物料配赠费用-销售员工物料','促销费-物料配赠费用-69999')
  return(res)
}


#' BW001报表的处理
#'
#' @param file 文件名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mrpt_bw_b001()
mrpt_bw_b001 <- function(file="data-raw/mrpt04/mrpt01_bw/B001_04_data.xlsx") {
  #library(readxl)
  res <- readxl::read_excel(file)
  return(res)

}




