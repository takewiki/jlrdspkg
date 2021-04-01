#' 计算数值百分比
#'
#' @param value 数据
#' @param base 基数
#' @param digit 小数位数
#'
#' @return 返回值
#' @export
#'
#' @examples
#' rpt_percent()
rpt_percent <- function(value,base,digit=2) {
  ncount <- length(value)
  r <- lapply(1:ncount, function(i){
    print(i)
    print(base[i])

    if(is.na(base[i])){
      res <- 0
    }else{
      if(base[i] ==0){
        res <-0
      }else{
        res <- round((value[i] /(base[i] +0.1-0.1) -1)*100,digit)
      }

    }
  })
  res <- unlist(r)

}


#' 将向量转化为格式字符串
#'
#' @param vect 向量
#'
#' @return 返回值
#' @export
#'
#' @examples
#' vect_to_string()
vect_to_string <- function(vect=letters) {
  res <-paste("'",vect,"'",collapse = ',',sep = '')
  return(res)

}
