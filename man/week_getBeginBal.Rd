% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weekly.R
\name{week_getBeginBal}
\alias{week_getBeginBal}
\title{获取每个月的期初数}
\usage{
week_getBeginBal(
  conn = tsda::conn_rds("jlrds"),
  year = 2020,
  weekNo = 9,
  type = "jala"
)
}
\arguments{
\item{conn}{连接}

\item{year}{年份}

\item{weekNo}{周次}

\item{type}{类型}
}
\value{
返回值
}
\description{
获取每个月的期初数
}
\examples{
week_getBeginBal()
}
