% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/own.R
\name{own_getBalanceSheet}
\alias{own_getBalanceSheet}
\title{获取资产负债表中的往来数据}
\usage{
own_getBalanceSheet(
  conn = tsda::conn_rds("jlrds"),
  FYear = 2020,
  FMonth = 7,
  FUnit = "yuan",
  digit = 2
)
}
\arguments{
\item{conn}{连接}

\item{FYear}{年}

\item{FMonth}{月}

\item{FUnit}{金额单位}

\item{digit}{保留小数}
}
\value{
返回值
}
\description{
获取资产负债表中的往来数据
}
\examples{
own_getBalanceSheet()
}
