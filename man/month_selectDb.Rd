% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monthly.R
\name{month_selectDb}
\alias{month_selectDb}
\title{从数据库中读取月报数据}
\usage{
month_selectDb(conn = tsda::conn_rds("jlrds"), year = 2020, month = 3)
}
\arguments{
\item{conn}{连接}

\item{year}{年份}

\item{month}{月份}
}
\value{
返回值
}
\description{
从数据库中读取月报数据
}
\examples{
month_selectDb()
}
