

library(jlrdspkg)
sap_costCenter_standardize(file="data-raw/mrpt01/SAPdata/成本中心划分总表.xlsx",
                                       FYear =2021,
                                       FPeriod =1,
                                       conn=tsda::conn_rds('jlrds'))

sap_rpt_readData(file="data-raw/mrpt01/SAPdata/SAP一月总表.XLSX",
                             FYear =2021,
                             FPeriod =1,
                             conn=tsda::conn_rds('jlrds'))

sap_rpt_costItem_readData(file="data-raw/mrpt01/SAPdata/SAP成本要素对应关系表.xlsx",
                                      FYear =2021,
                                      FPeriod =1,
                                      conn=tsda::conn_rds('jlrds'))





mrpt_bw_deal_batch(files="data-raw/mrpt01/BWdata",
                               FYear =2021,
                               FPeriod =1,
                               conn=tsda::conn_rds('jlrds'),

                               skip = 1)
