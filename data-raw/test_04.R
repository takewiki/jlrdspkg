

library(jlrdspkg)
sap_costCenter_standardize(file="data-raw/mrpt04/SAPdata/成本中心划分总表.xlsx",
                                       FYear =2021,
                                       FPeriod =4,
                                       conn=tsda::conn_rds('jlrds'))

sap_rpt_readData(file="data-raw/mrpt05/SAPdata/SAP_05_2021.XLSX",
                             FYear =2021,
                             FPeriod =5,
                             conn=tsda::conn_rds('jlrds'))

sap_rpt_costItem_readData(file="data-raw/mrpt04/SAPdata/SAP成本要素对应关系表.xlsx",
                                      FYear =2021,
                                      FPeriod =4,
                                      conn=tsda::conn_rds('jlrds'))





mrpt_bw_deal_batch(files="data-raw/mrpt05/BWdata",
                               FYear =2021,
                               FPeriod =5,
                               conn=tsda::conn_rds('jlrds'),

                               skip = 1)
