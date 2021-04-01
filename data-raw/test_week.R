conn=tsda::conn_rds('jlrds')
Fyear =2020
FweekNo =51
Ftype ='nature'
  #删除余额
  week_DelBal(conn = conn,Fyear = Fyear,FweekNo = FweekNo,Ftype = Ftype)
  #删除周报
  week_DelRpt(conn=conn,Fyear = Fyear,FweekNo = FweekNo,Ftype = Ftype)
  #删除周报统计信息
  week_DelStat(conn=conn,Fyear = Fyear,FweekNo = FweekNo,Ftype = Ftype)
  #更新周报
  week_deal(conn = conn,year = Fyear,weekNo = FweekNo,type = Ftype)
  #更新更报统计信息
  week_stat(conn=conn,year = Fyear,weekNo = FweekNo,type = Ftype)

