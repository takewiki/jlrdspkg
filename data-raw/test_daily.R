mydata2 <- rpt_daily_readExcel()
#mydata2 <- data_melt
View(mydata2)



rpt_daily_writeDb()

rpt_daily_sync()




startDate ='2020-02-29'
endDate = '2020-03-05'

mydata2_d <- mydata2[mydata2$FDate >= startDate & mydata2$FDate <= endDate,c('FRptItemNo','FRptItemName','FDate','FAmount')]

mydata3 <- dcast(mydata2_d,FRptItemNo+FRptItemName~FDate,fun.aggregate = sum)
View(mydata3)

openxlsx::write.xlsx(mydata3,'test.xlsx')

mydata4 <- rpt_daily_selectDb()
View(mydata4)
