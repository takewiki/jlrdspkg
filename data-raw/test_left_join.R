week_stat(year = 2020,weekNo = 34,type = 'nature')

last <-week_selectDb(weekNo = 32,type='nature')
names(last) <- c('FRptItemNo','FRptItemName','FLastAmt')
last <- last[,c('FRptItemNo','FLastAmt')]
current <-week_selectDb(weekNo = 33,type='nature')

res <-dplyr::left_join(current,last,by='FRptItemNo')
View(res)

week_update(FweekNo = 34)



month_stat(month = 8)





