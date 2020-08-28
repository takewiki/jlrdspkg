own_getRptTpl()

mydata2 <-own_getBalanceSheet(FUnit = 'wan')
mydata2;


mydata3 <- own_getMonthRpt(FUnit = 'wan')

mydata3


mydata4 <- own_deal(FUnit = 'wan',FMonth = 7)
View(mydata4)
