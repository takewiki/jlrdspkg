library(jlrdspkg)

# week_deal(year = 2020,weekNo = 9)
# week_deal(year = 2020,weekNo = 10)
# week_deal(year = 2020,weekNo = 10)


for (i in 9:31) {

  week_deal(year = 2020,weekNo = i)
}

#处理自然周

for (i in 9:30) {

  week_deal(year = 2020,weekNo = i,type = 'nature')
}

#week_deal(year = 2020,weekNo = 9,type='nature')


#测试统计信息
#mydata <-week_stat(weekNo = 9,year = 2020,type = 'jala')
#

for (i in 9:31) {

 week_stat(weekNo = i)
}

处理自然周的信息
for (i in 9:30) {

  week_stat(weekNo = i,type = 'nature')
}



mydata4 <- weekRpt_selectDB(endWeekNo = 10,FLevel = 2)
View(mydata4)
