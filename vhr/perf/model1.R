getwd()
setwd("C:/Users/Test User/Documents/Files/perf")
mumbai = read.csv("mumbai_coe.csv")
str(mumbai)
mumbai$Sr..no = NULL
mumbai$Name = NULL
mumbai$Leaves.already.availed.during.the.year = NULL
mumbai$No..of.times.leaves.availed.during.the.year = NULL
mumbai$Reason..Criticality.of.Leave = NULL
mumbai$Profile.of.employee = NULL
mumbai$Approved...Rejected.by  = NULL
table(mumbai$Status.of.Leave)

table(mumbai$Type.of.Leave)
levels(mumbai$Type.of.Leave)
levels(mumbai$Type.of.Leave)[levels(mumbai$Type.of.Leave) == ""] <- "Absent"
levels(mumbai$Type.of.Leave)[levels(mumbai$Type.of.Leave) == "-"] <- "Absent"
levels(mumbai$Type.of.Leave)[levels(mumbai$Type.of.Leave) == "Casual leave"] <- "Casual Leave"
levels(mumbai$Type.of.Leave)[levels(mumbai$Type.of.Leave) == "Earned leave"] <- "Earned Leave"
levels(mumbai$Type.of.Leave)[levels(mumbai$Type.of.Leave) == "Earned leave "] <- "Earned Leave"
levels(mumbai$Type.of.Leave) = c("Absent","Casual Leave","Casual Leave Confirmed COE","Earned Leave",
                                 "Earned Leave","Earned Leave Confirmed COE","LWP","Sick Leave","Sick Leave")
str(mumbai)
levels(mumbai$Type.of.Leave)

library(dummies)
df = dummy.data.frame(mumbai, names = c("Type.of.Leave", "Status.of.Leave"), sep ="_")
str(df)
library(lubridate)
df$Leave.Dates.From = as.Date(df$Leave.Dates.From, "%d-%m-%Y")
df$Leave.Dates.From.Date = day(df$Leave.Dates.From)
df$Leave.Dates.From.Month = month(df$Leave.Dates.From)
df$Leave.Dates.From.Year = year(df$Leave.Dates.From)

df$Leave.Dates.to = as.Date(df$Leave.Dates.to, "%d-%m-%Y")
df$Leave.Dates.to.Date = day(df$Leave.Dates.to)
df$Leave.Dates.to.Month = month(df$Leave.Dates.to)
df$Leave.Dates.to.Year = year(df$Leave.Dates.to)

df$Date.of.joining.the.company = as.Date(df$Date.of.joining.the.company, "%d.%m.%Y")
str(df)
df$From.my = (df$Leave.Dates.From.Month * 10000 + df$Leave.Dates.From.Year)
df$To.my = (df$Leave.Dates.to.Month * 10000 + df$Leave.Dates.to.Year)

View(df)
xx = (df$From.my == df$To.my)
table(xx)
sampl1 = subset(df, xx == FALSE)
sampl2 = subset(df, xx == TRUE)
write.csv(sampl1, "sampl1.csv", row.names = F)
nrow(df)
sampl1_new = read.csv("sampl1_new.csv")
View(sampl1_new)
dim(sampl2)
dim(sampl1_new)
names(sampl1_new) = names(sampl2)
dff = rbind(sampl2, sampl1_new)
dim(dff)
write.csv(dff, "new_format.csv", row.names = F)
str(dff)

dff2 = read.csv("new_format2.csv")
temp = dff2[,c(1,3:15)]
temp2 = aggregate(.~Employee.ID + From.my, data = temp, FUN = sum)

temp2$Employee = substr(as.character(temp2$Employee.ID), 1, 3)
str(temp2)
temp2$Employee = as.factor(temp2$Employee)



fxi_data = subset(temp2, temp2$Employee == "FXI")
str(fxi_data)
vhr_data = subset(temp2, temp2$Employee == "VHR")
str(vhr_data)

fxi = read.csv("fxi.csv")
fxi$March_2017 = as.numeric(as.character(fxi$March_2017))
fxi$April_2017 = as.numeric(as.character(fxi$April_2017))
fxi$May_2017 = as.numeric(as.character(fxi$May_2017))
final_fxi = merge(fxi_data, fxi, by = "Employee.ID", all.x = TRUE)

vhr = read.csv("vhr.csv")
str(vhr)
vhr$March_2017 = as.numeric(as.character(vhr$March_2017))
vhr$April_2017 = as.numeric(as.character(vhr$April_2017))
vhr$May_2017 = as.numeric(as.character(vhr$May_2017))
str(vhr)
str(vhr_data)
vhr_data$Employee.ID = as.factor(as.character(vhr_data$Employee.ID))
final_vhr = merge(vhr_data, vhr, by = "Employee.ID", all.x = TRUE)
str(final_vhr)
final_vhr = na.omit(final_vhr)

final_fxi$apr_mar = (final_fxi$April_2017 - final_fxi$March_2017)
final_fxi$may_apr = (final_fxi$May_2017 - final_fxi$April_2017)
final_vhr$apr_mar = (final_vhr$April_2017 - final_vhr$March_2017)
final_vhr$may_apr = (final_vhr$May_2017 - final_vhr$April_2017)


final_fxi$Employee = as.character(final_fxi$Employee)
final_vhr$Employee = as.character(final_vhr$Employee)

final_fxi_april = subset(final_fxi, final_fxi$From.my == 42017)
final_fxi_april2 <- na.omit(final_fxi_april)
fxi_model <- lm(apr_mar ~Type.of.Leave_Absent+Type.of.Leave_Casual.Leave+
                  Type.of.Leave_Casual.Leave.Confirmed.COE+Type.of.Leave_Earned.Leave+
                  Type.of.Leave_Earned.Leave.Confirmed.COE+Type.of.Leave_LWP+
                  Type.of.Leave_Sick.Leave+Leave.applied.for.no..of.days+
                  Status.of.Leave_Absent+Status.of.Leave_Approved+Status.of.Leave_Rejected,
                data = final_fxi_april2)
summary(fxi_model)
pp = predict(fxi_model)

plot(final_fxi_april2$apr_mar, pp, col = "red")

total_fxi = final_fxi
total_vhr = final_vhr
total_fxi$apr_mar_sign = ifelse(total_fxi$apr_mar >= 0,1,-1 )
total_fxi$may_apr_sign = ifelse(total_fxi$may_apr >= 0,1,-1 )
total_vhr$apr_mar_sign = ifelse(total_vhr$apr_mar >= 0,1,-1 )
total_vhr$may_apr_sign = ifelse(total_vhr$may_apr >= 0,1,-1 )

total_fxi$apr_mar_sign = as.factor(total_fxi$apr_mar_sign)
total_fxi$may_apr_sign = as.factor(total_fxi$may_apr_sign)
table(total_fxi$apr_mar_sign)
table(total_fxi$may_apr_sign)

total_vhr$apr_mar_sign = as.factor(total_vhr$apr_mar_sign)
total_vhr$may_apr_sign = as.factor(total_vhr$may_apr_sign)
table(total_vhr$apr_mar_sign)
table(total_vhr$may_apr_sign)

total_fxi2 = na.omit(total_fxi)
total_vhr2 = na.omit(total_vhr)

total_fxi_march = na.omit(subset(total_fxi2, total_fxi$From.my == 32017))
total_fxi_april = na.omit(subset(total_fxi2, total_fxi$From.my == 42017))
total_fxi_may = na.omit(subset(total_fxi2, total_fxi$From.my == 52017))
total_vhr_march = na.omit(subset(total_vhr2, total_vhr$From.my == 32017))
total_vhr_april = na.omit(subset(total_vhr2, total_vhr$From.my == 42017))
total_vhr_may = na.omit(subset(total_vhr2, total_vhr$From.my == 52017))

str(total_fxi_april)
cor(total_fxi_march[,c(10,19,20)])
cor(total_fxi_april[,c(10,19,20)])
cor(total_fxi_may[,c(10,19,20)])


cor(total_vhr_march[,c(10,19,20)])
cor(total_vhr_april[,c(10,19,20)])
cor(total_vhr_may[,c(10,19,20)])


# perf_fxi = read.csv("perf_fxi.csv")
# str(perf_fxi)
# perf_fxi$March_2017 = as.numeric(as.character(perf_fxi$March_2017))
# perf_fxi$April_2017 = as.numeric(as.character(perf_fxi$April_2017))
# perf_fxi$May_2017 = as.numeric(as.character(perf_fxi$May_2017))
# perf_fxi$apr_mar_diff = as.numeric(as.character(perf_fxi$apr_mar_diff))
# perf_fxi$may_apr_diff = as.numeric(as.character(perf_fxi$may_apr_diff))
# 
# perf_fxi2 = na.omit(perf_fxi)
# cor(perf_fxi2[,c(2:8)])
# str(perf_fxi2)
