library(dplyr)
library(stringr)
library(tidyr)
df1 <- read.csv("~/R/KCK_feedback.csv")
colnames(df1) <- c("date","functions","ERP_flag","other_list","ERP_time","ERP_conv","Jira_conv","other_time","feedback","contact","answer_time","corp_status")
df1 <- df1 %>% separate(other_list,c("ol1","ol2","ol3"),sep = ", ")
sys <- c(df1$ol1,df1$ol2,df1$ol3,rep("1C ERP",59))
sys <- sys[is.na(sys)==F]
df <- as.data.frame(table(sys))
colnames(df)=c("ИС","Частота")
df$ИС <- factor(df$ИС,levels = df$ИС[order(df$Частота,decreasing = T)])
#Популярность ИС по убыванию
ggplot(df,aes(y=Частота,x=ИС,fill=ИС))+
  geom_bar(stat = "identity")+
  labs(title = "Популярность ИС",y="Количество",x="ИС")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),legend.title = element_blank())
#Удобство работы в 1С
ggplot(df1,aes(x=ERP_conv,fill=factor(ERP_conv)))+
  geom_bar()+
  labs(title = "Оценка удобства работы в 1С ЕРП",y="Количество оценок",x="значение оценки")+
  theme(legend.title = element_blank())
#Удобство работы на Портеле тех. поддержки
ggplot(df1,aes(x=Jira_conv,fill=factor(Jira_conv)))+
geom_bar()+
labs(title = "Оценка удобства работы на Портале тех. поддержки",y="Количество оценок",x="значение оценки")+
theme(legend.title = element_blank())
df1$other_time[df1$other_time=="0,5 и 0,5"] <- "1"
df1$other_time=str_remove_all(df1$other_time,"[ А-я=]")
df1$ERP_time=str_remove_all(df1$ERP_time,"[ А-я=]")
df1$ERP_time=str_remove_all(df1$ERP_time,pattern = "^[1-9]-")
df1$other_time=str_remove_all(df1$other_time,pattern = "^[1-9]-")
df1$ERP_time=as.numeric(str_replace(df1$ERP_time,",","."))
df1$other_time=as.numeric(str_replace(df1$other_time,",","."))
df1$other_time[is.na(df1$other_time)==T] <- 0
#Гистограмма времени работы в 1С ЕРП
hist(df1$ERP_time,col = "lightblue",xlab = "Шкала времени",ylab = "Частота",main = "Гистограмма распределения рабочего времени в 1С ЕРП")
#Гистограмма времени работы в других системах (программах)
hist(df1$other_time,col = "orange",xlab = "Шкала времени",ylab = "Частота",main = "Распределение рабочего времени в других системах (программах)")
write.csv2(df1[,c(12,14,11,2)],file = "feedbacks.csv")
#Период прохождения опроса
df1$date[c(1,length(df1$date))]
#Дни наибольшей активности опрашиваемых
dates <- as.data.frame(table(str_split(df1$date," ",simplify = T)[,1]))
dates$Var1 <- as.Date(dates$Var1,format="%d.%m.%Y")
dates <- arrange(dates,Var1)
barplot(dates$Freq,names.arg = dates$Var1,col="green",main = "Активность опрашиваемых",ylab = "Число пройденных опросов")
# Сопоставление времени работы в ИС и оценок удовлетворенности опрошенных
pairs(df1[,c(7:10)])
