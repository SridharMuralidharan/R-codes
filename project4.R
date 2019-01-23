loan<-read.csv(file="D:\\MSBA\\Fall 2018\\MSBC5030\\Project 4\\LoanStats3c.csv",header=TRUE,sep=",",skip=1)

library(data.table)

library(dplyr)

install.packages("reshape2")
library(reshape2)


df_1 = loan[FALSE,]
df_1 <- loan %>%select(zip_code,verification_status,loan_amnt,annual_inc) %>%
  group_by(verification_status,zip_code) %>%
  summarize(mean(loan_amnt),mean(annual_inc))
View(df_1)

df_2 = loan[FALSE,]
df_2 <- loan %>%select(zip_code,verification_status,loan_amnt,annual_inc,term,sub_grade) %>%
  group_by(verification_status,zip_code,term,sub_grade) %>%
  summarize(mean(loan_amnt),mean(annual_inc))
View(df_2)
0
## Factor variables
factorvar1 <- melt(subset(loan, select = c(zip_code, verification_status,grade, sub_grade,home_ownership,term, purpose, loan_status ) ),
                   id.var = c('zip_code', 'verification_status','grade','sub_grade','term', 'purpose', 'loan_status','home_ownership'))
factorvar1 <- factorvar1 %>% group_by(zip_code, verification_status,grade,sub_grade,term, purpose, loan_status,home_ownership) %>% 
  summarise(frequency = n()) %>% 
  mutate(fraction = frequency/sum(frequency))

b<-dcast(factorvar1,zip_code+verification_status+grade+sub_grade+term+purpose+loan_status+home_ownership~loan$home_ownership)

b2<-dcast(factorvar1,zip_code+verification_status+grade+sub_grade+term+purpose+loan_status+home_ownership~loan$purpose)

newdata2<-subset(factorvar1,purpose=="debt_consolidation")


factorvar2 <- melt(subset(loan, select = c(zip_code, verification_status,grade, sub_grade,home_ownership, term, purpose, loan_status ) ),
                   id.var = c('zip_code', 'verification_status','term','sub_grade'))
factorvar2 <- factorvar1 %>% group_by(zip_code, verification_status,term,sub_grade, variable, value) %>% 
  summarise(frequency = n()) %>% 
  mutate(fraction = frequency/sum(frequency))

factorvar1$variable[992]


##library(plyr)
v<-acast(factorvar1,zip_code+verification_status~variable+value+frequency)
v1<-acast(factorvar1,zip_code+verification_status~variable+value+fraction)

a<-dcast(factorvar1,zip_code+verification_status~loan$loan_status)
freq_full<-sum(a[,6])
freq_current<-sum(a[,5])

count=0

count1=0
for(i in 1:length(loan$loan_status))
{
  if (loan$loan_status[i]=='Fully Paid')
  {
    count=count+1
  }
  else if (loan$loan_status[i]=='Current')
  {
    count1=count1+1
  }
}

newdata<-subset(factorvar1,home_ownership=="OWN")

c<-lm(as.numeric(loan$sub_grade)~as.numeric(loan$int_rate))
d<-boxplot((loan$sub_grade)~(loan$int_rate))

e<-lm(as.numeric(loan$pct_tl_nvr_dlq)~as.numeric(loan$int_rate))
summary(e)
e1<-boxplot(loan$pct_tl_nvr_dlq~loan$int_rate)

f<-lm(loan$annual_inc~loan$int_rate)
summary(f)
f1<-boxplot(loan$loan_amnt~loan$int_rate)
Meds = data.frame(x = 1:5, y = sapply(my_data, median))
abline(lm(as.numeric(int_rate)~as.numeric(loan_amnt), data=f1))
coefs <- coef(lm(loan_amnt ~int_rate, data = loan))
  ggplot(loan, aes(as.numeric(int_rate), as.numeric(loan_amnt), group = as.numeric(int_rate))) +
    geom_boxplot() +
    geom_abline(intercept = coefs[1], slope = coefs[2])

#sub_grade vs avg_loan_amount

g<-lm(as.numeric(loan$sub_grade)~(loan$loan_amnt))
summary(g)
Meds = data.frame(x = 1:5, y = sapply(my_data, median))
g1<-boxplot((loan$sub_grade),(loan$loan_amnt))




  

