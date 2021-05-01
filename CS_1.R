install.packages('tidyverse')
library(tidyverse)
install.packages('dplyr')
library(dplyr)
install.packages('ggplot2')
library(ggplot2)

daily_activity<-read.csv('C:/Users/Karan/OneDrive/Documents/dailyActivity_merged.csv')
head(daily_activity)
colnames(daily_activity)
min(daily_activity$Calories)

daily_sleep<-read.csv('C:/Users/Karan/OneDrive/Documents/sleepDay_merged.csv')
head(daily_sleep)

daily_steps<-read.csv('C:/Users/Karan/OneDrive/Documents/dailySteps_merged.csv')
head(daily_steps)
colnames(daily_steps)
summary(daily_steps)
glimpse(daily_steps)

minutes_sleep<-read.csv('C:/Users/Karan/OneDrive/Documents/minuteSleep_merged.csv')
head(minutes_sleep)

max(daily_sleep$TotalMinutesAsleep)
min(daily_sleep$TotalMinutesAsleep)

max(daily_steps$StepTotal)
min(daily_steps$StepTotal)

daily_steps%>%
  count(daily_steps$StepTotal==0)


filtered_steps<-daily_steps%>%
  filter(daily_steps$StepTotal>0)
head(filtered_steps)
min(filtered_steps$StepTotal)

ggplot(data=daily_sleep)+
  geom_smooth(mapping=aes(x=Id,y=TotalMinutesAsleep))+
  facet_wrap(~TotalTimeInBed)

head(filtered_steps)
tail(filtered_steps)
filtered_steps%>%
  count(filtered_steps$StepTotal==0)

filtered_steps%>%
  group_by(filtered_steps$Id)
head(filtered_steps)

summary_filtered_steps<-filtered_steps%>%
  group_by(Id)%>%
  summarise(sum_steps=sum(StepTotal),avg_steps=mean(StepTotal),number_step_entries=length(StepTotal))
head(summary_filtered_steps)
tail(summary_filtered_steps)
head(summary_filtered_steps)

summary_filtered_steps$step_level=case_when(
  summary_filtered_steps$avg_steps>=10000 ~ 'Very Active',
  summary_filtered_steps$avg_steps>=5000 ~ 'Mildly Active',
  summary_filtered_steps$avg_steps>=1000 ~ 'Little Active',
  summary_filtered_steps$avg_steps<1000 ~ 'Inactive'
)
ggplot(data=summary_filtered_steps)+
  geom_point(mapping=(aes(x=avg_steps,y=number_step_entries,color=step_level)))+
  facet_wrap(~step_level)
ggplot(data=summary_filtered_steps)+
  geom_smooth(mapping=(aes(x=number_step_entries,y=avg_steps)))
summary_sleep<-daily_sleep%>%
  group_by(Id)%>%
  summarise(sum_sleep_mins=sum(TotalMinutesAsleep),avg_sleep_mins=mean(TotalMinutesAsleep),number_sleep_entries=length(TotalMinutesAsleep))

head(summary_sleep)
max(daily_sleep$TotalMinutesAsleep)
min(daily_sleep$TotalMinutesAsleep)

summary_sleep$sleep_level=case_when(
  summary_sleep$avg_sleep_mins>=480 ~ 'Well Rested',
  summary_sleep$avg_sleep_mins>=360 ~ 'Moderately Rested',
  summary_sleep$avg_sleep_mins>=240 ~ 'Poorly Rested',
  summary_sleep$avg_sleep_mins<240 ~ 'Not Rested',
)
head(summary_sleep)

ggplot(data=summary_sleep)+
  geom_point(mapping=(aes(x=avg_sleep_mins,y=number_sleep_entries,color=sleep_level)))+
  facet_wrap(~sleep_level)
ggplot(data=summary_sleep)+
  geom_smooth(mapping=(aes(x=number_sleep_entries,y=avg_sleep_mins)))


sleep_steps<-merge(summary_sleep,summary_filtered_steps,on='Id')
head(sleep_steps)

ggplot(data=sleep_steps)+
  geom_bar(mapping=(aes(x=sleep_level,fill=step_level)))+
  labs(title='Sleep Level vs Step Level')

ggplot(data=sleep_steps)+
  geom_bar(mapping=(aes(x=step_level,fill=sleep_level)))+
  labs(title='Step Level vs Sleep Level')

ggplot(data=sleep_steps)+
  geom_line(mapping=(aes(x=avg_steps,y=avg_sleep_mins)))+
  labs(title='Average Steps vs Average Sleep')

ggplot(data=sleep_steps)+
  geom_line(mapping=(aes(x=avg_sleep_mins,y=avg_steps)))+
  labs(title='Average Sleep vs Average Steps')

ggplot(data=sleep_steps)+
  geom_line(mapping=(aes(x=avg_sleep_mins,y=avg_steps)))+
  labs(title='Steps vs Sleep based on Sleep Level')+
  facet_wrap(~step_level)
  
