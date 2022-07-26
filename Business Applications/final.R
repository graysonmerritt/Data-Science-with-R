library(tidyverse)
CFBeattendance = CFBeattendance %>% filter(!Team == 'UMass' & !is.na(Attendance))
CFBeattendance1 = CFBeattendance %>% select(-Month,-Day,-Site, - TV)
CFBeattendance1 = CFBeattendance1 %>% mutate(time_block = ifelse(Time <= '12:45 PM', "Morning", ifelse(Time >= '12:45 PM' & Time <='5:45 PM', 'Afternoon',  "Evening")))
rank_cfb = CFBeattendance1 %>% filter (Rank <=25 | Rank ==3 | Rank ==4 |Rank ==5 | Rank ==6 | Rank ==7 |Rank ==8 | Rank ==9)
rank_cfb = rank_cfb[-1248,]

model = lm(Fill.Rate ~ Rank + Current.Wins + Tailgating + SNOW + 
             TMAX + TMIN + Conference + time_block, data = rank_cfb)
summary(model)
confint(model)
autoplot(model)

ggplot(rank_cfb) + geom_histogram(aes(x=TMAX))
ggplot(rank_cfb) + geom_histogram(aes(x=TMIN))
rank_cfb %>% summarize(mean = mean(TMAX), mean2 = mean(TMIN), sd = sd(TMAX), sdmin = sd(TMIN))
xtabs(~Tailgating, data = rank_cfb) %>% prop.table()
ggplot(rank_cfb) + geom_boxplot(aes(x=Tailgating, y = Fill.Rate))
ggplot(rank_cfb) + geom_boxplot(aes(x=time_block, y = Fill.Rate))
ggplot(rank_cfb) + geom_boxplot(aes(x=Conference, y = Fill.Rate))

xtabs(~time_block, data = rank_cfb) %>% prop.table()
rank_cfb %>% summarize(mean = mean(Current.Wins))

options(scipen=999)
full = lm(Fill.Rate ~ . - Date - Team - Opponent - Stadium.Capacity - Result-Attendance - Time, data =rank_cfb)
step(full, direction = 'backward')
summary(full)


predict(model, list(Rank='10', Current.Wins = 4, Tailgating = TRUE, SNOW =0, TMAX = 72.2, TMIN =50.82695, Conference = 'Mid-American',time_block = 'Afternoon'),interval='prediction')




