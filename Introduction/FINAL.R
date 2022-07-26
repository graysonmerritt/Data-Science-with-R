library(tidyverse)
library(mosaic)
library(modelr)
library(effectsize)
library(moderndive)
library(rsample)
library(lubridate)

#2
150000 + 225000 * 55
150000 + 225000 * 83
18825000 - 14000000
#4
0.54 - 0.05 * 1 - 0.02 * 1 - 0.03 * (1 * 1)
#8
1- pnorm(16.6, mean = 16.3, sd=.21)
#14
473+ 50.1 *1 + 103
#16
prob_16 = nycflights13 %>% filter(carrier == 'AA' & dest == "DFW" & hour >= 16)
mean(~dep_delay, na.rm = TRUE, data = prob_16)
# 17
prob_17 = nycflights13 %>% filter(air_time >= 360) %>% 
  group_by(origin, dest, carrier) %>% summarize(count= n(),na.rm=TRUE) %>% arrange(count)
#18
prob_18 =nycflights13 %>% mutate(bad_delay = ifelse(dep_delay >=60, yes = 1, no=0))
prob_18 %>% filter(carrier=="WN") %>% group_by(hour,origin) %>% 
  summarize(prop_bad = prop(~bad_delay),na.rm=TRUE) %>% 
  ggplot() + geom_line(aes(x=hour, y=prop_bad, color=origin))
#19 
nycflights13 %>% filter(dest == "AUS") %>% group_by(carrier) %>% 
  summarize(count = n()) %>% arrange(count)
#20
prob_20 = nycflights13 %>% filter(dest== "AUS" | dest == "SAN")
prob_20 = prob_20 %>% mutate(arr_delay = arr_delay >= 60)
prob_20 = prob_20 %>% mutate(dep_delay = dep_delay >= 60)
xtabs(~arr_delay + dep_delay , data = prob_20) %>% prop.table(margin=2)
# 21
prob_21 = nycflights13 %>% filter(dest== "AUS" | dest == "SAN")
mean(arr_delay ~ dest, data=prob_21, na.rm =TRUE)
t.test(arr_delay ~ dest, data =prob_21, na.rm=TRUE)
# 22
prob_22 = nycflights13 %>% mutate(bad_arr_delay = ifelse(arr_delay >= 60, yes =1 , no =0))
prob_22 = prob_22 %>% filter(dest == "IAH" | dest == "HOU")
prob_22 = prob_22 %>% filter(is.na(arr_delay) == FALSE)
prop(bad_arr_delay ~ dest, success =1, data = prob_22)
prop.test(bad_arr_delay ~ dest, success = 1, data = prob_22)

#23
prob_23 = lm(log(quantity) ~ log(price) + season + ice + cartel + cartel:log(price) + cartel:ice, data = cartel)
coef(prob_23)
confint(prob_23)
get_regression_table(prob_23)

#28
dengue %>% ggplot() + geom_boxplot(aes(x=season, y=total_cases)) + facet_wrap(~city)

#29 
prob_29 = dengue %>% filter(city == "sj")
prob_29 %>% ggplot + geom_point(aes(x=specific_humidity, y= log(total_cases))) + 
  geom_smooth(aes(x=specific_humidity, y=log(total_cases)), method='lm') + 
  facet_wrap(~season)
# 30
model_1 = lm(log(total_cases) ~ + city + specific_humidity + season + specific_humidity:season, data=dengue)
get_regression_table(model_1)
#31
model_2 = lm(log(total_cases) ~ + city + specific_humidity + tdtr_k + season + specific_humidity:season, data=dengue)
get_regression_table(model_2)
standardize_parameters(model_2)
exp(1.44)


#32
base_line = lm(lights ~ T1 + RH_1 + T2 + RH_2 + T3 + RH_3 + 
                 T4 + RH_4 + T5 + RH_5 + T6 + RH_6 + T7 + RH_7 + T8 + RH_8 + 
                 T9 + RH_9 + T_out + Press_mm_hg + RH_out + Windspeed + Visibility + 
                 Tdewpoint, data=household_train)
rmse(base_line, household_test)
#33
bigger = lm(lights ~ (T1 + RH_1 + T2 + RH_2 + T3 + RH_3 + 
              T4 + RH_4 + T5 + RH_5 + T6 + RH_6 + T7 + RH_7 + T8 + RH_8 + 
              T9 + RH_9 + T_out + Press_mm_hg + RH_out + Windspeed + Visibility + 
              Tdewpoint)^2, data=household_train)
rmse(bigger, household_test)
# 35
household_train = household_train %>% mutate(date =mdy_hm(date))
household_train = household_train %>% mutate(hours=hour(date) %>% factor(),
                                             dayyy = wday(date)%>% factor())
biggest = lm(lights ~ (T1 + RH_1 + T2 + RH_2 + T3 + RH_3 + 
                        T4 + RH_4 + T5 + RH_5 + T6 + RH_6 + T7 + RH_7 + T8 + RH_8 + 
                        T9 + RH_9 + T_out + Press_mm_hg + RH_out + Windspeed + Visibility + 
                        Tdewpoint)^2 + hours + dayyy, data=household_train)
household_test = household_test %>% mutate(date =mdy_hm(date))
household_test = household_test %>% mutate(hour=hours(date) %>% factor(),
                                             dayyy = wday(date)%>% factor())
rmse(biggest, household_test)

#35
dallas_shelter = dallas_shelter %>%
  mutate(month = fct_relevel(month, "JAN", "FEB", "MAR",
                             "APR", "MAY", "JUN", "JUL",
                             "AUG", "SEP", "OCT", "NOV", "DEC"))
prob_35 = dallas_shelter %>% group_by(month, animal_breed) %>% filter(animal_breed == "PIT BULL") %>% summarize(count= n())
#36

dallas_shelter %>% filter(outcome_type== "ADOPTION") %>% summarize(count = n())
1060

prob_36 = dallas_shelter %>% group_by(animal_breed) %>% summarize(count_of_breed = n()) 
prob_b = dallas_shelter %>% group_by(animal_breed) %>% filter(outcome_type== "ADOPTION") %>% summarize(count2 = n())
prob_36 = prob_36 %>% filter(count_of_breed >=25)

#pap
12/27
#plot
13/25
# black
42/91
# guinea 
36/64
#dalmation
25/56
#labrad
1228/ 3224
#hamster
44/76
#prson
71/124

#37
dallas_shelter %>% filter(month == "SEP" & animal_type == "DOG") %>% summarize(count =n())
R= 2146/30
R
1- ppois(55,R)                                                                               
#38
prob_38 = dallas_shelter %>% filter(animal_type == "DOG" & chip_status == "SCAN CHIP")
p_return = prop(~outcome_type, success = "RETURNED TO OWNER", data = prob_38)
pbinom(10, size=37, prob = p_return)
