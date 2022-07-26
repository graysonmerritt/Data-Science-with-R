#load libraries
library(tidyverse)
library(mosaic)

# question 1 
# Recode the categorical variables in sensible, rather than alphabetical, order
capmetro_UT = mutate(capmetro_UT,
                     day_of_week = factor(day_of_week,
                                          levels=c("Mon", "Tue", "Wed","Thu", "Fri", "Sat", "Sun")),
                     month = factor(month,
                                    levels=c("Sep", "Oct","Nov")))
capmetro_UT %>% group_by(day_of_week,hour_of_day, month) %>%
  summarize(avg_boarding = mean(boarding)) %>% ggplot() + geom_line(aes(x=hour_of_day,y=avg_boarding, color=month)) + 
  facet_wrap(~day_of_week)


ggplot(capmetro_UT) + geom_point(aes(x=temperature, y=boarding, color= weekend)) + facet_wrap(~hour_of_day)


#question 2
#a 
billboard_a = billboard %>% group_by(performer,song)
billboard_a %>% summarize(count = n()) %>% arrange(desc(count))
#b
billboard_cutoff = billboard %>% filter(year != 1958 & year != 2021)
table_with_counts = billboard_cutoff %>% group_by(performer,song,year) %>% 
  summarize(total_count = n())
unique_song_count = table_with_counts %>% group_by(year) %>% 
  summarize(unique_songs = n())
ggplot(unique_song_count) + geom_line(aes(x=year,y=unique_songs))
#C
billboard_ten_week = billboard %>% group_by(performer,song) %>% 
  summarize(count = n()) %>%
  filter(count >=10)
billboard_19_artists = billboard_ten_week %>% group_by(performer) %>% 
  summarize(song_count =n()) %>% filter(song_count >=30)
ggplot(billboard_19_artists) + geom_col(aes(x=performer, y=song_count)) + 
  coord_flip()

# question 3
covid_italy = covid %>% filter(country == "Italy") 
covid_spain = covid %>% filter(country == "Spain")
lm_italy = lm((log(deaths)~ days_since_first_death), data = covid_italy)
lm_spain = lm((log(deaths)~ days_since_first_death), data = covid_spain)
coef(lm_italy) 
coef(lm_spain) 
covid %>% group_by(country) %>% ggplot() + geom_line(aes(x=days_since_first_death
                                                         ,y=log(deaths), 
                                                         color=country))

#question 4
#a
sclass_a = sclass %>% filter(year == 2011 & trim == "63 AMG")
boot_strap_a = do(10000) * mean(~mileage, data = resample(sclass_a))
confint(boot_strap_a, level=0.95)
#b 
sclass_b = sclass %>% filter(year == 2014 & trim == "550")
sclass_b = sclass_b %>% mutate(isBlack = ifelse(color == "Black", yes=TRUE, no=FALSE))
boot_strap_b = do(10000) * prop(~isBlack, data = resample(sclass_b))
confint(boot_strap_b,level=.95)

#question 5 
boot_milk = do(10000)*lm(log(sales) ~ log(price), data=resample(milk))
confint(boot_milk, level =.95)
head(boot_milk)
ggplot(boot_milk) + geom_histogram(aes(x=log.price.))

#question 6
boot_arm = do(10000) * diffprop(LonR_fold ~Sex, data = resample(armfold))
head(boot_arm)
confint(boot_arm, level = .95)
