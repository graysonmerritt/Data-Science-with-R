library(tidyverse)
library(mosaic)

#7
sim = do(10000) * nflip(161,.596)
sum(sim >=105) / 10000
#8
model = lm(JNJ ~ SPY, data = marketmodel)
coef(model)
#9 
model2 = lm(GOOG ~ SPY, data=marketmodel)
rsquared(model2)
#10
model3 = lm(TGT ~ SPY, data=marketmodel) 
boot_model3 = do(10000) * lm(TGT ~ SPY, data=resample(marketmodel) )
confint(boot_model3, level = .95)
#11
sclass = sclass %>% mutate(NY = ifelse(state=="NY", yes= TRUE, no = FALSE))
sclass = sclass %>% mutate(TX = ifelse(state=="TX", yes= TRUE, no = FALSE))
sclass = sclass %>% mutate(over100K = ifelse(mileage >= 100000, yes= TRUE, no = FALSE))

sclass %>%
  summarize(banana = n())

sclass %>% filter(state=="TX") %>% summarize(banana = n())
(2458/29466 ) *100
#12
sclass_NY = sclass %>% filter (NY == TRUE) 
boot_strap_NY = do(10000) * mean(~mileage, data = resample(sclass_NY))
confint(boot_strap_NY, level=.95)
#13
ggplot(sclass) + geom_histogram(aes(x=price)) + facet_wrap(~NY)
#14
xtabs(~over100K + TX, data = sclass) %>% prop.table(margin=2) %>% round(3)
xtabs(~over100K, data=sclass) %>% prop.table() %>% round(3)



NHANES_sleep = NHANES_sleep %>%
  mutate(HomeOwn_Binary = ifelse(HomeOwn == "Own", yes="Own_Yes", no="OwnNo"),
         isBlack = ifelse(Race_Ethnicity == "Black", yes="Black_Yes", no="BlackNo"),
         depressedAny = ifelse(Depressed != "None", yes="Depressed_Yes", no="DepressedNo"))
#15
prop(depressedAny ~ isBlack, data = NHANES_sleep)
#16
prop(depressedAny ~ HomeOwn_Binary, data=NHANES_sleep)
boot_sleep_Own = do(10000) * diffprop(depressedAny~HomeOwn_Binary,data=resample(NHANES_sleep))
confint(boot_sleep_Own, level = .95)
#17
mean(SleepHrsNight ~ HomeOwn_Binary, data=NHANES_sleep)
boot_sleep = do(10000) * diffmean(SleepHrsNight~HomeOwn_Binary,data=resample(NHANES_sleep))
confint(boot_sleep, level = .95)
#18
artwork_tate = artwork_tate  %>% mutate(post1950 = ifelse(year>=1950, yes = TRUE, no = FALSE))
mean(width~post1950, na.rm=TRUE, data=artwork_tate)
sd(width~post1950, na.rm=TRUE, data=artwork_tate)
#19
artwork = artwork_tate %>% group_by(artistId,name)
artwork %>% summarize(count = n()) %>% arrange(desc(count))
#20
artwork_tate %>% filter(artistGender == "Female") %>% group_by(acquisitionYear) %>%
  summarize(count = n()) %>%
  ggplot() + geom_line(aes(x=acquisitionYear, y=count))
