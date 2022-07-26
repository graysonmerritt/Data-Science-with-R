library(tidyverse)
library(mosaic)
library(moderndive)
#question 1
sim = do(100000) * nflip(2021, .024)
ggplot(sim) + 
  geom_histogram(aes(x=nflip), binwidth=1)
sum(sim >=70) /100000 

#question 2
e_shows = nbc_pilotsurvey %>% filter(Show == "Living with Ed" |
                                     Show == "My Name is Earl")
t.test(Q1_Happy ~ Show, data=e_shows)
0.4011371 - -0.1030341  

a_shows = nbc_pilotsurvey %>% filter(Show == "The Biggest Loser" |
                                       Show == "The Apprentice: Los Angeles")
t.test(Q1_Annoyed ~ Show, data=a_shows)
0.52455614 - 0.01743792
mean(Q1_Annoyed ~ Show, data=a_shows)

d_shows = nbc_pilotsurvey %>% filter(Show == "Dancing with the Stars")
prop.test(~Q2_Confusing >= 4, data=d_shows)
0.12893254 - 0.04453431 


#question 3
ebay = ebay %>% mutate (rev_ratio = rev_after/rev_before)
boot_ebay = do(10000) * diffmean(rev_ratio ~ adwords_pause, data = resample(ebay))
mean(rev_ratio ~ adwords_pause, data=ebay)
diffmean(rev_ratio ~ adwords_pause, data=ebay)
confint(boot_ebay,level=.95)
-0.01335299 - -0.09133019 

#question 4
prop(voted1998 ~ GOTV_call, data = turnout, success = 1)
prop.test(voted1998 ~ GOTV_call, data = turnout)
0.4515652 - 0.4327682 

#b 
prop.test(voted1998 ~ voted1996, data=turnout)
prop.test(voted1998 ~ AGE, data=turnout)
prop.test(voted1998 ~ MAJORPTY, data=turnout)

#c
turnout = turnout %>% select(-Got_GOTV)
turnout = turnout %>% select(-Actually_Voted)
turnout_matched = matchit(GOTV_call ~ voted1996 + AGE + MAJORPTY, data = turnout, 
                          ratio=5) %>% match.data
mean(AGE~GOTV_call, data = turnout_matched)
mean(AGE~GOTV_call, data = turnout)
xtabs(~voted1996 + GOTV_call, data=turnout_matched) %>%
  prop.table(margin=2)
xtabs(~MAJORPTY + GOTV_call, data=turnout_matched) %>%
  prop.table(margin=2)


prop(voted1998 ~ GOTV_call, data = turnout_matched, success = 1)
prop.test(~voted1998 + GOTV_call, data = turnout_matched, success=1)
0.5539849 - 0.5025468 

#question 5
ggplot(data =solder) + geom_boxplot(aes(x=Opening, y=skips))
ggplot(data =solder) + geom_boxplot(aes(x=Solder, y=skips))

#b
model= lm(skips ~ Opening + Solder + Opening:Solder, data=solder)
confint(model, level = .95)
get_regression_table(model)
coef(model)
