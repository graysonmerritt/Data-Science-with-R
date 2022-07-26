library(tidyverse)
library(mosaic)
library(effectsize)
library(moderndive)
library(rsample)
library(modelr)
library(lubridate)
# question 1
redline_model = lm(policies ~ minority + age + fire + income ,data=redlining)
coef(redline_model)
get_regression_table(redline_model, conf.level = .95, digits = 3)
rsquared(redline_model)

#question 2 
groceries %>% group_by(Store) %>% summarize(average_price_of_products = mean(Price)) %>% ggplot() + geom_col(aes(x=Store, y= average_price_of_products)) +coord_flip()
groceries %>% group_by(Product) %>% summarize(number_of_stores_selling_product = n()) %>% ggplot() + geom_col(aes(x= Product, y = number_of_stores_selling_product)) + coord_flip()
model1 = lm(Price ~ Type + Product, data = groceries)
view(get_regression_table(model1))
coef(model1)

model2 = lm(Price ~ Store + Product, data = groceries)
view(get_regression_table(model2))
coef(model2)
confint(model2)

groceries = mutate(groceries, Income10K = Income/10000)
model3 = lm(Price ~ Income10K + Product, data = groceries)
coef(model3)
get_regression_table(model3)
standardize_parameters(model3)


#question 3
small_model = lm(children ~ market_segment + adults + customer_type + is_repeated_guest, data = hotels_train)
big_model = lm(children ~ (. - arrival_date), data = hotels_train)
coef(big_model)
huge_model = lm(children ~ (. - arrival_date)^2, data = hotels_train)
coef(huge_model)

#making a new data frame so It doesn't mess with other models
hotels_train_e = hotels_train %>% mutate(arrival_date = ymd(arrival_date))
hotels_train_e = hotels_train_e %>% mutate(month = month(arrival_date) %>% factor())
hotels_test_e = hotels_test %>% mutate(arrival_date = ymd(arrival_date))
hotels_test_e = hotels_test_e %>% mutate(month = month(arrival_date) %>% factor())
engineered_model = lm(children ~ (. - arrival_date), data = hotels_train_e)

rmse(small_model, hotels_test) %>% round(4)
rmse(big_model, hotels_test) %>% round(4)
rmse(huge_model, hotels_test) %>% round(4)
rmse(engineered_model, hotels_test_e) %>% round(4)

rmse(small_model, hotels_train) %>% round(4)
rmse(big_model, hotels_train) %>% round(4)
rmse(huge_model, hotels_train) %>% round(4)
rmse(engineered_model, hotels_train_e) %>% round(4)

#Question 4
avg_home = mean(epl_2018.19_home$GF)/19
avg_away = mean(epl_2018.19_away$GF)/19
attack_strength = mean(epl_2018.19_away$GF + epl_2018.19_home$GF)

#1
liverpool_offense = (34+55)/attack_strength
tottenham_defense = (23+16)/attack_strength
liverpool_goals=liverpool_offense * tottenham_defense * avg_home
liverpool_goals

tottenham_offense = (33+34)/attack_strength
liverpool_defense = (12+10)/attack_strength
tottenham_goals = tottenham_offense * liverpool_defense *avg_away
tottenham_goals 

soccer_scores= tibble(liverpool=0:6, tottenham = 0:6)
soccer_scores = soccer_scores %>% expand(liverpool, tottenham) %>%
  mutate(prob = dpois(liverpool, liverpool_goals) * dpois(tottenham, tottenham_goals))
#find the 2-1 prob of home team winning
exp(-liverpool_goals) * liverpool_goals^2/factorial(2) * exp(-tottenham_goals) * tottenham_goals^1/factorial(1)

soccer_scores %>% filter(liverpool > tottenham) %>% summarize(sum(prob))
soccer_scores %>% filter(liverpool == tottenham) %>% summarize(sum(prob))
soccer_scores %>% filter(liverpool < tottenham) %>% summarize(sum(prob))

#2 
mancity_offense = (38+57)/attack_strength
arsenal_defense = (35+16)/attack_strength
mancity_goals=mancity_offense * arsenal_defense * avg_home
mancity_goals

arsenal_offense = (31+42)/attack_strength
mancity_defense = (11+12)/attack_strength
arsenal_goals = arsenal_offense * mancity_defense *avg_away
arsenal_goals 

soccer_scores2= tibble(mancity=0:6, arsenal = 0:6)
soccer_scores2 = soccer_scores2 %>% expand(mancity, arsenal) %>%
  mutate(prob = dpois(mancity, mancity_goals) * dpois(arsenal, arsenal_goals))
#find the 2-1 prob of home team winning
exp(-mancity_goals) * mancity_goals^2/factorial(2) * exp(-arsenal_goals) * arsenal_goals^1/factorial(1)

soccer_scores2 %>% filter(mancity > arsenal) %>% summarize(sum(prob))
soccer_scores2 %>% filter(mancity == arsenal) %>% summarize(sum(prob))
soccer_scores2 %>% filter(mancity < arsenal) %>% summarize(sum(prob))
