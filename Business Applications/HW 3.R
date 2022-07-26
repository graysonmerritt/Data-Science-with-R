library(tidyverse)
options(scipen=999)

model = lm(Sale_Price ~ Beds + Fireplaces, Year_Built, data = houses_mod)

ggplot() + geom_boxplot(aes(x= Central_Air, y = Sale_Price), data = houses_mod)

model1 = lm(Sale_Price ~ Central_Air, data = houses_mod)
summary(model1)
confint(model1)

model2 = lm(Sale_Price ~ Area + Central_Air, data = houses_mod)
ggplot(data = houses_mod, mapping = aes(x=Area, y=Sale_Price, color=Central_Air)) +
  geom_point() +
  geom_line(aes(y=predict(model2)))
summary(model2)

houses_mod = houses_mod %>% mutate(oppo = ifelse(Central_Air == 'N', 1, 0))
model3 = lm(Sale_Price ~ Area + oppo, data = houses_mod)
summary(model3)
-35191.652 + 57594.994   

model4 = lm(Sale_Price ~ Beds + Fireplaces  + Year_Built + Area + Central_Air, data = houses_mod)
summary(model4)
confint(model4)

predict(model4, list(Beds = 3, Fireplaces = 1, Area=2000,Year_Built =2005, Central_Air = 'Y'), interval = "prediction")
model5 = lm(Sale_Price ~ Beds + Fireplaces  + Garage_Type + Area + Central_Air, data = houses_mod)
summary(model5)
