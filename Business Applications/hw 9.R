model = glm(Buy ~ Income, data = ecommerce, family ='binomial')
summary(model)
exp(0.14940)

library(tidyverse)
fun <- function(x) exp(-9.34415+ 0.14940*x)/(1 + exp(-9.34415+ 0.14940*x))
ggplot(ecommerce, aes(x = Income, y = Buy)) +
  geom_point()  +
  stat_function(aes(), fun = fun)

predict(model, list(Income = 40), type='response')
predict(model, list(Income = 50), type='response')
predict(model, list(Income = 60), type='response')


predicted.buy = ifelse(predict(model, type = 'response') >= .5, 1,0)

xtabs(~ ecommerce$Buy + predicted.buy )
99/ (26+99)
23/(525+23)

correct = ifelse(predicted.buy == ecommerce$Buy,1,0)
sum(correct)/nrow(ecommerce)

xtabs(~ Buy, data = ecommerce) %>% prop.table

model2 = glm(Buy ~ Income + Married + Female, data = ecommerce, family ='binomial')
summary(model2)
exp(1.38040)
exp(1.36534)

model3 = glm(Buy ~ Income*Married, data = ecommerce, family ='binomial')
summary(model3)
0.23221    + -0.10152    
exp(0.13069)
mean(ecommerce$Income)
predict(model3, list(Married = 0, Income = 35.07875), type = 'response')


-15.70992 + 8.01401  
0.23221    + -0.10152    
fun.married <- function(x) exp(-7.69591+0.13069*x)/(1 + exp(-7.69591+0.13069*x))
fun.not.married <- function(x) exp(-15.70992+0.23221    *x)/(1 + exp(-15.70992+0.23221    *x))
ggplot(ecommerce, aes(x = Income, y = Buy, colour = factor(Married))) +
  geom_point() + xlim(0,80) +
  stat_function(aes(color = "1"), fun = fun.married) +
  stat_function(aes(color = "0"), fun = fun.not.married)
