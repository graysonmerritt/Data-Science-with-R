install.packages("car")
install.packages("ggfortify")
library(car)
library(ggfortify)

model= lm(mpg~engine.size+fuel,data=auto_new)
summary(model)
autoplot(model)

model2= lm(mpg~engine.size + I(engine.size^2) +fuel,data=auto_new)
summary(model2)

model3= lm(mpg~engine.size + I(engine.size^2) + I(engine.size^3) +fuel,data=auto_new)
summary(model3)

model4= lm(mpg~engine.size + I(engine.size^2) + I(engine.size^3) 
           + I(engine.size^4)+ I(engine.size^5)+ I(engine.size^6)
           + I(engine.size^7) + I(engine.size^8)+ I(engine.size^9)
           +fuel,data=auto_new)
summary(model4)

predict(model2,list(engine.size=5,fuel='diesel'))
predict(model2,list(engine.size=6,fuel='diesel'))
predict(model3,list(engine.size=5,fuel='diesel'))
predict(model3,list(engine.size=6,fuel='diesel'))
predict(model4,list(engine.size=5,fuel='diesel'))
predict(model4,list(engine.size=6,fuel='diesel'))

59.696-7.548

ggplot(auto_new, aes(x  = engine.size, y  = mpg, colour = fuel)) +
  geom_point() +
  geom_function(aes(color = "diesel"),fun = ~ 59.696 + -18.245 *(.x) + 2.146*(.x)^2) +
  geom_function(aes(color = "gas"),fun = ~  52.148 + -18.245 *(.x) + 2.146*(.x)^2)

model5= lm(log(mpg)~log(engine.size)+fuel,data=auto_new)
summary(model5)
autoplot(model5)
3.91526    -0.27814
ggplot(auto_new, aes(x  = log(engine.size), y  = log(mpg), colour = fuel)) +
  geom_point() +
  geom_function(aes(color = "diesel"),fun = ~ 3.91526     + -0.68178*(.x)) +
  geom_function(aes(color = "gas"),fun = ~  3.63712 + -0.68178*(.x))
-0.68178 * 100
exp(predict(model5,list(engine.size=5,fuel='diesel')))
exp(predict(model5,list(engine.size=6,fuel='diesel')))
ggplot(auto_new, aes(x  = engine.size, y  = mpg, colour = fuel)) +
  geom_point() +
  geom_function(aes(color = "diesel"),fun = ~ exp(3.91526 + -0.68178*log(.x))) +
  geom_function(aes(color = "gas"),fun = ~ exp( 3.63712 + -0.68178*log(.x)))

model6 = lm(log(Sales)~log(Price),data=car_freshener)
summary(model6)
exp(predict(model6,list(Price=1.8)))
