library(tidyverse)
library(mosaic)

model = lm(Critics ~ IMDB, data = movies)
summary(model)
coef(model)
confint(model)

predict(model, list(IMDB=7), interval = "prediction")

predict(model, list(IMDB=7), interval = "confidence")

model1 = lm(Critics ~ RottenTomatoes, data = movies)
summary(model1)
coef(model1)
confint(model1)

