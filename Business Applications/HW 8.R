library(tidyverse)
library(ggfortify)

netflix = netflix %>% mutate(lag1 = lag(NFLX))
ar1 = lm(NFLX ~lag1, data = netflix)
summary(ar1)
acf(residuals(ar1))
autoplot(ar1)
confint(ar1)
acf(netflix$NFLX)

predict(ar1,list(lag1=412.89))

netflix <- netflix %>% mutate(nflx_return = NFLX/lag(NFLX)-1,
                              sp_return = SP500/lag(SP500)-1)

market = lm(nflx_return ~ sp_return, data = netflix)
summary(market)
confint(market)

  
profiles <- read.csv("http://brianlukoff.com/sta235/okcupid.csv")

