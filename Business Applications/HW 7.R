library(ggfortify)
options(scipen=999)


# 5 as time marches on, we prdeict the atasales would increase at an increasing rate
# 6 
# 7 is it cuz RSE is in log(y) units?

lm1 = lm(sales ~ S1 + S2 + S3 + quarter, data = techsoft)
summary(lm1)
lm2 = lm(sales ~ S1 + S2 + S3 + quarter + I(quarter^2), data = techsoft)
summary(lm2)

predict(lm2, list(quarter=101, S1=1,S2=0,S3=0), interval='prediction')


predict(lm2, list(quarter=100, S1=0, S2=0, S3=0))
188.16346 - 189.1088 
-0.94534 * 1000000
residuals(lm2)[100] * 1000000


lm3 = lm(log(sales) ~ S1 + S2 + S3 + quarter + I(quarter^2), data = techsoft)
exp(predict(lm3, list(quarter=101, S1=1,S2=0,S3=0), interval='prediction'))
