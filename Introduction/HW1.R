library(tidyverse)
#1a
xtabs(~daft.punk + david.bowie, data=plays_top50) %>% prop.table(margin=2) %>% round(3)
#1b
xtabs(~johnny.cash +pink.floyd, data =plays_top50) %>% prop.table(margin=2)
#2A
xtabs(~danger,data=superbowl) %>% prop.table %>% round(2)
xtabs(~danger + funny, data = superbowl) %>% prop.table(margin=2) %>% round(2)
#2B
xtabs(~animals,data=superbowl) %>% prop.table %>% round(2)
xtabs(~animals + use_sex, data = superbowl) %>% prop.table(margin=2) %>% round(2)
#2C
xtabs(~celebrity,data=superbowl) %>% prop.table %>% round(2)
xtabs(~celebrity + patriotic, data = superbowl) %>% prop.table(margin=2) %>% round(2)
#3A
ggplot(profs) + 
  geom_histogram(aes(x=eval),binwidth=.1)
#3B
ggplot(profs) + 
  geom_boxplot(aes(x = native, y = eval))
#3C
ggplot(profs) + 
  geom_histogram(aes(x=eval)) + 
  facet_wrap(~gender, nrow=2) 
#3D
ggplot(profs) + 
  geom_point(aes(x=beauty, y=eval))
#4
# create objects for all the statistics for SAT verbal 
satv_mean = utsat %>% summarize(mean(SAT.V)) %>% round(2)
satv_mean
satv_SD = utsat %>% summarize(sd(SAT.V)) %>% round(2)
satv_SD
satv_IQR = utsat %>% summarize(IQR(SAT.V)) %>% round(2)
satv_IQR
satv_5thQ = utsat %>% summarize(quantile(SAT.V, .05)) %>% round(2)
satv_5thQ
satv_25thQ = utsat %>% summarize(quantile(SAT.V, .25)) %>% round(2)
satv_25thQ
satv_median = utsat %>% summarize(median(SAT.V)) %>% round(2)
satv_median
satv_75thQ = utsat %>% summarize(quantile(SAT.V,.75)) %>% round(2)
satv_75thQ
satv_95thQ = utsat %>% summarize(quantile(SAT.V,.95)) %>% round(2)
satv_95thQ
# create objects for all the statistics for SAT quantitative 
satq_mean = utsat %>% summarize(mean(SAT.Q)) %>% round(2)
satq_mean
satq_SD = utsat %>% summarize(sd(SAT.Q)) %>% round(2)
satq_SD
satq_IQR = utsat %>% summarize(IQR(SAT.Q)) %>% round(2)
satq_IQR
satq_5thQ = utsat %>% summarize(quantile(SAT.Q, .05)) %>% round(2)
satq_5thQ
satq_25thQ = utsat %>% summarize(quantile(SAT.Q, .25)) %>% round(2)
satq_25thQ
satq_median = utsat %>% summarize(median(SAT.Q)) %>% round(2)
satq_median
satq_75thQ = utsat %>% summarize(quantile(SAT.Q,.75)) %>% round(2)
satq_75thQ
satq_95thQ = utsat %>% summarize(quantile(SAT.Q,.95)) %>% round(2)
satq_95thQ
# create objects for all the statistics for GPA
gpa_mean = utsat %>% summarize(mean(GPA)) %>% round(2)
gpa_mean
gpa_SD = utsat %>% summarize(sd(GPA)) %>% round(2)
gpa_SD
gpa_IQR = utsat %>% summarize(IQR(GPA)) %>% round(2)
gpa_IQR
gpa_5thQ = utsat %>% summarize(quantile(GPA, .05)) %>% round(2)
gpa_5thQ
GPA_25thQ = utsat %>% summarize(quantile(GPA, .25)) %>% round(2)
GPA_25thQ
gpa_median = utsat %>% summarize(median(GPA)) %>% round(2)
gpa_median
gpa_75thQ = utsat %>% summarize(quantile(GPA,.75)) %>% round(2)
gpa_75thQ
gpa_95thQ = utsat %>% summarize(quantile(GPA,.95)) %>% round(2)
gpa_95thQ
# playground for discovering between college vs within college variability 
# looking at differences within colleges
utsat %>% group_by(School) %>% summarize(avg_satv = mean(SAT.V),
                                         avg_satq = mean(SAT.Q),
                                         avg_satc = mean(SAT.C),
                                         sd_satc = sd(SAT.C),
                                         iqr_satc = IQR(SAT.C)) %>% arrange(avg_satc)
#looking at differences across colleges
utsat %>% summarize(avg_satv = mean(SAT.V),
                    avg_satq = mean(SAT.Q),
                    avg_satc = mean(SAT.C),
                    sd_satc = sd(SAT.C),
                    iqr_satc = IQR(SAT.C)) %>% arrange(avg_satc)

# problem 5
# plot A
bikeshare %>% group_by(hr) %>% summarize(avgRental=mean(total)) %>% 
  ggplot() + geom_line(aes(x=hr, y=avgRental))
#Plot B
bikeshare %>% group_by(hr,workingday) %>% summarize(avgRental = mean(total)) %>%
  ggplot() + geom_line(aes(x=hr,y=avgRental)) + facet_wrap(~workingday)
# Plot C
bikeshare %>% filter(hr==9) %>% group_by(workingday, weathersit) %>% 
  summarize(avgRental = mean(total)) %>% ggplot() + 
  geom_col(aes(x=weathersit, y=avgRental)) + facet_wrap(~workingday)
