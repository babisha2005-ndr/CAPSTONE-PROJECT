## IMPORT AND EXPLORE THE DATASET.

library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(skimr)
library(DataExplorer)

turtle_reviews <- read.csv("turtle_reviews.csv", header = TRUE)





head(turtle_reviews)

dim(turtle_reviews)
str(turtle_reviews)
glimpse(turtle_reviews)
as_tibble(turtle_reviews)
turtle_reviews[is.na(turtle_reviews)]
sum(is.na(turtle_reviews))
summary(turtle_reviews)
skim(turtle_reviews)

## REMOVE UNNECESSARY COLUMNS AND RENAME COLUMNS.
turtle_reviews2 <- select(turtle_reviews, -language, -product, -platform, -review, -summary)
head(turtle_reviews2)
names(turtle_reviews2)[3] <- "remuneration"

names(turtle_reviews2)[4] <- "spending_score"

colnames(turtle_reviews2)
head(turtle_reviews2)
write.csv(turtle_reviews2, "turtle_rclean.csv")
tlr <- read.csv("turtle_rclean.csv", header = TRUE, row.names =1)
head(tlr)

## EXPLORATORY DATA ANALYSIS USING VISUALISATIONS. 


ggplot(tlr, aes(x = age)) + geom_histogram(bins=10) + labs(title = "HISTOGRAM OF AGE")
ggplot(tlr, aes(x = age)) + geom_density() + labs(title = "DENSITY PLOT OF AGE")
ggplot(tlr, aes(x = remuneration)) + geom_histogram(bins=10) + labs(title = "HISTOGRAM OF REMUNERATION")
ggplot(tlr, aes(x = remuneration)) + geom_density() + labs(title = "DENSITY PLOT OF REMUNERATION")
ggplot(tlr, aes(x = loyalty_points)) + geom_histogram(bins=15)+ scale_x_continuous(breaks = seq(0, 7000,500)) + labs(title = "HISTOGRAM OF LOYALTY POINTS")
ggplot(tlr, aes(x = loyalty_points)) + geom_density()  + labs(title = "DENSITY PLOT OF LOYALTY POINTS")
ggplot(tlr, aes(x = spending_score)) + geom_histogram(bins=10) + labs(title = "HISTOGRAM OF SPENDING SCORE")
ggplot(tlr, aes(x = spending_score)) + geom_density() + labs(title = "DENSITY PLOT OF SPENDING SCORE")


ggplot(tlr, aes(x = gender)) + geom_bar() + labs(title = "BAR CHART OF GENDER")
ggplot(tlr, aes(x = education)) + geom_bar() + labs(title = "BAR CHART OF EDUCATION")
ggplot(tlr, aes(x = education, fill = gender)) + geom_bar() + labs(title = "BAR CHART OF EDUCATION VS GENDER")
ggplot(tlr, aes(x = gender, fill = education)) + geom_bar() + labs(title = "BAR CHART OF GENDER VS EDUCATION")
ggplot(tlr, aes(x = gender, fill = education)) + geom_bar(position = "dodge") + labs(title = "BAR CHART OF GENDER VS EDUCATION")
ggplot(tlr, aes(x = gender, y = loyalty_points)) + geom_boxplot(fill = 'red', outlier.color = 'red') + labs(title = "BOXPLOT OF GENDER VS LOYALTY POINTS")
ggplot(tlr, aes(x = education, y = loyalty_points)) + geom_violin(fill = 'red') +geom_boxplot(fill = 'purple', width = 0.25,outlier.color = 'blue') + labs(title = "VIOLIN PLOT OF EDUCATION VS LOYALTY POINTS")
                                                                                        
ggplot(data = tlr, mapping=aes(x=age, y=loyalty_points)) +
  geom_point(color = 'yellow',
             alpha = 0.5,
             size = 1.5)+
  geom_smooth(method = 'lm',
              se = FALSE,
              linewidth = 1.5)+
  facet_wrap(~gender) + labs(title = "SCATTERPLOT OF AGE VS LOYALTY POINTS BASED ON GENDER")

ggplot(data = tlr, mapping=aes(x=age, y=remuneration)) +
  geom_point(color = 'yellow',
             alpha = 0.5,
             size = 1.5)+
  geom_smooth(method = 'lm',
              se = FALSE,
              linewidth = 1.5)+
  facet_wrap(~gender) + labs(title = "SCATTERPLOT OF AGE VS REMUNERATION BASED ON GENDER")


ggplot(data = tlr, mapping=aes(x=remuneration, y=loyalty_points)) +
  geom_point(color = 'yellow',
             alpha = 0.5,
             size = 1.5)+
  geom_smooth(method = 'lm',
              se = FALSE,
              linewidth = 1.5)+
  facet_wrap(~gender) + labs(title = "SCATTERPLOT OF REMUNERATION VS LOYALTY POINTS BASED ON GENDER")

ggplot(data = tlr, mapping=aes(x=spending_score, y=loyalty_points)) +
  geom_point(color = 'yellow',
             alpha = 0.5,
             size = 1.5)+
  geom_smooth(method = 'lm',
              se = FALSE,
              linewidth = 1.5)+
  facet_wrap(~gender) + labs(title = "SCATTERPLOT OF SPENDING SCORE VS LOYALTY POINTS BASED ON GENDER")


ggplot(data = tlr, mapping=aes(x=age, y=loyalty_points, color=education)) +
  geom_point(color = 'yellow',
             alpha = 0.5,
             size = 1.5)+
  geom_smooth(method = 'lm',
              se = FALSE,
              linewidth = 1.5) + labs(title = "SCATTERPLOT OF AGE VS LOYALTY POINTS BASED ON EDUCATION")

ggplot(data = tlr, mapping=aes(x=remuneration, y=loyalty_points, color=education)) +
  geom_point(color = 'yellow',
             alpha = 0.5,
             size = 1.5)+
  geom_smooth(method = 'lm',
              se = FALSE,
              linewidth = 1.5) + labs(title = "SCATTERPLOT OF REMUNERATION VS LOYALTY POINTS BASED ON EDUCATION")

ggplot(data = tlr, mapping=aes(x=age, y=remuneration, color=education)) +
  geom_point(color = 'yellow',
             alpha = 0.5,
             size = 1.5)+
  geom_smooth(method = 'lm',
              se = FALSE,
              linewidth = 1.5) + labs(title = "SCATTERPLOT OF AGE VS REMUNERATION BASED ON EDUCATION")

ggplot(data = tlr, mapping=aes(x=spending_score, y=loyalty_points, color=education)) +
  geom_point(color = 'yellow',
             alpha = 0.5,
             size = 1.5)+
  geom_smooth(method = 'lm',
              se = FALSE,
              linewidth = 1.5) + labs(title = "SCATTERPLOT OF SPENDING SCORE VS LOYALTY POINTS BASED ON EDUCATION")



ggplot(data = tlr, mapping=aes(x=age, y=loyalty_points, color=education)) +
  geom_point(color = 'yellow',
             alpha = 0.5,
             size = 1.5)+
  geom_smooth(method = 'lm',
              se = FALSE,
              linewidth = 1.5)+
  facet_wrap(~gender) + labs(title = "SCATTERPLOT OF AGE VS LOYALTY POINTS BASED ON EDUCATION AND GENDER")

ggplot(data = tlr, mapping=aes(x=age, y=remuneration, color=education)) +
  geom_point(color = 'yellow',
             alpha = 0.5,
             size = 1.5)+
  geom_smooth(method = 'lm',
              se = FALSE,
              linewidth = 1.5)+
  facet_wrap(~gender) + labs(title = "SCATTERPLOT OF AGE VS REMUNERATION BASED ON EDUCATION AND GENDER")


ggplot(data = tlr, mapping=aes(x=remuneration, y=loyalty_points, color=education)) +
  geom_point(color = 'yellow',
             alpha = 0.5,
             size = 1.5)+
  geom_smooth(method = 'lm',
              se = FALSE,
              linewidth = 1.5)+
  facet_wrap(~gender) + labs(title = "SCATTERPLOT OF REMUNERATION VS LOYALTY POINTS BASED ON EDUCATION AND GENDER")

ggplot(data = tlr, mapping=aes(x=spending_score
                               , y=loyalty_points, color=education)) +
  geom_point(color = 'yellow',
             alpha = 0.5,
             size = 1.5)+
  geom_smooth(method = 'lm',
              se = FALSE,
              linewidth = 1.5)+
  facet_wrap(~gender) + labs(title = "SCATTERPLOT OF SPENDING SCORE VS LOYALTY POINTS BASED ON EDUCATION AND GENDER")



## STATISTICAL ANALYSIS AND DESCRIPTIVE STATISTICS.

df_tlr <- tlr %>% group_by(education, gender) %>%
     summarise(sd_loyalty_points=sd(loyalty_points),
               sd_spending_score=sd(spending_score),
               sd_age=sd(age),
               sd_remuneration=sd(remuneration),groups = 'drop')
df_tlr
               

df_tlr1 <- tlr %>% group_by(education, gender) %>%
  summarise(mean_loyalty_points=mean(loyalty_points),
            mean_spending_score=mean(spending_score),
            mean_age=mean(age),
            mean_remuneration=mean(remuneration),.groups='drop')
df_tlr1

df_tlr2 <- tlr %>% group_by(education, gender) %>%
  summarise(var_loyalty_points=var(loyalty_points),
            var_spending_score=var(spending_score),
            var_age=var(age),
            var_remuneration=var(remuneration),.groups='drop')
df_tlr2





# DESCRIPTIVE STATISTICS.

summary(tlr$loyalty_points)
mean(tlr$loyalty_points)
sd(tlr$loyalty_points)
var(tlr$loyalty_points)

boxplot(tlr$loyalty_points)
qqnorm(tlr$loyalty_points)
qqline(tlr$loyalty_points)
shapiro.test(tlr$loyalty_points)
library(moments)
skewness(tlr$loyalty_points)
kurtosis(tlr$loyalty_points)

## MULTIPLE LINEAR REGRESSION.
tlr2 <- select(tlr, -gender, -education)

summary(tlr2)
dim(tlr2)


cor(tlr2)

library(psych)


corPlot(tlr2, cex=2)

## CREATE A NEW REGRESSION MODEL.

tr_model1 = lm(loyalty_points~spending_score+remuneration, data = tlr2)
summary(tr_model1)


tr_model2 = lm(loyalty_points~spending_score+remuneration+age, data = tlr2)
summary(tr_model2)

tr_model3 = lm(loyalty_points~spending_score+age, data = tlr2)
summary(tr_model3)

tr_model4 = lm(loyalty_points~remuneration+age, data = tlr2)
summary(tr_model4)





residuals <- rstandard(tr_model3)

qqnorm(residuals(tr_model3))
qqline(residuals(tr_model3))


# PREDICT LOYALTY POINTS USING tr_model3

predict_lp <- data.frame(spending_score = 43, age = 65)

predict(tr_model3, newdata = predict_lp)




               
    
