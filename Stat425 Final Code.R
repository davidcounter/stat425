library(dplyr)
library(ggplot2)
library(kableExtra)
library(tibble)
library(caret)
library(mltools)
library(data.table)
library("tidyverse")
library("dplyr")
library("rsample")
library("caret")
library("kableExtra")
library("doParallel")
library("microbenchmark")
library("foreach")
library("parallel")
library("gridExtra")
library("lubridate")
library("readr")
library("lmtest")
library("mltools")
library("data.table")
library("car")
library("bestNormalize")


mergedata = read.csv("mergedata.csv")
weather = read.csv("weather.csv", stringsAsFactors = FALSE)
mergetestdata = read.csv("mergedtestdata.csv")
sample = read.csv("sampleSubmission.csv")
drops <- c("X", "key", "codesum", "station_nbr")
mergedata <- mergedata[ , !names(mergedata) %in% drops]
mergetestdata <- mergetestdata[ , !names(mergetestdata) %in% drops]

mergedata$date <- as.Date(mergedata$date, "%Y-%m-%d")
mergedata$store_nbr <- as.factor(mergedata$store_nbr)
mergedata$item_nbr <- as.factor(mergedata$item_nbr)
mergedata$id = paste0(as.character(mergedata$store_nbr), "_", as.character(mergedata$item_nbr))


mergetestdata$date <- as.Date(mergetestdata$date, "%Y-%m-%d")
mergetestdata$store_nbr <- as.factor(mergetestdata$store_nbr)
mergetestdata$item_nbr <- as.factor(mergetestdata$item_nbr)
mergetestdata$id = paste0(as.character(mergetestdata$store_nbr), "_", as.character(mergetestdata$item_nbr))




weatherclean = weather
weatherclean[weatherclean == "M"] = NA
weatherclean[weatherclean == weatherclean$snowfall[37]] = 0
weatherclean$sunrise = as.numeric(weatherclean$sunrise)
weatherclean$sunset = as.numeric(weatherclean$sunset)
weatherclean[weatherclean == "-'"] = NA
weatherclean$date = as.Date(weatherclean$date, "%Y-%m-%d")

tibble(
  "Column" = colnames(weatherclean[-(1:3)]),
  "Percentage of Missing Values (%)" =  as.numeric(colMeans(is.na(weatherclean[-(1:3)]))) * 100
) %>%
  kable(caption = "Table 1: Percentage of Missing Values for Each Column of the Weather Dataset", digits = 4) %>%
  kable_styling("striped", full_width = FALSE, latex_options = "hold_position")


# missing_unit_values = sum(mergedata$units == 0)
# all_ids = unique(mergedata$id)
# length_each_id = unlist(map(all_ids, function(x) sum(mergedata$id == x)))
# zeros_per_id = unlist(map(all_ids, function(x) sum(mergedata$id == x & mergedata$units == 0)))
# valid_ids = all_ids[length_each_id != zeros_per_id]
# no_zeros = mergedata[mergedata$id %in% valid_ids,]
# 
# no_zeros$blackFriday = 0
# no_zeros$blackFriday[no_zeros$date == "2013-11-29"] = 1
# no_zeros$blackFriday[no_zeros$date == "2012-11-23"] = 1
# 
# mergetestdata$blackFriday = 0
# mergetestdata$blackFriday[mergetestdata$date == "2014-11-28"] = 1
# 
# 
# write.csv(no_zeros, file = "refined_data.csv", row.names = FALSE)
# write.csv(mergetestdata, file = "mergedtestdata.csv", row.names = FALSE)





refined_data = read.csv("refined_data.csv")

mergetestdata = read.csv("mergedtestdata.csv")

refined_data$date <- as.Date(refined_data$date, "%Y-%m-%d")
refined_data$store_nbr <- as.factor(refined_data$store_nbr)
refined_data$item_nbr <- as.factor(refined_data$item_nbr)

mergetestdata$date <- as.Date(mergetestdata$date, "%Y-%m-%d")
mergetestdata$store_nbr <- as.factor(mergetestdata$store_nbr)
mergetestdata$item_nbr <- as.factor(mergetestdata$item_nbr)




# p1 = refined_data %>% 
#   ggplot(aes(x = units)) +
#   geom_histogram(bins = 30) + 
#   xlim(0, 200) +
#   ylim(0, 15000) +
#   ggtitle("Figure 1: Histogram of Units")
# 
# p2 = refined_data %>% 
#   ggplot(aes(x = item_nbr, y = units)) + 
#   geom_point(col = "orange") +
#   ggtitle("Figure 2: Scatter Plot of Item Number vs Number of Units")
# 
# p3 = refined_data %>%
#     ggplot(aes(x = eventdist, fill = eventdist)) +
#     geom_bar() +
#     ggtitle("Figure 3: Bar plot of Number of Weather Events")
# 
# gridExtra::grid.arrange(p1, p2, p3, ncol = 3)


![Histogram of Units](plot1.png)
![Scatter Plot](plot2.png)
![Bar plot](plot3.png)

mergedtestdata = mergetestdata
refined_data_drop = refined_data[ , !names(refined_data) %in% "id"]

refined_data_one_hot = data.frame(one_hot(data.table(refined_data_drop)))

mergedtestdata_drop = mergedtestdata[ , !names(mergedtestdata) %in% c("id", "date")]

mergedtest_one_hot = data.frame(one_hot(data.table(mergedtestdata_drop)))

mergedtest_one_hot$store_nbr_35 = 0

SampleOLS <- sample


fullOLSModel <- lm(units ~ ., data = refined_data_one_hot)

mergedtest_one_hot$date = mergedtestdata$date

predictionOLS <- predict(fullOLSModel, newdata = mergedtest_one_hot)
predictionOLS[is.na(predictionOLS)] <- 0

predictionOLS[!mergedtestdata$id %in% refined_data$id] = 0
SampleOLS$units = predictionOLS

write.csv(x = SampleOLS, file =  "SampleOLS.csv", row.names = FALSE)




reg_model = lm(log(units + 1) ~ store_nbr + item_nbr + date + eventdist + day_of_week + weekend + month + blackFriday, data = refined_data)

predictions = predict(reg_model, mergetestdata)
predictions[!mergetestdata$id %in% refined_data$id] = 0
sampleSub = read.csv("sampleSubmission.csv")
sampleSub$units = predictions
write.csv(sampleSub,"testSubmission.csv", row.names = FALSE)




model = train(
units ~ store_nbr + item_nbr + date + eventdist + day_of_week + weekend + month, data = refined_data, method = "glmnet", trControl = trainControl("cv", number = 10), tuneLength = 10
)
predictions = predict(model, mergetestdata)
predictions[!mergetestdata$id %in% refined_data$id] = 0
sampleSub = read.csv("sampleSubmission.csv")
sampleSub$units = predictions
write.csv(sampleSub,"elastic2Submission.csv", row.names = FALSE)


{r, warning=FALSE}
cutoff=4/(nrow(refined_data)-length(reg_model$coefficients)-2)
plot(reg_model, which=4, cook.levels=cutoff)
influencePlot(reg_model, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )


refined_data = refined_data[-c(75361, 80334,162942), ]
model2 = lm(units ~ item_nbr + store_nbr + date + eventdist + day_of_week + weekend + month + blackFriday, data = refined_data)
par(mfrow = c(2, 2))
p5 = plot(model2)



m = yeojohnson(refined_data$units)
lam = m$lambda
y = ((((refined_data$unit+1)^lam)-1)/lam)
model3 = lm(y ~ item_nbr + store_nbr + date + eventdist + day_of_week + weekend + month + blackFriday, data = refined_data)
par(mfrow = c(2,2))
plot(model3)


refined_data <- read.csv("refined_data.csv")
refined_data$store_nbr <- as.factor(refined_data$store_nbr)
refined_data$item_nbr <- as.factor(refined_data$item_nbr)
refined_data$date <- as.Date(refined_data$date, "%Y-%m-%d")


SampleLASSO <- sample
mergedtestdata <- read.csv("mergedtestdata.csv")
mergedtestdata$store_nbr <- as.factor(mergedtestdata$store_nbr)
mergedtestdata$item_nbr <- as.factor(mergedtestdata$item_nbr)
mergedtestdata$date <- as.Date(mergedtestdata$date, "%Y-%m-%d")

units <- refined_data$units

modelmatrix <- model.matrix(lm(units~date +store_nbr+item_nbr+eventdist+day_of_week+weekend+month+blackFriday, family = gaussian, data = refined_data))[, -36]
y=seq(1, nrow(mergedtestdata), by = 1)
mergedtestdata$y = y
modelmatrixtest <- model.matrix(lm(y ~ date+store_nbr+item_nbr+eventdist+day_of_week+weekend+month+blackFriday, family = gaussian, data = mergedtestdata))

lambdas = 10^seq(3, -2, by = -.1)
CVglm = cv.glmnet(x = modelmatrix, y = units, family = "gaussian", alpha = 1)
GLMModel <- glmnet(x = modelmatrix, y=units,  family="gaussian", alpha = 1, lambda = CVglm$lambda.min)

predictionLASSO <- predict(GLMModel, newx = modelmatrixtest, type ="response")

predictions <- predictionLASSO
predictions[!mergedtestdata$id %in% refined_data$id] = 0
predictions[is.na(predictions)] <- 0

SampleLASSO$units <- predictions

write.csv(x = SampleLASSO, file =  "SampleLASSO.csv", row.names = FALSE)




refined_data <- read.csv("refined_data.csv")
refined_data$store_nbr <- as.factor(refined_data$store_nbr)
refined_data$item_nbr <- as.factor(refined_data$item_nbr)
refined_data$date <- as.Date(refined_data$date, "%Y-%m-%d")


SampleRIDGE <- sample
mergedtestdata <- read.csv("mergedtestdata.csv")
mergedtestdata$store_nbr <- as.factor(mergedtestdata$store_nbr)
mergedtestdata$item_nbr <- as.factor(mergedtestdata$item_nbr)
mergedtestdata$date <- as.Date(mergedtestdata$date, "%Y-%m-%d")

units <- refined_data$units

modelmatrix <- model.matrix(lm(units~date+store_nbr+item_nbr+eventdist+day_of_week+weekend+month+blackFriday, family = gaussian, data = refined_data))[, -36]
y=seq(1, nrow(mergedtestdata), by = 1)
mergedtestdata$y = y
modelmatrixtest <- model.matrix(lm(y ~ date+store_nbr+item_nbr+eventdist+day_of_week+weekend+month+blackFriday, family = gaussian, data = mergedtestdata))

lambdas = 10^seq(3, -2, by = -.1)
CVglm = cv.glmnet(x = modelmatrix, y = units, family = "gaussian", alpha = 0)
GLMModel <- glmnet(x = modelmatrix, y=units,  family="gaussian", alpha = 0, lambda = CVglm$lambda.min)

predictionRIDGE <- predict(GLMModel, newx = modelmatrixtest, type ="response")

predictions <- predictionRIDGE
predictions[!mergedtestdata$id %in% refined_data$id] = 0
predictions[is.na(predictions)] <- 0

SampleRIDGE$units <- predictions

write.csv(x = SampleRIDGE, file =  "SampleRIDGE.csv", row.names = FALSE)



refined_data <- read.csv("refined_data.csv")
refined_data$store_nbr <- as.factor(refined_data$store_nbr)
refined_data$item_nbr <- as.factor(refined_data$item_nbr)
refined_data$date <- as.Date(refined_data$date, "%Y-%m-%d")


SampleGLM <- sample
mergedtestdata <- read.csv("mergedtestdata.csv")
mergedtestdata$store_nbr <- as.factor(mergedtestdata$store_nbr)
mergedtestdata$item_nbr <- as.factor(mergedtestdata$item_nbr)
mergedtestdata$date <- as.Date(mergedtestdata$date, "%Y-%m-%d")

units <- refined_data$units

GLMModel<-glm(units~date+store_nbr+item_nbr+eventdist+day_of_week+weekend+month+blackFriday, family = gaussian, data = refined_data)

predictionGLM <- predict(GLMModel, mergedtestdata)
predictions <- predictionGLM
predictions[!mergedtestdata$id %in% refined_data$id] = 0

SampleGLM$units <- predictions

write.csv(x = SampleGLM, file =  "SampleGLM.csv", row.names = FALSE)








plot(reg_model)

