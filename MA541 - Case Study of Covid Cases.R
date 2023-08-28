

data <- read.csv("/Users/kislaynandan/Downloads/COVID-19_Daily_Cases__Deaths__and_Hospitalizations.csv")


str(data)

# 4 Analysis and Result



dim(data)



attach(data)

options(max.print=2000)
duplicated(data)

is.null(data)



library(ggplot2)
library(dplyr)

data$Date <- as.Date(data$Date, format = "%m/%d/%y")


# Figure 1 - Total Cases
ggplot(data, aes(x = Date, y = `Cases...Total`)) +
  geom_line() +
  labs(title = "Total Cases over Time",
       x = "Date",
       y = "Total Cases")

# Figure 2 - Total Deaths
ggplot(data, aes(x = Date, y = `Deaths...Total`)) +
  geom_line() +
  labs(title = "Total Deaths over Time",
       x = "Date",
       y = "Total Deaths")


# Figure 3 - Total Hospitalizations
ggplot(data, aes(x = Date, y = `Hospitalizations...Total`)) +
  geom_line() +
  labs(title = "Total Hospitalizations over Time",
       x = "Date",
       y = "Total Hospitalizations")


# 4.1 Z Test


females <- data$`Cases...Female`
males <- data$`Cases...Male`

mean_females <- mean(females)
mean_males <- mean(males)
sd_females <- sd(females)/sqrt(length(females))
sd_males <- sd(males)/sqrt(length(males))

library(BSDA)

z.test(females, males, alternative = "two.sided", sigma.x = 16.37, sigma.y = 13.49)

# 4.2 Linear Regression


model2 <- lm(`Cases...Total` ~ `Cases...Age.0.17` + `Cases...Age.18.29` + 
               `Cases...Age.30.39` + `Cases...Age.40.49` + `Cases...Age.50.59` + 
               `Cases...Age.60.69` + `Cases...Age.70.79` + `Cases....Age.80.`,
             data = data)
summary(model2)

plot(model2)


# 4.3 ANOVA Test

model <- aov(`Cases...Total` ~ `Cases...Age.0.17` + `Cases...Age.18.29` + 
               `Cases...Age.30.39` + `Cases...Age.40.49` + `Cases...Age.50.59` + 
               `Cases...Age.60.69` + `Cases...Age.70.79` + `Cases....Age.80.`, data=data)

summary(model)

# 4.4 Analysis of Categorical Data


library(Matrix)
datMat <- sparseMatrix(i = rep(1:length(data$Cases...Latinx), 6),
                       j = rep(1:6, each = length(data$Cases...Latinx)),
                       x = c(data$Cases...Latinx, 
                             data$Cases...Asian.Non.Latinx, 
                             data$Cases...Black.Non.Latinx, 
                             data$Cases...White.Non.Latinx,
                             data$Cases...Other.Race.Non.Latinx, 
                             data$Cases...Unknown.Race.Ethnicity))

rownames(datMat) <- names(data$Cases...Latinx)
colnames(datMat) <- c("Latinx", "Asian.Non.Latinx", "Black.Non.Latinx", 
                      "White.Non.Latinx", "Other.Race.Non.Latinx", 
                      "Unknown.Race.Ethnicity")

tableMat <- t(datMat) %*% datMat

chisq.test(tableMat)

a <- cor(data[c("Cases...Latinx", "Cases...Asian.Non.Latinx", "Cases...Black.Non.Latinx",
                "Cases...White.Non.Latinx", "Cases...Other.Race.Non.Latinx",
                "Cases...Unknown.Race.Ethnicity")])

heatmap(a)

# 4.5 Resampling



library(dplyr)

bootstrap_mean_cases <- function(data) {
  sampled_data <- sample_n(data, nrow(data), replace = TRUE)
  selected_cols <- select(sampled_data, matches("^Cases\\.\\.\\.Age\\.|^Cases\\....Age\\."))
  mean_cases <- mean(unlist(selected_cols))
  return(mean_cases)
}

set.seed(123)
bootstrap_samples <- replicate(1000, bootstrap_mean_cases(data))
quantile(bootstrap_samples, c(0.025, 0.975))

# 4.6 Linear Model Selection and Regularization

library(glmnet)
x <- model.matrix(`Cases...Total` ~ `Cases...Age.0.17` + `Cases...Age.18.29` + 
                    `Cases...Age.30.39` + `Cases...Age.40.49` + `Cases...Age.50.59` + 
                    `Cases...Age.60.69` + `Cases...Age.70.79` + `Cases....Age.80.`,
                  data = data)[,-1]
y <- data$`Cases...Total`

cv.ridge <- cv.glmnet(x = x, y = y, alpha = 0, 
                      lambda = seq(0, 1, by = 0.01), 
                      type.measure = "mse", 
                      nfolds = 10)

cv.ridge

best_lambda <- cv.ridge$lambda.min
ridge.model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
summary(ridge.model)

ridge.model

# 4.7 Moving Beyond Linearity

library(splines)
library(mgcv)
f <- Cases...Total ~ ns(`Cases...Age.0.17`) + ns(`Cases...Age.18.29`) + 
  ns(`Cases...Age.30.39`) + ns(`Cases...Age.40.49`) +     ns(`Cases...Age.50.59`) + 
  ns(`Cases...Age.60.69`) + ns(`Cases...Age.70.79`) + ns(`Cases....Age.80.`) + ns(`Cases...Age.Unknown`)


model <- gam(f, data = data)
summary(model)


