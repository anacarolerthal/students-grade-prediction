# get data
students <- read.csv("student-mat.csv", sep = ",")

# pivot_longer to get G1, G2, G3 in one column
students <- students %>%
  pivot_longer(cols = c("G1", "G2", "G3"), names_to = "G", values_to = "grades")

# make "G1" become 0, "G2" become 1, "G3" become 2
students$G[students$G=="G1"]<-0
students$G[students$G=="G2"]<-1
students$G[students$G=="G3"]<-2

# make "yes" become 1, "no" become 0
students$schoolsup[students$schoolsup=="yes"]<-1
students$schoolsup[students$schoolsup=="no"]<-0

# as.numeric to convert to numeric
students$G <- as.numeric(students$G)
students$schoolsup <- as.numeric(students$schoolsup)

# fit multilevel longitudinal model with lme4
model_slope3 <- lme4::lmer(grades ~ failures + schoolsup +  (0 + failures + schoolsup | G), data = students)
summary(model_slope3)

# plot using red dot
plot(model_slope3, col = "black")
plot(model_slope3)


model_slope3.rmse <- sqrt(mean(residuals(model_slope3)^2))

print(paste("RMSE:", model_slope3.rmse))
print(paste("AIC:", AIC(model_slope3)))

dados_train <- sample_frac(students, 0.8)
dados_test <- sample_frac(students, 0.2)

# fit model with train data
model_slope3 <- lme4::lmer(grades ~ failures + schoolsup +  (0 + failures + schoolsup | G), data = dados_train)

y_pred <- predict(model_slope3, dados_test)
y_true <- dados_test$grades

r2 <- r2mlm::r2mlm(model_slope3)
print(paste("R2:", r2))

rmse <- sqrt(mean((y_pred - y_true)^2))
print(paste("RMSE:", rmse))

# get MSE
mse <- mean((y_pred - y_true)^2)
print(paste("MSE:", mse))

# get R2
#r2 <- 1 - sum((y_true - y_pred)^2)/sum((y_true - mean(y_true))^2)
#print(paste("R2:", r2))

# get log-likelihood
ll <- logLik(model_slope3)
print(paste("Log-likelihood:", ll))