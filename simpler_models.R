students <- read.csv("student-mat.csv", sep = ",")

# create column GM with G1, G2, G3 mean
students$GM <- (students$G1 + students$G2 + students$G3)/3

# make "yes" become 1, "no" become 0
#students$schoolsup[students$schoolsup=="yes"]<-1
#students$schoolsup[students$schoolsup=="no"]<-0
#students$schoolsup <- as.numeric(students$schoolsup)

linear_model <- lm(GM ~ failures + schoolsup, data = students)
summary(linear_model)
print(paste("AIC:", AIC(linear_model)))

linear_model.rmse <- sqrt(mean(residuals(linear_model)^2))
print(paste("RMSE:", linear_model.rmse))


GGally::ggpairs(students, columns = 31:33,
        lower = list(continuous = "smooth"))
