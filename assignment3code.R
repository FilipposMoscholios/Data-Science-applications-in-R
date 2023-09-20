#install some usefull packages and libraries
install.packages("gtools")
install.packages("nortest")
library("nortest")
library("stringr")
library("gtools")
df = read.csv("Assignment_3_Data.csv", sep = ';')

# preprocessing our data
df$Y = str_replace(df$Y,"," , ".")
df$Y = as.numeric(df$Y)
df$X1 = str_replace(df$X1,"," , ".")
df$X1 = as.numeric(df$X1)
df$X2 = str_replace(df$X2,"," , ".")
df$X2 = as.numeric(df$X2)
df$X3 = str_replace(df$X3,"," , ".")
df$X3 = as.numeric(df$X3)
df$W = as.factor(df$W)

y_aov <- aov(Y ~ W, data = df)
x1_aov <- aov(X1 ~ W, data = df)
x2_aov <- aov(X2 ~ W, data = df)
x3_aov <- aov(X3 ~ W, data = df)

# provide a graphical representation of each of the continuous versus the categorical variable
par(mfrow=c(2,2)) 
boxplot(data=df, Y~W)
boxplot(data=df, X1~W)
boxplot(data=df, X2~W)
boxplot(data=df, X3~W)

# Output of each anova
summary(y_aov)
summary(x1_aov)
summary(x2_aov)
summary(x3_aov)

# Examine Homogeneity of variances
par(mfrow=c(2,2)) 
plot(y_aov, 1)
plot(x1_aov, 1)
plot(x2_aov, 1)
plot(x3_aov, 1)

bartlett.test(Y ~ W, data = df)
bartlett.test(X1 ~ W, data = df)
bartlett.test(X2 ~ W, data = df)
bartlett.test(X3 ~ W, data = df)

# Examine  Normality assumptions
par(mfrow=c(2,2)) 
plot(y_aov, 2)
plot(x1_aov, 2)
plot(x2_aov, 2)
plot(x3_aov, 2)

aov_residuals1 <- residuals(object = y_aov)
aov_residuals2 <- residuals(object = x1_aov)
aov_residuals3 <- residuals(object = x2_aov)
aov_residuals4 <- residuals(object = x3_aov)

shapiro.test(x = aov_residuals1)
shapiro.test(x = aov_residuals2)
shapiro.test(x = aov_residuals3)
shapiro.test(x = aov_residuals4)

# Scatter-plot matrix
colors <- c("red", "green", "blue")
pairs(df[,1:4], pch = 19,  cex = 1.5, col = colors[df$W])

# linear regression model Y~X1
model1 = lm(Y~X1, data = df)
coefficients(model1) 

# linear regression of Y on all the other variables including interactions
model_all <- lm(Y ~ X1 + X2 + X3 + W + W*X1 + W*X2 + W*X3, data = df)
summary(model_all)

# provide plots for examine model assumptions
par(mfrow=c(2,2)) 
plot(model_all)

# Stepwise Regression method
const_model<-lm(Y ~ 1,data = df)
step_RM<-step(const_model, scope=list(lower = ~ 1,upper= ~ X1 + X2 + X3 + W +W*X1 + W*X2 + W*X3),direction="both", data=df)


# final model
final_model <- lm(Y ~ X1 + X2 + W + W*X1 + W*X2 , data = df)
summary(final_model)

# Construct the z variable using quantcut function and provide the contigency table between z and w
z <- quantcut(df$X3, q = 4, na.rm = TRUE, c("25%_X3","50%_X3","75%_X3","100%_X3"))
table(z,df$W)

# run two way anova for y with main effects w+z, provide the coefficients of the model and examine the assumptions
anova_two_way <- aov(Y ~ W + z, data = df)
summary(anova_two_way)
coefficients(anova_two_way)

par(mfrow=c(2,2)) 
plot(anova_two_way)
two_way_model <- lm(anova_two_way)
two_way_residuals <- residuals(object = two_way_model)
shapiro.test(two_way_residuals)
lillie.test(two_way_residuals)

