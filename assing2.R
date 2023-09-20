install.packages("tidyverse")
install.packages("lattice")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("readxl")
library("readxl")
library("reshape2")
library("lattice")
library("ggplot2")
library("tidyverse")

df1 <- read.delim('salaries.txt',header=TRUE)
#(a)
t.test(df1$MALES, conf.level = 0.95)
#(b)
t.test(df1$FEMALES, conf.level = 0.95)
#(c)
t.test(df1$FEMALES,df1$MALES, conf.level = 0.9)
#(d)
t.test(df1$FEMALES,df1$MALES,alternative = "less", conf.level = 0.95)
#(e)
var.test(df1$FEMALES,df1$MALES, conf.level = 0.99)
#(f)
dff<- melt(df1)
dff
t.test(dff$value~dff$variable, conf.level = 0.9)


install.packages("readxl")
library("readxl")
df2<-read_excel("inquiries.xls")
df2

install.packages("tidyverse")
install.packages("lattice")
install.packages("ggplot2")
install.packages("reshape2")
library("reshape2")
library("lattice")
library("ggplot2")
library("tidyverse")

data_long<- melt(df2, "Day")
data_long
#2.(a)
ggplot(data = data_long, aes(x=Day, y=value)) + geom_boxplot(aes(fill=variable))#https://stackoverflow.com/questions/14604439/plot-multiple-boxplot-in-one-graph

#2.(b)
fit_aov<- aov(data_long$value~factor(data_long$Day))
summary(fit_aov)#reject null hypothesis that variances is equal per day
summary(lm(fit_aov))#Interpretation of parameters Intercept discribes the friday

install.packages("gplots")
library(gplots)
plotmeans(data_long$value~factor(data_long$Day),xlab="Method", ylab="units", main="Mean plot with 95% CI")
#(c) Tukey HSD method
TukeyHSD(fit_aov)#after obesrving the pvalue=0.995>0.05 from Tukey method we realize that the difference is not statistically significant 

#(d)
fit2_aov<- aov(data_long$value~data_long$variable)
summary(fit2_aov)
summary(lm(fit2_aov))
TukeyHSD(fit2_aov)# after Tukey test we observe the difference between sections all the combinations with news differ 
#so we exclude the News paramete. the differce of News wrt Bussiness and Sports gave as pvalue 0.96 and 0.052
# so those 2 pvalues are > than 0.05 which is a significance level for the test so we have evidence that the News
#doesnt affect our model and exlude it 

#(e)
data_significant <- filter(data_long, variable != "News")#exclude the News and re-estimate the parameters of the model
data_significant
fit_aov_e<- aov(data=data_significant,value~variable)
summary(fit_aov_e)
summary(lm(fit_aov_e))#interpretation of parameters
#(f) fit aov with main effects of the model
Two_way_aov <- aov(value ~ Day*variable, data = data_long)
summary(lm(Two_way_aov))#interpretation of parameters of  two way anova 
#(g)
slm1 <- step(Two_way_aov)
summary(slm1)
slm1$coefficients
lr= lm(slm1)
TukeyHSD(slm1)

#(h)
main.model <- aov(value ~ Day+variable, data = data_long)
constant.model<-aov(data=data_long,value ~ 1)
anova(constant.model,main.model)# taking the anova of 2 input models we conclude that we have strong evidence
# that our models differ because pvalue is less than a so we fail to reject our null hypothesis that models are equal

