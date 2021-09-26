library(magrittr) 
library(dplyr)
library(ggfortify)
library(ggplot2)
library(plyr)
library(gridExtra)
library(ggpubr)
require(qqplotr)
library("ggpubr")
library(moments)
library(hexbin)
library(ggmosaic)
theme_set(theme_minimal())

#Question 0
StudentPerformance <- read.csv("F:\\Semester 8\\Statistical inference\\Project\\StudentsPerformance.csv", header = TRUE)
sum(is.na(StudentPerformance))
#...............................................................................

#Question 1
selected.numerical <- StudentPerformance$G3
##Part a
bw <- 2 * IQR(selected.numerical) / length(selected.numerical)^(1/3)
selected.numerical.hist <- ggplot(as.data.frame(selected.numerical),
                                  aes(selected.numerical)) +
  geom_histogram(aes(y=..density..) ,
                 binwidth = bw,
                 alpha = 0.4)  +
  geom_density(linetype="dashed",
               alpha = 0.3,
               size=1) +
  labs(title = "Histogram for G3",
       x = "Score",
       y="Density")+
  theme(plot.title = element_text(hjust = 0.5))
selected.numerical.hist

##part b
selected.numerical.qq <- ggplot(as.data.frame(selected.numerical),
                                aes(sample = selected.numerical))+
  geom_qq()+
  geom_qq_line()+
  labs(title="QQ-plot for G3")+
  theme(plot.title = element_text(hjust = 0.5))

selected.numerical.qq

##part c
print(skewness(selected.numerical))

##part d
selected.numerical.boxplot <- ggplot(as.data.frame(StudentPerformance),
                                     aes(x = selected.numerical))+
  geom_boxplot()+
  labs(title="Boxplot for math score G3 ",
       xlab="score")+
  theme(plot.title = element_text(hjust = 0.5))

selected.numerical.boxplot

##part e
selected.numerical.mu <- mean(selected.numerical)
selected.numerical.median <- median(selected.numerical)
selected.numerical.var <- var(selected.numerical)
selected.numerical.std <- sd(selected.numerical)

##part f
selected.numerical.density <- ggplot(StudentPerformance,
                     aes(x = selected.numerical)) +
  geom_vline(xintercept = selected.numerical.mu,
             linetype="dashed",
             color = "red") +
  geom_vline(xintercept = selected.numerical.median,
             linetype="dashed",
             color = "blue") +
  geom_density(color = "green", size = 1)+
  stat_function(fun = dnorm, n = 101, args = list(mean = selected.numerical.mu,
                                                  sd = selected.numerical.std))+
  annotate("text", x = 7.5 , label = "Normal fit", y = 0.052, size = 3.4, angle = 52) +
  annotate("text", x = 7.5 , label = "Density fit", y = 0.033, size = 3.4, angle = 52, color="green") +
  annotate("text", x = selected.numerical.mu , label = "Mean", y = 0.033, size = 3.4, angle = 90, color="red") +
  annotate("text", x = selected.numerical.median , label = "Median", y = 0.033, size = 3.4, angle = 90, color="Blue") +
  labs(title="Density for G3")+
  theme(plot.title = element_text(hjust = 0.5))

selected.numerical.density


##part H
boxplot.stats(selected.numerical)
selected.numerical.boxplot


#-------------------------------------------------------------------------------
#Question 2
library(GGally)
Male <- dplyr::filter(StudentPerformance, sex=="M")
female <- dplyr::filter(StudentPerformance, sex=="F")
student.sex <- StudentPerformance$sex
##Part a
length(Male$sex)
length(female$sex)
Male.percentage <- length(Male$sex)/length(student.sex)
female.percentage <- length(female$sex)/length(student.sex)
Male.percentage
female.percentage

##Part b
data <- data.frame(
  sex = c("M", "F"),
  Value = c(Male.percentage*100, female.percentage*100)
)

sex.barplot <- ggplot(as.data.frame(StudentPerformance), aes(x = " ", color = sex, fill = sex)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha = 0.7) +
  labs(title="Stacked Barplot of sex", y = 'Frequency')+
  annotate("text", x = 1 , label = paste(toString(round(female.percentage*100, digit=3)),"%"), y = 1-female.percentage/2 , size = 3.4)+ 
  annotate("text", x = 1 , label = paste(toString(round(Male.percentage*100, digit=3)),"%"), y = Male.percentage/2 , size = 3.4) 
  

##Part c
sex.hbarplot <- ggplot(data, aes(x =sex, y=Value, color=sex, fill=sex)) + 
  geom_bar(stat="identity") +
  labs(title="hor-Barplot of sex", y = 'Frequency')+
  annotate("text", x = 1 , label = paste(toString(round(female.percentage*100, digit=3)),"%"), y = female.percentage*50 , size = 3.4)+ 
  annotate("text", x = 2 , label = paste(toString(round(Male.percentage*100, digit=3)),"%"), y = Male.percentage*50 , size = 3.4) +
  xlab("sex")+
  ylab("percentage")

sex.barplot
sex.hbarplot

##Part d
temp <- as.data.frame(StudentPerformance)
q2.violin <- ggplot(temp, aes(x=sex, y=G3, color= sex, fill= sex)) + 
  geom_violin()+
  ylab("G3 scores")+
  labs(title="Violin plot of Sex VS G3 scores")+
  theme(plot.title = element_text(hjust = 0.5))
  
q2.violin
#...............................................................................

#Question 3
numerical.first <- StudentPerformance$absences
numerical.second <- StudentPerformance$G2

##Part b
scatter.q3 <- ggplot(temp, aes(x= absences, y=G2))+
  geom_point(color="orange")+
  labs(title="scatterplot for G1 VS No.absences")+
  theme(plot.title = element_text(hjust = 0.5))
scatter.q3

##Part c
corr.q3 <- cor(numerical.first, numerical.second)
corr.q3


##part E
corr.test.q3 <- cor.test(numerical.first, numerical.second,
         alternative = "less",
         method = "pearson",
         conf.level = 0.95)

corr.test.q3

##Part F
twonum.and1cat.scatter <- ggplot(temp, aes(x= absences, y=G2, color= romantic, fill= romantic))+
  geom_point()+
  labs(title = "G2 vs absences with respect to being romantic scatter plot")+
  theme(plot.title = element_text(hjust = 0.5))

twonum.and1cat.scatter

##Part G
library(ggExtra)

q3.densigram.hex <- ggMarginal(ggplot(temp, aes(x = G2, y = absences)) +  geom_point(col="transparent")+geom_hex(bins=20), type= "densigram", margins = "both")
q3.densigram.hex
#Part H
q3.twoddenisty.hex <- ggMarginal(ggplot(temp, aes(x = G2, y = absences)) +ylim(c(-20,80))+xlim(c(0,30))+  geom_point(col="transparent")+stat_density2d(aes(fill=..level..), geom="polygon", color="pink"), type= "densigram", margins = "both")
q3.twoddenisty.hex

#.....................................................................................

#Question 4
#Part a
library(GGally)
featurePlot(x=temp[,1:8], y=temp[,8:16], plot="pairs")
ggpairs(dplyr::select_if(StudentPerformance, is.numeric), title = "Correlogram")
list.of.num <- c(4, 7, 10, 12, 13, 14, 15, 16)

#density, without failure
ggpairs(StudentPerformance[, list.of.num],
        upper = list(continuous = wrap("density", colour="blue")),
        lower = list(continuous = wrap("points", colour="red")))

#linear relationship
ggpairs(StudentPerformance[, list.of.num],
        upper = list(continuous = wrap("smooth", colour="blue")),
        lower = list(continuous = wrap("points", colour="red")))

#----
#b.
library(Hmisc)
col <- colorRampPalette(c("grey80", "white", "thistle1", "thistle2"))
StudentsPerformance.corr <- rcorr(as.matrix(dplyr::select_if(StudentPerformance, is.numeric)))
StudentsPerformance.corr.p <- StudentsPerformance.corr$P
StudentsPerformance.corr.p[is.na(StudentsPerformance.corr.p)] <- 1

M <- cor(dplyr::select_if(StudentPerformance, is.numeric))
library(corrplot)
corrplot(M, method = "color", col = col(400), type = "upper", order = "hclust", addCoef.col = "black", 
         tl.col = "blue", tl.srt = 45, p.mat = StudentsPerformance.corr.p, sig.level = 0.05, diag = FALSE)
#----

#c.


cols <- c("thistle2", "grey50")
library(scatterplot3d)
with(StudentPerformance, scatterplot3d(age, health, failures, main="3D scatterplot",
                                        pch = 16, color = cols[as.numeric(StudentPerformance$sex)]))

legend("right", legend = levels(StudentsPerformance$sex),
       col =  c("thistle2", "grey50"), pch = 16)

#...............................................................................
#Question 5
##Part a
q5.cat1 <- StudentPerformance$romantic
q5.cat2 <- StudentPerformance$sex
romantic <- StudentPerformance$romantic
sex <- StudentPerformance$sex
table <- table(romantic, q5.cat2)
print.table(table)
head(data.frame(table))
##Part b
combined.barplot.RS <- ggplot(temp, aes(x = romantic,color = sex, fill = sex)) +
  geom_bar(position = "dodge", alpha = 0.7) +
  labs(title="Romantic grouped barplot with sex", x="romantic")+
  annotate("text", x = 1.2 , label = table[1,2], y = table[1,2]+5 , size = 3.4) +
  annotate("text", x = 0.8 , label = table[1,1], y = table[1,1]+5 , size = 3.4) +
  annotate("text", x = 2.2 , label = table[2,2], y = table[2,2]+5 , size = 3.4) +
  annotate("text", x = 1.8 , label = table[2,1], y = table[2,1]+5 , size = 3.4) +
  theme(plot.title = element_text(hjust = 0.5))

combined.barplot.RS

##Part c
combined.segbarplot.RS <- ggplot(temp, aes(x = romantic,color = sex, fill = sex)) +
  geom_bar(alpha = 0.7) +
  annotate("text", x = 1 , label = table[1,2], y = table[1,2]/2 , size = 3.4) +
  annotate("text", x = 1 , label = table[1,1], y = table[1,2]+table[1,1]/2 , size = 3.4) +
  annotate("text", x = 2 , label = table[2,2], y = table[2,2]/2 , size = 3.4) +
  annotate("text", x = 2 , label = table[2,1], y = table[2,2]+table[2,1]/2 , size = 3.4) +
  labs(title="Romantic grouped barplot with sex", x="romantic")+
  theme(plot.title = element_text(hjust = 0.5))

combined.segbarplot.RS

##Part d
library(ggmosaic)
mos <- data.frame(
  sex=temp$sex,
  romantic=temp$romantic
)
mosaic.plot.RS <-  ggplot(data = mos) +
  geom_mosaic(aes( x = product(romantic, sex), fill=romantic))+
  annotate("text", x = 0.78 , label = paste(toString(round(table[1,2]/sum(table[,2]),4)*100),"%"), y = table[1,2]/sum(table[,2])/2 , size = 3.4)+
  annotate("text", x = 0.78 , label = paste(toString(round(table[2,2]/sum(table[,2]),4)*100),"%"), y = 1-table[2,2]/sum(table[,2])/2 , size = 3.4)+
  annotate("text", x = 0.28 , label = paste(toString(round(table[1,1]/sum(table[,1]),4)*100),"%"), y = table[1,1]/sum(table[,1])/2 , size = 3.4)+
  annotate("text", x = 0.28 , label = paste(toString(round(table[2,1]/sum(table[,1]),4)*100),"%"), y = 1-table[2,1]/sum(table[,1])/2 , size = 3.4)+
  labs(title="Mozais plot for Romantic VS sex")+
  theme(plot.title = element_text(hjust = 0.5))

mosaic.plot.RS
#...............................................................................
#Question 6
calculate_ci <- function(sampled_data, confidence_level) {
  sample_mean <- mean(sampled_data)
  stdDev <- sd(sampled_data)
  
  z_value <- qnorm((1 + confidence_level)/2)
  stdError <- stdDev / sqrt(length(sampled_data))
  CI <- c(sample_mean - z_value * stdError, sample_mean + z_value * stdError)
  return(CI)
}
#Part a
sampled_data <- sample(StudentPerformance$age, 100)
age.CI <- calculate_ci(sampled_data, 0.95)

#Part c
selected.numerical <- StudentPerformance$age
bwidth <- 2 * IQR(selected.numerical) / length(selected.numerical)^(1/3)
q6.age.hist <- ggplot(StudentPerformance, aes(x = age)) +
  geom_histogram(binwidth = 0.9, alpha = 0.4, color="lightsteelblue2", fill="lightsteelblue1") +
  labs(title = "Age Histogram", x = "Age") +
  geom_vline(xintercept =  mean(StudentPerformance$age), color = "Green") +
  geom_vline(xintercept =  age.CI[1], color = "red") +
  geom_vline(xintercept =  age.CI[2], color = "red")+
  theme(plot.title = element_text(hjust = 0.5))


q6.age.hist

#Part d
zdist.2tail.meantest <- function(sampled_data, null_value, alpha) {
  n <- length(sampled_data)
  x_bar <- mean(sampled_data)
  S <- sd(sampled_data)
  z_score <- abs((x_bar - null_value)) / (S/sqrt(n))
  p_value <- pnorm(z_score, lower.tail = FALSE)
  print(paste("p-value =", p_value))
}
zdist.2tail.meantest(sampled_data, 16 , 0.05)

#Part f
null.value <- 16
z_value <- abs(qnorm((1-0.05)/2))
errorTypeII <- pnorm(null.value + z_value * sd(sampled_data)/sqrt(length(sampled_data)) - mean(sampled_data))

#part g
power <- 1 - errorTypeII

#...............................................................................
#Question 7
##Part a
StudentsPerformance.sampled <- sample_n(StudentPerformance, 25)
x_bar <- mean(StudentsPerformance.sampled$G2) - mean(StudentsPerformance.sampled$G3)
s1 <- sd(StudentsPerformance.sampled$G2)
s2 <- sd(StudentsPerformance.sampled$G3)
s <- abs((x_bar - 0)) / (sqrt((s1^2/25) + (s2^2/25)))
pvalue <- 2*pt(s, df = 24, lower.tail = FALSE)
print(paste("p-value =", pvalue))


##Part b
G2.sample <- sample(StudentPerformance$G2, 100)
G3.sample <- sample(StudentPerformance$G3, 100)
zdist.2tail.meantest(G2.sample - G3.sample, 0, 0.05)

#...............................................................................
#Question 8
library(bootstrap)
##Part a
q8.chosen.numerical <- StudentPerformance$G2
boxplot(q8.chosen.numerical)

mean.CI <- c()
for(i in 1:30){
  bootsamp <- sample(q8.chosen.numerical, 100) 
  mean.CI <- c(mean.CI, mean(bootsamp)) 
}
q8.mean.Pa.CI <- quantile(mean.CI, c(0.025,0.975))

##Part b
get_mean <- function(x){
  mean(x)
}

boot.q8.mean <- bootstrap(x=q8.chosen.numerical, nboot=20, get_mean)
temp <- boot.q8.mean$thetastar
se <- sd(temp)
mu <- mean(temp)
t_s <- qt(0.975, df=19)
q8.mean.Pb.CI <- c(mu-t_s*se, mu+t_s*se)
q8.mean.Pb.CI <- quantile(boot.q8.mean$thetastar, c(0.025,0.975))

#...............................................................................
#Question 9
##Part a
average.grades <- (StudentPerformance$G1 + StudentPerformance$G2 + StudentPerformance$G3)/3
q9.data.frame <- data.frame(
  avg.grades = average.grades,
  failure = StudentPerformance$failures
)
q9.scatterplot <-  ggplot(q9.data.frame, aes(x = avg.grades, y= failure))+geom_point()
q9.scatterplot

avg.f0 <- dplyr::filter(q9.data.frame, failure==0)
avg.f1 <- dplyr::filter(q9.data.frame, failure==1)
avg.f2 <- dplyr::filter(q9.data.frame, failure==2)
avg.f3 <- dplyr::filter(q9.data.frame, failure==3)
ANOVA.avg_failure <- aov(q9.data.frame$avg.grades~as.factor(q9.data.frame$failure))
summary.ANOVA <- summary(ANOVA.avg_failure)
avg_failure.tukeyHSD <- TukeyHSD(ANOVA.avg_failure,
                          ordered = FALSE,
                          conf.level = 0.95)
avg_failure.tukeyHSD
plot(avg_failure.tukeyHSD, las = 1)
