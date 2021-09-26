library(ROCR)
library(ggplot2)
library(Deducer)
library(GGally)
library(olsrr)
library(caret)
library(psych)

#Q1
##Part a
Q1.chosenvar <- table(StudentPerformance[,c("internet","Mjob")])
Q1_CI_calculator <- function(sel1, sel2, data){
  sel1.names <- names(table(data[,sel1]))
  print(sel1.names)
  sel2.names <- names(table(data[,sel2]))
  print(sel2.names)
  print("----------------")
  n.names1 <- length(sel1.names)
  print(n.names1)
  n.names2 <- length(sel2.names)
  print(n.names2)
  print("------------------")
  tab <- table(StudentPerformance[, c(sel1, sel2)])
  print(tab)
  CI <- c()
  NCI <- c()
  i=1
  j=1
  for(i in 1:(n.names1-1)){
    k <- (i+1)
    for(j in k:n.names1){
      n_1 <- sum(tab[sel1.names[i],])
      #print(tab[sel1.names[i],])
      p_1 <- tab[sel1.names[i],sel2.names[1]]/n_1
      n_2 <- sum(tab[sel1.names[j],])
      p_2 <- tab[sel1.names[j],sel2.names[1]]/n_2
      SE <- sqrt(p_1*(1-p_1)/n_1+p_2*(1-p_2)/n_2)
      delta.p <- p_1 - p_2
      CI <- c(CI, delta.p + c(1,-1)*pnorm(0.975, lower.tail = F)*SE) 
      NCI <- c(NCI, sel1.names[i], sel1.names[j])
    }
  }
  return(data.frame(CI, NCI))
}
Q1_CI_calculator("Mjob", "sex", StudentPerformance)

#Part b
run_pchi <- function(sel1, sel2, data){
  tab <- table(StudentPerformance[, c(sel1, sel2)])
  sel1.names <- names(table(data[,sel1]))
  print(sel1.names)
  sel2.names <- names(table(data[,sel2]))
  print(sel2.names)
  print("----------------")
  n.names1 <- length(sel1.names)
  print(n.names1)
  n.names2 <- length(sel2.names)
  print(n.names2)
  print("------------------")
  x_2 <- 0
  for(i in 1:n.names1){
    for(j in 1:n.names2){
      expected <- sum(tab[sel1.names[i],])*sum(tab[,sel2.names[j]])/sum(tab)
      x_2 <- x_2 + (tab[i,j]-expected)**2/expected
    }
  }
  DF <- (n.names2-1)*(n.names1-1)
  print("X^2: ")
  print(x_2)
  print("DF: ")
  print(DF)
  print("P-value:")
  return(pchisq(x_2, DF, lower.tail = FALSE))
}

run_pchi("Mjob", "sex", StudentPerformance)

#Q2
set.seed(1)
small.sample <- StudentPerformance[sample(nrow(StudentPerformance), 13), ]$internet
table(small.sample)
simulation.output <- c()
no.simulation <- 10000
for(i in 1:no.simulation){
  mn <- mean(sample(0:1, 13, replace = TRUE))
  simulation.output <- c(simulation.output, mn)
}
pval <- length(simulation.output[simulation.output>=10/13])/no.simulation
p<-ggplot(data.frame(simulation.output), aes(x=simulation.output)) + 
  geom_histogram(color="black", fill="lightsalmon", binwidth = .010)+
  geom_vline(aes(xintercept=10/13),
             color="orange", linetype="dashed", size=1)+
  ggtitle("Randomized distribution")+
  annotate("text", x = 11/13 , label = 'p-value =', y = 1500 , size = 3.4) +
  annotate("text", x = 0.97 , label = pval , y = 1500 , size = 3.4) +
  theme(plot.title = element_text(hjust = 0.5))
p

#Q3
#Part a

Q3.chosen.categvar <- StudentsPerformance$Fjob
N <- 100
table(Q3.chosen.categvar)
Q3.unbiasedsample <- sample(StudentsPerformance$Fjob,
                            N,
                            replace = FALSE)

Q3.biasedsample <- sample(StudentsPerformance$Fjob,
                        N,
                        prob = ifelse(Q3.chosen.categvar == "services", 0.8, 0.2))
ub.tab <- table(Q3.unbiasedsample)
b.tab <- table(Q3.biasedsample)

ub.tab
chisq.test(ub.tab,
           p = c(prop.table(table(Q3.chosen.categvar))))
b.tab
chisq.test(b.tab, 
           p = c(prop.table(table(Q3.chosen.categvar))))

#Part b
#Q3.b.sampled <- StudentPerformance[sample(nrow(StudentPerformance), N), ]
q3.tab<-table(StudentPerformance[,c("Mjob","Fjob")])
chisq.test(table(StudentPerformance[,c("Mjob","Fjob")]))
sel <- c('at_home','services','other')
chisq.test(q3.tab[,sel])
q3.tab[,sel]

#Q4
##Part b
Q4.model1 <- lm(G3~G2, data = StudentPerformance)
Q4.model2 <- lm(G3~studytime, data = StudentPerformance)
leastquare.first <- sum((Q4.model1$residuals)^2)
leastquare.second <- sum((Q4.model2$residuals)^2)

Q4.model1$coefficients
Q4.model2$coefficients

my_graph <- ggplot(StudentPerformance, aes(x = G2, y = G3)) +
  geom_point(col='orange') +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1)+
  ggtitle("G3 Vs G2 scatter plot+linear regression line")

my_graph

my_graph <- ggplot(StudentPerformance, aes(x = studytime, y = G3)) +
  geom_point(col='orange') +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1)+
  ggtitle("G3 Vs Study time scatter plot+linear regression line")

my_graph

#Part d
anova(lm(G3~G2+studytime, data = StudentPerformance))
anova(lm(G3~studytime, data = StudentPerformance))
anova(lm(G3~G2, data = StudentPerformance))
#DF=0!!!!!!:|:|:|:|:|:|?

ars.second <- 1-(1-cor(StudentPerformance$studytime, StudentPerformance$G3)^2)*(394/392)
ars.first <- 1-(1-cor(StudentPerformance$G2, StudentPerformance$G3)^2)*(394/392)

#Part f
samples <- StudentPerformance[sample(nrow(StudentPerformance), 100), ]
samples.train <- samples[1:90,]
samples.test <- samples[91:100,]
Q4.sampled.firstmodel <- lm(G3~G2, data = samples.train)
Q4.sampled.secondmodel <- lm(G3~studytime, data = samples.train)

predictsample<-function(intercept, slope, x, y){
  x.hat <- intercept + slope*x
  correct <- sum(x.hat == y)
  return(x.hat)
}
pred.G2 <- predictsample(Q4.sampled.firstmodel$coefficients[1],
              Q4.sampled.firstmodel$coefficients[2],
              samples.test$G2,
              samples.test$G3)
pred.st <- predictsample(Q4.sampled.secondmodel$coefficients[1],
              Q4.sampled.secondmodel$coefficients[2],
              samples.test$studytime,
              samples.test$G3)
actual <- samples.test$G3
pred.tb<- data.frame(pred.G2,pred.st, actual)
res.tb<- data.frame(abs(pred.G2-actual),abs(pred.st-actual))
#how to return percision????
SE <- 0.7277
m<-1.4323
c( m + c(1,-1)*pnorm(0.975, lower.tail = F)*SE)
SE <- 0.05126
m<-1.26504
c( m + c(1,-1)*pnorm(0.975, lower.tail = F)*SE)

#Q5
##Part a
featurePlot(x=temp[,1:8], y=temp[,8:16], plot="pairs")
G2 <- StudentPerformance$G2
G1 <- StudentPerformance$G1
G3 <- StudentPerformance$G3
absences <- StudentPerformance$absences
studytime <- StudentPerformance$studytime
goout <- StudentPerformance$goout
selected <- data.frame(G1,G2, absences,studytime,goout)
ggpairs(selected, title = "Correlogram")

##Part b
Q5.MLR <- lm(G3~G1+G2+absences+studytime+goout, data = StudentPerformance)
summary(Q5.MLR)

#Part e
forward.p <- ols_step_forward_p(Q5.MLR, details = TRUE)
plot(forward.p)

backward.p <- ols_step_backward_p(Q5.MLR, details = TRUE)
plot(backward.p)

Q5.MLR.enhanced <- lm(G3~G1+G2+absences+failures, data = StudentPerformance)
summary(Q5.MLR.enhanced)

#Part e
train_test <- trainControl(method = "cv", number = 5)
Q5.Partb.crossvalidation <- train(G3 ~ G1 + G2 + absences + studytime + goout,   
                     data = StudentPerformance,                        
                     trControl = train_test,             
                     method = "lm") 

Q5.Parte.crossvalidation <- train(G3 ~ G1 + G2 + absences,   
                                  data = StudentPerformance,                        
                                  trControl = train_test,             
                                  method = "lm") 


Q5.Partb.crossvalidation
Q5.Parte.crossvalidation


#Q6
##a
Response <- StudentPerformance$absences
N <- length(Response)
for (i in 1:N){
  if (Response[i]<8){
    Response[i] <- 0
  }else{
    Response[i] <- 1
  }
}
Q6.glm <- glm(Response~ G1 + G2 + G3 + failures + studytime + goout + romantic  , data = StudentPerformance )
summary(Q6.glm)

##b
df <- data.frame(boxLabels = c("romanticyes"), 
                 boxOdds = c(1.069946), 
                 boxCILow = c(0.9822706), 
                 boxCIHigh = c(1.165447))

ggplot(data = df,
       mapping = aes(y = forcats::fct_inorder(f = rev(x = boxLabels)))) +
  geom_vline(xintercept = 1) +
  geom_point(mapping = aes(x = boxOdds)) +
  geom_errorbarh(mapping = aes(xmin = boxCILow,
                               xmax = boxCIHigh)) +
  coord_trans(x = scales::exp_trans()) +
  scale_x_continuous(breaks = log(x = 0.5 * (1:10)),
                     minor_breaks = NULL,
                     labels = (0.5 * (1:10))) +
  labs(x = "Exponentiated Odds Ratio",
       y = "") +
  ggtitle("odds ratio curve for romanticyes")

py <- function(x, mdl) {
  return ((abs(summary(mdl)$coefficients[3])*x/(1-x)) / (1 + (abs(summary(mdl)$coefficients[3])*x/(1-x))))
}

Pu <- py(seq(0, 1.01, 0.01), Q6.glm)
Pe<-seq(0, 1.01, 0.01)
plot(Pu, Pe, type = "l", col = "orange") +
  abline(a=0, b=1)+
  title("OR curve")


#Part c
a<- rocplot(Q6.glm)
a$labels$title<- "Roc"
a

#Part d
summary(Q6.glm)

#Part e
Q6.glm.enhanced <- glm(Response~G2 + G3 + failures + romantic  , data = StudentPerformance )
summary(Q6.glm.enhanced)
a<- rocplot(Q6.glm.enhanced)
a$labels$title<- "Roc enhanced"
a

#Part f



#Q7
StudentPerformance <- read.csv("F:\\Semester 8\\Statistical inference\\Project\\P2\\StudentsPerformance.csv", header = TRUE)
Response <- StudentPerformance$G1 + StudentPerformance$G2 + StudentPerformance$G3
N <- length(Response)
for (i in 1:N){
  if (Response[i]<25){
    Response[i] <- 1
  }else{
    Response[i] <- 0
  }
}
Q8.glm <- glm(Response~.  , data = StudentPerformance )
summary(Q8.glm)
p.values <- coef(summary(Q8.glm))[,4]
significant.variables <- c()
column.names <- colnames(StudentPerformance)
for(i in 1:23){
  if(p.values[i]<0.05){
    significant.variables <- c(significant.variables, names(p.values)[i])
  }
}
significant.variables
Q8.glm.enhanced <- glm(Response~ failures + absences + G3, data = StudentPerformance )
summary(Q8.glm.enhanced)
fitted <- Q8.glm.enhanced$fitted.values
p <- (exp(fitted))/(1+exp(fitted))

pred <- prediction( p, Response)
perf <- performance(pred,"tpr","fpr")
plot(perf)
cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
subset.cutoff <- subset(cutoffs, fpr < 0.2)
head(subset.cutoff)
threshold <- subset.cutoff$cut[1]

predicted.labels <- c()
for(prediction in p){
  if(prediction>threshold){
    predicted.labels <- c(predicted.labels, 1)
  }else{
    predicted.labels <- c(predicted.labels, 0)
  }
}
counter<-0
for(i in 1:395){
  if(predicted.labels[i]==Response[i]){
    counter<-counter+1
  }
}
print(100*counter/395)
