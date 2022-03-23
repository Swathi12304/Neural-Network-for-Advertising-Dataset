require(neuralnet)
library(ggplot2) 
library(neuralnet)
data <- read.csv('C:/Users/HP/OneDrive/Desktop/advertising.csv', header=TRUE)
attach(data)
str(data)



#Plotting the data
ggplot(data, aes(Sales)) + geom_histogram(binwidth=4, colour="black", fill="green") +
  labs(x= "TV",y= "Newspaper" , title = "Sales")


ggplot(data, aes(Sales) ) + geom_bar(aes(fill = as.factor(Sales))) + 
  scale_fill_discrete(name="Sales",
                      labels=c( "Newspaper","TV", "Radio")) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x= "TV",y= "Newspaper" , title = "Sales")

ggplot(data, aes(x=TV, y=Newspaper)) + geom_line() +
  theme_classic() +
  labs(title = "Sales", x= "TV", y= "Newspaper") +
  theme(plot.title = element_text(hjust = 0.5))

scaleddata<-scale(data)
normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
maxmindata<-as.data.frame(lapply(data,normalize))

trainset<- maxmindata[1:150, ]
testset<- maxmindata[151:200, ]

library(neuralnet)
nn <- neuralnet(Sales~Newspaper+Radio+TV, data = trainset, hidden=c(2,1), linear.output = FALSE, threshold = 0.01)
nn$result.matrix
plot(nn)

nn$result.matrix

temp_test<- subset(testset, select = c("Newspaper", "Radio", "TV"))
head(temp_test)
nn.results<- compute(nn,temp_test)
results<- data.frame(actual=testset$Sales, prediction= nn.results$net.result)

results


nn1 = neuralnet(Sales~Newspaper+Radio+TV, data = data, hidden=c(2,1), act.fct="logistic", linear.output=FALSE)
plot(nn1)

nn2_backprop1<- neuralnet(Sales~Newspaper+Radio+TV, data = data, hidden=c(2,1), algorithm = "backprop", learningrate = 0.0001)
plot(nn2_backprop1)

nn3= neuralnet(Sales~Newspaper+Radio+TV, data = data, hidden=c(2,1), act.fct = "logistic", linear.output = FALSE)
plot(nn3)
