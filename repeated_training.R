library(dplyr)
library(caret)
library(ggplot2)
library(rpart.plot)

set.seed(55) #set seed for replication

df <- read.csv("winequality-red.csv")
df$quality <- (df$quality >= 6) |> factor(levels=c("TRUE","FALSE"))

ctrl <- trainControl(method="cv",
                     number=10)

#set up function to retrain the model using different data partitions
g <- function(){
  train.index <- createDataPartition(df$quality,list=F)
  train.set <- df[train.index,]
  test.set <- df[-train.index,]
  model <- train(data=train.set,
                 quality~ alcohol:sulphates, #only using the alcohol and sulphates features, 
                 method="glm", #logistic regression
                 trControl = ctrl)
  #Generate list of couple real class vs probabilty
  output <- data.frame(real.class = test.set$quality,
                       proba = predict(model,test.set,type="prob")[,'TRUE'])
  
  #function to compute metrics
  f <- function(x){
    prediction <- (output$proba >= x) |> factor(levels=c("TRUE","FALSE"))
    CM <- confusionMatrix(prediction,
                          reference=test.set$quality)
    CMF <-  confusionMatrix(prediction,
                            reference=test.set$quality,
                            positive="FALSE")
    
    #return a dataframe with the relevant metrics
    c(X=x,
      CM$overall['Accuracy'],
      CM$byClass[c('Sensitivity','Precision','Prevalence')],
      Neg = CMF$byClass[c('Sensitivity','Prevalence')])
  }
  lapply(0:100/100,f) |> do.call(what=rbind) |> data.frame() -> result
  result[which.max(result$Accuracy),]$X -> max.acc
  result[result$X==0.5,]$Accuracy -> acc.50
  delta=max(result$Accuracy) - acc.50
  c(ACC=max(result$Accuracy),
            MACC=max.acc,
            DACC=delta)
}
replicate(200,g()) -> max.acc2
# pdf("../images/montecarlo_accuracy.pdf")
# max.acc |> table() |> plot()
# dev.off()

max.acc2 |> t() |> data.frame() -> PL
PL |> select(MACC) |> table() |> data.frame()
PL |> ggplot() +
  theme_minimal() + 
  geom_boxplot(aes(DACC)) + 
  xlab("Max Accuracy - Accuracy with 0.5 probability threshold") -> boxp
ggsave("../images/boxplot_delta.pdf",boxp)


PL[,'MACC'] |> table() |> data.frame() |> 
  ggplot() + 
  theme_minimal() + 
  # theme(axix.text.x = element_text(angle=45))+
  geom_bar(aes(Var1,Freq),stat="identity") +
  labs(x="optimal threshold",
       y="Frequency") -> plot_montecarlo

ggsave("../images/montecarlo_glm_freq.pdf", height=6,width=12,unit="cm")
PL

PL |> group_by(MACC) |> summarize(mean(DACC),
                                  var(DACC))
