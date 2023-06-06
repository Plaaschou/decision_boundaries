library(caret)
library(dplyr)
library(ggplot2)

#Set a seed for replication
set.seed(57)

#import dataset 
df <- read.csv("winequality-red.csv") 
colnames(df) <- make.names(colnames(df)) #standardize feature names
df$quality <- (df$quality >= 6) |> factor(levels=c("TRUE","FALSE")) #quality feature is replaced with logical (factor)

#Partition dataset into training and testing sets
train.index <- createDataPartition(df$quality,list=F) 
train.set <- df[train.index,]
test.set <- df[-train.index,]

#Train the model using naive bayes classifier
model <- train(data = train.set,
               quality ~.,
               method="nb")

# summary(model$finalModel) 
predict(model,test.set)-> predictions #Vector of prediction using probability threshold of 0.5
predict(model,test.set,type="prob")[,'TRUE']-> proba #return probabilities p(x) instead of using 0.5
confusionMatrix(predictions,test.set$quality,positive="TRUE") #confusion matrix with p>=0.5

#set up a function to record metrics for different probability thresholds x
f <- function(x){
  CM <- confusionMatrix(factor(proba >= x, levels=c("TRUE","FALSE")),
                        reference=test.set$quality)
  c(X = x,
    CM$overall['Accuracy'],
    CM$byClass[c('Sensitivity','Specificity','Precision','F1')])
}
metrics <- lapply(0:1000/1000,f) |> do.call(what=rbind) |> data.frame()

#retrieve highest proba threshold such that precision is greater than 0.8
metrics[which.max(metrics$Precision >= 0.8)+1,]
CM(0.277)

#Retrieve proba threshold with highest accuracy and compare with 0.5
metrics[c(501,which.max(metrics$Accuracy)),c('X','Accuracy')]

#Return metrics for probability threshold x
CM <- function(x) {
  confusionMatrix(factor(predict(model,test.set,type="prob")[,'TRUE'] >= x,levels=c("TRUE","FALSE")),
                  reference = test.set$quality)
}

#Compute F_0.5 and review associated metrics
metrics |> mutate(F05 = (1+0.5^2)*Sensitivity*Precision/(0.5^2*Precision + Sensitivity)) -> metrics
metrics[which.max(metrics$F05),]$X -> p_F05
CM(p_F05)

#Compute F2 and review associated metrics
metrics |> mutate(F2 = (1+2^2)*Sensitivity*Precision/(2^2*Precision + Sensitivity)) -> metrics 
metrics[which.max(metrics$F2),]$X -> p_F2
CM(p_F2)
CM(0.5)

#Retrieve proba threshold with highest F1 score
metrics[which.max(metrics$F1),][['X']] -> p
CM(p)

#Plot metrics against probability thresholds.
metrics |> ggplot() + 
  geom_line(aes(X,Sensitivity,color="TPR")) +
  geom_line(aes(X,1-Specificity,color="FPR")) + 
  geom_line(aes(X,Accuracy,color="ACC")) + 
  geom_line(aes(X,Precision,color="PPV")) + 
  # geom_line(aes(X,F1,color="F1")) + 
  # geom_line(aes(X,F05,color="F0.5")) + 
  # geom_line(aes(X,F2,color="F2")) + 
  labs(x="probability threshold",
       y=NULL,
       color="Metric") +
  theme_minimal() -> wine_quality_metrics
ggsave("../images/wine_quality_metrics_vg.pdf",
       wine_quality_metrics,
       height = 8,
       width = 14,
       unit = "cm")

#Generate ROC FPR->TPR plot
metrics[order(metrics$Specificity,metrics$Sensitivity),] |>
  mutate(D = duplicated(Specificity)) |>
  filter(!D) |> ggplot() + 
  geom_line(aes(1-Specificity,Sensitivity)) + 
  geom_function(fun=function(x) x,linetype="dashed") +
  labs(x="False Positive Rate") +
  theme_minimal() -> ROC_FPR_TPR
ggsave("../images/roc1_vg.pdf",
       ROC_FPR_TPR,
       height=8,
       width=8,
       unit="cm")
