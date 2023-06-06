library(caret)
library(dplyr)
library(ggplot2)

set.seed(57)

defaultW <- getOption("warn") 
options(warn = -1) 

df <- read.csv("winequality-red.csv")
colnames(df) <- make.names(colnames(df))
df$quality <- (df$quality >= 7) |> factor(levels=c("TRUE","FALSE"))
ctrl <- trainControl(method="boot",number=10)

train.index <- createDataPartition(df$quality,list=F)
train.set <- df[train.index,]
test.set <- df[-train.index,]

model <- train(data = train.set,
               quality ~.,
               method="glm",
               trControl = ctrl)

# summary(model$finalModel) 
predict(model,test.set)-> predictions
predict(model,test.set,type="prob")[,'TRUE']-> proba
confusionMatrix(predictions,test.set$quality,positive="TRUE")

f <- function(x){
  CM <- confusionMatrix(factor(proba >= x, levels=c("TRUE","FALSE")),
                        reference=test.set$quality)
  c(X = x,
    CM$overall['Accuracy'],
    CM$byClass[c('Sensitivity','Specificity','Precision','F1')])
}
metrics <- lapply(0:1000/1000,f) |> do.call(what=rbind) |> data.frame()




metrics[which.max(metrics$Precision >= 0.8)+1,]
CM(0.277)

metrics[c(501,which.max(metrics$Accuracy)),c('X','Accuracy')]

CM <- function(x) {
  confusionMatrix(factor(predict(model,test.set,type="prob")[,'TRUE'] >= x,levels=c("TRUE","FALSE")),
                  reference = test.set$quality)
}

metrics |> mutate(F05 = (1+0.5^2)*Sensitivity*Precision/(0.5^2*Precision + Sensitivity)) -> metrics
metrics[which.max(metrics$F05),]$X -> p_F05
CM(p_F05)


metrics |> mutate(F2 = (1+2^2)*Sensitivity*Precision/(2^2*Precision + Sensitivity)) -> metrics 
metrics[which.max(metrics$F2),]$X -> p_F2
CM(p_F2)
CM(0.5)

metrics[which.max(metrics$F1),][['X']] -> p

CM(p)

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
# metrics |> ggplot() + 
#   geom_line(aes(Precision,Sensitivity)) + 
#   labs(x="False Positive Rate") +
#   theme_minimal() -> ROC_PREC_TPR
# ggsave("../images/roc2.pdf",
#        ROC_PREC_TPR,
#        height=8,
#        width=8,
#        unit="cm")
