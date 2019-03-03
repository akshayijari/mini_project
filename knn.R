heart <- read.csv("D:/mini_project/mini_first/heart.csv")

head(heart)

str(heart)

heart.subset <- heart[c('age','sex','cp','trestbps','chol','fbs','restecg','thalach','exang','oldpeak','slope','ca','thal','target')]
head(heart.subset)

normalize <- function(x)
            {
              return( ( x-min(x)) / ( max(x) - min(x) ))
}
summary(heart.subset)
heart.subset.n <- as.data.frame(lapply(heart.subset[,1:13],normalize))

head(heart.subset.n)
set.seed(123)
dat.d <- sample(1:nrow(heart.subset.n),size=nrow(heart.subset.n)*0.6998914224,replace = FALSE)

train.heart <- heart.subset[dat.d,]
test.heart <- heart.subset[-dat.d,]

train.heart_labels <- heart.subset[dat.d,14]
head(train.heart_labels)


test.heart_labels <- heart.subset[-dat.d,14]
head(test.heart_labels)

#install.packages('class')
library(class)

NROW(train.heart_labels)

knn.30 <- knn(train = train.heart, test=test.heart, cl=train.heart_labels, k=30)
knn.31 <- knn(train = train.heart, test=test.heart, cl=train.heart_labels, k=31)

ACC.30 <- 100*sum(test.heart_labels == knn.30)/NROW(test.heart_labels)
ACC.31 <- 100*sum(test.heart_labels == knn.31)/NROW(test.heart_labels)

ACC.30
ACC.31

table(knn.30 ,test.heart_labels)
knn.30

table(knn.31,test.heart_labels)
knn.31

library(caret)

confusionMatrix(table(knn.30 ,test.heart_labels))
value <- table(knn.30 ,test.heart_labels)
value

value[1][3]
i=1
k.optm=1
for(i in 1:35)
{
  knn.mod <- knn(train=train.heart,test=test.heart,cl=train.heart_labels,k=i)
  k.optm[i] <- 100* sum(test.heart_labels==knn.mod)/NROW(test.heart_labels)
  k=i
  cat(k,'=',k.optm[i],'\n')
  
}
plot(k.optm,type="b",xlab="k- Value",ylab="Accuracy level")

