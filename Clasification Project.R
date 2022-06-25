setwd("D:/A KULIAH KVN/6. BA KMMI/project")
datap=read.csv("D:/A KULIAH KVN/6. BA KMMI/project/Dataset_3.csv",dec=".")

library(ggplot2)
library(dplyr)
datap$Penilaian=as.numeric(datap$Penilaian)
datap$Ulasan=as.numeric(datap$Ulasan)
datap$Installs=gsub("+","",as.character(datap$Ukuran))
datap$Installs=as.numeric(datap$Installs)
datap$Harga=gsub("$","",as.character(datap$Harga))
datap$Harga=as.numeric(gsub("\\$","",datap$Harga))
harga=datap$Harga 
harga[3581]
library(tidyverse)
library(stringr)
mega_bytes <- as.numeric(str_remove_all(datap$Ukuran, "M"))  
mega_bytes[is.na(mega_bytes)] <- 0
kilo_bytes <- as.numeric(str_remove_all(datap$Ukuran, "k")) / 1000
kilo_bytes[is.na(kilo_bytes)] <- 0
size_bytes <- kilo_bytes + mega_bytes
size_bytes[size_bytes==0] <- NA
summary(size_bytes) 
datap$Ukuran <- size_bytes
datap$Ukuran=as.numeric(datap$Ukuran)

#statdes dan check missing value
summary(datap)
sum(is.na(datap))
#mengatasi missing value dgn median 
datap$Penilaian[is.na(datap$Penilaian)]=median(datap$Penilaian,na.rm=TRUE)
datap$Ulasan[is.na(datap$Ulasan)]=median(datap$Ulasan,na.rm=TRUE)
datap$Harga[is.na(datap$Harga)]=median(datap$Harga,na.rm=TRUE)
datap$Ukuran[is.na(datap$Ukuran)]=median(datap$Ukuran,na.rm=TRUE)
datap$Installs[is.na(datap$Installs)]=median(datap$Installs,na.rm=TRUE)

sum(is.na(datap))
summary(datap)
str(datap)
datap <- subset(datap, Kategori != "1.9")

#EDA
datap$Tipe[datap$Tipe=="0"]<-NA
datap$Tipe[datap$Tipe=="NaN"] <- NA
a =datap %>% select(Tipe, Installs,Kategori) %>% filter(Tipe!=c("NA")) %>%
  group_by(Kategori) %>% arrange(Kategori)
ggplot(a, aes(x=Tipe, y=Installs, fill=Tipe))+
  geom_bar(stat="identity")+
  labs(x="Tipe",y="Installs",fill="Types",title="Installations berdasarkan Tipe App")+
  theme(legend.position = "None",axis.text.y = element_text(angle = 90))

b=datap%>%select(Penilaian, Kategori, Tipe)%>% filter(Kategori!="1.9")
ggplot(b, aes(x=Kategori, y=Penilaian, fill = Tipe)) +
  geom_bar(position='dodge',stat='identity') +
  coord_flip()+
  ggtitle("Jumlah Penilaian berdasarkan Kategori and Tipe")

#histogram Penilaian
med = median(subset(datap$Penilaian, datap$Penilaian >= 0.01))
mean = mean(subset(datap$Penilaian, datap$Penilaian >= 0.01))
ggplot(aes(x =Penilaian), data = datap )+
geom_histogram(aes(y=..density..),colour="black", fill="white")+
  geom_density(alpha=0.1, fill="red")+
  ggtitle('Histogram Penilaian')+
  geom_vline(xintercept = med, col = 'red')+
  geom_vline(xintercept = mean, col = 'blue')

#Jumlah Content Rating
c=datap%>%select(Penilaian_Konten)%>% filter(Penilaian_Konten!="")
ggplot(c, aes(x = Penilaian_Konten))+
  geom_bar(fill = 'green')+
  coord_flip()+
  scale_y_log10()+
  ggtitle('Penilaian Konten')

#Ukuran
d=datap
ggplot(d,aes(x = round(Ukuran)))+
  geom_histogram(fun.y = count, geom ='line', fill = '#31965D')+
  geom_vline(xintercept = median(subset(datap,!is.na(d$Ukuran))$Ukuran), col = 'red')+
  geom_vline(xintercept = mean(subset(datap,!is.na(d$Ukuran))$Ukuran), col = 'yellow')+
  ggtitle('Jumlah App berdasarkan Ukuran')+scale_y_log10()+
  xlab('Ukuran (Mb)')

#jumlah berdasarkan genre
topgenres = group_by(datap, Genres)%>%
  summarise(n = n())%>%
  arrange(desc(n))
topgenres = head(topgenres,15)
m= datap$Genres %in% topgenres$Genres
topgenres = datap[m,]
ggplot(aes(x = Genres), data = topgenres)+
  geom_bar(fill = 'tomato')+
  coord_flip()+
  ggtitle('Top 15 Genre')

#Jumlah Aplikasi terbanyak berdasar kategori
temp1 <- as.data.frame(table(datap$Kategori))
g1 <- ggplot(temp1, mapping = aes(x=reorder(Var1,Freq), y=Freq, fill=Freq))+
  geom_col()+
  scale_fill_gradient(high = "#363fe6",low = "#54C7EF" )+
  coord_flip()+
  geom_text(aes(label = temp1$Freq),nudge_y = 60,col = "#040b5b")+
  geom_hline(yintercept = mean(temp1$Freq), linetype = 5, col = "Red")+
  labs(title="Jumlah Aplikasi Google Play Store Berdasarkan Kategori",subtitle = "Aplikasi berkategori Family merupakan yang terbanyak beredar di Google Play",
       x="Kategori", y="Jumlah Aplikasi")+
  theme(legend.position = "right",panel.grid.major.y = element_blank())
g1


#Instal vs Harga
ggplot(aes(x = Installs, y = Harga), data = subset(datap, Tipe == 'Paid'))+
  geom_jitter(alpha = 1, , color = 'tomato')+
  coord_flip(ylim = c(0,50))+
  ggtitle('Installs vs. Harga')
#The cheaper app more likely it will be downloaded and installed.

#kategory vs konten rating
e=subset(datap, Kategori != '1.9')
ggplot(e,aes(x = Kategori))+
  geom_bar(aes(fill = Penilaian_Konten))+
  coord_flip()+
  scale_y_log10()+
  ggtitle('Kategori vs Penilaian Konten')

#subsetting for Type
paidapp <- subset(datap, Tipe == "Paid")
#group the apps so we can get the mean, median and number of the price
paidappgroup <- paidapp%>%
  group_by(Kategori)%>%
  summarise(mean_Price = mean(Harga), n = n(), median_Harga = median(Harga))
#Kategori vs. Harga Rata-rata
ggplot(aes(x =Kategori, y =mean_Price ), data = paidappgroup)+
  geom_bar(stat = 'identity', position = 'dodge', fill = 'tomato')+
  coord_flip()+ 
  geom_text(aes(label = n), hjust=-0.17,size=3.5)+
  ggtitle('Kategori vs. Harga Rata-rata')

#Category vs price
ggplot(aes(y = Harga, x = Kategori), data = paidapp)+
  geom_point(alpha = 0.2, color = 'tomato')+
  coord_flip()+
  scale_y_log10()+
  ggtitle('Harga vs Kategori')

#top 10 paid
f=datap%>% filter(Tipe == "Paid") %>%
  group_by(Kategori) %>% summarize(total_Installs = sum(Installs)) %>%
  arrange(desc(total_Installs)) %>% head(10) %>% 
  ggplot(aes(x = Kategori, y = total_Installs)) +
  geom_bar(stat="identity", width=.5, fill="#91D3B4") + labs(title= "Top10 Kategori Berbayar" ) +
  coord_flip()
f

#boxplot
g <- datap[-which(datap$Kategori=='1.9'),] 
ggplot(datap, aes(x=Tipe,y=Penilaian)) +
  geom_boxplot(fill="lavender") +
  ggtitle("Boxplot Penilaian Aplikasi") +
  ylab("Penilaian")+xlab("")

library(highcharter)
hcboxplot(x = datap$Ukuran, var = datap$Tipe, yAxis.bottom = 0, outliers = TRUE, color = "#fb4901", fillColor = "lightblue") %>%
  hc_chart(type = "column") %>%
  hc_add_theme(hc_theme_ffx()) %>%
  hc_title(text = "Ukuran aplikasi (MB) berdasarkan Jenis Aplikasi")

z=read.csv("D:/A KULIAH KVN/6. BA KMMI/project/Dataset_3.csv",dec=".")
j = subset(z, z$Installs != 'Free')
#plotting a bar graph for level of installs.
ggplot(aes(x = Installs), data = j )+
  geom_bar(fill = 'tomato')+
  coord_flip()+
  ggtitle('Installs')
          


#coreleation plot
databaru=datap
databaru$Kategori=as.numeric(databaru$Kategori)
databaru$Tipe=as.numeric(databaru$Tipe)
databaru$Genres=as.numeric(databaru$Genres)
df<-datap %>% select (`Penilaian`,'Ulasan','Ukuran','Installs','Harga')
df<-na.omit(df)
M<-cor(df)
library(corrplot)
library(RColorBrewer)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method = "color", col = col(200),
         type = "lower", order = "hclust", number.cex = .7,
         addCoef.col = "black", 
         tl.col = "black", tl.srt = 30,
         sig.level = 0.01, insig = "blank", 
         diag = FALSE)
M
head(databaru)
chisq.test(datap$Kategori, datap$Penilaian_Konten, correct=FALSE)
chisq.test(datap$Genres, datap$Penilaian_Konten, correct=FALSE)
#pvalue bernilai kurang dari taraf signifikansi 0.05 sehingga tolak H0
#H0= variabel Kategori independen dari variabel Penilaian Konten

#PEMODELAN
#1 regresi linear
linearmodel=lm(Harga~Installs,data=datap)
linearmodel = lm(Penilaian~Installs, data = datap)
linearmodel
summary(linearmodel)
#linearmodel=lm(Harga~Kategori,data=datap)
linearmodel=lm(Harga~Installs,data=datap)

#2. naive bayes
# Create Data Partition
library(caret)
set.seed(123)
index <- createDataPartition(datap$Tipe, p=0.80, list=FALSE)
# select 80% of the data for Training
train <- datap[index,] # Data Frame [baris, kolom]
dim(train)
# use the remaining 20% of data to testing the models
test <- datap[-index,]
dim(test)
#Cek Proporsi Data
prop.table(table(train$Tipe))
library(ROSE)
over <- ovun.sample(Tipe~., data = train, method = "over", N=14802)$data
#Recheck again
table(over$Tipe)
#pemodelan dgn over sampling
library(e1071)
library(caTools)
library(caret)
set.seed(120)
classifier_o=naiveBayes(as.factor(Tipe)~., data=over)
classifier_o
#confussion matrix
cm1=confusionMatrix(predict(classifier_o, test), as.factor(test$Tipe))
cm1
#confusion matrix
cm2=confusionMatrix(predict(classifier_o, train), as.factor(train$Tipe))
cm2


#EVALUATION METRIC
#Calculate Recall, Precision,F1-Score
y_predtest<-predict(classifier_o,newdata=test)
cm <- confusionMatrix(as.factor(y_predtest),as.factor(test$Tipe), mode= "prec_recall")
cm

###
dataapps=read.csv("D:/A KULIAH KVN/6. BA KMMI/project/Dataset_3.csv",dec=".")
Penilaian<-cut(dataapps$Penilaian,breaks=c(1,3.5,5),labels=c("Low","High"),right = TRUE)
dataapps$Penilaian=Penilaian
#2. naive bayes
# Create Data Partition
library(caret)
library(ROSE)
library(dplyr)
library(randomForest)
library(e1071)
index1<-createDataPartition(dataapps$Penilaian,p=0.70,list=FALSE)

##########Pembagian Data##################
#select 70% of the data for training
train<-dataapps[index1,]
dim(train)
#use the remaining to testing the models
test<-dataapps[-index1,]
dim(test)
#cek proporsi data training
table(train$Penilaian)
High<-which(train$Penilaian=="High")
Lower<-which(train$Penilaian=="Low")
length(High)
length(Lower)
#Cek Proporsi Data
prop.table(table(Penilaian))
library(ROSE)
oversample<-sample(Lower,length(High),replace=TRUE)
over<-train[c(oversample,High),]
table(over$Penilaian)

#pemodelan
set.seed(123)  
classifier_o <- naiveBayes(as.factor(Penilaian) ~ ., data = over)
y_predtest<-predict(classifier_o, over)
cm<-table(y_predtest,over$Penilaian)
confusionMatrix(cm)
y_predtest2<-predict(classifier_o, test)
cm2<-table(y_predtest2,test$Penilaian)
confusionMatrix(cm2)

y_predtest<-predict(classifier_o,newdata=test)
cm <- confusionMatrix(as.factor(y_predtest),as.factor(test$Penilaian), mode= "prec_recall")
cm


#Recheck again
table(over$Tipe)
#pemodelan dgn over sampling
library(e1071)
library(caTools)
library(caret)
set.seed(120)
classifier_o=naiveBayes(as.factor(Penilaian)~., data=over)
classifier_o
#confussion matrix
cm1=confusionMatrix(predict(classifier_o, test), as.factor(test$Penilaian))
cm1
#confusion matrix
cm2=confusionMatrix(predict(classifier_o, train), as.factor(train$Tipe))
cm2

#3
library(randomForest)
train[,2]=as.numeric(train[,2])
s=train[,c(3:4,6)]
set.seed(123)
rf <- randomForest(Penilaian ~ ., data = s, importance=TRUE,pr0ximity=TRUE,na.action = na.roughfix)
rf <- randomForest(Penilaian ~ ., data = )
print(rf)
library(caret)
p1 <- predict(rf, test)
mean((test$Penilaian-p1)^2)
postResample(pred=p1,obs=test$Penilaian)
plot(test$Penilaian,col='red',main='Actual vs Predicted Rating',pch=18,cex=0.7)
points(p1,col='blue',pch=18,cex=0.7)
legend('bottomright',legend=c('Actual','Predicted'),pch=18,col=c('red','blue'))
cm3 <- confusionMatrix(as.factor(p1),as.factor(test$Penilaian), mode= "prec_recall")
cm3

model_rf=train(Penilaian~.,tuneLength = 3, data = s, method = 
                 "rf", importance = TRUE,prOximity=TRUE,
               na.action=na.roughfix,
               trControl = trainControl(method = "cv",
                                        number = 5,
                                        savePredictions = "final",
                                      classProbs = T))
model_rf$pred
print(model_rf)
library(caret)
p1 <- predict(model_rf, train)
cm5 <- confusionMatrix(as.factor(p1), as.factor(train))
cm5




datap$Penilaian <- as.factor(datap$Penilaian)
# Decision tree with party
library(party)
mytree <- ctree(Penilaian~Harga, datap, controls=ctree_control(mincriterion=0.9, minsplit=50))
win.graph()
plot(mytree,type="simple")

# Misclassification error
tab <- table(predict(mytree), datap$Penilaian)
1-sum(diag(tab))/sum(tab)






#decision tree
library(rpart)
fit <- rpart(Penilaian~., data = train, method = 'class')
fit$variable.importance
barplot(fit$variable.importance)
#membuat plot decision tree
library(rattle)
fancyRpartPlot(fit)
#prediksi data testing
predict=predict(fit,newdata=test,type="Class")
#confusion matrix
table(predict,test$Penilaian)
confusionMatrix(data=predict,reference=test$Penilaian)
#variabel importance
fit$variable.importance
barplot(fit$variable.importance)


#HIERARKI CLUSTERING
#qq plot
library(MVN)

outlier=mvn(datap[,3:4],multivariateOutlierMethod="quan",
            showNewData=TRUE)
#buang data outlier terjauh dari observasi lain
summary(datap[,3:6])
#uji asumsi
library(car)
datap$SUM=rowSums(datap[,c(3:4,6)] )
cbind(datap,SUM=rowSums(datap[,c(3:4,6)]))
#tambah kolom baru isi nya sum per baris
multiko=vif(lm(datap$SUM~.,data=datap[,c(3:4,6)]))
library(factoextra)
metode_ward=fviz_nbclust(datap[,c(3:4,6)],FUN=hcut,method = "silhouette", hc_method="ward.D")
fviz_nbclust(pemuda[,2:8],FUN=hcut,method = "silhouette", hc_method="ward.D")
library(sparcl)
y = cutree(metode_ward, 2)
ColorDendrogram(metode_ward, y = y, labels = names(y), main = "Colored Dendogram (2 groups)", 
                branchlength = 80)
#hasil kelompok data dalam format data frame
anggotaward = cutree(metode_ward,2)
tabelward=data.frame(datap[,1], anggotaward) 
tabelward
hasil=data.frame(pemuda[,3:6], anggotaward)
hasil
kluster1=subset(hasil, anggotaward==1)
kluster2=subset(hasil, anggotaward==2)
kluster_1=sapply(kluster1, mean)
kluster_2=sapply(kluster2, mean)
mean_total=rbind(kluster_1,kluster_2)
mean_total





library(e1071)
s=train[,c(3:4,6)]
rf <- svm(Penilaian ~ ., data = s)
k=as.matrix(s)
s1=test[,c(3:4,6)]
library(caret)
p1 <- predict(rf, s1)
mean((test$Penilaian-p1)^2)

plot(test$Penilaian,col='red',main='Actual vs Predicted Rating',pch=18,cex=0.7)
points(p1,col='blue',pch=18,cex=0.7)
legend('bottomright',legend=c('Actual','Predicted'),pch=18,col=c('red','blue'))


##Random Forest
library(party)
library(randomForest)
library(caret)
library(e1071)
# Create the forest.
memory.limit(size=...)
memory.size() ### Checking your memory size
memory.limit() ## Checking the set limit
memory.limit(size=3500) ### expanding your memory _ here it goes beyond to your actually memory. This 56000 is proposed for 64Bit.
if(.Platform$OS.type == "windows") withAutoprint({
  memory.size()
  memory.size(TRUE)
  memory.limit()
})
memory.limit(size=100000)
write.csv(over,"randomfor.csv")
overrf=read.csv("D:/A KULIAH KVN/6. BA KMMI/project/randomfor.csv",dec=".")
na.omit(overrf)
overrf$Penilaian=factor(overrf$Penilaian)
out.forest <- randomForest(Penilaian ~ ., data = overrf,importance = TRUE,
                              proximity=TRUE)

print(out.forest)
attributes(output.forest)
# Predicting on train set
predTrain <- predict(output.forest, rating.over, type = "class")
# Checking classification accuracy
table(predTrain, rating.over$Penilaian) 
confusionMatrix(predTrain,rating.over$Penilaian)
# Predicting on Test set
predValid <- predict(output.forest, testing, type = "class")
confusionMatrix(predValid,testing$Penilaian)
plot(output.forest)
# Checking classification accuracy
mean(predValid == testing$Penilaian)                    
table(predValid,testing$Penilaian)
# To check important variables
importance(output.forest)        
varImpPlot(output.forest,
           main="Variable Importance")
varUsed(output.forest)


#KNN
dataknn<-dataapps %>% select(Ukuran,Harga,Ulasan,Installs,Penilaian)
head(dataknn)
view(dataknn)

##Generate a random number that is 90% of the total number of rows in dataset.
random <- sample(1:nrow(dataknn), 0.8 * nrow(dataknn))
##the normalization function is created
normal <-function(x) { (x -min(x))/(max(x)-min(x))}  

##Run nomalization on first 4 coulumns of dataset because they are the predictors
norm <- as.data.frame(lapply(dataknn[,c(1,2,3,4)], normal))
summary(app_norm)
view(app_norm)
##extract training set
app_train <- app_norm[ran,] 
##extract testing set
app_test <- app_norm[-ran,] 

##extract 5th column of train dataset because it will be used as 'cl' argument in knn function.
app_target_category <- data3[ran,5]
##extract 5th column if test dataset to measure the accuracy
app_test_category <- data3[-ran,5]

library(class)
##run knn function
#cek proporsi data training
table(app_test_category$Penilaian)

pr <- knn(app_train,app_test,app_target_category$Penilaian,k=90)
lengths(pr)
##create the confucion matrix
tb <- table(pr,app_test_category$Penilaian)
tb
table(app_train$Penilaian)
##check the accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tb)


