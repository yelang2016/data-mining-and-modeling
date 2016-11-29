



install.packages('DMwR')
install.packages('pROC')
install.packages('el071')

library('pROC')
library('DMwR')
library("e1071")

dt = read.csv("C://Users//yelang//Desktop//matlabyewu//r业务//svm//ww.csv") #读入数据
df =dt
sum(is.na(df))


df[complete.cases(df[,9]),] #去除婚否的缺失值df[complete.cases(df['MaritalStatus']),] 
df[complete.cases(df['VehiclePrice']),]
attach(df)
Age[Age == 0]=median (as.numeric(as.matrix(df['Age'])))  #所有缺失值换成38
VehiclePrice[VehiclePrice == 0] =0.5
df = data.frame(df,Age,VehiclePrice)

colnames(df) #所有列明
df=na.omit(df) #删除其他缺失值
sum(is.na(df)) #查看缺失值





table(df$FraudFound)

df<-df[,c(-8,-10,-14)]# 删掉这几列

set.seed(100) # for reproducing results
rowIndices <- 1 : nrow(df) # prepare row indices
sampleSize <- 0.8 * length(rowIndices) # training sample size
trainingRows <- sample (rowIndices, sampleSize) # random sampling
trainingData <- df[trainingRows, ] # training data
testData <- df[-trainingRows, ] # test data

prop.table(table(trainingData$FraudFound)) 
prop.table(table(testData$FraudFound)) 

trainingData$FraudFound<-as.factor(trainingData$FraudFound) 
trainingData<-SMOTE(FraudFound~.,trainingData,perc.over=400,perc.under=100) 
trainingData$FraudFound<-as.numeric(trainingData$FraudFound) 
# 我们再次用prop.table()函数检查结果的平衡性，确定我们已经让阴性、阳性数据达到相同。 
prop.table(table(trainingData$FraudFound)) 
##  
##   1   2  
## 


#tuned1 <- tune.svm(FraudFound ~ ., data = trainingData,  type="nu-classification" , gamma = 10^(-1:-4), cost = 10^(1:5)) # tune
#summary (tuned1) # to select best gamma and cost
svmfit <- svm (FraudFound ~ ., data = trainingData, type="nu-classification" , kernel = "radial", cost = 1000, gamma=0.0001, scale = FALSE) # radial svm, scaling turned OFF
print(svmfit)#打印svm模型概要
colnames(trainingData) #输出列明


compareTable <- cbind(testData[,12]+1, predict(svmfit, testData[,-12]))  # comparison table

View(compareTable)#观察预测结果
jg = as.numeric(predict(svmfit, testData[,-12])) #测试预测结果
testtarget = testData[,12]+1 #目标变量加1，为了和输出更好匹配
jglen =  length(jg)
count = length(which((testtarget==2)))
cheat = sum(abs((jg[1:count ]-testtarget[1:count ])))/as.numeric(length(jg[1:count ]))#欺诈模型错误率
cheataccuracy = 1-cheat# 欺诈模型正确率


count1 = length(which((testtarget==1)))#计算原始变量里面1的个数



cheatnot = sum(abs(na.omit(jg[count+1:jglen ]-testtarget[count+1:jglen ])))/count1
cheatnotaccuracy  = 1-cheatnot  # 非欺诈正确率

zaccuracy= 1- (sum(abs((jg-testtarget)))/as.numeric(length(jg)) ) #总的正确率


write.csv(compareTable,file="C://Users//yelang//Desktop//matlabyewu//r业务//svm//wwclear.csv")#写出数据,测试数据预测结果和测试数据本身目标值一起输出

