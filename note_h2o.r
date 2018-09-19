install.packages("h2o")
library(h2o)
h2o.init()
demo(h2o.glm)
## ??h20

## 教程来自：http://blog.itpub.net/16582684/viewspace-1255976/ 
# 4. 训练minist数据

# 下载 Train Dataset: http://www.pjreddie.com/media/files/mnist_train.csv
# 下载 Test Dataset: http://www.pjreddie.com/media/files/mnist_test.csv
res <- data.frame(Training = NA, Test = NA, Duration = NA)

#加载数据到h2o
train_h2o <- h2o.importFile(path = "E:\\github\\R\\mnist_train.csv")
test_h2o <- h2o.importFile(path = "E:\\github\\R\\mnist_test.csv")
y_train <- as.factor(as.matrix(train_h2o[, 1]))
y_test <- as.factor(as.matrix(test_h2o[, 1]))

##训练模型要很长一段时间，多个cpu使用率几乎是100%，风扇狂响。最后一行有相应的进度条可查看!!
model <- h2o.deeplearning(x = 2:785,  # column numbers for predictors
                          y = 1,   # column number for label
                          data = train_h2o,
                          activation = "Tanh",
                          balance_classes = TRUE,
                          hidden = c(100, 100, 100),  ## three hidden layers, 100 parameters each
                          epochs = 100)  ## times to iterated or be fractoinal

#输出模型结果
model
str(model)
## 评介性能
yhat_train <- h2o.predict(model, train_h2o)$predict
yhat_train <- as.factor(as.matrix(yhat_train))

yhat_test <- h2o.predict(model, test_h2o)$predict
yhat_test <- as.factor(as.matrix(yhat_test))

查看前100条预测与实际的数据相比较
y_test[1:100]
yhat_test[1:100]

## 查看并保存结果
library(caret)
res[1, 1] <- round(h2o.confusionMatrix(yhat_train, y_train)$overall[1], 4)
res[1, 2] <- round(h2o.confusionMatrix(yhat_test, y_test)$overall[1], 4)
print(res)


##===============Another example==================##
# https://blog.csdn.net/sinat_26917383/article/details/51219025 
#参考来自：h2o官方手册,h2o.deeplearning函数的示例
library(h2o)
h2o.init()
iris.hex <- as.h2o(iris)

## iris.dl <- h2o.deeplearning(x = 1:4, y = 6, training_frame = iris.hex)  #模型拟合,##ERRROR: response variable index 6 is out of range!! 
iris.dl <- h2o.deeplearning(x = 1:4, y = 5, training_frame = iris.hex)  #模型拟合
# now make a prediction
predictions <- h2o.predict(iris.dl, iris.hex)          #预测
as.data.frame(predictions)                             #预测数据变成数据框

performance = h2o.performance(model = iris.dl)
print(performance)
