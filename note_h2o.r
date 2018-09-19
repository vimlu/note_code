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
