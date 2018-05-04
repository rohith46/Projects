
############Digit recognization using Deep Neural Networks #################

#loading libraries
library(keras)
#install_keras() - one time 

# loading data 
train <- data.matrix(read.csv('train.csv', header = T))
test <- data.matrix(read.csv('test.csv', header = T))

dim(train)
dim(test)

train[,1] %>% table() %>% barplot(col=1:10)

train[4,-1] %>% matrix(nrow=sqrt(dim(train)[2]-1),byrow=T) %>% apply(2,rev)%>% t() %>% image(col=grey.colors(255))

train[3,-1] %>% matrix(nrow=sqrt(dim(train)[2]-1),byrow=T) %>% apply(2,rev)%>% t() %>% image(col=grey.colors(255))

train.label <- train[,1] %>% to_categorical()

train.feature <- train[,-1] %>% normalize()

test.feature <- test %>% normalize()

model <- keras_model_sequential()

model %>% 
  layer_dense(units=128,activation='relu',input_shape=c(784))%>%
  layer_dense(units=64,activation='relu')%>%
  layer_dense(units=32,activation='relu')%>%
  layer_dense(units=10,activation='softmax')

model %>%  compile(
          loss='categorical_crossentropy',
          optimizer='adam',
          metrics='accuracy'
            )

history <- model %>%  fit(train.feature, train.label, epochs=5, batch_size=5, validation_split=.1 )

plot(history)

pred<-model %>% predict_classes(test.feature,batch_size=5)

submission<-data.frame(ImageId=1:nrow(test),Label=pred)

write.csv(submission, file="submission.csv", row.names=F)
