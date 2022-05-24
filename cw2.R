library(readxl)
library(neuralnet)
library(grid)
library(MASS)
library("remotes")
library(MLmetrics)

UoW_energy<-read_excel("UoW_load.xlsx", sheet = "Sheet1")
twoInput<-read_excel("UoW_load.xlsx", sheet = "Sheet2")
threeInput<-read_excel("UoW_load.xlsx", sheet = "Sheet3")
fourInput<-read_excel("UoW_load.xlsx", sheet = "Sheet4")
NARX1<-read_excel("UoW_load.xlsx", sheet = "NARX1")
NARX2<-read_excel("UoW_load.xlsx", sheet = "NARX2")


fourInput2<-read_excel("UoW_load.xlsx", sheet = "Sheet5")


str(UoW_energy)

summary(UoW_energy)

dim(UoW_energy)

### our energy set has 500 rows



normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}



threeInput_norm<-as.data.frame(lapply(threeInput,normalize))

twoInput_norm<-as.data.frame(lapply(twoInput,normalize))

fourInput_norm<-as.data.frame(lapply(fourInput,normalize))

fourInput2_norm<-as.data.frame(lapply(fourInput2,normalize))

NARX1_norm<-as.data.frame(lapply(NARX1, normalize))

NARX2_norm<-as.data.frame(lapply(NARX2, normalize))



NARX1_train<-NARX1_norm[1:427,]
NARX1_test<-NARX1_norm[428:497,]

NARX2_train<-NARX2_norm[1:426,]
NARX2_test<-NARX2_norm[429:498,]


threeInput_train<-threeInput_norm[1:427,]
threeInput_test<-threeInput_norm[428:497,]

twoInput_train<-twoInput_norm[1:428,]
twoInput_test<-twoInput_norm[429:498,]


fourInput_train<-fourInput_norm[1:426,]
fourInput_test<-fourInput_norm[427:496,]

fourInput2_train<-fourInput2_norm[1:422,]
fourInput2_test<-fourInput2_norm[423:492,]
#--------------------------------------------------------------------------
set.seed(12345)
threeInputModel.h1.5.FALSE.nn<-neuralnet(tPlus1~tMinus2+tMinus1+t,
                                   data = threeInput_train,hidden = c(10),
                                   act.fct = "logistic",linear.output = FALSE)

set.seed(12345)
threeInputModel.h1.5.TRUE.nn<-neuralnet(tPlus1~tMinus2+tMinus1+t,
                                   data = threeInput_train,hidden = c(10),
                                   act.fct = "logistic",linear.output = TRUE)

plot(threeInputModel.h1.5.TRUE.nn)

set.seed(12345)
twoInput_model1<-neuralnet(tPlus1~tMinus1+t,data = twoInput_train, hidden = c(8), act.fct= "logistic")
plot(twoInput_model1)#error 5.22459 step 3523

twoInput_model1Result<-compute(twoInput_model1,twoInput_test[1:2])



predicted_tomorrow<-twoInput_model1Result$net.result

cor(predicted_tomorrow,twoInput_test$tPlus1)

twoInput_train_original<-as.data.frame(twoInput[1:428,"tPlus1"])
twoInput_train_original

twoInput_test_original<-as.data.frame(twoInput[429:498,"tPlus1"])
#twoInput_test_original

tomorrow_min<-min(twoInput_train_original)
#tomorrow_min

tomorrow_max<-max(twoInput_train_original)
#tomorrow_max


head(twoInput_train_original)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


tomorrow_pred <- unnormalize(predicted_tomorrow, tomorrow_min, tomorrow_max)
tomorrow_pred 


RMSE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(twoInput_test_original)))

MAE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(twoInput_test_original)))  
MAPE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(twoInput_test_original)))

plot(data.frame(twoInput_test_original, tomorrow_pred) ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)

final_result <- cbind(twoInput_test_original, tomorrow_pred)
final_result


#-----------------------------------------------------------------------------------------


#rmse <- function(error)#sqrt(mean(error^2))
#{
#  data = error^(2)
# dataMean = lapply(data, mean, na.rm = TRUE)
# dataSQRT = lapply(dataMean,sqrt)
#return(dataSQRT)
#}
#error<-(twoInput_test_original -tomorrow_pred)

pred_RMSE<-rmse(error)
pred_RMSE


#MAEtest <- function(error)
#{#
#dataAbs <- abs(error)
#dataMean = lapply(dataAbs, mean, na.rm = TRUE)
#return(dataMean)
#}


#MAEtest(error)


#MAPEtest <- function(error, load_test_original_tomorrow)
#{
# newData <- error/ load_test_original_tomorrow
## newData <- abs(newData)
# dataMean = lapply(newData, mean, na.rm = TRUE)
#return(dataMean)
#}













set.seed(12345)
twoInput_model<-neuralnet(tomorrow~today+yesterday,data = twoInput_train)
#error 7.431 step 1909
plot(twoInput_model)


set.seed(12345)
twoInput_model4<-neuralnet(tomorrow~today+yesterday,data = twoInput_train, hidden = c(2,4))
plot(twoInput_model4)#error 5.079745 step 1662

set.seed(12345)
twoInput_model5<-neuralnet(tomorrow~today+yesterday,data = twoInput_train, hidden = c(2,5))
plot(twoInput_model5)#error 5.129677 step 3987

set.seed(12345)
twoInput_model6<-neuralnet(tomorrow~today+yesterday,data = twoInput_train, hidden = c(3))
plot(twoInput_model6)#error 5.22119 step 6390

set.seed(12345)
twoInput_model7<-neuralnet(tomorrow~today+yesterday,data = twoInput_train, hidden = c(3,2))
plot(twoInput_model7)#error 4.565485 step 1480

set.seed(12345)
twoInput_model8<-neuralnet(tomorrow~today+yesterday,data = twoInput_train, hidden = c(10,8))
plot(twoInput_model8)#error 4.5531 step 1873

twoInput_model8Result<-compute(twoInput_model8,twoInput_test[1:2])
predicted_tomorrow<-twoInput_model8Result$net.result
cor(predicted_tomorrow,twoInput_test$tomorrow)

twoInput_train_original<-twoInput[2:428,"tomorrow"]
twoInput_test_original<-twoInput[429:498,"tomorrow"]

tomorrow_min<-min(twoInput_train_original)
tomorrow_max<-min(twoInput_train_original)
head(twoInput_train_original)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


tomorrow_pred <- unnormalize(predicted_tomorrow, tomorrow_min, tomorrow_max)
tomorrow_pred 

rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(twoInput_test_original - tomorrow_pred)
pred_RMSE<-rmse(error)
pred_RMSE


set.seed(12345)
twoInput_model9<-neuralnet(tPlus1~t+tMinus1,data = twoInput_train, hidden = c(3,4))
plot(twoInput_model9)#error 4.5531 step 1873


#--------3 input--------





set.seed(12345)
threeInputModel.h1.5.FALSE.nn<-neuralnet(tPlus1~tMinus2+tMinus1+t,
                                         data = threeInput_train,hidden = c(10),
                                         act.fct = "logistic",linear.output = FALSE)

plot(threeInputModel.h1.5.FALSE.nn)


set.seed(12345)
threeInputModel.h1.5.TRUE.nn<-neuralnet(tPlus1~tMinus2+tMinus1+t,
                                        data = threeInput_train,hidden = c(10),
                                        act.fct = "logistic",linear.output = TRUE)

plot(threeInputModel.h1.5.TRUE.nn)


set.seed(12345)
threeInputModel.h1.10.h2.5.FALSE.nn<-neuralnet(tPlus1~tMinus2+tMinus1+t,
                                         data = threeInput_train,hidden = c(10,5),
                                         act.fct = "logistic",linear.output = FALSE)
plot(threeInputModel.h1.10.h2.5.FALSE.nn)




set.seed(12345)
threeInputModel.h1.10.h2.5.TRUE.nn<-neuralnet(tPlus1~tMinus2+tMinus1+t,
                                               data = threeInput_train,hidden = c(10,5),
                                               act.fct = "logistic",linear.output = TRUE)
plot(threeInputModel.h1.10.h2.5.TRUE.nn)


set.seed(12345)

threeInputModel.h1.24.h2.8.FALSE.nn<-neuralnet(tPlus1~tMinus2+tMinus1+t,
                                               data = threeInput_train,hidden = c(24,8),
                                               act.fct = "logistic",linear.output = FALSE)

plot(threeInputModel.h1.24.h2.8.FALSE.nn)


set.seed(12345)

threeInputModel.h1.24.h2.8.TRUE.nn<-neuralnet(tPlus1~tMinus2+tMinus1+t,
                                               data = threeInput_train,hidden = c(24,8),
                                               act.fct = "logistic",linear.output = TRUE)

plot(threeInputModel.h1.24.h2.8.TRUE.nn)


threeInputModel.h1.23.h2.3.TRUE.nn<-neuralnet(tPlus1~tMinus2+tMinus1+t,
                                              data = threeInput_train,hidden = c(23,3),
                                              act.fct = "logistic",linear.output = TRUE)

plot(threeInputModel.h1.23.h2.3.TRUE.nn)


#-----------4 input-------------


fourInputModel.h1.12.TRUE.nn<-neuralnet(tPlus1~tMinus3+tMinus2+tMinus1+t,
                                        data = fourInput_train,hidden = c(12),
                                        act.fct = "logistic",linear.output = TRUE)
plot(fourInputModel.h1.12.TRUE.nn)

#fourInputModel.h1.12.FALSE.nn<-neuralnet(tPlus1~tMinus3+tMinus2+tMinus1+t,data = fourInput_train,hidden = c(12),act.fct = "logistic",linear.output = FALSE)
#plot(fourInputModel.h1.12.FALSE.nn)
#--------------------------------------------
#fourInputModel.h1.12.h2.6.FALSE.nn<-neuralnet(tPlus1~tMinus3+tMinus2+tMinus1+t,data = fourInput_train,hidden = c(12,6),act.fct = "logistic",linear.output = FALSE)
#plot(fourInputModel.h1.12.h2.6.FALSE.nn)

fourInputModel.h1.12.h2.6.TRUE.nn<-neuralnet(tPlus1~tMinus3+tMinus2+tMinus1+t,
                                              data = fourInput_train,hidden = c(12,6),
                                              act.fct = "logistic",linear.output = TRUE)
plot(fourInputModel.h1.12.h2.6.TRUE.nn)

#---------------------------------------------------------

fourInputModel.h1.18.h2.4.TRUE.nn<-neuralnet(tPlus1~tMinus3+tMinus2+tMinus1+t,
                                             data = fourInput_train,hidden = c(18,4),
                                             act.fct = "logistic",linear.output = TRUE)

plot(fourInputModel.h1.18.h2.4.TRUE.nn)

fourInputModel.h1.18.h2.4.FALSE.nn<-neuralnet(tPlus1~tMinus3+tMinus2+tMinus1+t,
                                             data = fourInput_train,hidden = c(18,4),
                                             act.fct = "logistic",linear.output = FALSE)

plot(fourInputModel.h1.18.h2.4.FALSE.nn)


#----------------------------------------------------------------------------
fourInputModel.h1.20.h2.9.TRUE.nn<-neuralnet(tPlus1~tMinus3+tMinus2+tMinus1+t,
                                              data = fourInput_train,hidden = c(12),
                                              act.fct = "logistic",linear.output = TRUE)

plot(fourInputModel.h1.20.h2.9.TRUE.nn)



fourInputModel.h1.20.h2.9.TRUE.nn_Result<-compute(fourInputModel.h1.20.h2.9.TRUE.nn,fourInput_test[1:4])


predicted_tomorrow<-fourInput2Model.h1.13.h2.3.TRUE.nn_Result$net.result

cor(predicted_tomorrow,fourInput2_test$tPlus1)


fourInput_train<-fourInput_norm[1:426,]
fourInput_test<-fourInput_norm[427:496,]



fourInput2_train_original<-as.data.frame(twoInput[1:422,"tPlus1"])
twoInput_train_original

fourInput2_test_original<-as.data.frame(twoInput[423:492,"tPlus1"])
twoInput_test_original

tomorrow_min<-min(fourInput2_train_original)
#tomorrow_min

tomorrow_max<-max(fourInput2_train_original)
#tomorrow_max


head(fourInput2_train_original)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


tomorrow_pred <- unnormalize(predicted_tomorrow, tomorrow_min, tomorrow_max)
tomorrow_pred 


RMSE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(fourInput2_test_original)))

MAE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(fourInput2_test_original)))  
MAPE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(fourInput2_test_original)))

plot(data.frame(fourInput2_test_original, tomorrow_pred) ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)


final_result <- cbind(fourInput2_test_original, tomorrow_pred)
final_result







#----t,t-1,t-2,t-7----#

#fourInput2Model.h1.13.h2.3.FALSE.nn<-neuralnet(tPlus1~tMinus7+tMinus2+tMinus1+t,data = fourInput2_train,hidden = c(13,3),act.fct = "logistic",linear.output = FALSE)

#plot(fourInput2Model.h1.13.h2.3.FALSE.nn)
#---------------------------------------------------------------4


fourInput2Model.h1.13.h2.3.TRUE.nn<-neuralnet(tPlus1~tMinus7+tMinus2+tMinus1+t,data = fourInput2_train,hidden = c(13,3),act.fct = "logistic",linear.output = TRUE)

plot(fourInput2Model.h1.13.h2.3.TRUE.nn)


fourInput2Model.h1.13.h2.3.TRUE.nn_Result<-compute(fourInput2Model.h1.13.h2.3.TRUE.nn,fourInput2_test[1:4])


predicted_tomorrow<-fourInput2Model.h1.13.h2.3.TRUE.nn_Result$net.result

cor(predicted_tomorrow,fourInput2_test$tPlus1)


fourInput2_train<-fourInput2_norm[1:422,]
fourInput2_test<-fourInput2_norm[423:492,]

fourInput2_train_original<-as.data.frame(twoInput[1:422,"tPlus1"])
twoInput_train_original

fourInput2_test_original<-as.data.frame(twoInput[423:492,"tPlus1"])
twoInput_test_original

tomorrow_min<-min(fourInput2_train_original)
#tomorrow_min

tomorrow_max<-max(fourInput2_train_original)
#tomorrow_max


head(fourInput2_train_original)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


tomorrow_pred <- unnormalize(predicted_tomorrow, tomorrow_min, tomorrow_max)
tomorrow_pred 


RMSE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(fourInput2_test_original)))

MAE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(fourInput2_test_original)))  
MAPE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(fourInput2_test_original)))

plot(data.frame(fourInput2_test_original, tomorrow_pred) ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)


final_result <- cbind(fourInput2_test_original, tomorrow_pred)
final_result




#fourInput2Model.h1.19.h2.6.FALSE.nn<-neuralnet(tPlus1~tMinus7+tMinus2+tMinus1+t,data = fourInput2_train,hidden = c(19,6),act.fct = "logistic",linear.output = FALSE)

#plot(fourInput2Model.h1.19.h2.6.FALSE.nn)

#------------------------------------------------------------------3

fourInput2Model.h1.19.h2.6.TRUE.nn<-neuralnet(tPlus1~tMinus7+tMinus2+tMinus1+t,data = fourInput2_train,hidden = c(19,6),act.fct = "logistic",linear.output = TRUE)

plot(fourInput2Model.h1.19.h2.6.TRUE.nn)

fourInput2Model.h1.19.h2.6.TRUE.nn_Result<-compute(fourInput2Model.h1.19.h2.6.TRUE.nn,fourInput2_test[1:4])


predicted_tomorrow<-fourInput2Model.h1.19.h2.6.TRUE.nn_Result$net.result

cor(predicted_tomorrow,fourInput2_test$tPlus1)


fourInput2_train<-fourInput2_norm[1:422,]
fourInput2_test<-fourInput2_norm[423:492,]

fourInput2_train_original<-as.data.frame(twoInput[1:422,"tPlus1"])
twoInput_train_original

fourInput2_test_original<-as.data.frame(twoInput[423:492,"tPlus1"])
twoInput_test_original

tomorrow_min<-min(fourInput2_train_original)
#tomorrow_min

tomorrow_max<-max(fourInput2_train_original)
#tomorrow_max


head(fourInput2_train_original)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


tomorrow_pred <- unnormalize(predicted_tomorrow, tomorrow_min, tomorrow_max)
tomorrow_pred 


RMSE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(fourInput2_test_original)))

MAE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(fourInput2_test_original)))  
MAPE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(fourInput2_test_original)))

plot(data.frame(fourInput2_test_original, tomorrow_pred) ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)


final_result <- cbind(fourInput2_test_original, tomorrow_pred)
final_result




#fourInput2Model.h1.22.h2.8.FALSE.nn<-neuralnet(tPlus1~tMinus7+tMinus2+tMinus1+t,data = fourInput2_train,hidden = c(22,8),act.fct = "logistic",linear.output = FALSE)

#plot(fourInput2Model.h1.22.h2.8.FALSE.nn)


#------------------------------------------------------------------2
fourInput2Model.h1.22.h2.8.TRUE.nn<-neuralnet(tPlus1~tMinus7+tMinus2+tMinus1+t,data = fourInput2_train,hidden = c(22,8),act.fct = "logistic",linear.output = TRUE)

plot(fourInput2Model.h1.22.h2.8.TRUE.nn)



fourInput2Model.h1.22.h2.8.TRUE.nn_Result<-compute(fourInput2Model.h1.22.h2.8.TRUE.nn,fourInput2_test[1:4])


predicted_tomorrow<-fourInput2Model.h1.22.h2.8.TRUE.nn_Result$net.result

cor(predicted_tomorrow,fourInput2_test$tPlus1)


fourInput2_train<-fourInput2_norm[1:422,]
fourInput2_test<-fourInput2_norm[423:492,]

fourInput2_train_original<-as.data.frame(twoInput[1:422,"tPlus1"])
twoInput_train_original

fourInput2_test_original<-as.data.frame(twoInput[423:492,"tPlus1"])
twoInput_test_original

tomorrow_min<-min(fourInput2_train_original)
#tomorrow_min

tomorrow_max<-max(fourInput2_train_original)
#tomorrow_max


head(fourInput2_train_original)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


tomorrow_pred <- unnormalize(predicted_tomorrow, tomorrow_min, tomorrow_max)
tomorrow_pred 


RMSE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(fourInput2_test_original)))

MAE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(fourInput2_test_original)))  
MAPE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(fourInput2_test_original)))

plot(data.frame(fourInput2_test_original, tomorrow_pred) ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)


final_result <- cbind(fourInput2_test_original, tomorrow_pred)
final_result



#fourInput2Model.h1.24.h2.6.FALSE.nn<-neuralnet(tPlus1~tMinus7+tMinus2+tMinus1+t,data = fourInput2_train,hidden = c(24,6),act.fct = "logistic",linear.output = FALSE)

#plot(fourInput2Model.h1.24.h2.6.FALSE.nn)


#------------------------------------------------------1

fourInput2Model.h1.24.h2.6.TRUE.nn<-neuralnet(tPlus1~tMinus7+tMinus2+tMinus1+t,data = fourInput2_train,hidden = c(9),act.fct = "logistic",linear.output = TRUE)

plot(fourInput2Model.h1.24.h2.6.TRUE.nn)

fourInput2Model.h1.24.h2.6.TRUE.nn_Result<-compute(fourInput2Model.h1.24.h2.6.TRUE.nn,fourInput2_test[1:4])


predicted_tomorrow<-fourInput2Model.h1.24.h2.6.TRUE.nn_Result$net.result

cor(predicted_tomorrow,fourInput2_test$tPlus1)


fourInput2_train<-fourInput2_norm[1:422,]
fourInput2_test<-fourInput2_norm[423:492,]

fourInput2_train_original<-as.data.frame(fourInput2[1:422,"tPlus1"])
twoInput_train_original

fourInput2_test_original<-as.data.frame(fourInput2[423:492,"tPlus1"])
#twoInput_test_original

tomorrow_min<-min(fourInput2_train_original)
#tomorrow_min

tomorrow_max<-max(fourInput2_train_original)
#tomorrow_max


head(fourInput2_train_original)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


tomorrow_pred <- unnormalize(predicted_tomorrow, tomorrow_min, tomorrow_max)
tomorrow_pred 


RMSE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(fourInput2_test_original)))

MAE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(fourInput2_test_original)))  
MAPE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(fourInput2_test_original)))

plot(data.frame(fourInput2_test_original, tomorrow_pred) ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)


final_result <- cbind(fourInput2_test_original, tomorrow_pred)
final_result












#------Narx

set.seed(12345)
NARX1.model<-neuralnet(tPlus1~tMinus2+tMinus1+t+t9+t10,data = NARX1_train,hidden = c(15),act.fct = "logistic",linear.output = TRUE)

plot(NARX1.model, rep = "best")

NARX1.model_Result<-compute(NARX1.model,NARX1_test[1:5])


predicted_tomorrow<-NARX1.model_Result$net.result

cor(predicted_tomorrow,NARX1_test$tPlus1)

NARX1_train<-NARX1_norm[1:427,]
NARX1_test<-NARX1_norm[428:497,]



NARX1_train_original<-as.data.frame(NARX1[1:427,"tPlus1"])


NARX1_test_original<-as.data.frame(NARX1[428:497,"tPlus1"])
#twoInput_test_original

tomorrow_min<-min(NARX1_train_original)
#tomorrow_min

tomorrow_max<-max(NARX1_train_original)
#tomorrow_max


head(NARX1_train_original)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


tomorrow_pred <- unnormalize(predicted_tomorrow, tomorrow_min, tomorrow_max)



RMSE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(NARX1_test_original)))

MAE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(NARX1_test_original)))  

MAPE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(NARX1_test_original)))

plot(data.frame(NARX1_test_original, tomorrow_pred) ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)


final_result <- cbind(NARX1_test_original, tomorrow_pred)
final_result






NARX2_train<-NARX2_norm[1:426,]
NARX2_test<-NARX2_norm[429:498,]


set.seed(12345)
NARX2.model<-neuralnet(tPlus1~t10+tMinus1+t,data = NARX2_train,hidden = c(16,3),act.fct = "logistic",linear.output = TRUE)

plot(NARX2.model, rep = "best")

NARX2.model_Result<-compute(NARX2.model,NARX2_test[1:3])


predicted_tomorrow<-NARX2.model_Result$net.result

cor(predicted_tomorrow,NARX2_test$tPlus1)


NARX2_train<-NARX2_norm[1:426,]
NARX2_test<-NARX2_norm[429:498,]



NARX2_train_original<-as.data.frame(NARX2[1:426,"tPlus1"])


NARX2_test_original<-as.data.frame(NARX2[429:498,"tPlus1"])
#twoInput_test_original

tomorrow_min<-min(NARX2_train_original)
#tomorrow_min

tomorrow_max<-max(NARX2_train_original)
#tomorrow_max


head(NARX2_train_original)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


tomorrow_pred <- unnormalize(predicted_tomorrow, tomorrow_min, tomorrow_max)



RMSE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(NARX2_test_original)))

MAE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(NARX2_test_original)))  

MAPE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(NARX2_test_original)))

plot(data.frame(NARX2_test_original, tomorrow_pred) ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)


final_result <- cbind(NARX2_test_original, tomorrow_pred)
final_result


#----------------------------------------------------------------


set.seed(12345)
NARX1.model.h1.24.h2.6.nn<-neuralnet(tPlus1~tMinus2+tMinus1+t+t9+t10,data = NARX1_train,hidden = c(8),act.fct = "logistic",linear.output = TRUE)

plot(NARX1.model, rep = "best")

NARX1.model.h1.24.h2.6.nn_Result<-compute(NARX1.model.h1.24.h2.6.nn,NARX1_test[1:5])


predicted_tomorrow<-NARX1.model.h1.24.h2.6.nn_Result$net.result

cor(predicted_tomorrow,NARX1_test$tPlus1)

NARX1_train<-NARX1_norm[1:427,]
NARX1_test<-NARX1_norm[428:497,]



NARX1_train_original<-as.data.frame(NARX1[1:427,"tPlus1"])


NARX1_test_original<-as.data.frame(NARX1[428:497,"tPlus1"])
#twoInput_test_original

tomorrow_min<-min(NARX1_train_original)
#tomorrow_min

tomorrow_max<-max(NARX1_train_original)
#tomorrow_max


head(NARX1_train_original)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


tomorrow_pred <- unnormalize(predicted_tomorrow, tomorrow_min, tomorrow_max)



RMSE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(NARX1_test_original)))

MAE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(NARX1_test_original)))  

MAPE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(NARX1_test_original)))

plot(data.frame(NARX1_test_original, tomorrow_pred) ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)


final_result <- cbind(NARX1_test_original, tomorrow_pred)
final_result

#-----------------------

set.seed(12345)
NARX1.model<-neuralnet(tPlus1~tMinus2+tMinus1+t+t9+t10,data = NARX1_train,hidden = c(12,2),act.fct = "logistic",linear.output = TRUE)

plot(NARX1.model, rep = "best")

NARX1.model_Result<-compute(NARX1.model,NARX1_test[1:5])


predicted_tomorrow<-NARX1.model_Result$net.result

cor(predicted_tomorrow,NARX1_test$tPlus1)

NARX1_train<-NARX1_norm[1:427,]
NARX1_test<-NARX1_norm[428:497,]



NARX1_train_original<-as.data.frame(NARX1[1:427,"tPlus1"])


NARX1_test_original<-as.data.frame(NARX1[428:497,"tPlus1"])
#twoInput_test_original

tomorrow_min<-min(NARX1_train_original)
#tomorrow_min

tomorrow_max<-max(NARX1_train_original)
#tomorrow_max


head(NARX1_train_original)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


tomorrow_pred <- unnormalize(predicted_tomorrow, tomorrow_min, tomorrow_max)



RMSE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(NARX1_test_original)))

MAE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(NARX1_test_original)))  

MAPE(as.numeric(unlist(tomorrow_pred)),as.numeric(unlist(NARX1_test_original)))

plot(data.frame(NARX1_test_original, tomorrow_pred) ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)


final_result <- cbind(NARX1_test_original, tomorrow_pred)
final_result
