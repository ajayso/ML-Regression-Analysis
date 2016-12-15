install.packages("Quandl")
library(Quandl)
install.packages("devtools")
library(devtools)
install_github("quandl/quandl-r")
library(TTR)

goldpricesAllUp =  Quandl("LBMA/GOLD")

# Calculate the ROC 
roc  = goldpricesAllUp$`USD (AM)`
yy = ROC(roc, type="discrete")*100


gpdata = data.frame(goldpricesAllUp$Date,goldpricesAllUp$`USD (AM)`,yy)
gpdata = gpdata[gpdata$goldpricesAllUp.Date > "2015-01-01",]
model = lm ( goldpricesAllUp..USD..AM..~., data = gpdata)
paste('y =', coef(model)[[2]], '* x', '+', coef(model)[[1]])

slopedata <- array(1:length(gpdata$goldpricesAllUp.Date))
print("I am the winner")
slopedata = slope(gpdata)
# Calculate the Slope 

slope = function (data){
  slopedata <- array(1:length(data$goldpricesAllUp.Date))
 
  for  (i in 1: length(data$goldpricesAllUp.Date)){
    
    
    
    if (
      (!is.na(data$yy[i])) & 
      (!is.na(data$yy[i+1]))
    )
    {  
      x1 = data$yy[i]
      x2 = data$yy[i+1]
      y1 = data[i,2]
      y2 = data[i+1,2]
      ya = y2-y1
      xa = x2-x1
      za= ya/xa
      slopedata[i+1] = za
      
    }
    
  }
  return (slopedata)
}


roc_ratio = RationRoc(gpdata)
so_index = 14
SO  = Stochastic_Oscillator(gpdata,so_index)



# Ratio of ROC


RationRoc = function (data){
  roc_ratio = array(1:length(data$goldpricesAllUp.Date))
  for  (i in 1: length(data$goldpricesAllUp.Date)){
    if (!is.na(data$yy[i])) {
      x1 = data$yy[i]
      x2 = data$yy[length(data)]
      print(x1)
      print(x2)
      xa = x1/x2
      print (xa)
      roc_ratio[i] = xa
      
    }
  }
  return (roc_ratio)
}

# Calculate the Slope 

Stochastic_Oscillator  = function (data, n ){
  
  StochasticOscillator  <- array(1:length(data$goldpricesAllUp.Date))
  BuySellFlag  <- array(1:length(data$goldpricesAllUp.Date))
  #Ln = lowest price over past n days
  goldpricedata = data$goldpricesAllUp..USD..AM..
 
  #Hn= highest price over past n days
  
  
  for  (i in 1: length(data$goldpricesAllUp.Date)){
    if (n+i <length(data$goldpricesAllUp.Date)){
    
      
      dx = n+i
   
      xdata = goldpricedata[i:dx]
    
      Ln = which.min(xdata)
      if (Ln==0){
        print("Asshole")
      }
     
      Hn = which.max(xdata)
     
      #P(x) = price on day x 
      px = data[i,2]
      # %K = (P(x) ??? Ln)/ (Hn ??? Ln) x 100%..
      pa = px -xdata[Ln]
     
      na = xdata[Hn]-xdata[Ln]
     
      ma= pa/na
      
      StochasticOscillator[i] = ma *100
      if (is.na(StochasticOscillator[i]))
        next
      if (StochasticOscillator[i] < 20)
        BuySellFlag[i]= 1
      else 
        BuySellFlag[i]= 0
      
      if (StochasticOscillator[i] > 80)
        BuySellFlag[i]= -1
    }
    else {
      BuySellFlag[i]= -2
    }
  }
  
  SX = data.frame(StochasticOscillator,BuySellFlag )
  return(SX)
}


xfinaldata = data.frame(gpdata, slopedata,roc_ratio,SO)
xfinaldata= xfinaldata[!xfinaldata$BuySellFlag=="NoClassification",]

#xfinaldata$BuySellFlag = as.factor(xfinaldata$BuySellFlag)
#xfinaldata= na.omit(xfinaldata)
trainingdataLength = 0.6 * length(xfinaldata$goldpricesAllUp.Date)
testingdatalength = 0.4 * length(xfinaldata$goldpricesAllUp.Date)
train <- xfinaldata[1:trainingdataLength,]
test <- xfinaldata[trainingdataLength:testingdatalength,]

lr_model <- glm(train$BuySellFlag ~ train$goldpricesAllUp..USD..AM..+ train$yy+ train$slopedata + train$roc_ratio + train$StochasticOscillator,data=train)
anova(lr_model, test="Chisq")

library(pscl)
pR2(lr_model)
predict <- predict(lr_model,newdata= test, type = 'response')
table(test$BuySellFlag, predict > 0.5)


library(rpart)
library(rpart.plot)
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)	
fit <- rpart(BuySellFlag~train$goldpricesAllUp..USD..AM..+ train$yy + train$slopedata,method = "class",data=train)
