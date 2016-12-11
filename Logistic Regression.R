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
model = lm ( goldpricesAllUp..USD..AM..~., data = gpdata)
paste('y =', coef(model)[[2]], '* x', '+', coef(model)[[1]])

slopedata <- array(1:length(gpdata$goldpricesAllUp.Date))
slope(gpdata,slopedata)

roc_ratio <- array(1:length(gpdata$goldpricesAllUp.Date))
RationRoc(data,roc_ratio)

StochasticOscillator  <- array(1:length(gpdata$goldpricesAllUp.Date))
Stochastic_Oscillator(data,StochasticOscillator)

# Calculate the Slope 

slope = function (data, slopedata){
  
  for  (i in  length(gpdata$goldpricesAllUp.Date)){
    x1 = data$yy[i]
    x2 = data$yy[i+1]
    y1 = data[i,2]
    y2 = data[i+1,2]
    # y2 formula goes here......
    print (y2-y1)
    print (x2-x1)
    slopedata[i+1] = (y2-y1)/(x2-x1)
    
  }
}


# Ratio of ROC


RationRoc = function (data, roc_ratio){
  
  for  (i in  length(data)){
    x1 = data$yy[i]
    x2 = data$yy[length(data)]
   
    roc_ratio(i+1) = (x1/x2)
    
  }
}

# Calculate the Slope 

Stochastic_Oscillator  = function (data, StochasticOscillator, n ){
  #Ln = lowest price over past n days
  goldpricedata = data$goldpricesAllUp.Date
  
  #Hn= highest price over past n days
  
  
  for  (i in  length(data)){
    xdata = goldpricedata[i:n,1]
    Ln = which.min(xdata)
    Hn = which.max(xdata)
    #P(x) = price on day x 
    px = data[1,i]
    # %K = (P(x) - Ln)/ (Hn - Ln) x 100%..
    StochasticOscillator(i) = (px -Ln)/(Hn-Ln) *100
    
  }
}


