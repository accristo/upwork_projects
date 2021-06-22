#Upwork assignment
#Problem 1: Gas mileage prediction

#(A) 
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gridExtra)

plot_car_data <- function(dat){
  df_auto_mp <- read.table(dat, header=TRUE)
  df_auto_mp <- df_auto_mp %>% select(mpg, horsepower, acceleration, weight, cylinders, 
                                      displacement)
  df_names <- data.frame(x1=names(df_auto_mp)[-1], x2=
                           c("Horse Power", "Acceleration", "Weight","Cylinders", "Displacement"))
  df_plots <- apply(df_auto_mp, 2, function(b){
    df_auto_mp %>% 
      ggplot(aes(mpg, b)) +
      geom_point() +
      xlab(b)+
      ylab("Miles per Gallon")
  })
  df_plots[-1]
}
plot_car_data("auto_mpg.data")

#(A*)
plot_car_data <- function(dat){
  df_auto_mp <- read.table(dat, header=TRUE)
  df_auto_mp <- df_auto_mp %>% select(mpg, horsepower, acceleration, weight, cylinders, 
                                      displacement)
  par(mfrow=c(2,3))
  plot(df_auto_mp$horsepower, df_auto_mp$mpg, xlab="Horse Power", ylab= "Miles per Gallon", frame.plot=TRUE)
  plot(df_auto_mp$acceleration, df_auto_mp$mpg, xlab="Acceleration", ylab= "Miles per Gallon",frame.plot=TRUE)
  plot(df_auto_mp$weight, df_auto_mp$mpg, xlab="Weight", ylab= "Miles per Gallon",frame.plot=TRUE)
  plot(df_auto_mp$cylinders, df_auto_mp$mpg, xlab="Number of Cylinders", ylab= "Miles per Gallon",frame.plot=TRUE)
  plot(df_auto_mp$displacement, df_auto_mp$mpg, xlab="Displacement", ylab= "Miles per Gallon",frame.plot=TRUE)
}
plot_car_data("auto_mpg.data")
dev.off()

#(B)
predict_mpg <- function(p, a, w){
  df_auto_mp <- read.table(auto-mp.data, header=TRUE)
  fit_mp <- df_auto_mp %>% lm(mpg~horsepower+acceleration+weight, data=.)
  y_hat <- predict(fit_mp, newdata=data.frame(horsepower=p, acceleration=a, weight=w))
  y_hat
}
predict_mpg(100, 15, 3500)
predict_mpg(c(100, 110, 120, 150), c(10,15,20,25), c(2000, 2200, 3500, 4000))  

#Problem 2: Fitting cosines to a wave
#(A)
first_model <- function(data){
  cosine_data <- read.table(data, header=TRUE)
  fit_cosine1 <- cosine_data %>% mutate(cosx=cos(x)) %>% lm(y~cosx, data=.)
  fit_cosine1$coefficients
}
first_model("dataxy.csv")

#(B)
second_model <- function(data){
  cosine_data <- read.table(data, header=TRUE)
  fit_cosine2 <- cosine_data %>% mutate(cosx=cos(x), cos3x=cos(3*x)) %>% 
    lm(y~cosx+cos3x, data=.)
  fit_cosine2$coefficients
}
second_model("dataxy.csv")

#(C)
third_model <- function(data){
  cosine_data <- read.table("dataxy.csv", header=TRUE)
  fit_cosine3 <- cosine_data %>% mutate(cosx=cos(x), cos3x=cos(3*x), cos5x=cos(5*x)) %>% 
    lm(y~cosx+cos3x+cos5x, data=.)
  fit_cosine3$coefficients
}
third_model("dataxy.csv")

#(D)
plotmodels <- function(filename){
  cosine_data <- read.table(filename, header=TRUE)
  fit_cosine1 <- cosine_data %>% mutate(cosx=cos(x)) %>% lm(y~cosx, data=.)
  yhat1 <- cosinedata %>% predict.lm(fit_cosine1, data=.)
  fit_cosine2 <- cosine_data %>% mutate(cosx=cos(x), cos3x=cos(3*x)) %>% 
    lm(y~cosx+cos3x, data=.)
  yhat2 <- cosinedata %>% predict.lm(fit_cosine2, data=.)
  fit_cosine3 <- cosine_data %>% mutate(cosx=cos(x), cos3x=cos(3*x), cos5x=cos(5*x)) %>% 
    lm(y~cosx+cos3x+cos5x, data=.)
  yhat3 <- cosinedata %>% predict.lm(fit_cosine3, data=.)
  
  p0 <- ggplot()+geom_point(aes(cosine_data$x,cosine_data$y))+xlab("x")+ylab("y")
  p1 <- p0+ggtitle("Data")
  p2 <- p0+geom_line(aes(cosine_data$x, yhat1), color="green", size=1)+ggtitle("Data and first model")
  p3 <- p0+geom_line(aes(cosine_data$x, yhat2), color="red", size=1)+ggtitle("Data and second model")
  p4 <- p0+geom_line(aes(cosine_data$x, yhat3), color="blue", size=1)+ggtitle("Data and third model")
  grid.arrange(p1, p2, p3, p4, ncol=2, nrow=2)
}
plotmodels("dataxy.csv")

#Problem 3: Workforce transfers
#(A)
fenwick_workforce <- function(years, initial, rates){
  period <- seq(1,years,1)
  AISt <- initial
  AIS <- data.frame(Agr=initial[1], Ind=initial[2], Serv=initial[3])
  for(n in period){
    AISt1 <- rates%*%AISt
    AISt <- AISt1
    AIS[n+1,1] <- ceiling(AISt1[1])
    AIS[n+1,2] <- ceiling(AISt1[2])
    AIS[n+1,3] <- ceiling(AISt1[3])
    n <- n+1
  }
  AIS
}
initial <- c(30000, 40000, 30000)
rates <- matrix(c(0.75, 0.2, 0.12, 
                  0.1, 0.55, 0.22, 
                  0.15, 0.25, 0.66), 
                ncol = 3, byrow = T)
fenwick_workforce(10, initial, rates)

rates <- matrix(c(0.70, 0.25, 0.15, 
                  0.2, 0.60, 0.20, 
                  0.1, 0.15, 0.65), 
                ncol = 3, byrow = T)
initial <- c(50000, 20000, 30000)
fenwick_workforce(15, initial, rates)

#(B)
library(tidyverse)
library(ggplot2)
plot_workforce_time <- function(years, initial, rates){
  period <- seq(1,years,1)
  AISt <- initial
  AIS <- data.frame(Agr=initial[1], Ind=initial[2], Serv=initial[3], pt=1)
  for(n in period){
    AISt1 <- rates%*%AISt
    AISt <- AISt1
    AIS[n+1,1] <- ceiling(AISt1[1])
    AIS[n+1,2] <- ceiling(AISt1[2])
    AIS[n+1,3] <- ceiling(AISt1[3])
    AIS[n+1,4] <- n+1
    n <- n+1
  }
  AIS %>% ggplot(aes(x=pt))+
    geom_line(aes(y=Agr), col="red")+
    geom_line(aes(y= Ind), col="purple")+
    geom_line(aes(y=Serv), col="green")+
    xlab("years")+
    ylab("number")
}
M <- matrix(c(0.75, 0.2, 0.12, 
              0.1, 0.55, 0.22, 
              0.15, 0.25, 0.66), 
            ncol = 3, byrow = T)
init <- c(20000, 30000, 50000)
plot_workforce_time(20, init, M)

