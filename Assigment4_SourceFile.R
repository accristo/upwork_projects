#Submission template
#The function definitions should be submitted as a single source file named assignment4.R, with the following contents:

plot_car_data <- function(filename){
  df_auto_mp <- read.table(filename, header=TRUE)
  df_auto_mp <- df_auto_mp %>% select(mpg, horsepower, acceleration, weight, cylinders, 
                                      displacement)
  par(mfrow=c(2,3))
  plot(df_auto_mp$horsepower, df_auto_mp$mpg, xlab="Horse Power", ylab= "Miles per Gallon", frame.plot=TRUE)
  plot(df_auto_mp$acceleration, df_auto_mp$mpg, xlab="Acceleration", ylab= "Miles per Gallon",frame.plot=TRUE)
  plot(df_auto_mp$weight, df_auto_mp$mpg, xlab="Weight", ylab= "Miles per Gallon",frame.plot=TRUE)
  plot(df_auto_mp$cylinders, df_auto_mp$mpg, xlab="Number of Cylinders", ylab= "Miles per Gallon",frame.plot=TRUE)
  plot(df_auto_mp$displacement, df_auto_mp$mpg, xlab="Displacement", ylab= "Miles per Gallon",frame.plot=TRUE)
}

predict_mpg <- function(p,a,w){
  df_auto_mp <- read.table("auto-mp.data", header=TRUE)
  fit_mp <- df_auto_mp %>% lm(mpg~horsepower+acceleration+weight, data=.)
  y_hat <- predict(fit_mp, newdata=data.frame(horsepower=p, acceleration=a, weight=w))
  y_hat
}

first_model <- function(filename){
  cosine_data <- read.table(data, header=TRUE)
  fit_cosine1 <- cosine_data %>% mutate(cosx=cos(x)) %>% lm(y~cosx, data=.)
  fit_cosine1$coefficients
}

second_model <- function(filename){
  cosine_data <- read.table(data, header=TRUE)
  fit_cosine2 <- cosine_data %>% mutate(cosx=cos(x), cos3x=cos(3*x)) %>% 
    lm(y~cosx+cos3x, data=.)
  fit_cosine2$coefficients
}

third_model <- function(filename){
  cosine_data <- read.table("dataxy.csv", header=TRUE)
  fit_cosine3 <- cosine_data %>% mutate(cosx=cos(x), cos3x=cos(3*x), cos5x=cos(5*x)) %>% 
    lm(y~cosx+cos3x+cos5x, data=.)
  fit_cosine3$coefficients
}

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