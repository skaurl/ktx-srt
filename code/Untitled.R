install.packages("ggplot2")
install.packages("forecast")
install.packages("TSA")
install.packages("teseries")
install.packages("Metrics")
library(ggplot2)
library(forecast)
library(TSA)
library(tseries)
library(Metrics)

data<-read.csv("/Users/kimnamhyeok/PycharmProjects/ktx-srt/data/dataset.csv", header = T, fileEncoding = "utf-8")
data<-data['고속철도.여객수.계']
data

X04041912<-ts(data, start = c(2004, 4), frequency = 12)
X04041912
plot(X04041912, main = "", ylab = "")
ggseasonplot(X04041912, main = "", ylab = "")

X11011912<-ts(data[82:189,], start = c(2011, 1), frequency = 12)
X11011912
plot(X11011912, main = "", ylab = "")
ggseasonplot(X11011912, main = "", ylab = "")

Z11011912<-data[82:189,]
Ztime<-c(1:108)

lm11011912<-lm(Z11011912~Ztime)
lm11011912
summary(lm11011912)
anova(lm11011912)
plot(x=Ztime, y=Z11011912, main = "", ylab = "")
lines(predict(lm11011912), col="red", main = "", ylab = "")

lm11011912_2<-lm(formula = Z11011912 ~ Ztime + I(Ztime^2))
lm11011912_2
summary(lm11011912_2)
anova(lm11011912_2)
plot(x=Ztime, y=Z11011912, main = "", ylab = "")
lines(predict(lm11011912_2), col="red", main = "", ylab = "")

unlm11011912<-lm(log(Z11011912)~Ztime)
unlm11011912
summary(unlm11011912)
anova(unlm11011912)
plot(x=Ztime, y=Z11011912, main = "", ylab = "")
lines(Ztime, exp(0.006421*Ztime+8.240057), col = "red", main = "", ylab = "")

shapiro.test(Z11011912)
shapiro.test(residuals(lm11011912))
shapiro.test(residuals(lm11011912_2))
shapiro.test(residuals(unlm11011912))

par(mfrow=c(1,2))
plot(lm11011912, main = "")
plot(lm11011912_2, main = "")
plot(unlm11011912, main = "")
par(mfrow=c(1,1))

plot(x=Ztime, y=Z11011912, main = "", ylab = "")
lines(predict(lm11011912), main = "",lty = 1, col = 2)
lines(predict(lm11011912_2), main = "",lty = 2, col = 4)
lines(Ztime, exp(0.006421*Ztime+8.240057), main = "",lty = 4, col = 6)
legend('topleft',legend = c("1","2","un"), lty = c(1,2,4), col =c(2,4,6))

par(mfrow=c(1,2))
Acf(X11011912, main = "")
Pacf(X11011912, main = "")
Acf(diff(X11011912, lag = 1), main = "")
Pacf(diff(X11011912, lag = 1), main = "")
Acf(diff(X11011912, lag = 2), main = "")
Pacf(diff(X11011912, lag = 2), main = "")
Acf(diff(X11011912, lag = 12), main = "")
Pacf(diff(X11011912, lag = 12), main = "")
Acf(sqrt(X11011912), main = "")
Pacf(sqrt(X11011912), main = "")
Acf(diff(sqrt(X11011912), lag = 1), main = "")
Pacf(diff(sqrt(X11011912), lag = 1), main = "")
Acf(diff(sqrt(X11011912), lag = 2), main = "")
Pacf(diff(sqrt(X11011912), lag = 2), main = "")
Acf(diff(sqrt(X11011912), lag = 12), main = "")
Pacf(diff(sqrt(X11011912), lag = 12), main = "")
par(mfrow=c(1,1))

adf.test(X11011912,k=0)
adf.test(diff(X11011912,lag=1),k=0)
adf.test(diff(X11011912,lag=2),k=0)
adf.test(diff(X11011912,lag=12),k=0)
adf.test(sqrt(X11011912),k=0)

auto.arima(sqrt(X11011912))

for(a in 0:1) {
  for(b in 1:1) {
    for(c in 0:1) {
      for(d in 0:1) {
        for(e in 1:1) {
          for(f in 0:1) {
            print(c(a,b,c,d,e,f))
            print(AIC(arima(sqrt(X11011912), order = c(a,b,c), seasonal = list(order = c(d,e,f), frequency = 12))))
            print(BIC(arima(sqrt(X11011912), order = c(a,b,c), seasonal = list(order = c(d,e,f), frequency = 12))))
          }
        }
      }
    }
  }
}

par(mfrow=c(1,2))
Acf(X11011912 - (fitted(arima(sqrt(X11011912), order = c(0,1,1), seasonal = list(order = c(0,1,1), frequency = 12))))^2, main = "")
Pacf(X11011912 - (fitted(arima(sqrt(X11011912), order = c(0,1,1), seasonal = list(order = c(0,1,1), frequency = 12))))^2, main = "")
Acf(X11011912 - (fitted(arima(sqrt(X11011912), order = c(0,1,1), seasonal = list(order = c(1,1,1), frequency = 12)))^2), main = "")
Pacf(X11011912 - (fitted(arima(sqrt(X11011912), order = c(0,1,1), seasonal = list(order = c(1,1,1), frequency = 12)))^2), main = "")
Acf(X11011912 - (fitted(arima(sqrt(X11011912), order = c(1,1,1), seasonal = list(order = c(0,1,1), frequency = 12)))^2), main = "")
Pacf(X11011912 - (fitted(arima(sqrt(X11011912), order = c(1,1,1), seasonal = list(order = c(0,1,1), frequency = 12)))^2), main = "")
Acf(X11011912 - (fitted(arima(sqrt(X11011912), order = c(1,1,1), seasonal = list(order = c(1,1,1), frequency = 12)))^2), main = "")
Pacf(X11011912 - (fitted(arima(sqrt(X11011912), order = c(1,1,1), seasonal = list(order = c(1,1,1), frequency = 12)))^2), main = "")
par(mfrow=c(1,1))
