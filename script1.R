library(psych)
library(corrplot)
library(tseries)
library(lmtest)
library(bbmle)

data <- read.csv("DaneZ4.csv", sep = ";")
data

set.seed(293487)
inx <- sample(1:1000, 750)
data.train <- data[inx, ]
data.test <- data[-inx, ]

boxplot(data.train)
boxplot(data.test)

describe(data)
summary(data)
#brak braków danych
wzp.zm <- function(dat) 
{
  sd(dat)/mean(dat)
}
#wsp zmienności
zm.df <- t(as.data.frame(lapply(as.list(data), wzp.zm)))
colnames(zm.df) <- "wsp.zm"

boxplot(data)
hist(data[, 1], freq = T)
lines(density(data[, 1]))


hist.dnorm <- function(dat, i)
{
  g <- dat[, i]
  
  h <- hist(g, breaks = 10, density = 10,
            col = "lightgray", xlab = "X", main = as.character(colnames(dat)[i])) 
  xfit <- seq(min(g), max(g), length = 40) 
  yfit <- dnorm(xfit, mean = mean(g), sd = sd(g)) 
  yfit <- yfit * diff(h$mids[1:2]) * length(g) 
  
  lines(xfit, yfit, col = "black", lwd = 2)
}

par(mfrow = c(3, 1))
for (x in 1:3) 
{
  hist.dnorm(data, x)
}
par(mfrow = c(1, 1))
###############zad2
cor(data)

par(mfrow = c(1, 3))

plot(data$y ~ data$x)
plot(data$y ~ data$x2)
plot(data$x ~ data$x2)

plot(log(data$y) ~ data$x)
plot(log(data$y) ~ data$x2)
plot(data$x ~ data$x2)
###############zad3
model <- lm(y ~ ., data.train)
summary(model)

shapiro.test(model$residuals)
#odrzucamy H0 reszty nie są normalne

plot(model$residuals, type = "l")

bptest(model)
#heteroskedastyczność występuje

acf(model$residuals)
#nie występuję autokorelacja

#czy można stosować?
#nie bo reszty wyszły heteroskedastyczne


#zad4
nll <- function(theta0, theta1, theta2)
{
  attach(data)
  mu = exp(theta0 + x*theta1 + x2*theta2)
  -sum(y*(log(mu)) - mu)
}

est <- stats4::mle(minuslog=nll, start=list(theta0 = 2, theta1 = 0, theta2 = 0))
summary(est)

est2 <- mle2(minuslog=nll, start=list(theta0 = 2, theta1 = 0, theta2 = 0))
