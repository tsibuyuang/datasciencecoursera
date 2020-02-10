x <- c(1,3,5)
y <- c(3,2,10)
rbind(x,y)

x <- list(2, "a", "b", TRUE)
x[[2]]

x <- 1:4
y <- 2
x + y

x <- c(3, 5, 1, 10, 12, 6)
x[x<6] <- 0
x[x %in% 1:5] <- 0

#Read from CSV
x <- read.csv("./R-Programming/W1-Quiz/data/hw1_data.csv")
head(x)
nrow(x)
tail(x,2)
x[47,]
summary(x)

#remove NA
y <- na.omit(x)
summary(y)

z <- subset(y, Ozone >31 & Temp > 90, select = c(Ozone, Temp,Solar.R, Month ))
summary(z)

z1 <- subset(x, Month == 6, select = c(Ozone, Temp,Solar.R, Month ) )
summary(z1)
z1

z2 <- subset(x, Month == 5, select = c(Ozone, Temp,Solar.R, Month ) )
summary(z2)
z2
