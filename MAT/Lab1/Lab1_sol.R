###       STATYSTYKA MATEMATYCZNA       ###
###           LABORATORIUM 1            ###
###           Rozwiazania               ###


# Zadanie 1 ---------------------------------------------------------------

# a)

curve(dnorm(x), from = -6, to = 6)
curve(dnorm(x,1,1), from = -6, to = 6, col = 'blue', add = T)
curve(dnorm(x,2,1), from = -6, to = 6, col = 'red', add = T)
legend("topright", c("N(0,1)", "N(1,1)", "N(2,1)"),
       fill = c("black", "blue", "red"), cex = 0.5)
title("Gęstości")

curve(pnorm(x, 0, 1), from = -3, to = 6)
curve(pnorm(x, 1, 1), xlim = c(-3, 5), col = 'blue', add = TRUE)
curve(pnorm(x, 2, 1), xlim = c(-3, 5), col = 'red', add = TRUE)
legend("topleft", c("N(0,1)", "N(1,1)", "N(2,1)"),
       fill = c("black", "blue", "red"), cex = 0.5)
title("Dystrybuanta")

# funkcja przeżycia - ogon dystrybuanty
curve(1 - pnorm(x, 0, 1), from = -3, to = 5)
curve(1 - pnorm(x, 1, 1), from = -3, to = 5, col = 'blue', add = TRUE)
curve(1 - pnorm(x, 2, 1), from = -3, to = 5, col = 'red', add = TRUE)
legend ("topright", c("N(0,1)","N(1,1)","N(2,1)"),
        fill=c("black","blue", "red"), cex = 0.5)
title("Funkcja przeżycia")

# b) N(0, 1), N(0, 0.5), N(0, 2)

curve(dnorm(x, 0, 1), from = -6, to = 5, ylim = c(0,1))
curve(dnorm(x, 0, 0.5), from = -3, to = 5, col = 'blue', add = TRUE)
curve(dnorm(x, 0, 2), from = -6, to = 5, col = 'red', add = TRUE)
legend ("topright", c("N(0,1)","N(0,0.5)","N(0,2)"),
        fill=c("black","blue", "red"), cex = 0.5)
title("Gęstości")

curve(pnorm(x, 0, 1), xlim = c(-5, 5), ylim = c(0,1), col = 1)
curve(pnorm(x, 0, 0.5), col = 'blue', add = TRUE)
curve(pnorm(x, 0, 2), col = 'red', add = TRUE)
legend ("topright", c("N(0,1)","N(0,0.5)","N(0,2)"),
        fill=c("black","blue", "red"), cex = 0.5)
title("Dystrybuanta")

curve(1 - pnorm(x, 0, 1), xlim = c(-3, 5), ylim = c(0, 1))
curve(1 - pnorm(x, 0, 0.5), xlim = c(-3, 5), col = 'blue', add = TRUE)
curve(1 - pnorm(x, 0, 2), xlim = c(-3, 5), col = 'red', add = TRUE)
legend ("topright", c("N(0,1)","N(0,0.5)","N(0,2)"),
        fill=c("black","blue", "red"), cex = 0.5)
title("Funkcja przeżycia")


# Zadanie 2 ---------------------------------------------------------------

pnorm(3, 0, 1) - pnorm(-3, 0, 1)

# graficznie 
curve(dnorm(x), -4,4)
abline(v=-3, col="red")
abline(v=3,col="red")


# Zadanie 3 ---------------------------------------------------------------

# a) 
pnorm(179, 173, 6)

# b)
pnorm(180, 173, 6) - pnorm(167, 173, 6)

# c)
1 - pnorm(181, 173, 6)

# d)
# 60% jest mniejszych
qnorm(0.6, 173, 6)


# Zadanie 4 ---------------------------------------------------------------

# a)
qnorm(0.95, 0, 1)

# b)
qnorm(0.975, 0, 1)

# c)
qt(0.95, 10)

# d)
qt(0.99, 20)

# e)
qchisq(0.9, 4)

# f)
qchisq(0.95, 10)

# g)
qf(0.95, 2, 10)

# h)
qf(0.99, 3, 18)


# Zadanie 5 ---------------------------------------------------------------

# a)
curve(dgamma(x, 1, 1), xlim = c(0, 6), ylim = c(0, 1))
curve(dgamma(x, 0.5, 1), xlim = c(0, 6), col = 'blue', add = TRUE)
curve(dgamma(x, 2, 1), xlim = c(0, 6), col = 'red', add = TRUE)
curve(dgamma(x, 3, 1), xlim = c(0, 6), col = 'green', add = TRUE)
legend ("topright", c("Ga(1,1)","Ga(0.5,1)","Ga(2,1)", "Ga(3,1)"),
        fill=c("black","blue","red", 'green'), cex = 0.5)

# b) 
curve(dgamma(x, 2, 1), xlim = c(0, 6), ylim = c(0, 1.2))
curve(dgamma(x, 2, 2), xlim = c(0, 6), col = 'blue', add = TRUE)
curve(dgamma(x, 2, 3), xlim = c(0, 6), col = 'red', add = TRUE)
legend ("topright", c("Ga(2,1)","Ga(2,2)","Ga(2,3)"),
        fill=c("black","blue","red"), cex = 0.5)

# Zadanie 6 ---------------------------------------------------------------

# a)
curve(dchisq(x, 5), xlim = c(0, 80), ylim = c(0, 0.2))
curve(dchisq(x, 10), xlim = c(0, 80), col = 'blue', add = TRUE)
curve(dchisq(x, 40), xlim = c(0, 80), col = 'red', add = TRUE)
legend ("topright", c("Chi^2(5)","Chi^2(10)","Chi^2(40)"),
        fill=c("black","blue","red"), cex = 0.5)
# b)
curve(dchisq(x,5),from=0,to=150,ylab="y")
curve(dnorm(x,5,sqrt(2*5)),from=0,to=150,col="red",add=T)

curve(dchisq(x,100),from=0,to=200,col="green")
curve(dnorm(x,100,sqrt(2*100)),from=0,to=150,col="red",add=T)

curve(dchisq(x,1000),from=0,to=2000)
curve(dnorm(x,1000,sqrt(2*1000)),from=0,to=2000,col="red",add=T)


# Zadanie 7 ---------------------------------------------------------------

curve(dt(x, 1), xlim = c(-5, 5), ylim = c(0, 0.4))
curve(dt(x, 5), col = 'blue', add = TRUE)
curve(dt(x, 30), col = 'red', add = TRUE)
legend ("topright", c("t-Student(1)","t-Student(5)","t-Student(30)"),
        fill=c("black","blue","red"), cex = 0.5)

curve(dt(x, 5), xlim = c(-5, 5), ylim = c(0,0.4))
curve(dnorm(x, 0, 1), xlim = c(-5, 5), col = 'blue', add = TRUE)
legend ("topright", c("t-student(5)","N(0,1)"),
        fill=c("black","blue"), cex = 0.5)

curve(dt(x, 40), xlim = c(-5, 5), ylim = c(0,0.4))
curve(dnorm(x, 0, 1), xlim = c(-5, 5), col = 'blue', add = TRUE)
legend ("topright", c("t-student(40)","N(0,1)"),
        fill=c("black","blue"), cex = 0.5)


# Zadanie 8 ---------------------------------------------------------------

# a)
curve(df(x, 10, 5), ylim = c(0, 1), xlim = c(0, 3))
curve(df(x, 10, 10), col = 'blue', add = TRUE)
curve(df(x, 10, 20), col = 'red', add = TRUE)
legend ("topright", c("F-Sned(10,5)","F-Sned(10,10)", "F-Sned(10,20)"),
        fill=c("black","blue","red"), cex = 0.5)

# b)
curve(df(x, 5, 2), ylim = c(0, 1.1))
curve(df(x, 3, 2), col = 'blue', add = TRUE)
curve(df(x, 2, 2), col = 'red', add = TRUE)
legend ("topright", c("F-Sned(5,2)","F-Sned(3,2)", "F-Sned(2,2)"),
        fill=c("black","blue","red"), cex = 0.5)

# c)
curve(df(x, 2, 1), ylim = c(0, 1.1), xlim = c(0, 2))
curve(df(x, 2, 5), col = "blue", add = TRUE)
curve(df(x, 2, 10), col = "red", add = TRUE)
curve(df(x, 2, 20), col = "green", add = TRUE)
legend ("topright", c("F-Sned(2,1)","F-Sned(2,5)", "F-Sned(2,10)",
                      "F-Sned(2,20)"),
        fill=c("black","blue","red", "green"), cex = 0.5)



# Zadanie 9 ---------------------------------------------------------------

curve(dbeta(x, 1, 1), ylim = c(0, 3), xlim = c(0, 1))
curve(dbeta(x, 2, 2), col = "blue", add = TRUE)
curve(dbeta(x, 2, 5), col = "red", add = TRUE)
curve(dbeta(x, 5, 2), col = "green", add = TRUE)
legend ("topright", c("Beta(1,1)","Beta(2,2)", "Beta(2,5)",
                      "Beta(5,2)"),
        fill=c("black","blue","red", "green"), cex = 0.5)


# Zadanie 10 --------------------------------------------------------------

x <- 0:10
barplot(dbinom(x, 10, 0.5), ylim = c(0, 0.3), 
        names.arg = x)
barplot(dbinom(x, 10, 0.25), ylim = c(0, 0.3), names.arg = x)


# Zadanie 13 --------------------------------------------------------------

# a)
1 - pexp(1000, 0.0001)
1 - pexp(10000, 0.0001)
1 - pexp(30000, 0.0001)

# b)
qexp(0.1, 0.0001)























