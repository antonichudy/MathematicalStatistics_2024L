
# Zadanie 6 ---------------------------------------------------------------

samochody <- read.csv2("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=samochody.csv")

samochody$zp <- 1 / samochody$mpg * 3.7851 / 1.609 * 100 

# b)
stem(samochody$zp)

# c)
hist(samochody$zp)

# d)

zp2 <- na.omit(samochody$zp)
hist(samochody$zp)
density(zp2, kernel = "epanechnikov")

plot(density(zp2, kernel = "epanechnikov"))
hist(samochody$zp, breaks = 37)
plot(density(zp2, kernel = "epanechnikov", bw = 0.1)) # wspolczynnik wygladzajacy


plot(density(zp2, kernel = "rectangular"))
plot(density(zp2, kernel = "triangular"))


# e)
boxplot(samochody$zp)

# f)

mean(samochody$zp, na.rm = TRUE)
median(samochody$zp, na.rm = TRUE)
var(samochody$zp, na.rm = TRUE)
sd(samochody$zp, na.rm = TRUE)
diff(range(samochody$zp, na.rm = TRUE))
IQR(samochody$zp, na.rm = TRUE)

library(moments)
kurtosis(samochody$zp, na.rm = TRUE)
skewness(samochody$zp, na.rm = TRUE)

# g)
quantile(samochody$zp, c(0.05, 0.1, 0.9, 0.95), na.rm = TRUE)

# h)
mean(samochody$zp, na.rm = TRUE, trim = 0.05)


# Zadanie 7 ---------------------------------------------------------------

x <- samochody$zp

malo <- x[x <= 7]
malo
srednio <- x[x > 7 & x <= 10]
duzo <- x[x > 10]

kat <- c("malo", "srednio", "duzo")
licznosci <- c(length(malo), length(srednio), length(duzo))
licznosci
barplot(licznosci, names = kat)

100*licznosci/sum(licznosci)

# 2 sposob kodowania
x.kod <- cut(x,breaks=c(-Inf,7,10,Inf))
head(x, 10)
x.kod=cut(x,breaks=c(-Inf,7,10,Inf),labels=c("malo", "srednio", "duzo"))
t=table(x.kod)
barplot(t)


# Zadanie 8 ---------------------------------------------------------------

boxplot(samochody$zp~samochody$producent,horizontal=T,names=c("A","E","J"))

zpA <- samochody$zp[samochody$producent == 1]
zpE <- samochody$zp[samochody$producent == 2]
zpJ <- samochody$zp[samochody$producent == 3]

boxplot(zpA, zpE, zpJ, horizontal = F, names = c("A", "E", "J"))

# Zadanie 9 ---------------------------------------------------------------

boxplot(samochody$zp)
boxplot(samochody$zp ~ samochody$cylindry)


# Zadanie 10 --------------------------------------------------------------

zp10 <- samochody$zp[samochody$waga < 2500]
mean(zp10)
median(zp10)
sd(zp10)
library(moments)
library(dplyr)

skewness(zp10)
samochody %>% 
  filter(waga < 2500) %>% 
  summarise(srednia = mean(zp),
            mediana = median(zp),
            sd = sd(zp),
            skewness = skewness(zp))


# Zadanie 11 --------------------------------------------------------------

# a)
df <- samochody[samochody$rok >= 79 & samochody$rok <= 81, 'moc']
summary(df)
b <- boxplot(df)
b$out

# b)
quantile(df, 0.95, na.rm = T)

# Zadanie 12 --------------------------------------------------------------

# a)
df <- samochody$przysp[samochody$waga > 2500 & samochody$waga < 3000]
boxplot(df)

# b)
quantile(df, 0.75)


# Zadanie 14 --------------------------------------------------------------

boxplot(samochody$przysp~samochody$prod)

przyspA=samochody$przysp[samochody$producent==1]
przyspJ=samochody$przysp[samochody$producent==3]
boxplot(przyspA, przyspJ, names = c("A", "J"))


# Lista 3


# Zadanie 1 ----------------------------------------------------------------

x1 <- rnorm(20)
hist(x)
x2 <- rnorm(200)
plot(ecdf(x1))
plot(ecdf(x2), add = T, col = 2)
curve(pnorm(x), add = T, lw = 3, col = 3)


# Zadanie 2 ----------------------------------------------------------------

N <- 500
Y <- rcauchy(N)
Y

mediana <- c()
srednia <- c()

for (i in 1:N){
  mediana[i] <- median(Y[1:i])
  srednia[i] <- mean(Y[1:i])
}

plot(1:N, srednia, type = "l", col = 1)
lines(1:N, mediana, type = "l", col = 2)
abline(h = 0, col = 3)





