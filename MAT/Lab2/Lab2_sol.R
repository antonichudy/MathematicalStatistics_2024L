


# Zadanie 11 --------------------------------------------------------------

# dgeom ozn. rozklad geometryczny, w R zdefiniowany: P(Y=k)=p*(1-p)^k, k=0,1,...
# w zadaniu nosnik zaczyna sie od 1, P(X=k)=P(Y=k-1), dla k=1,2,...
# a) 

dgeom(0, 0.1)
dgeom(1, 0.1)
dgeom(2, 0.1)
dgeom(3, 0.1)

# b)
1 - pgeom(10, 0.1)

# Zadanie 12 --------------------------------------------------------------

phyper(0, 5, 195, 10)

N=200
M=5
n=10
k=0
choose(M,k)*choose(N-M,n-k)/choose(N,n)



# Zadanie 15 --------------------------------------------------------------

# a)

# Generujemy probe losowa U_1,V_1,...,U_n,V_n z rozkladu jednostajnego:
n<-10000
u<-runif(n)
v<-runif(n)

#Zaznaczmy te pkt na wykresie, dodajemy wykres funkcji y=x^2 dla 0<x<1
plot(u,v,xlim=c(0,1),ylim=c(0,1))
curve(x*x, col="red", type="l", xlim=c(0,1), add=T, lwd=3)

#Zliczamy pkt z naszej probki, ktore sa pod wykresem funkcji y=x^2:
u
v
u<v*v
number <- sum(u<v*v) 
number
pole<-number/n
# przyblizenie numeryczne całki
pole


### ---------------------------- LISTA 2 ----------------------------------

# Zadanie 1 ---------------------------------------------------------------

kobiety <- c(17364, 56128, 11239, 8170)
stan <- c("panny", "mężatki", "wdowy", "rozwódki")

# a)

pie(kobiety)
pie(kobiety, labels = stan)
pie(kobiety, paste(stan, kobiety, sep = "\n"), col = c(2, 3, 4, 6))

# b)

barplot(kobiety)
barplot(kobiety, names.arg = stan)
barplot(kobiety, names.arg = paste(stan, kobiety, sep = "\n"))


# Zadanie 2 ---------------------------------------------------------------

dane <- read.csv("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=stacje.csv")


table(dane)
prop.table(table(dane)) 

pie(table(dane))
barplot(table(dane))

# Zadanie 3 ---------------------------------------------------------------

notowania <- c(23.30, 24.50, 25.30, 25.30, 24.30, 24.80, 
               25.20, 24.50, 24.60, 24.10, 24.30, 26.10, 
               23.10, 25.50, 22.60, 24.60, 24.30, 25.40, 
               25.20, 26.80)

plot(1:20, notowania, type = "l")

## 2 sposob
x=scan(nlines=2)
23.30     24.50     25.30     25.30     24.30     24.80     25.20     24.50     24.60     24.10  
24.30     26.10     23.10     25.50     22.60     24.60     24.30     25.40     25.20     26.80 

x
plot(1:20,x)
plot(x)
plot(x,type="l")
plot(x,type="b")

# Zadanie 4 ---------------------------------------------------------------

butelki <- read.csv("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=butelki.csv")
butelki$strength

# a) 
cisnienie <- butelki$strength * 0.0068947
butelki$cisnienie=butelki$strength * 0.0068947

# b) 
hist(cisnienie)
hist(cisnienie, breaks = 6)
hist(cisnienie, breaks = 14)
hist(cisnienie,prob=T) 


# c)
h <- hist(cisnienie, breaks = 14)
h$mids #srodki klas
h$counts #licznosci klas
lines(h$mids, h$counts, col = 3, lwd = 3)

# d)

stem(cisnienie)
stem(cisnienie, 2)

# e)

boxplot(cisnienie)
summary(cisnienie)
# Q_3+1.5*IQR
# Q_1-1.5*IQR

# f)

var(cisnienie)
sd(cisnienie)
IQR(cisnienie)
diff(range(cisnienie))  

#install.packages("moments")
library(moments)
skewness(cisnienie)
kurtosis(cisnienie)

cv=sd(cisnienie)/abs(mean(cisnienie))
cv

# g)

quantile(cisnienie, 0.05)
quantile(cisnienie, 0.1)
quantile(cisnienie, 0.25)
quantile(cisnienie, 0.5)
quantile(cisnienie, 0.75)
quantile(cisnienie, 0.9)
quantile(cisnienie, 0.95)

# h)

mean(cisnienie, trim = 0.1)
mean(cisnienie)
median(cisnienie)

lapply(0:49 / 100, function(x) {
  mean(cisnienie, trim = x)
}) -> trim_means

plot(1:length(trim_means), trim_means)




