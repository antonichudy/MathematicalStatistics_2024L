# Zadanie 6 ---------------------------------------------------------------

przed <- scan(nlines = 1)
27 21 34 24 30 27 33 31 22 27

po <- scan(nlines = 1)
29 32 29 27 31 26 35 30 29 28

alpha <- 0.05
var.test(przed, po)

### I sposób -------------------------------------------------------

# z_i = przed_i - po_i

# test na sprawdzenie czy rozkład jest normalny -> 
shapiro.test(przed - po)
# p-value = 0.504 > alpha
# nie ma podstaw do odrzucenia H0
# zakładamy, że rozkład jest normalny

# H0: mu_z = 0
# H1: mu_z < 0

t.test(przed - po, mu = 0, alternative = "less")
# p-value = 0.09322 > alpha -> nie ma podstaw do odrzucenia H0


### II sposób -------------------------------------------------------

# H: mu_przed = mu_po
# K: mu_przed < mu_po

# zbadamy czy rozklad normalny
shapiro.test(przed) # p-value = 0.6694 > alpha -> ok
shapiro.test(po) # p-value = 0.666 > alpha -> ok

# zakładamy, że rozkład "przed" i "po" jest normalny

t.test(przed, po, alternative = "less", paired = TRUE)
# p-value = 0.09322 > alpha -> nie ma podstaw do odrzucenia H0



# Zadanie 8 ---------------------------------------------------------------

# H0: p = 0.04
# H1: p > 0.04

p0 <- 0.04
p_d <- k/n
n <- 200
k <- 14
alpha <- 0.05

### I sposób -------------------------------------------------------------

# statystyka testowa
t <- (p_d - p0) / sqrt((p0 * (1 - p0))/n)
# 2.165064

# obszar krytyczny
qnorm(1 - alpha)
# [1.644854, Inf)
# Odp: t należy do obszaru krytycznego, zatem odrzucamy H0

### II sposób -------------------------------------------------------------

prop.test(k, n, p = 0.04, alt = "greater")
# p-value = 0.02359 < alpha -> odrzucamy H

binom.test(k, n, p = 0.04, alt = "greater")
# p-value = 0.03121 < alpha -> odrzucamy H



# Zadanie 9 ---------------------------------------------------------------

# H: p_tech = p_lic
# K: p_tech > p_lic


### I sposób ---------------------------------------------------------

k_tech <- 455
n_tech <- 700
p_d_tech <- k_tech / n_tech

k_lic <- 517
n_lic <- 1320
p_d_lic <- k_lic / n_lic

p <- (k_tech + k_lic) / (n_tech + n_lic)
n <- (n_tech * n_lic) / (n_tech + n_lic)

# statystyka testowa

t <- (p_d_tech - p_d_lic) / (sqrt((p * (1 - p)) / n))

# 11.05804

# obszar krytyczny

qnorm(1 - alpha)

# [1.644854, Inf)

# Odp: t należy do obszaru krytycznego -> odrzucamy H0

### II sposób ---------------------------------------------------------

prop.test(c(455, 517), c(700, 1320), alt = "gr")

# p-value < 2.2e-16 < alpha -> odrzucamy H0


# Zadanie 10 ---------------------------------------

n=20
p0=0.2
alpha=0.02

k <- 0:n 
F1 <- pbinom(k,n, p0) 
F1
Fk <- data.frame(k,F1=round(F1, 10)) 
Fk
Fk[8:9,]

# widac, ze
# F(7)=0.9678573<0.98<F(8)=0.9900182

# Zatem bierzemy c=8 i otrzymujemy test 
# fi(x1,...x20)=1 gdy T>8
# fi(x1,...x20)=0 gdy T<8

#rozmiar tego testu to P(T>c)= 1- F(c)
c <- 8
1 - pbinom(c,n, p0) 

# metoda symulacyjna
?rbinom
m=10000 
rbinom(n, 1, p0)
wyniki <- replicate(m, 
                    { 
                      x <- rbinom(n, 1, p0) #n-eltowa proba z rozkl b(1,p0)
                      Te <- sum(x)
                      Te > c
                    }) 

rozmiar <- sum(wyniki)/m
rozmiar


#randomizacja
# fi(x1,...x20)=1 gdy T>8
# fi(x1,...x20)=gamma gdy T=8
# fi(x1,...x20)=0 gdy T<8

gamma=(alpha-(1- pbinom(c,n, p0)))/dbinom(c,n, p0)
gamma

m=10000 
wyniki=replicate(m, 
                 { 
                   x=rbinom(n, 1, p0) #n-eltowa proba z rozk?adu b(1,p0)
                   Te2=sum(x)
                   (Te2 > c) | (Te2==c & runif(1)<gamma)
                 }) 

rozmiar=sum(wyniki)/m
rozmiar


# Zadanie 12 -----------------------------------------

data(Loblolly)
head(Loblolly)

# H0: mu = 40
# H1: mu != 40

x <- Loblolly$height[Loblolly$age==15]
t.test(x, mu=40)

p <- t.test(x, mu=40)$p.value
p

# 
alpha=0.05
p < 0.05
# pvalue wieksze od alpha zatem brak podstaw do odrzucenia H0









