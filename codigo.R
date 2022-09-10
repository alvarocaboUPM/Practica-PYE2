Data <- read.csv(file=paste('PYE2DataSet62.csv',sep=), header=TRUE)


#####################################                                      APARTADo 1                                      #####################################  

#Data$sleeptime
set.seed(2022)

# RESUMEN DE LA VARIABLE # El resumen constata que el mínimo valor de la variable tiempo de sueño es 0.7409,
# el máximo 22.0834, el primer cuartil 5.684 y el tercer cuartil 12.6437. La mediana es 8.5268 y la media 9.4383.
summary(Data$sleeptime)

hist(Data$sleeptime, main = "Histograma de sleeptime", xlab = "Tiempo de sueño", ylab = "Frecuencia")

# El diagrama de caja y bigotes aplica lo resumido en la función summary.
# Los extremos del bigote corresponden a los valores mínimo y máximo; los extremos de la caja,
# el primer y tercer cuartil; y la línea central gruesa la media.
boxplot(Data$sleeptime)

# La función skewness calcula el coeficiente de asimetría de la distribución. Como es > 0, la distribución es asimétrica a la derecha
library(e1071)
skewness(Data$sleeptime)
# La función kurtosis calcula el grado de apuntamiento de una curva de distribución. Como el valor es < 0, es platicúrtica, es decir, aplanada.
kurtosis(Data$sleeptime)

#Data$Steps
set.seed(2022)
# RESUMEN DE LA VARIABLE # El resumen constata que el mínimo valor de la variable pasos es 6757, el máximo 12468, el primer cuartil 9445 y el tercer cuartil 11534 La mediana es 9759 y la media 10056.
summary(Data$steps)
hist(Data$steps, main = "Histograma de pasos", xlab = "Pasos", ylab = "Frecuencia")
# El diagrama de caja y bigotes aplica lo resumido en la función summary. Los extremos del bigote corresponden a los valores mínimo y máximo; los extremos de la caja, el primer y tercer cuartil; y la línea gruesa la media.
boxplot(Data$steps)
library(e1071)
#El coeficiente de asimetría es < 0, por tanto la distribución es asimétrica a la izquierda
skewness(Data$steps)
# El grado de apuntamiento es < 0, por lo tanto es platicúrtica (aplanada)
kurtosis(Data$steps)

##Estimadores

set.seed(2022)
library(MASS)
fitdistr(Data$sleeptime,c("normal"))
fitdistr(Data$sleeptime,c("gamma"),lower=c(0,0))
fitdistr(Data$sleeptime,c("exponential"))

#Histogramas De los datos obtenidos mediante fitdistr
set.seed(2022)
hist(rnorm(n = 10000, mean = 9.43834229,sd = 4.72768808), breaks = 20, main = "Distribución normal de Sleeptime")
hist(rgamma(n = 10000, shape = 3.737861003, rate = 0.396029549 ), breaks = 20, main = "Distribución gamma de Sleeptime")
hist(rexp(n=10000,rate=0.105950809 ), breaks = 20, main = "Distribución exponencial de Sleeptime")


#Test de Kolmogorov-Smirnov

set.seed(2022)
ks.test(Data$sleeptime, rnorm(n = 10000, mean = 9.07435097,sd = 4.56415104))
ks.test(Data$sleeptime, rgamma(n = 10000, shape = 3.735498332, rate = 0.411654761))
ks.test(Data$sleeptime, rexp(n=10000,rate=0.110200719))

##MUESTREO
# Muestras de tamaño 20 para Data$Age

set.seed(2022)
muestra20 <- sample(Data$Age, 20)
muestra20

#Media muestral, varianza y proporciónn M/V de 30 muestras de tamaño 20

set.seed(2022)
library(MASS)

#Inicializaremos 3 vectores vacios: uno para almacenar 30 medias de las 30 muestras de tamaño 20, otro para 30 varianzas y el último para 30 proporciones.
# Inicializamos también un contador para que un bucle almacene y calcule los elementos.

i <- 1
vector30me <- vector(mode = "integer", length = 30)
vector30va <- vector(mode = "integer", length = 30)
vector30pr <- vector(mode = "integer", length = 30)

while (i<31){

  muestraaux <- Data[sample(1:nrow(Data),20),]
  vector30me[i] <- mean(muestraaux$Age)
  vector30va[i] <- var(muestraaux$Age)
  vector30pr[i] <- sum(muestraaux$Sex == 'M')/sum(muestraaux$Sex == 'V')
  i <- i+1 
  
}

# Vector con las medias de las 30 muestras
vector30me
hist(vector30me, main = "Histograma de medias de 30 muestras de edad de tamaño 20")
boxplot(vector30me)
# Los datos se distribuyen como una distribución normal de media 32.078949003
#y desviación típica 0.23457134 . Lo representamos gráficamente.
fitdistr(vector30me,"normal")
hist(rnorm (n = 30, mean = 32.07894900, sd = 0.23457134 ), breaks = 20, main = "Distribución normal de 30 medias de 30 muestras de edad de tamaño 20")

# Vector con las varianzas de las 30 muestras
vector30va
hist(vector30va, main = "Histograma de varianzas de 30 muestras de edad de tamaño 20")
boxplot(vector30va)
# Los datos se distribuyen como una distribución normal de media 10.6304976 y desviación típica 1.1112192 .
# Lo representamos gráficamente.
fitdistr(vector30va,"normal")
hist(rnorm (n = 30, mean = 10.6304976 , sd = 1.1112192 ), breaks = 20, main = "Distribución normal de 30 varianzas de 30 muestras de edad de tamaño 20")

# Vector de 30 proporciones Mujeres/Varones 
vector30pr
hist(vector30pr, main = "Histograma de proporciones de 30 muestras de edad de tamaño 20")
boxplot(vector30pr)
# Los datos se distribuyen como una distribución normal de media 1.06314700 y desviación típica 0.14930377 .
#Lo representamos gráficamente.
fitdistr(vector30pr, "normal")
hist(rnorm (n = 30, mean = 1.06314700 , sd = 0.14930377 ), breaks = 20, main = "Dist.normal de 30 prop. M/V de 30 muestras de tamaño 20")


#Media muestral, varianza y proporción M/V de 50 muestras de tamaño 20

set.seed(2022)
j <- 1 
vector50me <- vector(mode = "integer", length = 50)
vector50va <- vector(mode = "integer", length = 50)
vector50pr <- vector(mode = "integer", length = 50) 
while (j<51){ 
  
  muestraaux <- Data[sample(1:nrow(Data),20),]
  vector50me[j] <- mean(muestraaux$Age) 
  vector50va[j] <- var(muestraaux$Age) 
  vector50pr[j] <- sum(muestraaux$Sex == 'M')/sum(muestraaux$Sex == 'V') 
  j <- j+1 
  
} 
vector50me
hist(vector50me, main = "Histograma de medias de 50 muestras de edad de tamaño 20")
boxplot(vector50me)
fitdistr(vector50me,"normal")
hist(rnorm (n = 50, mean = 32.08115855 , sd = 0.22136250 ), breaks = 20, main = "Distribución normal de medias de 50 muestras de edad de tamaño 20")

vector50va
hist(vector50va, main = "Histograma de varianzas de 50 muestras de edad de tamaño 20")
boxplot(vector50va)
fitdistr(vector50va,"normal")
hist(rnorm (n = 30, mean = 10.6500481 , sd = 1.0796792 ), breaks = 20, main = "Distribución normal de varianzas de 50 muestras de edad de tamaño 20")

vector50pr
hist(vector50pr, main = "Histograma de proporciones M/V de 50 muestras de edad de tamaño 20")
boxplot(vector50pr)
fitdistr(vector50pr, "normal")
hist(rnorm (n = 30, mean = 1.05403349 , sd = 0.14752282 ), breaks = 20, main = "Dist.norm de prop. M/V de 50 muestras de tamaño 20")


#Media muestral, varianza y proporción M/V de 100 muestras de tamaño 20

set.seed(2022)
k <- 1 
vector100me <- vector(mode = "integer", length = 100)
vector100va <- vector(mode = "integer", length = 100)
vector100pr <- vector(mode = "integer", length = 100)

while (k<101){ 
  
  muestraaux <- Data[sample(1:nrow(Data),20),]
  vector100me[k] <- mean(muestraaux$Age)
  vector100va[k] <- var(muestraaux$Age)
  vector100pr[k] <- sum(muestraaux$Sex == 'M')/sum(muestraaux$Sex == 'V')
  k <- k+1 
  
}

vector100me
hist(vector100me, main = "Histograma de medias de 100 muestras de edad de tamaño 200")
boxplot(vector100me)
fitdistr(vector100me,"normal")
hist(rnorm (n = 100, mean = 32.11404420 , sd = 0.23874507 ), breaks = 50, main = "Distribución normal de medias de 100 muestras de edad de tamaño 20")

vector100va
hist(vector100va, main = "Histograma de varianzas de 100 muestras de edad de tamaño 200")
boxplot(vector100va)
fitdistr(vector100va,"normal")
hist(rnorm (n = 30, mean = 10.6450621 , sd = 1.0411270 ), breaks = 30, main = "Distribución normal de varianzas de 100 muestras de edad de tamaño 20")

vector100pr
hist(vector100pr, main = "Histograma de prop. M/V de 100 muestras de edad de tamaño 200")
boxplot(vector100pr)
fitdistr(vector100pr,"normal")
hist(rnorm (n = 30, mean = 1.07253535 , sd = 0.14536607 ), breaks = 30, main = "Dist. normal de prop. M/V de 100 muestras de tamaño 20")

#####################################                                      APARTADo 2                                      #####################################   
set.seed(2022)
# Media de tiempo de sueño
mean(Data$sleeptime) 
# Varianza de tiempo de sueño
var(Data$sleeptime) 
# Media de pasos 
mean(Data$steps) 
# Varianza de pasos 
var(Data$steps)


set.seed(2022)

# Generamos una muestra aleatoria de tamaño 20 
muestra20a <- Data[sample(1:nrow(Data),20),]
# Calculamos los mismos parámetros que el apartado anterior 
# Media y varianza de tiempo de sueño
mean(muestra20a$sleeptime) 
var(muestra20a$sleeptime) 
# Media y varianza de pasos 
mean(muestra20a$steps) 
var(muestra20a$steps)





#2.a.1.- Estimar media y varianza de sleeptime y steps
#Con todos los datos
set.seed(2022)
# Media de tiempo de sueño
mean(Data$sleeptime)
## [1] 9.438342
# Varianza de tiempo de sueño
var(Data$sleeptime)
## [1] 22.35327
# Media de pasos
mean(Data$steps)
## [1] 10055.64
# Varianza de pasos
var(Data$steps)

#Con una muestra de tamaño 20
set.seed(2022)
# Generamos una muestra aleatoria de tamaño 20
muestra20a <- Data[sample(1:nrow(Data),20),]
# Calculamos los mismos parámetros que el apartado anterior
# Media y varianza de tiempo de sueño
mean(muestra20a$sleeptime)

var(muestra20a$sleeptime)
# Media y varianza de pasos
mean(muestra20a$steps)
var(muestra20a$steps)

#2.a.2.- Estimar media y varianza de sleeptime y steps entre mujeres
#Con todos los datos

set.seed(2022)
# Seleccionamos únicamente las mujeres
mujeres <- Data[Data$Sex =='M',]
# Media y varianza de tiempo de sueño
mean(mujeres$sleeptime)
var(mujeres$sleeptime)
# Media y varianza de pasos
mean(mujeres$steps)
var(mujeres$steps)

#Con una muestra de tamaño 200
set.seed(2022)
# Seleccionamos una muestra de solo mujeres
muestra20b <- mujeres[sample(1:nrow(mujeres),20),]
# Media y varianza de tiempo de sueÃ±o
mean(muestra20b$sleeptime)
var(muestra20b$sleeptime)
# Media y varianza de pasos
mean(muestra20b$steps)
var(muestra20b$steps)

#2.a.3.- Estimar media y varianza de sleeptime y steps entre hombresCon todos los datos
set.seed (2022)
#Ahora seleccionamos solo a los hombres
hombres <- Data[Data$Sex =='V',]
# Media y varianza de tiempo de sueño
mean(hombres$sleeptime)
var(hombres$sleeptime)
# Media y varianza de pasos
mean(hombres$steps)
var(hombres$steps)

#Con una muestra de tamaño 200
set.seed(2022)
# Seleccionamos una muestra de solo hombres
muestra20c <- hombres[sample(1:nrow(hombres),200),]
# Media y varianza de tiempo de sueño
mean(muestra20c$sleeptime)
var(muestra20c$sleeptime)
# Media y varianza de pasos
mean(muestra20c$steps)
var(muestra20c$steps)


#2.b.1.- Estimar intervalos de confianza al 0.9, 0.95 y 0.99. Media, varianza y proporción.
#NORMALIDAD
#Para media, hombres y mujeres.
SLEEPTIME
set.seed(2022)
library(stats)
# Sleeptime, mujeres, al 90% de confianza
t.test(x = muestra20b$sleeptime, conf.level = 0.9)

# Sleeptime, mujeres, al 95% de confianza
t.test(x = muestra20b$sleeptime, conf.level = 0.95)

# Sleeptime, mujeres, al 50% de confianza
t.test(x = muestra20b$sleeptime, conf.level = 0.99)

# Sleeptime, hombres, al 90% de confianza
t.test(x = muestra20c$sleeptime, conf.level = 0.9)

# Sleeptime, hombres, al 95% de confianza
t.test(x = muestra20c$sleeptime, conf.level = 0.95)

# Sleeptime, hombres, al 99% de confianza
t.test(x = muestra20c$sleeptime, conf.level = 0.99)

#STEPS
# Steps, mujeres, al 90% de confianza
t.test(x = muestra20b$steps, conf.level = 0.9)

# Steps, mujeres, al 95% de confianza
t.test(x = muestra20b$steps, conf.level = 0.95)

# Steps, mujeres, al 99% de confianza
t.test(x = muestra20b$steps, conf.level = 0.99)

# Steps, hombres, al 90% de confianza
t.test(x = muestra20c$steps, conf.level = 0.9)

# Steps, hombres, al 95% de confianza
t.test(x = muestra20c$steps, conf.level = 0.95)

# Steps, hombres, al 99% de confianza
t.test(x = muestra20c$steps, conf.level = 0.99)

#ara varianza. Hombres y mujeres.
#SLEEPTIME
set.seed(2022)
## Mujeres y hombres en 0.9
library(rcompanion)
Data$a <- (muestra20b$sleeptime - mean(muestra20b$sleeptime))^2
ic.sleeptime <- groupwiseMean(a ~ 1, data = Data, conf = 0.9, digits = 3)
ic.sleeptime

Data$b <- (muestra20c$sleeptime - mean(muestra20c$sleeptime))^2
ic.sleeptime <- groupwiseMean(b ~ 1, data = Data, conf = 0.9, digits = 3)
ic.sleeptime

#Mujeres y hombres en 0.95
Data$c <- (muestra20b$sleeptime - mean(muestra20b$sleeptime))^2
ic.sleeptime <- groupwiseMean(c ~ 1, data = Data, conf = 0.95, digits = 3)
ic.sleeptime

Data$d <- (muestra20c$sleeptime - mean(muestra20c$sleeptime))^2
ic.sleeptime <- groupwiseMean(d ~ 1, data = Data, conf = 0.95, digits = 3)
ic.sleeptime

#Mujeres y hombres en 0.99
Data$e <- (muestra20b$sleeptime - mean(muestra20b$sleeptime))^2
ic.sleeptime <- groupwiseMean(e ~ 1, data = Data, conf = 0.99, digits = 3)
ic.sleeptime

Data$f <- (muestra20c$sleeptime - mean(muestra20c$sleeptime))^2
ic.sleeptime <- groupwiseMean(f ~ 1, data = Data, conf = 0.99, digits = 3)
ic.sleeptime

#STEPS
set.seed(2022)
## Mujeres y hombres en 0.9
library(rcompanion)
Data$a <- (muestra20b$steps - mean(muestra20b$steps))^2
ic.steps <- groupwiseMean(a ~ 1, data = Data, conf = 0.9, digits = 3)

Data$b <- (muestra20c$steps - mean(muestra20c$steps))^2
ic.steps <- groupwiseMean(b ~ 1, data = Data, conf = 0.9, digits = 3)
ic.steps

#Mujeres y hombres en 0.95
Data$c <- (muestra20b$steps - mean(muestra20b$steps))^2
ic.steps <- groupwiseMean(c ~ 1, data = Data, conf = 0.95, digits = 3)
ic.steps

Data$d <- (muestra20c$steps - mean(muestra20c$steps))^2
ic.steps <- groupwiseMean(d ~ 1, data = Data, conf = 0.95, digits = 3)
ic.steps
#
#Mujeres y hombres en 0.99
Data$e <- (muestra20b$steps - mean(muestra20b$steps))^2
ic.steps <- groupwiseMean(e ~ 1, data = Data, conf = 0.99, digits = 3)
ic.steps

Data$f <- (muestra20c$steps - mean(muestra20c$steps))^2
ic.steps <- groupwiseMean(f ~ 1, data = Data, conf = 0.99, digits = 3)
ic.steps

#Para proporciones. Hombres y mujeres.
#SLEEPTIME
set.seed(2022)
# En horasTotales guardamos el valor del total de las horas dormidas en
#la muestra
horasTotales <- sum(muestra20a$sleeptime)
# En muj1 y homb1 separamos los hombres y mujeres de la muestra
muj1 <- muestra20a[muestra20a$Sex=="M",]
hom1 <- muestra20a[muestra20a$Sex=="V",]
nrow(muj1)

nrow(hom1)
# Guardamos la suma de las horas de sueño por separado que duermen hombres y mujeres
a <-sum(muj1$sleeptime)
b <-sum(hom1$sleeptime)
#La función prop.test calcula intervalos de confianza en proporciones
library(DescTools)
# Proporción sleeptime_mujeres/sleeptime_totales con confianza 90%
prop.test(a,horasTotales, conf.level = 0.9)

# Proporción sleeptime_mujeres/sleeptime_totales con confianza 95%
prop.test(a,horasTotales, conf.level = 0.95)

# Proporción sleeptime_mujeres/sleeptime_totales con confianza 99%
prop.test(a,horasTotales, conf.level = 0.99)

# Proporción sleeptime_hombres/sleeptime_totales con confianza 90%
prop.test(b,horasTotales, conf.level = 0.9)

# Proporción sleeptime_hombres/sleeptime_totales con confianza 95%
prop.test(b,horasTotales, conf.level = 0.95)

# Proporción sleeptime_hombres/sleeptime_totales con confianza 99%
prop.test(b,horasTotales, conf.level = 0.99)

#STEPS
set.seed(2022)
# En pasosTotales guardamos el valor del total de los pasos dados en la muestra
pasosTotales <- sum(muestra20a$steps)
# En muj2 y homb2 separamos los hombres y mujeres de la muestra
muj2 <- muestra20a[muestra20a$Sex=="M",]
hom2 <- muestra20a[muestra20a$Sex=="V",]
nrow(muj2)

nrow(hom2)

# Guardamos la suma de los pasos por separado que dan hombres y mujeres
c <-sum(muj2$steps)
d <-sum(hom2$steps)
# Proporción pasos_mujeres/pasosTotales con confianza 90%
prop.test(c,pasosTotales, conf.level = 0.9)

# Proporción pasos_mujeres/pasosTotales con confianza 95%
prop.test(c,pasosTotales, conf.level = 0.95)

# Proporción pasos_mujeres/pasosTotales con confianza 99%
prop.test(c,pasosTotales, conf.level = 0.99)

# Proporción pasos_hombres/pasosTotales con confianza 90%
prop.test(d,pasosTotales, conf.level = 0.9)

# Proporción pasos_hombres/pasosTotales con confianza 95%
prop.test(d,pasosTotales, conf.level = 0.95)

# Proporción pasos_hombres/pasosTotales con confianza 99%
prop.test(d,pasosTotales, conf.level = 0.99)

#BOOTSTRAP
#Para media.
#SLEEPTIME
set.seed(2022)
# Nivel de confianza del 90%
library(boot)
Mboot = boot(Data$sleeptime, function(x,i) mean(x[i]), R=1000)
mean(Mboot$t[,1])

boot.ci(Mboot, conf = 0.9, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])


set.seed(2022)
# Nivel de confianza del 95%
library(boot)
Mboot = boot(Data$sleeptime, function(x,i) mean(x[i]), R=1000)
mean(Mboot$t[,1])

boot.ci(Mboot, conf = 0.95, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])

set.seed(2022)
# Nivel de confianza del 99%
library(boot)
Mboot = boot(Data$sleeptime, function(x,i) mean(x[i]), R=1000)
mean(Mboot$t[,1])

boot.ci(Mboot, conf = 0.99, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])

#STEPS
set.seed(2022)
# Nivel de confianza del 90%
library(boot)
Mboot = boot(Data$steps, function(x,i) mean(x[i]), R=1000)
mean(Mboot$t[,1])

boot.ci(Mboot, conf = 0.9, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])

set.seed(2022)
# Nivel de confianza del 95%
library(boot)
Mboot = boot(Data$steps, function(x,i) mean(x[i]), R=1000)
mean(Mboot$t[,1])

boot.ci(Mboot, conf = 0.95, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])

set.seed(2022)
# Nivel de confianza del 99%
library(boot)
Mboot = boot(Data$steps, function(x,i) mean(x[i]), R=1000)
mean(Mboot$t[,1])

boot.ci(Mboot, conf = 0.99, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])

#Para varianza.
#SLEEPTIME
set.seed(2022)
# Nivel de confianza del 90%
library(boot)
Mboot = boot(Data$sleeptime, function(x,i) var(x[i]), R=1000)
var(Mboot$t[,1])

boot.ci(Mboot, conf = 0.9, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])

set.seed(2022)
# Nivel de confianza del 95%
library(boot)
Mboot = boot(Data$sleeptime, function(x,i) var(x[i]), R=1000)
var(Mboot$t[,1])

boot.ci(Mboot, conf = 0.95, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])

set.seed(2022)
# Nivel de confianza del 99%
library(boot)
Mboot = boot(Data$sleeptime, function(x,i) var(x[i]), R=1000)
var(Mboot$t[,1])

boot.ci(Mboot, conf = 0.99, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])

set.seed(2022)
# Nivel de confianza del 90%
library(boot)
Mboot = boot(Data$steps, function(x,i) var(x[i]), R=1000)
var(Mboot$t[,1])

boot.ci(Mboot, conf = 0.9, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])
set.seed(2022)
# Nivel de confianza del 95%
library(boot)
Mboot = boot(Data$steps, function(x,i) var(x[i]), R=1000)
var(Mboot$t[,1])

boot.ci(Mboot, conf = 0.95, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])
set.seed(2022)
# Nivel de confianza del 99%
library(boot)
Mboot = boot(Data$steps, function(x,i) var(x[i]), R=1000)
var(Mboot$t[,1])

boot.ci(Mboot, conf = 0.99, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])
#2.b.2.- Estimar intervalos de confianza al 0.9, 0.95 y 0.99. Diferencia de medias, razón de
#varianzas y proporción.
#NORMALIDAD
#Para diferencias de medias entre H y M
#SLEEPTIME
set.seed(2022)
library(stats)
t.test(x = muestra20b$sleeptime, y = muestra20c$sleeptime, conf.level = 0.9)

## 10.321188 8.336065
t.test(x = muestra20b$sleeptime, y = muestra20c$sleeptime, conf.level = 0.95)

t.test(x = muestra20b$sleeptime, y = muestra20c$sleeptime, conf.level = 0.99)

#STEPS
set.seed(2022)
library(stats)
t.test(x = muestra20b$steps, y = muestra20c$steps, conf.level = 0.9)
t.test(x = muestra20b$steps, y = muestra20c$steps, conf.level = 0.95)
##
t.test(x = muestra20b$steps, y = muestra20c$steps, conf.level = 0.99)
##

#Para razón de varianzas entre H y M.
#SLEEPTIME
set.seed(2022)
var.test(x = muestra20b$sleeptime, y = muestra20c$sleeptime, conf.level = 0.9)
##
var.test(x = muestra20b$sleeptime, y = muestra20c$sleeptime, conf.level = 0.95)
##
var.test(x = muestra20b$sleeptime, y = muestra20c$sleeptime, conf.level = 0.99)
##
#STEPS
set.seed(2022)
var.test(x = muestra20b$steps, y = muestra20c$steps, conf.level = 0.9)
##
var.test(x = muestra20b$steps, y = muestra20c$steps, conf.level = 0.95)
##
var.test(x = muestra20b$steps, y = muestra20c$steps, conf.level = 0.99)
##
#BOOTSTRAP
#Para diferencias de medias entre H y M.
#SLEEPTIME
#Al 90% de confianza
set.seed(2022)
library(boot)
Mboot = boot(muestra20b$sleeptime, function(x,i) mean(x[i]), R=1000)
mean(Mboot$t[,1])

boot.ci(Mboot, conf = 0.9, type=c("norm","basic", "perc"))

Mboot
##


hist(Mboot$t[,1])
##############################
library(boot)
Mboot = boot(muestra20c$sleeptime, function(x,i) mean(x[i]), R=1000)
mean(Mboot$t[,1])

boot.ci(Mboot, conf = 0.9, type=c("norm","basic", "perc"))
Mboot
##

hist(Mboot$t[,1])


set.seed(2022)
#Al 95% de confianza
library(boot)
Mboot = boot(muestra20b$sleeptime, function(x,i) mean(x[i]), R=1000)
mean(Mboot$t[,1])

boot.ci(Mboot, conf = 0.95, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])
##############################
library(boot)
Mboot = boot(muestra20c$sleeptime, function(x,i) mean(x[i]), R=1000)
mean(Mboot$t[,1])

boot.ci(Mboot, conf = 0.95, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])

#Al 99% de confianza
set.seed(2022)
library(boot)
Mboot = boot(muestra20b$sleeptime, function(x,i) mean(x[i]), R=1000)
mean(Mboot$t[,1])

boot.ci(Mboot, conf = 0.99, type=c("norm","basic", "perc"))

Mboot
##
hist(Mboot$t[,1])
##############################
library(boot)
Mboot = boot(muestra20c$sleeptime, function(x,i) mean(x[i]), R=1000)
mean(Mboot$t[,1])

boot.ci(Mboot, conf = 0.99, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])
#STEPS
#Al 90% de confianza
set.seed(2022)
library(boot)
Mboot = boot(muestra20b$steps, function(x,i) mean(x[i]), R=1000)
mean(Mboot$t[,1])


boot.ci(Mboot, conf = 0.9, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])
##############################
library(boot)
Mboot = boot(muestra20c$steps, function(x,i) mean(x[i]), R=1000)
mean(Mboot$t[,1])

boot.ci(Mboot, conf = 0.9, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])
#Al 95% de confianza
set.seed(2022)
library(boot)
Mboot = boot(muestra20b$steps, function(x,i) mean(x[i]), R=1000)
mean(Mboot$t[,1])

boot.ci(Mboot, conf = 0.95, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])
##############################
library(boot)
Mboot = boot(muestra20c$steps, function(x,i) mean(x[i]), R=1000)
mean(Mboot$t[,1])

boot.ci(Mboot, conf = 0.95, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])
#Al 99% de confianza
set.seed(2022)
library(boot)
Mboot = boot(muestra20b$steps, function(x,i) mean(x[i]), R=1000)
mean(Mboot$t[,1])

boot.ci(Mboot, conf = 0.99, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])
##############################
library(boot)
Mboot = boot(muestra20c$steps, function(x,i) mean(x[i]), R=1000)
mean(Mboot$t[,1])

boot.ci(Mboot, conf = 0.99, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])
#Para razón de varianzas entre H y M.
#SLEEPTIME
#Al 90% de confianza
set.seed(2022)
library(boot)
Mboot = boot(muestra20b$sleeptime, function(x,i) var(x[i]), R=1000)
var(Mboot$t[,1])

boot.ci(Mboot, conf = 0.9, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])
##############################
library(boot)
Mboot = boot(muestra20c$sleeptime, function(x,i) var(x[i]), R=1000)
var(Mboot$t[,1])

boot.ci(Mboot, conf = 0.9, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])
#Al 95% de confianza
set.seed(2022)
library(boot)
Mboot = boot(muestra20b$sleeptime, function(x,i) var(x[i]), R=1000)
var(Mboot$t[,1])

boot.ci(Mboot, conf = 0.95, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])
##############################
library(boot)
Mboot = boot(muestra20c$sleeptime, function(x,i) var(x[i]), R=1000)
var(Mboot$t[,1])

boot.ci(Mboot, conf = 0.95, type=c("norm","basic", "perc"))

Mboot
##
hist(Mboot$t[,1])
#
#Al 99% de confianza
set.seed(2022)
library(boot)
Mboot = boot(muestra20b$sleeptime, function(x,i) var(x[i]), R=1000)
var(Mboot$t[,1])

boot.ci(Mboot, conf = 0.99, type=c("norm","basic", "perc"))

Mboot
##
hist(Mboot$t[,1])
##############################
library(boot)
Mboot = boot(muestra20c$sleeptime, function(x,i) var(x[i]), R=1000)
var(Mboot$t[,1])

boot.ci(Mboot, conf = 0.99, type=c("norm","basic", "perc"))

Mboot
##
hist(Mboot$t[,1])
#STEPS
#Al 90% de confianza
set.seed(2022)
library(boot)
Mboot = boot(muestra20b$steps, function(x,i) var(x[i]), R=1000)
var(Mboot$t[,1])

boot.ci(Mboot, conf = 0.9, type=c("norm","basic", "perc"))
Mboot
##
hist(Mboot$t[,1])
##############################
library(boot)
Mboot = boot(muestra20c$steps, function(x,i) var(x[i]), R=1000)
var(Mboot$t[,1])

boot.ci(Mboot, conf = 0.9, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])
#AL 95% de confianza
set.seed(2022)
library(boot)
Mboot = boot(muestra20b$steps, function(x,i) var(x[i]), R=1000)
var(Mboot$t[,1])

boot.ci(Mboot, conf = 0.95, type=c("norm","basic", "perc"))

Mboot
##
hist(Mboot$t[,1])
##############################
library(boot)
Mboot = boot(muestra20c$steps, function(x,i) var(x[i]), R=1000)
var(Mboot$t[,1])

boot.ci(Mboot, conf = 0.95, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])
#Al 99% de confianza
set.seed(2022)
library(boot)
Mboot = boot(muestra20b$steps, function(x,i) var(x[i]), R=1000)
var(Mboot$t[,1])

boot.ci(Mboot, conf = 0.99, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])
##############################
library(boot)
Mboot = boot(muestra20c$steps, function(x,i) var(x[i]), R=1000)
var(Mboot$t[,1])

boot.ci(Mboot, conf = 0.99, type=c("norm","basic", "perc"))

Mboot
##

hist(Mboot$t[,1])
#####################################                                      APARTADo 3                                      #####################################  
#3.- ESTIMACIÓN BAYESIANA
#Proporción de individuos de nacionalidad española (Pe) = 25-30%
#En una muestra n = 200 hay nE españoles
#Pe sigue una distribución beta con alfa= 5 y beta = 10
#3.1.- Obtener Pe tras la información aportada por la muestra (dist. a priori)
set.seed(2022)
#Generamos una muestra aleatoria de 200
muestra20 <- Data[sample(1:nrow(Data),200),]
#ne es el número de españoles y q el número de no-españoles
nE <- sum(muestra20$Nation == "SP")
nE
## [1] 63
q <- 200-nE
q
## [1] 137
alfa <- 5
beta <- 10
# La media a priori es
mediapriori <- alfa/(alfa+beta)
mediapriori

#Distribución a priori
#dapri <- z^(alfa-1) *(1-z)^(beta-1)
#funveros <- z^nE * (1-z)^q
#dapost <- z^(63+4) * (1-z)^(137+9) ~ beta(alfa=68, beta=147)
pe <- (nE+alfa)/((nE+alfa)+(q+beta))
pe

#3.2.- Obtener el IC con 95% de confianza para la Pe
set.seed(2022)
#Calculamos nivel de significación (1-nivel_confianza). Dividimos entre 2 para conocer extremos
# 1-0.95 = 0.05
# 0.05/2 = 0.025
extremo1<- qbeta(0.025, 68, 147)
ic1 <- quantile(extremo1, 0.025)
ic1
## 2.5%
## 0.2559599
extremo2 <- qbeta(0.975, 68, 147)
ic2 <- quantile(extremo2, 0.975)
ic2
## 97.5%
## 0.3798358
sprintf("El intervalo es (%f, %f)", ic1, ic2)

#3.3.- Estatura Data$Height con N~(170,7). Estimar estatura.
set.seed(2022)
# Seleccionamos una muestra de solo españoles, franceses e italianos de la muestra usada anteriormente
alturas <- muestra20[muestra20$Nation == "SP" | muestra20$Nation == "FR" | muestra20$Nation == "IT",]
# Calculamos su media muestral , su varianza muestral, su desv.típica y su tamaño de muestra

mediamuestral <- mean (alturas$height)
mediamuestral

varianzamuestral <- var(alturas$height)
varianzamuestral

desvtmuestral <- sqrt(varianzamuestral)
desvtmuestral

n <- nrow(alturas)
n

# Distribución a priori: X ~ (170, 7)
# n0 = varianza_muestral /varianza_poblacional^2
n0 <- varianzamuestral/7^2
n0

#factorEscala = desvTipica/raÃ-z(n+n0)
factorEscala <- desvtmuestral/sqrt(n + n0)
factorEscala

#mediaPosteriori = n*x' + n0*mu0/(n+n0)
mediaPosteriori <- (n*mediamuestral + n0*170)/(n+n0)
# Estimamos que la estatura media a posteriori con varianza conocida es:
mediaPosteriori


#####################################                                      APARTADo 4                                      #####################################  
#4.- CONTRASTES (PARAMÉTRICOS Y NO PARAMÉTRICOS)
#4.a.- CONTRASTES PARAMÉTRICOS
set.seed(2022)
library(TeachingDemos)
# Creamos dos muestras de tamaño 200 de la variable IMC
sample1 <- sample(Data$IMC, size = 200)
sample2 <- sample(Data$IMC, size = 200)
# Guardamos en a el cuartil 25%
a <- quantile(sample1,0.25)
a
## 25%
## 24.52199# Realizamos un contaste con la hipótesis nula de "el valor del cuartil25% es menor o igual que la media muestral" y la hipótesis alternativa de"el valor del cuartil 25% es mayor que la media muestral"
# El p-valor = 1, por lo tanto no rechazamos la hipótesis nula yconfirmamos que el cuartil 25% es menor a la media.
t.test(sample1, alternative = "less", mu = a)
##

# En b guardamos el valor del cuartil 75% y hacemos otro contraste, esta vez con la hipótesis nula de "La media muestral es menor o igual que el
#cuartil 75%" y la hipótesis alternativa de "La media muestral es mayor
#que el cuartil 75%"
# El p-valor vuelve a ser 1, con lo que aceptamos la hipótesis nula de que la media es menor o igual que el tercer cuartil.
b <- quantile(sample1, 0.75)
b


t.test(sample1, alternative = "greater", mu = b)
##


# Contraste: la varianza de sample1 es mayor que 1.0, con media desconocida.
# P-valor = 1.54e-06; por lo que rechazamos la hipótesis nula de que la
#varianza es mayor que 1 y aceptamos la alternativa de que es menor
sigma.test(sample1, sigma = 1, alternative = "less")
##

# Contraste si la diferencia entre las medias de Sample1 y Sample 2 = 0
# A un nivel de confianza de 95%, el p-valor es 0.6984 y podemos aceptar la hipótesis nula de que la diferencia entre medias es 0.
t.test(sample1, sample2)
##

# Contraste: varianza Sample1/varianza Sample2 = 1
# A un nivel de confianza de 0.95, p-valor es 0.8883 y podemos aceptar la hipótesis nula
var.test(sample1, sample2)
##

#4.b.- CONTRASTES NO PARAMÃ‰TRICOS
#4.b.1.- Contrastes de distribuciÃ³n
set.seed(2022)
library(MASS)
sample <- sample(Data$IMC, size = 200)
fitdistr(sample, c("normal"))

comp <- rnorm(200,24.77849520,0.77247751)
cor.test(sample, comp, conf.level = 0.95)
##
ks.test(sample, comp, conf.level = 0.95)

Parameter(s)

#4.b.2.- Contrastes de independencia
set.seed(2022)
library(lmtest)
# Tomamos una muestra de tamaño 200 de la variable IMC y la nombramos como Sample
sample <- sample(Data$IMC, size = 200)
# Creamos un modelo lineal con la variable IMC, que corresponde a las variables altura (height) y peso (weight)
modelo <- lm(IMC ~ Data$height + Data$weight, data = Data)
# Realizamos el contraste de independencia con el test de Durbin - Watson
dwtest(modelo, sample)
##

#4.b.3.- Contrastes de homogeneidad
set.seed(2022)
sample <- sample(Data$IMC, size = 200)
# Realizamos el contraste de independencia con el test de Wilcoxon
wilcox.test(sample)
##

#####################################                                      APARTADo 5                                      #####################################  













