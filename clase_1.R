# libraries
library(tidyverse)
library(rio)
library(boot)

# Simulen un experimento en el que lanzan una moneda.

round(runif(1))

# Generen 500 n´umeros binarios donde 1=cara, 0=ceca.

round(runif(500))


# Grafiquen c´omo evoluciona la proporci´on estimada de caras a
# medida que aumenta n.

props <- map_dbl(1:500, ~ mean(round(runif(.x))))

plot(props,
     type = "l",
     xlab = "# de lanzamientos ",
     ylab = " promedio ")

abline (h = 0.5, col = " red")

# Defina el siguiente estimador alternativo de µ

x_tilde <- function(x){
  sum(x)/(length(x)-1)
}


# Obtenga 5 observaciones aleatorias provenientes de una
# distribuci´on N(15, 9^2).

  
obs_aleat <- rnorm(n = 5,mean = 15,sd =  9)

# Calcule el estimador en la muestra (donde n = 5).


x_tilde(obs_aleat)

# Replique 10.000 veces el procedimiento y dibuje el histograma.

obs_aleat <- map_dbl(1:10000,~ x_tilde(rnorm(n =5,mean = 15,sd = 9)))

hist(obs_aleat,main="Histograma de Xtilde (para n=5)",breaks=50)
abline(v=15,lwd=3,lty=2,col="red")


 # Ahora, repitan el experimento para una muestra de tama˜no n = 1000.

obs_aleat <- map_dbl(1:10000,~ x_tilde(rnorm(n =1000,mean = 15,sd = 9)))

hist(obs_aleat,main="Histograma de Xtilde (para n=1000)",breaks=50)
abline(v=15,lwd=3,lty=2,col="red")

# 
# Consideremos 2 estimadores alternativos de la media poblacional:
#   • Estimador 1: media aritm´etica
# • Estimador 2: media ponderada, que asigne m´as peso a la
# primera mitad de las observaciones que a la segunda mitad
# 

# (1) Prueben que ambas medidas son insesgadas y consistentes.


n <- 100 #100 observaciones
w <- c(rep((1+0.5)/n, n/2), rep((1-0.5)/n, n/2)) #pesos del Estimador 2
sum(w) #verifico que los pesos suman 1

Xtilde <- function(x){sum(w*x)} #Defino la funci�n del Estimador 2

estimaciones_Xraya <- map_dbl(1:10000,~ mean(rnorm(100, 15, 9)))

estimaciones_Xtilde <- map_dbl(1:10000, ~Xtilde(rnorm(100, 15, 9)))


# (3) Calculen ambas varianzas muestrales y prueben que la media
# aritm´etica es m´as eficiente.

var(estimaciones_Xraya) #es más eficiente
var(estimaciones_Xtilde)


rm(list = ls())

# TCL
# • Generamos 1000 muestras de cierto n que provengan de una
# distribuci´on U(0,1).

muestras <- map(1:1000, ~ runif(50))

# • Calculamos el promedio (media muestral) para las 1000
# muestras

muestras_hist <- map_dbl(muestras,~ mean(.x))


hist(muestras_hist,main="Histograma (para n=100)",breaks=50)


rm(list = ls())
# Utilice la base de datos marketing.csv que contiene datos de
# ventas (sales) y gastos de publicidad a trav´es de distintos
# canales: youtube, facebook y newspaper. Calcule el intervalo de
# confianza al 95 % de los valores medios de cada variable.

marketing_df <- import("marketing.csv")

conf_int_extracter <- function(x){
  test <- t.test(x,conf.level = .95)
  min <- test$conf.int[1]
  max <- test$conf.int[2]
  df <- data.frame(min = min,
                   max = max)
  return(df)
}


map2_df(marketing_df, 1:4, ~mutate(conf_int_extracter(.x), empresa = names(marketing_df)[.y]))

# Un call center est´a interesado en determinar la duraci´on esperada
# de una llamada telef´onica de la forma m´as precisa posible. Los
# requerimientos son que el intervalo al 95 % de la duraci´on media de
# la llamada deber´ıa tener una amplitud de 1 minuto. Supongan que
# en el call center se llev´o a cabo una prueba piloto a partir de la cual
# se estim´o que la dispersi´on de la duraci´on de las llamadas es de 5
# minutos. ¿Cu´al es el tama˜no muestral que se requiere para estimar
# la duraci´on esperada de las llamadas con la precisi´on deseada?



# • Volvamos al ejemplo de marketing.csv
# • Ahora queremos calcular el IC del coeficiente de correlaci´on
# entre sales y youtube.
# • ¿Qu´e signo esperan? Calculen dicho coeficiente

cor(marketing_df$youtube, marketing_df$sales) #esperamos una alta correlación

data <- marketing_df %>% 
  select(youtube,sales)

# definimos función para el bootstrap
fc_cor <- function(d,i){
  d <- data[i,]
  return(cor(d[1],d[2]))
}

set.seed(0)
my_boot <- boot(data = data, statistic = fc_cor, R = 1000)


my_boot$t #para ver las 1000 replicaciones
sd(my_boot$t) #el error est�ndar
mean(my_boot$t)-my_boot$t0 #el sesgo
#histograma y qqplot
plot(my_boot)
#intervalo de confianza:
boot.ci(boot.out = my_boot, type = c("norm", "basic", "perc", "bca"))

