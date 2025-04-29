install.packages("pacman")
library(pacman)
p_load(actuar, dplyr)
install.packages("moments")
library(moments)
install.packages("ggplot2")
library(ggplot2)

#Simulacion de datos
options(options(options(scipen = 999)
                set.seed(12345)
                
# Generar 2 vectores de 1000 datos con Bernoulli
A = rbinom(1000, 1, 0.5)
B = rbinom(1000, 1, 0.5)
                
# Probabilidad marginal
P_A = mean(A)
P_B = mean(B)
# Probabilidad condicional
# Seleccionar los elementos de A donde B es = 1
P_A_B = mean(A[B == 1]) # ya que es una variable binaria de 0 a 1

# La media de estos valores es equivalente a la proporción de veces que
# A es 1 cuando B es 1
cat("La probabilidad condicional es de:", P_A_B,"\n")
                
# Probabilidad de la intersección de A y B
P_A_y_B = mean(A == 1 & B == 1)
cat("La probabilidad de intersección es de:", P_A_y_B)
cat("La probabilidad con marginales es de: ", P_A*P_B,"\n")

## Pujede que sean un tanto diferentes, pues la muestra esta condicionada a 1000
##observaciones, entre mas sean mas se pareceran.
                
### Media, Varianza y correlación de datos SIM
# Simulación de datos de reclamaciones y edad
set.seed(12345)
edad = rnorm(5000, mean = 45, sd = 2) # Edad de los asegurados
reclam = rlnorm(5000, meanlog = 2.5, sdlog = 2.5) # Distr. de reclamaciones
                
# Estimación de la media, varianza y correlación
media_reclam = mean(reclam) #Quitar NA con na.rm = T
varianza_reclam = var(reclam)
correlacion = cor(edad, reclam)
list(media_reclam, format(varianza_reclam, big.mark = ","), correlacion)
                
### Prueba de hipotesis
set.seed(12345)
reclam_anteriores = rlnorm(1000, meanlog = -1.5, sdlog = 2.1)
reclam_actuales = rlnorm(1000, meanlog = 2, sdlog = 1.3)
                
# Estimación de la media y el error estandar
mean_recla_ante = mean(reclam_anteriores)
mean_recla_act = mean(reclam_actuales)
desv_ant = sd(reclam_anteriores) / sqrt(length(reclam_anteriores))
desv_act = sd(reclam_actuales)/ sqrt(length(reclam_actuales))
                
# Intervalo de confianza al 95%
alpha = 0.05
z = qnorm(1-alpha/2)
ci_lower = mean_recla_ante - z*desv_ant
ci_upper = mean_recla_ante + z*desv_ant
                
ci2_lower = mean_recla_act - z*desv_act
ci2_upper = mean_recla_act + z*desv_act
                
# Resultados de las medias para ambos años
mean_recla_ante
mean_recla_act
                
# Resultados de las medias para ambos años
cat("La media de las reclamaciones anteriores son de:", mean_recla_ante, "\n")
cat("La media de las reclamaciones actuales son de:", mean_recla_act, "\n")

# Resultados de los IC
cat("Los intervalos de las reclamaciones anteriores son:[", ci_lower, ",", ci_upper, "]\n")
cat("Los intervalos de las reclamaciones actuales son:[", ci2_lower, ",", ci2_upper, "]\n")
                
# Pruebas de hipotesis para comparar medias
t.test(reclam_anteriores, reclam_actuales, paired = FALSE)
#FALSE, pues las muestras son individuales
                
### Media y varianza condicional
set.seed(12345)
X = rlnorm(1000, meanlog = 0.5, sdlog = 1.2)
Y = rpareto(1000, shape = 2, scale = 90)
                
# Media de X 
E_X = mean(X)
                
# Varianza de X
Var_x = var(X)
                
# Media condicional de X dado Y
E_X_Y = rep(E_X, length(Y))
                
# Varianza condicional de X dado Y
Var_X_Y = rep(Var_x, length(Y))
                
# Verificar las propiedades
# E[E[X|Y]] = E[X]
E_E_X_Y = mean(E_X_Y)
                
# E[Var(X|Y)] + Var(E[X|Y]) = Var(x)
E_Var_X_Y = mean(Var_X_Y)
Var_E_X_Y = var(E_X_Y)

# Resultados
# Media condicional
cat("E[X]:", E_X, "\n")
cat("E[E[X|Y]]:", E_E_X_Y, "\n")
# Esperanza de varianza y varianza de esperanza
cat("E[Var(X|Y)]:", E_Var_X_Y, "\n")
cat("Var(E[X|Y]):", Var_E_X_Y, "\n")
# Varianza condicional
cat("Var(x)", Var_x, "\n")
cat("E[Var(X|Y)]+Var(E[X|Y]):", E_Var_X_Y + Var_E_X_Y, "\n")
                
## Técnica Bootstrap
set.seed(12345)
reclam = rlnorm(20, meanlog = 2.5, sdlog = 2.5)
bootstrap_mse = function(data, n_bootstrap){
mse_values = numeric(n_bootstrap)
for(i in 1:n_bootstrap){
sample_data = sample(data, replace = TRUE)
 mse_values[i] = mean((sample_data - mean(sample_data))^2)
}
return(mse_values)}
              
# Estimación del MSE usando bootstrap
n_bootstrap = 1000
mse_valores <- bootstrap_mse(reclam, n_bootstrap)
mse_estimate = mean(mse_valores)
cat("El MSE es:", format(mse_estimate, big.mark = ","), "\n")


IC_inf <- quantile(mse_valores, 0.025)
IC_sup <- quantile(mse_valores, 0.975)
                
cat("IC inferior", IC_inf, "\n")
cat("IC superior", IC_sup, "\n")
                
#########   GRÁFICA DE DENSIDAD MSE 
plot(density(mse_valores))


#--------------RECLAM.CSV
reclamaciones<- c(144, 134,185,	141,	205,	126,	123,	152,	123,	215, 170, 165,	180,	175,	160,	185,	168,	172,	178,	169)

bootstrap_mse<- function(data, n_bootstrap){
mse values <- numeric(n_bootstrap)
  for (i in 1:n _bootstrap) {
    sample_data <- sample (data, replace = TRUE)
    mse_values[i] <- mean((sample_data - mean(sample_data))^2)}
  return (mean(mse_values))}


bootstrap_mse <- function (data,n_bootstrap){
mse_ values <-numeric (n_bootstrap)
for (i in 1:n _bootstrap) {
sample_data <- sample (data,replace = TRUE)
    mse values[i]<-mean sample data - mean(sample_data))^2)}


# Estimación del MSE usando bootstrap
n_bootstrap1 = 10000
mse_estimate1 <- bootstrap_mse(reclamaciones, n_bootstrap1)
mse_desv1 = mean(mse_estimate1)
cat("El MSE es:", format(mse_desv1, big.mark = ","), "\n")

#IC del MSE
alpha=0.05
z<- qnorm(1-alpha/2)
                
IC_inf1<- mse_estimate1-z*mse_desv1
IC_sup1 <- mse_estimate1+z*mse_desv1
cat("IC inferior", IC_inf1, "\n")
cat("IC superior", IC_sup1, "\n")

#--------GRÁFICA DE DENSIDAD MSE
plot(density(mse_estimate1,mse_desv1), 
     main = "Distribucion Bootstrap del MSE",
     xlab =  "MSE",
     ylab = "Density",
     col="#C71F3C", lwd=2)
abline(v=mse_estimate1, col="#86D0B9", lwd=2, lty=3)
abline(v=IC_inf1, col="#D0DCEF", lwd=2, lty=3)
abline(v=IC_sup1, col="#D0DCEF", lwd=2, lty=3)                
