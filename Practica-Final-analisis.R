

datos <- read.csv(file="D:/Desktop/Master-DataScience/1er Cuatrimestre/Tecnicas y métodos de Ciencia de Datos/Datos-Manuel/Datos2.csv",
                  header=TRUE,
                  sep=";")

datos$target2 <- datos$TARGET * 1 # Pasamos los datos TRUE-FALSE  a datos numéricos 1-0 respectivamente.


# Sacamos el número de datos que tenemos
n = length(datos$target2)

# Sacamos el número de datos favorables
x = sum(datos$target2 == 1)

# Obtenemos el número de casos desfavorables
n_x = n - x

# Alfa y beta son los conocimientos a priori que nos dá el experto
alpha = 5
beta = 95

# Alfa y beta a posteriori son:
# alpha_post = alpha + n_casos_favorables
# beta_pos = beta + (n_datos - n_casos_favorables)
alpha_post = alpha + x 
beta_post = beta + n_x

# Declaramos el intervalo de probabilidad
IP = 0.95


p = seq(0,1, length=n)
plot(p, dbeta(p, alpha_post, beta_post), ylab="density", type ="l", col=4)


# Diapo 13 del tema 2
q_Lower <- qbeta((1-IP)/2, alpha_post, beta_post, log = FALSE)
q_Upper <- qbeta((1+IP)/2, alpha_post, beta_post, log = FALSE)

q_Lower_pri <- qbeta((1-IP)/2, alpha, beta, log = FALSE)
q_Upper_pri <- qbeta((1+IP)/2, alpha, beta, log = FALSE)
# Calculamos la probabilidad de que la probabilidad sea menor a 0.02
ph0 = pbeta(0.05, alpha_post, beta_post)

p_upper = pbeta(0.055, alpha_post, beta_post)
p_upper - ph0