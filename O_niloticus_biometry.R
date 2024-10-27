
# Cargar datos

url <- "https://raw.githubusercontent.com/sermomon/biometry/refs/heads/main/Oreochromis_niloticus_F300.csv"
data <- read.csv(url)

# Limpiar datos
data$X <- NULL
data <- na.omit(data)

# Gráfico L-W

plot(data$L, data$W, xlab="L [cm]", ylab="W [g]", col="skyblue",
     main="O. niloticus")

# Modelo lineal

model1 <- lm(data$W ~ data$L)
abline(model1)

# Transformación log-log

data$log_e_L <- log(data$L)
data$log_e_W <- log(data$W)

plot(data$log_e_L, data$log_e_W, xlab="LOGe L [cm]", ylab="LOGe W [g]", col="skyblue",
     main="O. niloticus")

# Ajustar modelo lineal transformado

model2 <- lm(data$log_e_W ~ data$log_e_L)
abline(model2)

# Obtener parámetros del ajuste

n <- model2$coefficients[[1]] # intercepto
m <- model2$coefficients[[2]] # pendiente

# Mostrar modelo en forma potencial

a <- exp(m)
b <- n

plot(data$L, data$W, xlab="L [cm]", ylab="W [g]", col="skyblue",
     main="O. niloticus")
curve(exp(n)*x^m, add=TRUE, col="red", lwd=2)

# Obtener R2

summary(model2)
R2 <- summary(model2)$r.squared

# Calcular el MRE

pred <- predict(model2)
data$Wpred <- exp(pred)

MRE <- mean(abs((data$W - data$Wpred) / data$W))

