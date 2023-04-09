# MEL - PROYECTO FINAL
# Por: - Juan Camilo Pico
#      - Rodrigo Vera
#      - Juan Camilo Garcia

library(tidyverse)

# Cargar datos
columnas <- c("ID", "AGE", "GENDER", "EDUCATION", "Country", "Ethnicity",
              "Neuroticm", "Extraversion", "Openness", "Agreeableness",
              "Conscientiousness", "Impulsive", "SensationSeeking", "Alcohol",
              "Amphet", "Amyl", "Benzos", "Caff", "Cannabis", "Choc", 
              "Cocaine", "Crack", "Ecstasy", "Heroin", "Ketamine",
              "Legalh", "LSD", "Meth", "Mushrooms", "Nicotine",
              "Semer", "VSA")
datos <- read.csv("drug_consumption.data", header=FALSE, 
                  col.names = columnas)[-1]


# Definir las categorías con base en la documentación ---------------------

# Edad
datos <- datos %>% 
  mutate(AGE = case_when(AGE == -0.95197 ~ '18-24',
                         AGE == -0.07854 ~ '25-34',
                         AGE == 0.49788 ~ '35-44',
                         AGE == 1.09449 ~ '45-54',
                         AGE == 1.82213 ~ '55-64',
                         AGE == 2.59171 ~ '65+'),
         # Género
         GENDER = case_when(GENDER == 0.48246 ~ "Female",
                            GENDER == -0.48246 ~ "Male"),
         
         # Educación
         EDUCATION = case_when(EDUCATION == -2.43591 ~ 'Left school before 16 years',
                               EDUCATION == -1.73790 ~ 'Left school at 16 years',
                               EDUCATION == -1.43719 ~ 'Left school at 17 years',
                               EDUCATION == -1.22751 ~ 'Left school at 18 years',
                               EDUCATION == -0.61113 ~ 'Some college or university',
                               EDUCATION == -0.05921 ~ 'Professional certificate',
                               EDUCATION == 0.45468 ~ 'University degree',
                               EDUCATION == 1.16365 ~ 'Masters degree',
                               EDUCATION == 1.98437 ~ 'Doctorate degree'),
         # Pais
         Country = case_when(Country == -0.09765 ~ 'Australia',
                             Country == 0.24923 ~ 'Canada',
                             Country == -0.46841 ~ 'New Zealand',
                             Country == -0.28519 ~ 'Other',
                             Country == 0.21128 ~ 'Republic of Ireland',
                             Country == 0.96082 ~ 'UK',
                             Country == -0.57009 ~ 'USA'),
         # Etnia
         Ethnicity = case_when(Ethnicity == -0.50212 ~ 'Asian',
                               Ethnicity == -1.10702 ~ 'Black',
                               Ethnicity == 1.90725 ~ 'Mixed-Black/Asian',
                               Ethnicity == 0.12600 ~ 'Mixed-White/Asian',
                               Ethnicity == -0.22166 ~ 'Mixed-White/Black',
                               Ethnicity == 0.11440 ~ 'Other',
                               Ethnicity == -0.31685 ~ 'White'))

# Crear categorías de consumidor y no consumidor
for(col in 13:31){
  datos[,col][datos[,col] %in% c('CL0', 'CL1', 'CL2')] <- 0
  datos[,col][datos[,col] %in% c('CL3', 'CL4', 'CL5', 'CL6')] <- 1
  datos[,col] <- as.numeric(datos[,col])
}

# Modelo cannabis 1 ---------------------------------------------------------

# Elegir únicamente datos para el consumo de cannabis
y <- datos$Cannabis
can <- cbind(datos[,1:12], y)

# Ajustar regresión logística
fit <- glm(data=can, y~., family='binomial') # 33 variables
summary(fit)

# Significancia global del modelo
ep <- fit$null.deviance - fit$deviance
1-pchisq(ep, 33)


# Agrupar categorías con pocos datos --------------------------------------

datos2 <- datos
datos2 <- datos2 %>% 
  mutate(AGE = case_when(AGE == '65+' ~ '55+',
                         AGE == '55-64' ~ '55+',
                         TRUE ~ AGE),
         
         # Educación
         EDUCATION = case_when(EDUCATION == 'Left school before 16 years' ~ 'Left school',
                               EDUCATION == 'Left school at 16 years' ~ 'Left school',
                               EDUCATION == 'Left school at 17 years' ~ 'Left school',
                               EDUCATION == 'Left school at 18 years' ~ 'Left school',
                               EDUCATION == 'Some college or university' ~ 'Left college',
                               EDUCATION == 'Professional certificate' ~ 'University or professional',
                               EDUCATION == 'University degree' ~ 'University or professional',
                               EDUCATION == 'Masters degree' ~ 'Postgraduate',
                               EDUCATION == 'Doctorate degree' ~ 'Postgraduate',
                               TRUE ~ EDUCATION),
         # Pais
         Country = case_when(Country == 'New Zealand' ~ 'Australia',
                             Country == 'Republic of Ireland' ~ 'UK',
                             Country == 'USA' ~ 'NorthA',
                             Country == 'Canada' ~ 'NorthA',
                             TRUE ~ Country))

# Quitar variable de ethincity
datos2 <- datos2[,-5]


# Modelo cannabis 2 -------------------------------------------------------

# Elegir únicamente datos para el consumo de cannabis
y <- datos2$Cannabis
can <- cbind(datos2[,c(1:11,28)], y)

# Ajustar regresión logística
fit2 <- glm(data=can, y~., family='binomial') # 22 variables
summary(fit2)

# Significancia global del modelo
ep2 <- fit2$null.deviance - fit2$deviance
1-pchisq(ep2, 22)

# Cual modelo es mejor
ep3 <- fit2$deviance-fit$deviance
1-pchisq(ep3, 11)


# Modelo reducido ---------------------------------------------------------

fit3 <- glm(data=can, y~AGE+GENDER+EDUCATION+Country+Openness+Conscientiousness+
              SensationSeeking+Nicotine, family='binomial') # 22 variables
summary(fit3)

# Cual modelo es mejor
ep4 <- fit3$deviance-fit2$deviance
1-pchisq(ep4, 4)

# Significancia global
global <- fit3$null.deviance-fit3$deviance
1-pchisq(global, 15)

# Supuestos ---------------------------------------------------------------

# Multicolinealidad
library(car)

vif(fit3)

# Datos atípicos
plot(rstudent(fit3))

# Residuos estudentizados

x <- model.matrix(data=can, y~AGE+GENDER+EDUCATION+Country+Openness+Conscientiousness+
                    SensationSeeking+Nicotine)
H = x%*%solve(t(x)%*%x)%*%t(x)
Identidad = diag(length(y))
e = (Identidad-H)%*%y
#View(e)
index = c(1:length(y))
plot(index,e, main="Residuales estudentizados")
abline(h=0)

plot(residuals.glm(fit3, type="pearson"))

# distancia de cook
CD = cooks.distance(fit3)
plot(CD)
max(CD)
qf(0.50,15,nrow(can)-15)
CDordenado = sort(CD, decreasing = TRUE)
CDordenado[2]

# Preguntas -----------------------------------------------------------

# 0. efecto de responsabilidad (conscientiousness)

exp(fit3$coefficients[14])

# 1. El consumo de cannabis es igual en el UK y en NorthA

x <- model.matrix(data=can, y~AGE+GENDER+EDUCATION+Country+Openness+
                    Conscientiousness+SensationSeeking+Nicotine)
var_beta <- as.matrix(solve(t(x)%*%diag(fit3$fitted.values*(1-fit3$fitted.values))%*%x))

cc <- c(rep(0,9), -1, 0, 1, rep(0, 4))

theta <- cc%*%fit3$coefficients

ep <- (theta^2)/(t(cc)%*%var_beta%*%cc)
1-pchisq(ep, 1)

# 2. Interacción entre educación y apertura a la experiencia

fit4 <- glm(data=can, y~AGE+GENDER+EDUCATION+Country+Openness+Conscientiousness+
              SensationSeeking+Nicotine+Openness*EDUCATION, family='binomial') #  variables
summary(fit4)

ep <- fit3$deviance-fit4$deviance
1-pchisq(ep, 3)

# 3. intervalo de confianza de p

cc <- c(1, 0, 0, 0, 0, 1, 0, 0, 1,
        0, 0, 1, 1, -1.2, 0.5, 1)

theta <- cc%*%fit3$coefficients

sup=theta+qnorm(0.975)*sqrt(t(cc)%*%var_beta%*%cc)
inf=theta-qnorm(0.975)*sqrt(t(cc)%*%var_beta%*%cc)

# Intervalo de confianza de la probabilidad
supp=exp(sup)/(1+exp(sup))
infpp=exp(inf)/(1+exp(inf))
inter=c(infpp,supp)
inter
