## TP Final

getwd()
setwd('C:/Data/Uni/2021/1er Semestre/Estadística Aplicada/USA_CrimeRateStudy')
datos = read.table('grupo8Data.txt', header=TRUE)

par(mfrow=c(1,1))
qqnorm(modelo0$residuals)
qqline(modelo0$residuals, col="red")
qqnorm(modelo1$residuals)
qqline(modelo1$residuals, col="red")
plot(modelo0, 1)
plot(modelo1, 1)

## Hip?tesis de estudio: A mayor nivel de castigo, menor cantidad de crime rates

# Ver diferencias entre Norte y Sur
modelo_So = glm(So~Crime, family="binomial", data=datos)
summary(modelo_So)
plot(Crime~So, data=datos)
boxplot(Crime~So, data=datos) 
d_So = modelo_So$null.deviance - modelo_So$deviance
pvalor_So = 1 - pchisq(d_So,1)
# Las medianas del Norte y Sur son casi iguales. No vale la pena incluir esta variable
modelo0 = lm(Crime~., data=datos)
summary(modelo0)
plot(modelo0$fitted.values, modelo0$residuals, main="Modelo con todas")
abline(h=0, col="red")
plot(modelo0, 1)
qqnorm(modelo0$residuals)
qqline(modelo0$residuals, col="red")
plot(modelo0, 2)
209.1/mean(datos_sin_So$Crime)
# Un modelo con la variable So no cumple con varios supuestos. Adem?s tiene un p-valor de 0.9798

mean(datos[,16])

datos_sin_So = datos[,-2]

## PRIMER MODELO
modelo1 = lm(Crime~., data=datos_sin_So)
summary(modelo1)
plot(modelo1, 1)
# Buen pvalor del modelo, muchas variables tienen un pvalor malo. R^2 no est? tan mal.
# El RSE es un poco alto
205.8/mean(datos_sin_So$Crime)
# El procentaje de error es aproximadamente un 23%. No es muy bueno. 

plot(modelo1$fitted.values, modelo1$residuals, main="Segundo modelo")
# Podemos observar que los residuos tienen una relativamente buena forma (quiz?s un poco parab?lica)
abline(h=0, col="red")
# Tiene cierta estructura, pero se agrupa en el medio. Parece ser heteroced?stico.
qqnorm(modelo1$residuals)
qqline(modelo1$residuals, col="red") # No es normal

## TRANSFORMACIONES PRIMER MODELO
# Ajustamos el modelo1 a una transformaci?n cuadr?tica
modelo_cuadratico = lm(sqrt(Crime)~., data=datos_sin_So)
plot(modelo_cuadratico, 1)
plot(modelo_cuadratico$fitted.values, modelo_cuadratico$residuals, main="sqrt(Y)")
abline(h=0, col="red")
summary(modelo_cuadratico)
qqnorm(modelo_cuadratico$residuals, main="sqrt(Y)")
qqline(modelo_cuadratico$residuals, col="red")
# No hay normalidad en los errores. Este modelo no va.

# Transformaci?n con log
modelo_log = lm(log(Crime)~., data=datos_sin_So)
summary(modelo_log)
plot(modelo_log$fitted.values, modelo_log$residuals, main="log(Y)")
abline(h=0, col="red")
plot(modelo_log, 1)
qqnorm(modelo_log$residuals, main="log(Y)")
qqline(modelo_log$residuals, col="red")
plot(modelo_log, 2) # La normalidad de los errores es mala

# Transformaci?n con ^2
modelo_alCuad = lm(Crime^2~., data=datos_sin_So)
summary(modelo_alCuad)
plot(modelo_alCuad, 1)
plot(modelo_alCuad, 2)

## OUTLIERS
# Nos fijamos si hay posibles outliers
plot(modelo1$fitted.values, studres(modelo1))
abline(h=0)
abline(h=3, col="red", lty=2)
abline(h=-3, col="red", lty=2) #Hay un dato que supera el Y=3 --> outlier
plot(modelo1)
which(abs(studres(modelo1))>3) # Es el valor 11 el outlier
datos_sin_outlier_11 = datos_sin_So[-11,]
# Sacamos el outlier y volvemos graficar
modelo2 = lm(Crime~., data=datos_sin_outlier_11)
summary(modelo2)
plot(modelo2$fitted.values, studres(modelo2))
abline(h=0)
abline(h=3, col="red", lty=2)
abline(h=-3, col="red", lty=2)
qqnorm(modelo2$residuals)
qqline(modelo2$residuals)
# Aparece un nuevo outlier. Hay que ver si sirve sacarlo
which(abs(studres(modelo2))>3) 
datos_sin_outliers = datos_sin_outlier_11[-18,]

modelo3 = lm(Crime~., data=datos_sin_outliers)
plot(modelo3$fitted.values, studres(modelo3), main="Sin outliers")
abline(h=0)
abline(h=3, col="red", lty=2)
abline(h=-3, col="red", lty=2)
# Sacando los dos outliers, ahora no hay m?s.
plot(modelo3, 1)
qqnorm(modelo3$residuals)
qqline(modelo3$residuals, col="red")
# Los errores no siguen normalidad. No cumple con el supuesto de normalidad en los errores.
summary(modelo3)
157.3/mean(datos$Crime)
# Volvemos a los datos originales.

# Observamos qu? tanta relaci?n hay entre las variables regresoras
library(corrplot)
cor(datos_sin_So)
corrplot(cor(datos_sin_So), method="ellipse")
# Podemos observar bastante colinealidad entre algunas variables.

## ESTRATEGIA SELECCI?N DE VARIABLES
# Con los datos originales, buscamos plantear la estrategia Stepwise de selecci?n de variables,
# para que el algoritmo nos seleccione las variables que con m?s significancia rechazan H0.
summary(modelo1)
modelo_ajustado = step(modelo1)
summary(modelo_ajustado) # R^2 baj? (tambi?n hay menos variables). RSE y el estad?stico F mejoraron.
plot(modelo_ajustado$fitted.values, modelo_ajustado$residuals, main="Post Stepwise") # Sigue cierta forma, aunque se abre un poco
abline(h=0, col="red")
qqnorm(modelo_ajustado$residuals)
qqline(modelo_ajustado$residuals, col="red") # Dentro de los modelos que vimos, es una de las "mejores" normalidades

195.5/mean(datos_stepeados$Crime)
# 0.216 --> El porcentaje de error es de aproximadamente 22%. Mejor? respecto del primer
# modelo.

plot(modelo_ajustado$fitted.values, studres(modelo_ajustado), main="Posibles outliers")
abline(h=0)
abline(h=3, col="red", lty=2)
abline(h=-3, col="red", lty=2)
# No hay outliers con este modelo


## INTERACCIONES Y LUEGO SELECCI?N 
# Realizamos interacciones acorde a lo que muestra el corrplot
modelo180 = lm(Crime~.+Po1*Po2+NW*Ed+Wealth*M+Ineq*Ed+Wealth*Ed+Wealth*Po1+Wealth*Po2+Ineq*Wealth, data=datos_sin_So)
summary(modelo180)
plot(modelo180, 1)
plot(modelo180, 2) #Nos Mejora el R? pero la normalidad de los residuos es muy mala
modelo180_step = step(modelo180)
summary(modelo180_step)
plot(modelo180_step, 1)
plot(modelo180_step, 2) # No tiene una buena normalidad
qqnorm(modelo180$residuals)
qqline(modelo180$residuals, col="red")

# Ver interacciones en datos originales y luego stepearlo
modelo1_con_interac = lm(Crime~. ^2, data=datos_sin_So)
summary(modelo1_con_interac)
# Debido a la alta correlaci?n entre muchas variables, no permite analizar las estimaciones, errores est?ndar, t-valores y p-valores de la mayor?a de las regresoras. 

corrplot(cor(datos_sin_So), method="ellipse")
# Observando este gr?fico, observamos que entre Po1 y Po2 hay una correlaci?n muy fuerte.
# Wealth tiene correlaciones fuertes con muchas variables.
# Lo mismo ocurre con Prob
# Intentaremos sacarlas de la muestra

datos_sin_So_Po2 = datos_sin_So[,-c(4,11, 12)]
names(datos_sin_So_Po2)
modelo_sin_Po2 = lm(Crime~.^2, data=datos_sin_So_Po2)
summary(modelo_sin_Po2)
# Hay muchas regresoras con poca significancia. Volveremos al modelo ajustado anterior.

## PRIMER SELECCI?N Y LUEGO INTERACCIONES
summary(modelo_ajustado)
# Estudiar correlaciones entre variables del modelo ajustado
datos_stepeados = datos_sin_So[,-c(14, 5, 8, 4, 7, 11)]
modelo_ajustado_interac = lm(Crime~. ^2, data=datos_stepeados)
summary(modelo_ajustado_interac)
corrplot(cor(datos_stepeados), method="ellipse")
# Luego de observar las interacciones, podemos ver que casi ninguna regresora es significante.
# Por ende, no consideramos realizar interacciones.

## TRANSFORMACIONES SOBRE MODELO AJUSTADO
# Ver si hay que realizar transformaciones
plot(modelo_ajustado, 1)
plot(modelo_ajustado, 2)
# Tiene cierta forma. Al abrirse un poco, quiz? sea conveniente hacer una transformaci?n de log(Y)


# Transformaci?n log sobre el modelo ajustado
modelo_ajustado_log = lm(log(Crime)~., data=datos_stepeados)
summary(modelo_ajustado_log)
plot(modelo_ajustado_log, 1)
plot(modelo_ajustado_log$fitted.values, modelo_ajustado_log$residuals, main="log(Y)")
abline(h=0, col="red")
qqnorm(modelo_ajustado_log$residuals, main="log(Y)")
qqline(modelo_ajustado_log$residuals, col="red")
plot(modelo_ajustado_log, 2) # La normalidad de los errores es bastante mala.
plot(modelo_ajustado_log, 5)
plot(modelo_ajustado_log$fitted.values, studres(modelo_ajustado_log))
abline(h=0)
abline(h=3)
abline(h=-3)
# Este modelo tampoco es bueno.

# Transformaci?n sqrt sobre el modelo ajustado
modelo_ajustado_sqrt = lm(sqrt(Crime)~., data=datos_stepeados)
summary(modelo_ajustado_sqrt)
plot(modelo_ajustado_sqrt, 1)
plot(modelo_ajustado_sqrt$fitted.values, modelo_ajustado_sqrt$residuals, main="sqrt(Y)")
abline(h=0, col="red")
qqnorm(modelo_ajustado_sqrt$residuals, main="sqrt(Y)")
qqline(modelo_ajustado_sqrt$residuals, col="red")
plot(modelo_ajustado_sqrt, 2)
# El modelo de los residuos contra los predichos pareciera ser ligeramente mejor, pero la 
# normalidad de los residuos pareciera haber empeorado.

# Transformaci?n ^2 sobre el modelo ajustado
modelo_ajustado_alCuad = lm(Crime^2~., data=datos_stepeados)
plot(modelo_ajustado_alCuad, 1)
plot(modelo_ajustado_alCuad, 2)
# En esta transformaci?n los el gr?fico de los residuos contra los predichos empeor? y la normalidad
# de los residuos tambi?n.

plot(modelo1, 2)
plot(modelo_ajustado, 2)
plot(modelo_ajustado, 1)








#1. Is at least one of the predictors X1 , X2 , . . . , Xp useful in predicting the response?
#2. Do all the predictors help to explain Y, or is only a subset of the predictors useful?
#3. How well does the model fit the data?
#4. Given a set of predictor values, what response value should we predict,
# and how accurate is our prediction?


#####

# 2 mejores modelos
plot(modelo1$fitted.values, modelo1$residuals, main="Segundo modelo")
plot(modelo_ajustado$fitted.values, modelo_ajustado$residuals, main="Modelo selecci?n de variables")
abline(h=0, col="red")

qqnorm(modelo1$residuals, main="Segundo modelo")
qqline(modelo1$residuals, col="red")

qqnorm(modelo_ajustado$residuals, main="Modelo selecci?n de variables")
qqline(modelo_ajustado$residuals, col="red")


summary(modelo1)
summary(modelo_ajustado)

## PREDICCIONES PRIMER MODELO
summary(modelo1)

predict(modelo1, newdata = data.frame(M=mean(datos$M), Ed=mean(datos$Ed), Po1=mean(datos$Po1), Po2=mean(datos$Po2), LF=mean(datos$LF), M.F=mean(datos$M.F), Pop=mean(datos$Pop), NW=mean(datos$NW), U1=mean(datos$U1), U2=mean(datos$U2), Wealth=mean(datos$Wealth), Ineq=mean(datos$Ineq), Prob=mean(datos$Prob), Time=mean(datos$Time)), interval="prediction")
# Teniendo en cuenta las medias de cada variable, a nivel 5% un pr?ximo estado tendr?a una tasa de cr?menes entre [481.5, 1328.7]

predict(modelo1, newdata = data.frame(M=mean(datos$M), Ed=mean(datos$Ed), Po1=mean(datos$Po1)+3, Po2=mean(datos$Po2)+3, LF=mean(datos$LF), M.F=mean(datos$M.F), Pop=mean(datos$Pop), NW=mean(datos$NW), U1=mean(datos$U1), U2=mean(datos$U2), Wealth=mean(datos$Wealth), Ineq=mean(datos$Ineq)-4, Prob=mean(datos$Prob)+0.02, Time=mean(datos$Time)+10), interval="prediction")
# Teniendo en cuenta lo mismo que el predict anterior, pero modificando ciertas variables a prop?sito con  el fin de que la tasa de cr?menes disminuya, a nivel 5% un nuevo estado tendr?a una tasa de cr?menes de entre [208.2, 1275.766]

## INTERVALO CONFIANZA
predict(modelo1, newdata = data.frame(M=mean(datos$M), Ed=mean(datos$Ed), Po1=mean(datos$Po1), Po2=mean(datos$Po2), LF=mean(datos$LF), M.F=mean(datos$M.F), Pop=mean(datos$Pop), NW=mean(datos$NW), U1=mean(datos$U1), U2=mean(datos$U2), Wealth=mean(datos$Wealth), Ineq=mean(datos$Ineq), Prob=mean(datos$Prob), Time=mean(datos$Time)), interval="confidence")
# A nivel 5%, el intervalo de confianza entre los que puede encontrarse el mean de la variable respuesta es [843.95, 966.22]


#//
predict(modelo1, newdata = data.frame(Time=35, Prob=0.06, Po1=12), interval="prediction")



## INTENTAMOS SACAR LAS VARIABLES MENOS SIGNIFICATIVAS A MANO
datos_nuevos10 = datos_stepeados[,-c(4,5)]
modelo90 = lm(Crime~., data=datos_nuevos10)
summary(modelo90)
plot(modelo90, 1)
plot(modelo90, 2)
## SACAMOS LOS OUTLIERS
datos_nuevos10_1 = datos_nuevos10[-c(11,19,29),]
modelo90_1 = lm(Crime~., data=datos_nuevos10_1)
summary(modelo90_1)
plot(modelo90_1, 1)
plot(modelo90_1, 2) #La normalidad es muy mala



## ESTUDIO DE VARIABLES INDIVIDUALES
attach(datos)
plot(datos)

modelo_20 = lm(Crime~.+(1/Prob)-Prob, data=datos_sin_So)
summary(modelo_20)
plot(modelo_20,1)
plot(modelo_20, 2)
# Es muy dif?cil poder identificar y modificar de esta manera correctamente. Nos quedaremos con el modelo con Stepwise


# //
library(lmtest)
library(car)
bptest(modelo1)
bptest(modelo_ajustado)
# Esto muestra que ambos modelos no tienen tanto problema de homocedasticidad


