library(openxlsx)

datos <- read.xlsx("C:\\Users\\LENOVO V14-G2\\Documents\\est\\parcial2\\data_uni-Bi.xlsx")

head(datos)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Estimacion por prueba de hipotesis: Media

#Tomaremos la siguiente afirmacion como la hipotesis planteada: El ingreso anual de los empleados es mayor de 45.
#Con esto en cuenta, tenemos las siguientes hipotesis:

#H0: El ingreso anual de los empleados es igual o menor a 45
#H1: El ingreso anual de los empleados es mayor de 45.

#Como queremos saber si es mayor, usaremos el siguiente comando y los siguientes parametros:
#t.test(datos$Ingr_Anu_USD, alternative = "greater", mu=45 )
#- En este caso estamos utilizando la bsae de datos "Datos", por ello va en el primer parametro (de donde salen los datos). Dentro de este usaremos la variable del ingreso anual ($Ingr_Anu_USD)
#- Tomamos la opcion donde queremos hallarlo para la hipotesis alternativa, en la cual nos dice que es "mayor", asi que utilizamos "greater"
#- Finalmente,  en este caso nuestro valor hipotetico es 45, asi que le asignamos a mu (media hipotetica).

t.test(datos$Ingr_Anu_USD, alternative = "greater", mu=45 )

#Nos otorga el siguiente resultado

#One Sample t-test

#data:  datos$Ingr_Anu_USD
#t = 1.9145, df = 49, p-value = 0.0307
#alternative hypothesis: true mean is greater than 45
#95 percent confidence interval:
#  45.40521      Inf
#sample estimates:
#  mean of x 
#48.26 

#Analicemos los resultados de la prueba:

# mean of x: Nos da la media para x, el cual seria 48.25

#t (Estadistico de la prueba): 1.9145, df (grados de libertad {n-1}: 49, p-value (valor p): 0.0307)

#Usando un intervalo de confianza de 95%, se llega a la siguiente conclusion: alternative hypothesis: true mean is greater than 45
# Lo que significa que, en pocas palabras que la media es mayor al valor que colocamos (45)

#Si comparamos el p_value (0.0307) con el valor de alpha (0.05), y teniendo en cuenta el resto de los resultados de la prueba, se puede llegar a:

#Existen suficientes resultados estadisticos significativos para afirmar que el ingreso de los empleados esta por encima o es mayor de 45 con un nivel de confianza de 95%.

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Estimacion por intervalos de confianza: Media

#En este caso estaremos calculando el puntaje de la aprobacion del gobierno en un intervalo del 90%

#Para el caso de 90%, el valor z que le corresponde es aquel que deje 95% a la izquierda:

qnorm(0.95)

#Para poder encontrar los intervalos, necesitaremos lo siguiente:

#media muestral - error_muestral * qnorm(valor correspondiente)
#media muestral + error_muestral * qnorm(valor correspondiente)

#En este caso, el error muestral puede definirse como 
#error_muestral <- sd(datos)/sqrt(n)

error_muestral <- sd(datos$Punt_aprob)/sqrt(50)
error_muestral

#Entonces, solo reemplazamos la media muestral y obtendriamos los intervalos
intervalo_inf <- mean(datos$Punt_aprob) - error_muestral * qnorm(0.95)
intervalo_sup <- mean(datos$Punt_aprob) + error_muestral * qnorm(0.95)

intervalo_inf
intervalo_sup

#(Solamente para confirmar, calculamos la media de los datos)
mean(datos$Punt_aprob)

#Entonces podemos decir que el promedio del puntaje del gobierno nacional esta entre 51.90 y 58.69 con una confianza del 90%.


########################################################################################################################################################################
#Estimacion por prueba de hipotesis: Proporcion

#Tomaremos la siguiente afirmacion como la hipotesis planteada: Las mujeres constituyen mas del 60% de todos los encuestados.
#De esto, tomaremos la proporcion de mujeres sobre todas las personas encuestadas.

#Con esto en cuenta, tenemos las siguientes hipotesis:

#H0: La proporcion de las mujeres es igual o menor a 60%
#H1: La proporcion de las mujeres es mayor de 60%.

#Para poder hallar la proporcion, solo debemos contar la cantidad de mujeres en la categoria datos$Genero.

datag <- c(datos$Género)

prop_m <- table(datag)["Mujer"]

prop_m

#En este caso, la proporcion es de 26/50

prop_mujeres <- 26/50
prop_mujeres

#Con esto en cuenta, podemos hallar el estadistico para la prueba, que se constituye de la siguiente formula

z <- (26/50 - 0.60) / sqrt(0.60 * (1 - 0.60) / 50)
z

#Ahora, para hallar el valor-p, usamos la condicion de la hipotesis alternativa.

p_value <- 1 - pnorm(z, lower.tail=FALSE)  
p_value

#Como usamos un intervalo de confianza de 95%, donde el alpha es 0.05%
#Podemos concluir que como el p-valor es mayor que el alpha, no hay evidencias suficientes para rechazar la hipotesis nula con un intervalo de confianza de 95%

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Estimacion por intervalos de confianza: Proporcion

#Vamos a calcular la proporcion de personas que NO participaron en las elecciones mas recientes con intervalo de confianza del 99%.

#Con esto en cuenta, tendremos que calcular la proporcion de la gente que respondio NO y el total de votantes.

propdatos <- c(datos$Parti_elecc)

prop_no <- table(propdatos)["No"]

prop_no

#En ese caso, la proporcion seria 9/50.

prop <- 9/50

prop


#Junto a esto, tenemos que calcular el error muestral de la proporcion, que se consigue reemplazando la siguiente formula

#error_muestral <- sqrt(prop*(1-prop)/n)

#n siendo el numero de elementos
#prop siendo la proporcion

error_muestral <- sqrt(prop*(1-prop)/50)

error_muestral

#Con esto, podemos conseguir nuestros intervalos inferiores y superiores:

#Intervalo inferior:
#proporción muestral - error_muestral * qnorm(valor correspondiente)

#Intervalo superior:
#proporción muestral + error_muestral * qnorm(valor correspondiente)

#En este caso, como vamos a calcular teniendo en cuenta un intervalo del 99%, se tomaria el 0.995


prop - error_muestral * qnorm(0.995)
prop + error_muestral * qnorm(0.995)

#Con esto, podemos concluir que hay una proporcion de la gente que NO participo en las ultimas elecciones esta entre 0.04 y 0.31, es decir 4% y 31%, con una confianza del 99%.

########################################################################################################################################################################
#Estimacion por prueba de hipotesis: Varianza

#Tomaremos la siguiente afirmacion como la hipotesis planteada: La edad de las personas encuestadas tiene una varianza menor que 20.
#Con esto en cuenta, tenemos las siguientes hipotesis:

#H0: La varianza de las edades encuestadas es mayor o igual a 80. 
#H1: La varianza de las edades encuestadas es menor que 80.


#Usaremos el siguiente comando y los siguientes parametros:


stests::var.test(datos$Edad, alternative='less',
                 null.value=80, conf.level=0.95)

#Con esto en mente, aunque la varianza de los datos es de 60.09, no hay evidencias suficientes para rechazar la hipotesis nula con un nivel de confianza del 95%
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Estimacion por intervalos de confianza: Varianza

#En este caso estaremos calculando el puntaje de la calificacion de liderazgo en un intervalo del 95%

#Para poder encontrar los intervalos de confianza, en este caso del 95%, se calculan en ambos lados de la distribucion: para este caso serian 0.975 y 0.025
#Junto a esto, son los grados de libertad de los datos.

qchisq(0.975,49)
qchisq(0.025,49)

#Con esto en mente, podemos hallar los limites del intervalo:

#Intervalo inferior:
((50-1)*var(datos$Califi_lid))/qchisq(0.975,49)

#Intervalo superior:
((50-1)*var(datos$Califi_lid))/qchisq(0.025,49)

#Con esto, la varianza de la puntuacion de liderazgo esta entre 1.33 y 2.97 con una confianza del 99%