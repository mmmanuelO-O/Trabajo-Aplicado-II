#Ejercicios

# 1. El Instituto Nacional de Estadística y Censos (INEC, 2020) estimó que el ingreso per cápita promedio de Costa Rica en julio de 2020 es 326483 colones, con un
# margen de error de ±11423 colones calculado al 95 % de confianza. Construya el intervalo de confianza para el promedio e interprételo.
######################################################################################Versión con las herramientas del profe - NO LA VOY A USAR
confianza<-95
z <- qnorm((1+(confianza/100))/2)
n <- 13440 
mean <- 326483 
sigma <- 11423  

margin_error <- z * (sigma / sqrt(n))
lower_bound <- mean - margin_error
upper_bound <- mean + margin_error

cat("Intervalo de confianza del 95%:", lower_bound, "-", upper_bound)

- X -
############################################################################################Versión Modificada
Muestra_Total_Población <- 326483 
Margen_Error<- 11423 #Con confianza del 95%

Límite_Inferior <- Muestra_Total_Población - Margen_Error
Límite_Superior <- Muestra_Total_Población + Margen_Error

cat("Intervalo de confianza del 95%:", Límite_Inferior, "-", Límite_Superior)

- X - 
# 2. Según la Encuesta Nacional de Hogares, de julio de 2020, el 26.2 % de los hogares se categorizan como pobres en Costa Rica, con un margen de error de ±1.2 puntos
#porcentuales, calculado al 95 % de confianza (INEC, 2020). Construya el intervalo de confianza para el porcentaje e interprételo.
PorcentajeMuestral <- 26.6  
Tamaño_Muestra <- 3564.4    
MargenError <- 1.2 #Con confianza del 95%

LímiteInferior <- PorcentajeMuestral - MargenError
LímiteSuperior <- PorcentajeMuestral + MargenError

cat("Intervalo de confianza al 95%:", LímiteInferior, "-", LímiteSuperior)

- X -
# 3. En una escala de 0 a 10, las personas encuestadas por el Centro de Investigación y Estudios Políticos (CIEP, 2020) calificaron al Tribunal Supremo de Elecciones de
# Costa Rica con una nota promedio de 6.6, con una desviación estándar muestral de 2.5. A sabiendas de que la muestra es de 927 personas, calcule el margen de
# error al 95 %, construya el intervalo de confianza e interprételo.
Muestra <- 927
SD <- 2.5
NotaPromedio <- 6.6
Confianza <- 95 #Para efectos de la formula se usa 1.96

Margen_Error <- 1.96 * (SD/sqrt(Muestra))
                        print(Margen_Error)

                        
LímiteInferior <- NotaPromedio - Margen_Error
LímiteSuperior <- NotaPromedio + Margen_Error
cat("Intervalo de confianza al 95%:", LímiteInferior, "-", LímiteSuperior)

- X -
# 4. Según una encuesta preelectoral de enero hecha por el CIEP (2018), la intención de voto para el Partido Restauración Nacional era de 17 % y para el Partido Acción
# Ciudadana de 11 %, con una muestra de 798 personas decididas a votar. Calcule los márgenes de error al 95 %, construya los intervalos de confianza para ambas
# estimaciones e interprételos.

Muestra <- 798
IntenciónVoto_PAC <- 11
IntenciónVoto_PRN <- 17
Confianza <- 95 #Para efectos de la formula se usa 1.96

######### Partido Restauración Nacional
SD_PRN <- sqrt(17*(100 - 17))

Margen_Error_PRN <- 1.96*sqrt(17*(100-17)/798)
print(Margen_Error_PRN)

LímiteInferior <- IntenciónVoto_PRN - Margen_Error_PRN
LímiteSuperior <- IntenciónVoto_PRN + Margen_Error_PRN
cat("Intervalo de confianza al 95%:", LímiteInferior, "-", LímiteSuperior)

######### Partido Acción Ciudadana
SD_PAC <- sqrt(11*(100 - 11))

Margen_Error_PAC <- 1.96*sqrt(11*(100-11)/798)
print(Margen_Error_PAC)

LímiteInferior <- IntenciónVoto_PAC - Margen_Error_PAC
LímiteSuperior <- IntenciónVoto_PAC + Margen_Error_PAC
cat("Intervalo de confianza al 95%:", LímiteInferior, "-", LímiteSuperior)


- X -

# 5. Comparando los intervalos de confianza obtenidos en el punto 4, ¿qué le parece que se puede concluir?

- X -

# 6. Los resultados del censo de 2011 del INEC (2012) dicen que la población total de Costa Rica es de 4301712 personas, pero no se indica ningún margen de error.
# Explique por qué.

- X -

# 7. Alguien postula que el estado de las carreteras de su país es deficiente y que las personas lo valorarían con una nota por debajo de 6.3. Otra persona es más bien
# optimista y cree que la media estaría por encima de 6.4. Una encuesta, aplicada a 600 personas, encuentra que en promedio la ciudadanía da una calificación de 6.2
# con una desviación estándar de 1.2.

      # a. Pruebe, calculando el valor p con R, la hipótesis nula de que la media en la
      # valoración de las carreteras es igual a 6.3 versus la hipótesis alternativa de
      # que es menor a 6.3, como aducía el crítico.

            #Hipótesis Nula: La media es 6.3
            #Hipótesis Alternativa: La media es menor a 6.3

Media <- 6.2
ParámetroHipotético1 <- 6.3
SD <- 1.2
Muestra <- 600

z <- (Media - ParámetroHipotético1) / (SD/sqrt(Muestra))
p_value <- pnorm(z, lower.tail = TRUE)
p_value

- X -
      # b. Pruebe, con R, la hipótesis nula de que la media en la valoración de las
      # carreteras es igual a 6.4 versus la hipótesis alternativa de que es mayor a 6.4,
      # como opinaba la voz optimista.

            #Hipótesis Nula: La media es 6.4
            #Hipótesis Alternativa: La media es mayor a 6.4

Media <- 6.2
ParámetroHipotético2 <- 6.4
SD <- 1.2
Muestra <- 600

z <- (Media - ParámetroHipotético2) / (SD/sqrt(Muestra))
p_value <- pnorm(z, lower.tail = FALSE)
p_value

- X -
