empleados <- read.csv("empleados.csv")
uceva <- read.csv("UCEVA.csv")
taller <- read.csv("Talleres.csv")
?read.csv("empleados.csv")
?read.csv("Auto")
library(descr)
library(fdth)
library(ISLR)
library(ggplot2)
##Grafica de cantidad de hombres y mujeres personalizada
barplot(table(datos$sexo), col=c("green", "red"), main = "Empleados\n Sexo",
        xlab="sexo", ylab="frecuencias")

table3=freq(datos$sexo)
##Grafica de la cantidad de hombres y mujeres sencilla 

##Gráfica de hombres y mujeres pero en pastel
pie(table(datos$sexo))
    pie(table(datos$sexo), col=c("green","red"))
    ##Grafica de hombres y mujeres pero en pastel personalizada
    
    table3
    ## datos$sexo
    ## Frequency Percent
    ## Hombre 258 54.43
    ## Mujer 216S 45.57
    ## Total 474 100.00
    tabla4=freq(datos$categorÃ.a)
    names(datos)
    
    datos <- read.csv("empleados.csv")
    
    #Muestra el encabezado de la tabla en cada una de sus columnas
    head = datos
    
    #final de la tabla 
    tail(datos)
    
    #Resumen del contenido en las columnas 
    summary(datos)
    
    #Cantidad de columnas que contiene
    length(datos)
    
    #Muestra los nombres de las columnas 
    names(datos)
    
    #dimensión del archivo 
    dim(datos)
    
    ##Grafica 
    
    str(empleados)
    
    
    ##el nombre del dataset es empleados
    
    
    dist=fdt(datos$edad , breaks="Sturges")
    
    head(empleados)
    
    
    dist=fdt(datos$edad, 5)
    
    
    mean(datos$edad)
    
    str(empleados)
    
    
    mean(datos$edad, na.rm=T)
    
 
    
    dist=fdt(empleados$edad , breaks="Sturges")
    dist
    #///////////////////////////////////////////////////////////////////////
    
    uceva
    
    
    barplot(table(uceva$ID_TIPO_DOCUMENTO), main = "Origen\n carro",
            xlab="origen", ylab="frecuencias")
    
    dim(uceva)
    
    pie(table(uceva$ID_SEXO_BIOLOGICO))
    #///////////////////////////////////////////////////////////////////////
    
    taller
    
    hist(empleados$tiempomes, breaks="Sturges")
    
    plot(dist, type="cfh")
    
    
    plot(dist, type="cfp", col=c("red"))
    
    pie(table(empleados$sexo), col=c("green","red"))
    
    barplot(table(datos$sexo), col=c("green", "red"), main = "Empleados\n Sexo",
            xlab="sexo", ylab="frecuencias")
    str(empleados)
    
    quantile(empleados$experiencia, probs=c(0.25,0.75), na.rm=T)
    boxplot(empleados$experiencia, na.rm=T)
    
    
    str(Auto)
    
    table(Auto)
    
    ##///////////////////////////////////////////////////////////////////////////
    
    barplot(table(Auto$origin), main = "Origen\n carro",
            xlab="origen", ylab="frecuencias")
    
    pie(table(Auto$year))
    
    head(Auto)
    table1=freq(Auto$cylinders, plot=F)
    table1
    
    pie(table1)
    
    pie(table(Auto$cylinders))
    
    pie(table(Auto$origin))
    
    ## VARIABLES CUANTITAIVAS
    ##     Escoja su directorio 
    
    # Lea los datos
    
    distribucion=fdt(Auto$year, k = 5)# Seleccionamos como dataframe el archivo empleados,
    
    plot(distribucion, type = 'fh') # h - Histograma
    
    plot(distribucion, type = 'fp') # p - Poligono
    
    plot(distribucion, type = 'cfh') # c - acomulado,  f - frecuencia,   h - histograma 
    
    plot(distribucion, type = 'cfp') # c - acomulado,  f - frecuencia,   p - POLIGONO
    
    
    distribucion2=fdt(Auto$cylinders, k = 5)# Seleccionamos como dataframe el archivo empleados,
    
    plot(distribucion2, type = 'fh') # h - Histograma
    
    plot(distribucion2, type = 'fp') # p - Poligono
    
    plot(distribucion2, type = 'cfh') # c - acomulado,  f - frecuencia,   h - histograma 
    
    plot(distribucion2, type = 'cfp') # c - acomulado,  f - frecuencia,   p - POLIGONO
    
    distribucion3=fdt(by(Auto$year,Auto$cylinders, mean, na.rm = T,k = 5))
    distribucion3
    
    plot(distribucion3, type = 'cfp') # c - acomulado,  f - frecuencia,   p - POLIGONO
    
    
    abline(v = mean(Auto$year), col = 'black')
    abline(v = median(Auto$year), col = 'red')
    abline(v = mlv(Auto$year, method = 'mfv'), col = 'orange')
    plot(table1, type = 'fh', main = 'Histograma - Frecuencia Absoluta', xlab = 'Displacement', ylab = 'Frecuencia Abosluta', col=c('#CC79A7','#009E73','#999999','blue','black','red','pink','white','orange','purple'))
    
    # MEDIDAS NUMERICAS 
    
    
    
    
    # MEDIDAS DE TENDENCIA CENTRAL 
    
    
    # MEDIA ARITMETICA
    
    mean(Auto$year) # Promedio - Media
    # NA - Valores vacios, que el usuario no dio
    barplot(mean(Auto$year))
    
    barplot(by(Auto$cylinders ,Auto$year, mean, na.rm = T),col=c("green","red","blue","yellow","pink"),
            main = "Relacion pistones , año de fabricacion ",
            xlab="Año de modelo", ylab="Cantidad de cilindros")
    
    barplot(by(Auto$cylinders ,Auto$year, mean, na.rm = T),col=c("green","red","blue","yellow","pink"),
            main = "Relacion pistones , año de fabricacion ",
            xlab="Año de modelo", ylab="Cantidad de cilindros")
    
    plot(by(Auto$cylinders ,Auto$year, mean),type="l", main = "Relacion pistones , año de fabricacion ",
         xlab="Año de modelo", ylab="Cantidad de cilindros")
    
    plot(by(Auto$acceleration ,Auto$year, mean),type="l", main = "Relacion pistones , año de fabricacion ",
         xlab="Año de modelo", ylab="Cantidad de cilindros")
    
    
    barplot(by(Auto$acceleration ,Auto$cylinders, mean, na.rm = T),col=c("green","red","blue","yellow","pink"),
            main = "Relacion pistones , aceleracion del auto ",
            xlab="cantidad de cilindros", ylab="Aceleracoin del Auto")
    
    barplot(by(Auto$horsepower ,Auto$origin, mean, na.rm = T),col=c("green","red","blue","yellow","pink"),
            main = "Relacion origen , caballos de fuerza ",
            xlab="Lugar donde se fabrico", ylab="Caballos de fuerza del auto")
    
    
    
    
    sd(Auto$year, na.rm = T)
    
    dim(Auto)
    
    
    
    
    quantile(Auto$year, probs = c(0.01 ,0.25, 0.5, 0.75), na.rm = T) # quantile() - Funcion para ver cuantiles
    # Podemos poner los quantiles que NECESITEMOS en este caso tenemos : 
    #   0.05 = 5%
    #   0.5 = 50%
    #   0.8 = 80%
    
    barplot(quantile(Auto$year, probs = c(0.01 ,0.25, 0.5, 0.75), na.rm = T))
    boxplot(datos$edad, na.rm = T) # Grafico de CAJA y BIGOTES
    # En el boxplot(), la raya superior coincide con el valor mas grande, y la raya inferior coincide con 
    # el valor mas pequeño
    
    # El lado inferior de la caja (la base), corresponde al primer cuantil (25%)
    
    # La RAYA NEGRA dentro de la caja corresponde a la MEDIANA (cuantil 2 - 50%)
    
    # La parte superior de la caja es el cuantil 3 - 75% 
    
    
    
    
    
    
    # Metodo 2 
    
    # Rango Intercuartilico(RIQ) - es la diferencia entre el cuantil 3 y el cuantil 1 
    
    quantile(Auto$year, probs = c(0.25, 0.75), na.rm = T)
    
    # 0.25 -- Es el cuantil 1
    # 0.75 -- Es el cuantil 3
    
    # Calculo del Rango Intercuartilico(RIQ)
    
    RIQ = 72 - 54 ; RIQ    
    
    
    # Bigote Superior = cuartil(3) + 1.5RIQ 
    
    big_sup = 72 + (1.5*18); big_sup
    # Los valores por encima al bigote superior van a ser VALORES EXTRAÑOS
    
    
    # Bigote Inferior = cuartil(1) - 1.5RIQ
    
    big_infe = 54 - (1.5*RIQ); big_infe
    # Los valores por debajo al bigote inferior va a ser VALORES EXTRAÑOS
    
    
    
    datos$sexo = factor(datos$sexo, labels = c('Hombre', 'Mujer')) # Cambiamos la etiqueta de h y m 
    # y tambien cambiamos el factor 
    
    plot(x= datos$sexo, y= datos$edad, col= c("red", "blue"), na.rm = T)
    # Se pone plot, porque quiero que me coloque en el eje x=sexo y en el eje y=edad 
    
    # Los circulos que se ven, en este caso encima del bigote superior, son los datos raros-extraños
    
   # ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            
            # MEDIDAS FORMAS
            
            
            library(moments) # Necesitamos activar la libreria para poder utilizar las medidas de forma
    
    url = 'https://raw.githubusercontent.com/fhernanb/datos/master/medidas_cuerpo'
    # Con esta opcion podemos traer bases de datos de la red al RStudio
    
    medidas = read.table(file=url, header = T)
    # Aqui leemos el archivo de la web (URL), y lo guardamos en una variable
    
    head(medidas, 10)
    # Con la funcion head() miramos las primeras filas de informacion, por defecfo 6 
    
    dim(medidas)
    # Con la funcion dim() podemos ver la dimension de la base de datos  
    
    
    hist(medidas$altura) # hist() para mostrar el histograma de las alturas
    
    
    
    
    skewness(medidas$altura) # skewness() para calcular la ASIMETRIA 
    
    # el valor simetrico es 0
    # Cualquier valor negativo mostrara asimetria negativa
    # Cualquier valor positivo mostrara asimetria positiva
    
    
    
    
    kurtosis(medidas$altura) # kurtosis() para calcular la curtosis
    


# Gráfico de la función de distribución de Poisson en R

# Rejilla de valores del eje X
x <- 0:50

#-----------
# lambda: 5
#-----------
lambda <- 5
plot(ppois(x, lambda), type = "s", lwd = 2,
     main = "Función de distribución",
     xlab = "Número de eventos", ylab = "F(x)")
# Modelo Poisson
# Rejilla de valores del eje X
x <- 0:50

#-----------
# lambda: 5
#-----------
lambda <- 5
plot(dpois(x, lambda), type = "h", lwd = 2,
     main = "Función de masa de probabilidad",
     ylab = "P(X = x)", xlab = "Número de eventos")

#----------------------------------------------------------

# Gráfico de la función de distribución de Poisson en R

# Rejilla de valores del eje X
x <- 0:50

#-----------
# lambda: 5
#-----------
lambda <- 5
plot(ppois(x, lambda), type = "s", lwd = 2,
     main = "Función de distribución",
     xlab = "Número de eventos", ylab = "F(x)")

#-----------
# lambda: 10
#-----------
lambda <- 10
lines(ppois(x, lambda), type = "s", lwd = 2, col = 2)

#-----------
# lambda: 20
#-----------
lambda <- 20
lines(ppois(x, lambda), type = "s", lwd = 2, col = 3)

# Legend
legend("bottomright", legend = c("5", "10", "20"),
       title = expression(lambda), title.adj = 0.75,
       lty = 1, col = 1:3, lwd = 2, box.lty = 0)





