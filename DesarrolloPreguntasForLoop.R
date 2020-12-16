##1) Cargue las bases de datos incorporado en cada una de ellas la variable "tamanio",donde indique de que tamaño es la empresa de ese país.(1 pto)

### Se instalan paquetes para cargar la base de dato.
install.packages('readr')
library('readr')
install.packages("csv")
library("csv")
### Se designan las variables indicadoras del atributo.
nivelCuatro <- "empresa grande"
nivelTres <- "empresa mediana"
nivelDos <- "empresa micro"
nivelUno <- "empresa pequeña"

### Se designa la variable tamanio en cada data.
grandes_peru <- cbind(grandes_peru, tamanio=nivelCuatro)
grandes_chile <- cbind(grandes_chile, tamanio=nivelCuatro)
grandes_colombia <- cbind(grandes_colombia, tamanio=nivelCuatro)

medianas_peru <- cbind(medianas_peru, tamanio=nivelTres)
medianas_chile <- cbind(medianas_chile, tamanio=nivelTres)
medianas_colombia <- cbind(medianas_colombia, tamanio=nivelTres)

pequena_peru <- cbind(pequena_peru, tamanio=nivelDos)
pequena_chile <- cbind(pequena_chile, tamanio=nivelDos)
pequena_colombia <- cbind(pequena_colombia, tamanio=nivelDos)

micro_peru <- cbind(micro_peru, tamanio=nivelUno)
micro_chile <- cbind(micro_chile, tamanio=nivelUno)
micro_colombia <- cbind(micro_colombia, tamanio=nivelUno)

#####################################################################################################################################################################################################################################
##2) Reuna todas las bases en una sola y defina de qué tipología (tipo de datos) son cada una de las variables que se encuentran en la data.(1 pto)

### Se reúne la base de datos como un uno común, con un vector.
conjuntoDeBasesEnUno<- rbind(grandes_chile, grandes_colombia, grandes_peru, medianas_chile, medianas_colombia, medianas_peru, micro_chile, micro_colombia, micro_peru, pequena_chile, pequena_colombia, pequena_peru)

### Se define la tipologia de los datos, siendo a que corresponde a cada variable que se encuentra en el data  
### Se crea una funcion que dentro recorra el vector del conjunto comun del data. 
### tipologiaDeBases: list -> String

tipologiaDeBase <- function(conjuntoDeBasesEnUno){
  for (tipologia in 1:length(conjuntoDeBasesEnUno)) {
    #### Se guarda la variable typeof para que de la respuesta correpondiente.
    tipoDeDato <- typeof(conjuntoDeBasesEnUno[tipologia])
    print(paste("La variable que se encuentra en la data de tipología",tipologia,"es de tipo de dato:",tipoDeDato))
  }
}

### Respuesta: se prueba oficialmente.
tipologiaDeBase(conjuntoDeBasesEnUno)

####################################################################################################################################################################################################################################
##3) Determine a través del uso de condicionales y/o for cuántas observaciones tiene Peru versus Chile.(2 pto)

### Se crea una función que mantenga el recorrido de la base de datos que es un vector.
obsTotales <- function(conjuntoDeBasesEnUno){
  ### Se establece la regla de ceros para que cambie los elementos.
  contadorDePeru <- 0
  contadorDeChile <- 0
  for (obs in 1:(length(conjuntoDeBasesEnUno[["pais"]]))){
    ### Condición.
    if((conjuntoDeBasesEnUno)[["pais"]][obs] == "peru"){
      contadorDePeru <- contadorDePeru + 1
      ### Condición.
    }else if((conjuntoDeBasesEnUno)[["pais"]][obs] == "chile"){
      contadorDeChile <- contadorDeChile + 1 
    }
  }
  print(paste("Peru tiene:", contadorDePeru,"observaciones. Versus Chile que tiene:", contadorDeChile,"observaciones."))
}

### Respuesta: se prueba oficialmente.
obsTotales(conjuntoDeBasesEnUno)

####################################################################################################################################################################################################################################
##4) Determine a través del uso de condicionales y/o for ¿cuál es el país con mayor ingresos de explotación para los años que considera la muestra.(2 pto)

### Se cambia la data a números.
conjuntoDeBasesEnUno[["ingresos"]] <- as.numeric(conjuntoDeBasesEnUno[["ingresos"]])

### Se crea una función que mantenga el recorrido de la base de datos que es un vector.
paisMaxIngreso <- function(conjuntoDeBasesEnUno){
  ### Se establece la regla de ceros para que cambie los elementos.
  cPe <- 0
  cCh <- 0
  cCo <- 0
  for (valores in 1:nrow(conjuntoDeBasesEnUno)){
    ### Se diseñan las condiciones igual que el ejercicio anterior. Se guardan los valores de la columna de los ingresos diferenciando el país. Con esto se busca diferenciar la posición del país e ingreso.
    if(conjuntoDeBasesEnUno[valores,2] == "peru"){
      ### Se agrega la regla de agregar el contador de la columna 3, es decir el de ingresos.
      cPe <- cPe + conjuntoDeBasesEnUno[valores,3]
    }else if(conjuntoDeBasesEnUno[valores,2] == "chile"){
      cCh <- cCh + conjuntoDeBasesEnUno[valores,3]
    }else if(conjuntoDeBasesEnUno[valores,2] == "colombia"){
      cCo <- cCo + conjuntoDeBasesEnUno[valores,3]
    }
  }
  ### Se crea vector para determinar el orden como posición específicamente respecto a los países.
  n <- c(cPe,cCh,cCo)
  ### Se crea una función LOOP la cual guarda el vector de países.
  ox <- function(n){
    #### Se agrega una fórmula de prueba para que reconozca la lógica.
    m <- ((n[1]+n[2]+n[3]) / length(n))
    ### Se establece el for para que recorra el vector y se pueda orden con el LOOP sin tener un orden especifico el país pero si el ingreso de aquel.
    for (a in 1:length(n)) {
      for (b in 1:(length(n)-1)) {
        if(n[b] > n[b+1]){
          tmp <- n[b]
          n[b] <- n[b+1]
          n[b+1] <- tmp
        }
      }
    }
  }
  ### Se crea un vector que calcula el máximo del vector anteriormente creado de los países.
  maximo <- max(n)
  #### Se diseñan las condiciones para que se designe el país correspondiente en el cálculo del LOOP y el vector respecto a el ingreso.
  if( cPe == maximo){
    pm <- "Peru"
  }else if(cCh == maximo){
    pm <- "Chile"
  }else if(cCo == maximo){
    pm <- "Colombia"
  }
  print(paste("Conclusión:", pm,"es el país con mayor ingresos de explotación de la muestra con:", n[3]))
}

## Respuesta: se prueba oficialmente.
paisMaxIngreso(conjuntoDeBasesEnUno)

####################################################################################################################################################################################################################################
## 5)Genere una variable(columna) , donde si el país es Chile multiplique la tasa de interés por 0,1, cuando sea Peru le sume 0,3 y, y finalmente si es Colombia divida por 10 (2ptos).Use condicionales y/o for.

### Se cambia la data a números.
conjuntoDeBasesEnUno[["tasa_interes"]] <- as.numeric(conjuntoDeBasesEnUno[["tasa_interes"]])

### Se crea un vector que almacene los cálculos y datos de la nueva columna
nt <- c()

### Se crea un for que recorra la columna.
for (vti in 1:nrow(conjuntoDeBasesEnUno)) {
  ### Diseño condición: se designa la posición de la columna que identifica el país. 
  if(conjuntoDeBasesEnUno[vti,"pais"] == "peru"){
    ### Se guarda el cálculo de la variable con el vector.
    nt <- c(nt, conjuntoDeBasesEnUno[[vti,"tasa_interes"]]+0.3)
  }else if(conjuntoDeBasesEnUno[vti,"pais"] == "chile"){
    nt <- c(nt, conjuntoDeBasesEnUno[[vti,"tasa_interes"]]*0.1)
  }else if(conjuntoDeBasesEnUno[vti,"pais"] == "colombia"){
    nt <- c(nt, conjuntoDeBasesEnUno[[vti,"tasa_interes"]]/10)
  }
}

## Respuesta: se prueba oficialmente.
#### Se crea el reconocimiento del vector con la tabla.
conjuntoDeBasesEnUno <- cbind(conjuntoDeBasesEnUno, nt = nt)

####################################################################################################################################################################################################################################
## 6)Reemplace en la columna exportaciones con 1 cuando es mayor a 2,1, con un 2 cuando es menor 2,1y un 3 cuando es igual a 2,1, redondee al primer decimal la variable(2 ptos). Use condicionales y/o for.

### Se cambia la data a números.
conjuntoDeBasesEnUno[["exportaciones"]] <- as.numeric(conjuntoDeBasesEnUno[["exportaciones"]])

### Se le señalan a los datos del data que debe redondear a la unidad. 
conjuntoDeBasesEnUno[["exportaciones"]] <- round(conjuntoDeBasesEnUno[["exportaciones"]],1)

#### Se establecen las variables que reemplazaran el redondeo de datos.
a <- 1
b <- 2
c <- 3

### Se crea un for que recorra la columna del data.
for (condicion in 1:(length(conjuntoDeBasesEnUno[["exportaciones"]]))) {
  ### Se establece la condición de la data.
  if(conjuntoDeBasesEnUno[["exportaciones"]][condicion] == 2.1){
    ### se guarda la variable que reemplaza en la condición.
    conjuntoDeBasesEnUno[["exportaciones"]][condicion] <- c
  }else if(conjuntoDeBasesEnUno[["exportaciones"]][condicion] < 2.1){
    conjuntoDeBasesEnUno[["exportaciones"]][condicion] <- b
  }else if(conjuntoDeBasesEnUno[["exportaciones"]][condicion] > 2.1){
    conjuntoDeBasesEnUno[["exportaciones"]][condicion] <- a
  }
}

####################################################################################################################################################################################################################################
## 7)Gráfique algunas variables seleccionadas, las cuales puedan responder a una pregunta que se haga con respecto a los datos.

### Se define la pregunta y respuesta.
### Pregunta: Graficar nivel de endeudamiento en total de algunos países latinoamericanos, específicamente: Peru, Chile y Colombia.
#### Decisión: Se crea un for que recorra la columna de endeudamiento de la data por países. 

### Se cambia la data a números.
conjuntoDeBasesEnUno[["endeudamiento"]] <- as.numeric(conjuntoDeBasesEnUno[["endeudamiento"]])

### Se definen las variables nuevas del LOOP.
ePe <- 0
eCh <- 0
eCo <- 0  

### Se crea un for que recorra la columna de endeudamiento.
for (en in 1:nrow(conjuntoDeBasesEnUno)){
  #### Diseño condición del país en su posición como país.
  if(conjuntoDeBasesEnUno[en,2] == "peru"){
    #### Almacenamiento de la variable en la posición endeudamiento.
    ePe <- ePe + conjuntoDeBasesEnUno[en,8]
  }else if(conjuntoDeBasesEnUno[en,2] == "chile"){
    eCh <- eCh + conjuntoDeBasesEnUno[en,8]
  }else if(conjuntoDeBasesEnUno[en,2] == "colombia"){
    eCo <- eCo + conjuntoDeBasesEnUno[en,8]
  }
}

#### Se crean 2 vectores que almacenen las variables anteriormente definidas.
endeudamiento <- c(ePe, eCh, eCo)
pais <- c("peru","chile","colombia")

#### Se construye el vector que diseña el grafico.
barplot(names.arg = pais, height = endeudamiento, main = "Nivel de endeudamiento de algunos paises latinoamericanos", sub = "Paises: Peru, Chile y Colombia", xlab = "Pais", ylab = "Nivel de endeudamiento")






