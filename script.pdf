# 01.Script em R calculcar a moda da relação ao número de cilindros
# do mtcars
# moda > elemento que aparece mais vezes no conjunto de dados

cilindro <- mtcars$cyl
contador <- table(cilindro)

moda <- names(contador)[contador == max(contador)]

cat("A moda dos números de cilindro é", moda)

# 02.Script em R para calcular por meio de uma função a variancia
#populacional do número de prisões pro assalto do USArrests

varianciaPopulacional <- function(x) {
  numeroElementos <- 0
  soma <- 0
  somaQuadrada <- 0
  
  #calucal a soma e o número de elementos
  for(i in x){
    numeroElementos <- numeroElementos + 1
    soma <- soma + i
  }
  
  #Calculo da média
  media <- soma / numeroElementos
  
  #calcula a soma dos quadrados das diferenca da média
  for (i in x){
    diferenca <- i - media
    somaQuadrada <- somaQuadrada + diferenca^2
  }
  #calculo da var populacional
  varianciaPop <- somaQuadrada / numeroElementos
  
  return(varianciaPop)
}

varianciaPopulacional <- varianciaPopulacional(USArrests$Assault)
cat("A variancia populacional do número de assaltos é: ", varianciaPopulacional, "\n")


#03. Script em R para calcular a média, a mediana,
#a variância e o desvio-padrão das POPULAÇÕES de USArrests

#Sumario de USArrests
summary(USArrests)

#MEDIDAS DE LOCALIZAÇÃO
#MEDIA
mean(USArrests$UrbanPop)

#MEDIANA
median(USArrests$UrbanPop)

#DISPERSÃO
#VARIANCIA
var(USArrests$UrbanPop)

#DESVIO-PADRÃO
sd(USArrests$UrbanPop)


