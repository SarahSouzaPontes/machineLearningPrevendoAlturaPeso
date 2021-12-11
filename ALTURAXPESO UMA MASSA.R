#GERANDO MASSA DE NUMEROS EM X E Y E DEPOIS TREINANDO O MODELO
getwd()


## Exercício 1 - Massa de dados aleatória

# Criando a massa de dados (apesar de aleatória, y possui 
# uma relação com os dados de x)
x <- seq(0, 100)
y <- 2 * x + 35

# Imprimindo as variáveis
x
y

# Gerando uma distribuição normal
y1 <- y + rnorm(101, 0, 50)
y1
hist(y1)

# Crie um plot do relacionamento de x e y1
plot(x, y1, pch = 19, xlab = 'X', ylab = 'Y')

# Crie um modelo de regressão para as duas variáveis x e y1
modelo <- lm(y1 ~ x)
modelo
class(modelo)
#UMA VARIAVEL PREDITORA

# Capture os coeficentes
a <- modelo$coefficients[1]
b <- modelo$coefficients[2]

#_____GRAFICO DE DISPERSAO VERIFICAR COMPORTAMENTO DE MAIS DE UMA VARIÁVEL
# Fórmula de Regressão
y2 <- a + b*x
#ALIMENTANDO Y A PARTIR X PARA APRENDER OS COEFICIENTES

# Visualize a linha de regressão
lines(x, y2, lwd = 2)

# Simulando outras possíveis linhas de regressão
#LINHA SAO OUTRAS REGRESSOES
y3 <- (y2[51]-50*(b-1))+(b-1)*x
y4 <- (y2[51]-50*(b+1))+(b+1)*x
y5 <- (y2[51]-50*(b+2))+(b+2)*x
lines(x,y3,lty=3)
lines(x,y4,lty=3)
lines(x,y5,lty=3)

#____________________________________________________________________
##  Pesquisa sobre idade e tempo de reação

# Criando os dados
Idade <- c(9,13,14,21,15,18,20,8,14,23,16,21,10,12,20,
           9,13,5,15,21)

Tempo <- c(17.87,13.75,12.72,6.98,11.01,10.48,10.19,19.11,
           12.72,0.45,10.67,1.59,14.91,14.14,9.40,16.23,
           12.74,20.64,12.34,6.44)
#QUANDO MAIS O TEMPO AUMENTA MENOR O TEMPO TE REACAO
#Gráfico de Dispersão (ScatterPlot)
plot(Idade, Tempo, 
     xlab = 'Idade', 
     ylab = 'Tempo de Reação')

# Crie um modelo de regressão IDADE É A PREDITORA
modelo <- lm(Tempo ~ Idade)
modelo

# Calcule a reta de regressão
y <- a + b*x
reta <- 25.8134 - 0.9491 * Idade
#RETA TEM AS PREVISOES

# Gráfico da reta
lines(Idade,reta)

#____________________________________________________________

# Relação entre altura e peso

#LISTAS DE ALTURA E PESO
#PESO EM CM
#PESO EM QUILOGRAMAS
# Criando os dados
alturas = c(176, 154, 138, 196, 132, 176, 181, 169, 150, 175)
pesos = c(82, 49, 53, 112, 47, 69, 77, 71, 62, 78)

#ALTURA NO EIXO X E PESO NO X
#CORRELACAO POSITIVA E NAO CASUALIDADE
plot(alturas, pesos, pch = 16, cex = 1.3, col = "blue", 
     main = "Altura x Peso", 
     ylab = "Peso Corporal (kg)", 
     xlab = "Altura (cm)")

#ver correlacao primeiro e depois ver o modelo
# Modelo de regressão
modelo <- lm(pesos ~ alturas)
#peso target e altura preditora

# Visualizando o modelo
modelo
#sumario do modelo residuos e r ao quadrado coeficiente de determinacao de 0 a 100 
#quanto maior melhor
summary(modelo)
# Gere a linha de regressão 8.854

#PEGAR OS COEFICIENTES DO MODELO
#Coefficients:
#  (Intercept)      alturas  
#-70.4627       0.8528  
#insere na função abline
abline(-70.4627, 0.8528)
#coeficiente aquilo que o modelo aprende

# Previsões de pesos com base na nova lista de alturas
#LISTA DE ALTURAS NUNCA VISTA PELO MODLEO
alturas2 = data.frame(c(179, 152, 134, 197, 131, 178, 185, 162, 155, 172))
#FUNCAO PREDICT PRECISA DADOS EM DATAFRAME POR ISSO ALTURA2
previsao <- predict(modelo, alturas2)
previsao

#PREVISAO
#1        2        3        4        5        6        7 
#79.63709 60.87462 47.22918 96.69388 42.11214 79.63709 83.90129 
#8        9       10 
#73.66721 57.46326 78.78425 


# Plot
plot(alturas, pesos, pch = 16, cex = 1.3, 
     col = "blue", 
     main = "Altura x Peso", 
     ylab = "Peso (kg)", 
     xlab = "Altura (cm)")

# Construindo a linha de regressão PASSANDO O MODELO COMO PARAMETRO
abline(lm(pesos ~ alturas)) 
#81% E ACURACIA

# Obtendo o tamanho de uma das amostras de dados
num <- length(alturas)
num
#CALCULAR COMPRIMENTO DA ALTURA==>10

# Gerando um gráfico com os valores residuais
#ERROS DO MODELO
#LOOK FOR PERCEORRE CADA UMA DAS ALTURAS E BUSCAR  A FUNCAO LINES PARA CADA ALTURA E PESO
for (k in 1: num)  
  lines(c(alturas[k], alturas[k]), 
        c(pesos[k], pesos[k]))

# Gerando gráficos com a distribuição dos resíduos
par(mfrow = c(2,2))
#DIVIDIDU A ÁREA DE PROTAGEM 2 POR 2 
plot(modelo)

