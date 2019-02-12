#Willians 11/2/2019
#usando o pacote neuralnet
#para classificar as especies de iris do pacote iris
#Instalar o pacote neuralnet
#usando multicamada
#install.packages("neuralnet")
library(neuralnet) 

#binarizar a class iris
myiris = iris #clone de iris
#binarizar as colunas
#nesse comando aonde setosa for verdadeiro vira 1 e falso vira 0
myiris = cbind(myiris, myiris$Species == 'setosa')
#mostrar a binarizaçao
head(myiris)
tail(myiris) #verificar se o restante é falso
myiris = cbind(myiris, myiris$Species == 'versicolo')
myiris = cbind(myiris, myiris$Species == 'virginica')
summary(myiris)
#colocar os nomes da coluna de acordo com a class
names(myiris)[6]='setosa'
names(myiris)[7]='versicolo'
names(myiris)[8]='virginica'
summary(myiris)

#dividir os dados em treino e teste
amostra = sample(2,150,replace=T, prob = c(0.7,0.3))
myiristreino = myiris[amostra==1,]
myiristeste = myiris[amostra==2,]
dim(myiristreino)
dim(myiristeste)

#criar o modelo da rede neural, modelado para resolver essa situação
modeloir= neuralnet(setosa + versicolo + virginica ~ Sepal.Length + Sepal.Width + 
                      Petal.Length + Petal.Width, myiristreino, hidden = c(5,4))
print(modeloir)
plot(modeloir)

#testar o modelo com os dados de teste
teste  = compute(modeloir, myiristeste[,1:4])
teste$net.result

#Criar um dataframe os resultados e colocar o nome da coluna ao maior peso atribuido
resultado = as.data.frame(teste$net.result)
names(resultado)[1]='setosa'
names(resultado)[2]='versicolo'
names(resultado)[3]='virginica'
head(resultado)

#criar uma terceira coluna para criar a matris de confusao
resultado$class = colnames(resultado[,1:3])[max.col(resultado[,1:3], ties.method = 'first')]
head(resultado)

resultado

#criar a matriz de confusão para saber quanto acertei
confusao = table(resultado$class, myiristeste$Species)
#usar essa soma diagonal quando tem mais de dois atributos
sum(diag(confusao)*100/ sum(confusao))
confusao




