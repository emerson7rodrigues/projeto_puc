#Instalaçao de pacotes necessarios
install.packages("caret")
install.packages("tidyverse")
install.packages("randomForest")

#Carregando bibliotecas
library(caret)
library(tidyverse)
library(randomForest)

#INTEGRACAO

  #Importando os datasets criados
  anunciosDFImoveis <- read.csv("dados/DFImoveisTratado.csv")
  anunciosWImoveis <- read.csv("dados/WImoveisTratado.csv")
  
  #Concatenando os datasets
  anunciosIntegradoPred <- rbind(anunciosDFImoveis, anunciosWImoveis)
  
  #Estrutura do novo datasheet
  str(anunciosIntegradoPred)

#TRATAMENTOS INICIAIS

  #Tratamento de outliers
  #PrecoOut
  anunciosFinalPred <- anunciosIntegrado[-which(anunciosIntegrado$Preco %in% 
                                              boxplot(anunciosIntegrado$Preco, plot=F)$out),]
  #AreaOut
  anunciosFinalPred <-anunciosFinalPred[-which(anunciosFinalPred$Area %in% 
                                         boxplot(anunciosFinalPred$Area, plot=F)$out),]
  
  str(anunciosFinalPred)
  
  # Definindo os valores mínimos esperados
  min_area <- 30  # Area mínima 30m2
  min_preco <- 100000  # Preco mínimo 100k
  
  
  # Filtrando o dataframe com base nos valores mínimos esperados
  anunciosFiltradoPred <- subset(anunciosFinalPred, 
                             Area >= min_area & 
                               Preco >= min_preco)
  
  str(anunciosFiltradoPred)
  
  #Tratar valores ausentes (remover todos os NA do dataset)
  anunciosFiltradoPred <- na.omit(anunciosFiltradoPred)
  
  str(anunciosFiltradoPred)
  
  # Verificando registros nulos nas colunas do dataframe
  nulos <- colSums(is.na(anunciosFiltradoPred))
  nulos
  
  str(anunciosFiltradoPred)
  
  #Remover dados faltantes (Anuncios sem preco)
  anunciosFiltradoPred <- anunciosFiltradoPred[!is.na(anunciosFiltradoPred$Preco),]
  
  str(anunciosFiltradoPred)
  
#TESTE DE CORRELACAO

  #Teste de correlação Preço x Quartos
  corPrecoQuarto <- cor.test(anunciosFiltradoPred$Preco, anunciosFiltradoPred$Quartos, method = "pearson")
  paste0("O coeficiente de correlação Preco x Quartos é: ",corPrecoQuarto)
  
  
  #Teste de correlação Preço x Banheiros
  corPrecoBanheiros <- cor.test(anunciosFiltradoPred$Preco, anunciosFiltradoPred$Banheiros, method = "pearson")
  paste0("O coeficiente de correlação Preco x Banheiros é: ",corPrecoBanheiros)
  
  #Teste de correlação Preço x Area
  corPrecoArea <- cor.test(anunciosFiltradoPred$Area, anunciosFiltradoPred$Area, method = "pearson")
  paste0("O coeficiente de correlação Preco x Banheiros é: ",corPrecoArea)


#ANALISE PREDITIVA

  # DIVIDINDO OS CONJUNTOS DE DADOS TREINAMENTO E TESTE
  
  # Definindo a semente para reproduzibilidade
  set.seed(123)
  
  # Criando índices para divisão dos dados
  indices_Treino <- createDataPartition(anunciosFiltradoPred$Preco, p = 0.7, list = FALSE)
  dados_Treino <- anunciosFiltradoPred[indices_Treino, ]
  dados_Teste <- anunciosFiltradoPred[-indices_Treino, ]
  
  #MODELO 1: REGRESSAO LINEAR MULTIPLA
  modelo_lm <- lm(Preco ~ ., data = dados_Treino)
  
  # Resumo do modelo
  summary(modelo_lm)
  
  # Converta 'Quadra' para fator usando os níveis do conjunto de treino
  dados_Teste$Quadra <- factor(dados_Teste$Quadra, levels = levels(dados_Treino$Quadra))
  
  # Faça previsões no conjunto de teste
  previsoes_lm <- predict(modelo_lm, newdata = dados_Teste)
  
  # Avalie o desempenho do modelo
  rmse_lm <- sqrt(mean((previsoes_lm - dados_Teste$Preco)^2))
  cat("RMSE (Regressão Linear Múltipla):", rmse_lm, "\n")
