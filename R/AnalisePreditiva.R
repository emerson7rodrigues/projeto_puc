#Instalaçao de pacotes necessarios
install.packages("caret")
install.packages("tidyverse")
install.packages("randomForest")
install.packages("class")

#Carregando bibliotecas
library(caret)
library(tidyverse)
library(randomForest)
library(class)

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
  anunciosFinalPred <- anunciosIntegradoPred[-which(anunciosIntegradoPred$Preco %in% 
                                              boxplot(anunciosIntegradoPred$Preco, plot=F)$out),]
  #AreaOut
  anunciosFinalPred <-anunciosFinalPred[-which(anunciosFinalPred$Area %in% 
                                         boxplot(anunciosFinalPred$Area, plot=F)$out),]
  
  str(anunciosFinalPred)
  
  # Definindo os valores mínimos esperados
  min_area <- 30  # Area mínima 30m2
  min_preco <- 100000  # Preco mínimo 100k
  max_ban <- 10
  
  
  # Filtrando o dataframe com base nos valores mínimos esperados
  anunciosFiltradoPred <- subset(anunciosFinalPred, 
                             Area >= min_area & 
                               Preco >= min_preco &
                               Banheiros <= max_ban)
  
  str(anunciosFiltradoPred)
  
  #Tratar valores ausentes (remover todos os NA do dataset)
  anunciosFiltradoPred <- na.omit(anunciosFiltradoPred)
  
  str(anunciosFiltradoPred)
  
  # Verificando registros nulos nas colunas do dataframe
  nulos <- colSums(is.na(anunciosFiltradoPred))
  nulos
  
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
  
  # MODELO 1: ARVORES DE DESCISAO
  
    # Ajuste do modelo Random Forest
    modelo_rf <- randomForest(Preco ~ ., data = dados_Treino, ntree = 500)
    
    # Previsões no conjunto de teste
    previsoes_rf <- predict(modelo_rf, newdata = dados_Teste)
    
    # Desvio Padrão dos Resíduos
    dpr_rf <- sd(previsoes_rf - dados_Teste$Preco)
    cat("Desvio Padrao dos Resídudos: ", dpr_rf, "\n")
    
    # Erro absoluto médio (MAE)
    mae_rf <- mean(abs(previsoes_rf - dados_Teste$Preco))
    cat("Erro Absoluto Médio (MAE):", mae_rf, "\n")
    
    # Erro percentual medio (MAPE)
    mape_rf <- mean(abs((previsoes_rf - dados_Teste$Preco) / dados_Teste$Preco)) * 100
    cat("Erro Percentual Médio (MAPE):", mape_rf, "%\n")
    
    # Desempenho do modelo
    rmse_rf <- sqrt(mean((previsoes_rf - dados_Teste$Preco)^2))
    cat("RMSE (Árvores de Decisão):", rmse_rf, "\n")
    
    
    #HIPERPARAMETROS
    
    # Ajuste do modelo Random Forest com hiperparametros
    modelo_rf <- randomForest(Preco ~ ., data = dados_Treino, ntree = 500,
                              mtry = 4,
                              nodesize =10)
    
    #RODAR NOVAMENTE
  
  # MODELO 2: k-Nearest Neighbors (KNN)
    library(class)
    
    #Remover valores ausentes de treino e teste
    dados_Treino <- dados_Treino[complete.cases(dados_Treino[, colum_Pred]), ]
    dados_Teste <- dados_Teste[complete.cases(dados_Teste[, colum_Pred]), ]
    
    # Número de vizinhos (k)
    k <- 5
    
    # Colunas preditoras
    colum_Pred <- c("Quadra", "Area", "Quartos", "Banheiros")
    
    # Ajuste do modelo KNN
    modelo_knn <- knn(
      train = dados_Treino[, colum_Pred],
      test = dados_Teste[, colum_Pred],
      cl = dados_Treino$Preco, 
      k = k)
    
    # Resumo do modelo
    summary(modelo_knn)
    
    # Previsoes
    previsoes_knn <- as.vector(modelo_knn$test$y)

    
    # Desempenho do modelo
    rmse_knn <- sqrt(mean((modelo_knn - dados_Teste$Preco)^2))
    cat("RMSE (k-Nearest Neighbors):", rmse_knn, "\n")
  
  
  # MODELO 3: REGRESSAO LINEAR MULTIPLA
    modelo_lm <- lm(Preco ~ ., data = dados_Treino)
    
    # Resumo do modelo
    summary(modelo_lm)
    
    # Convertendo 'Quadra' para fator usando os níveis do conjunto de treino
    dados_Teste$Quadra <- factor(dados_Teste$Quadra, levels = levels(dados_Treino$Quadra))
    
    # Remover dados ausentes
    dados_Treino <- na.omit(dados_Treino)
    dados_Teste <- na.omit(dados_Teste)
    
    # Previsões no conjunto de teste
    previsoes_lm <- predict(modelo_lm, newdata = dados_Teste)
    
    # Desvio Padrão dos Resíduos
    dpr_lm <- sd(previsoes_lm - dados_Teste$Preco)
    cat("Desvio Padrao dos Resídudos: ", dpr_lm, "\n")
    
    # Erro absoluto médio (MAE)
    mae_lm <- mean(abs(previsoes_lm - dados_Teste$Preco))
    cat("Erro Absoluto Médio (MAE):", mae_lm, "\n")
    
    # Erro percentual medio (MAPE)
    mape_lm <- mean(abs((previsoes_lm - dados_Teste$Preco) / dados_Teste$Preco)) * 100
    cat("Erro Percentual Médio (MAPE):", mape_lm, "%\n")
    
    # Desempenho do modelo
    rmse_lm <- sqrt(mean((previsoes_lm - dados_Teste$Preco)^2))
    cat("RMSE (Regressão Linear Múltipla):", rmse_lm, "\n")
    
    
  # MODELO 4: GRADIENT BOOST REGRESSOR
    install.packages("gbm")
    library(gbm)
    
    #Convertendo quadra para fator
    dados_Treino$Quadra <- as.factor(dados_Treino$Quadra)
    
    
    #Grade de hiperparametros
    hyper_grid <- expand.grid(
      n.trees = c(50, 100, 150),
      interaction.depth = c(3, 5, 7),
      shrinkage = c(0.01, 0.1, 0.2)
    )
    
    # Inicializar a lista
    resultados <- list()
      
    # Pesquisa em grade
    for (i in 1:nrow(hyper_grid)) {
      modelo_gbm <- gbm(Preco ~ ., data = dados_Treino, distribution = "gaussian",
                        n.trees = hyper_grid$n.trees[i],
                        interaction.depth = hyper_grid$interaction.depth[i],
                        shrinkage = hyper_grid$shrinkage[i])
      
      previsoes_gbm <- predict(modelo_gbm, newdata = dados_Teste, n.trees = hyper_grid$n.trees[i])
      
      dpr_gbm <- sd(previsoes_gbm - dados_Teste$Preco)
      mae_gbm <- mean(abs(previsoes_gbm - dados_Teste$Preco))
      mape_gbm <- mean(abs((previsoes_gbm - dados_Teste$Preco) / dados_Teste$Preco)) * 100
      
      resultados[[i]] <- c(dpr_gbm, mae_gbm, mape_gbm, hyper_grid[i, ])
    }
      
    # Encontre os melhores hiperparâmetros
    melhores_parametros <- hyper_grid[which.min(sapply(resultados, `[`, 1)), ]
      
    # Imprima os melhores hiperparâmetros
    cat("Melhores Hiperparâmetros:\n")
      
    #Treino
    modelo_gbm <- gbm(Preco ~ ., data = dados_Treino, distribution = "gaussian", n.trees = 100, interaction.depth = 3)
    
    # Previsoes no conjunto teste
    previsoes_gbm <- predict(modelo_gbm, newdata = dados_Teste, n.trees = 100)
    
    # Desvio Padrão dos Resíduos
    dpr_gbm <- sd(previsoes_gbm - dados_Teste$Preco)
    cat("Desvio Padrao dos Resídudos: ", dpr_gbm, "\n")
    
    # Erro absoluto médio (MAE)
    mae_gbm <- mean(abs(previsoes_gbm - dados_Teste$Preco))
    cat("Erro Absoluto Médio (MAE):", mae_gbm, "\n")
    
    # Erro percentual medio (MAPE)
    mape_gbm <- mean(abs((previsoes_gbm - dados_Teste$Preco) / dados_Teste$Preco)) * 100
    cat("Erro Percentual Médio (MAPE):", mape_gbm, "%\n")

    #Desempenho do modelo
    erro <- sqrt(mean((previsoes_gbm - dados_Teste$Preco)^2))
    print(paste("RMSE:", erro))
    