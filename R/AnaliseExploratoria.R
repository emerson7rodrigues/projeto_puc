#INTEGRAÇAO DOS DATASETS

#Importando os datasets criados
anunciosDFImoveis <- read.csv("dados/DFImoveisTratado.csv")
anunciosWImoveis <- read.csv("dados/WImoveisTratado.csv")

#Concatenando os datasets
anunciosIntegrado <- rbind(anunciosDFImoveis, anunciosWImoveis)

#Estrutura do novo datasheet
str(anunciosIntegrado)

#ANÁLISE EXPLORATORIA DOS DADOS

#Carregando as bibliotecas
library(plotly)

#Distriuição dos preço dos imóveis (ggplot_ly)

#histPreco
histPreco <- plot_ly(anunciosIntegrado, x = ~Preco, type = 'histogram') %>%
  layout(title = 'Distribuição dos Preços dos Imóveis',
         xaxis = list(title = 'Preço'),
         yaxis = list(title = 'Frequência'))

# Exibir o gráfico
histPreco

#Tratamento de outliers

#PrecoOut
anunciosFinal <- anunciosIntegrado[-which(anunciosIntegrado$Preco %in% 
                                            boxplot(anunciosIntegrado$Preco, plot=F)$out),]
#AreaOut
anunciosFinal <-anunciosFinal[-which(anunciosFinal$Area %in% 
                                       boxplot(anunciosFinal$Area, plot=F)$out),]

str(anunciosFinal)

#boxplotPreco
boxplotPreco2 <- plot_ly(anunciosFinal, 
                         type = 'box',
                         colors = c('#FF5653')) %>%
  add_boxplot(y=~Preco,
              name = 'Preço',
              color='#FF5653') %>%
  layout(title = 'Distribuição dos Preços dos Imóveis',
         yaxis = list(title = 'Valor'))

# Exibir o gráfico
boxplotPreco2

#boxplotArea
boxplotArea <- plot_ly(anunciosFinal, 
                       type = 'box',
                       colors = c('#0109F9')) %>%
  add_boxplot(y=~Area,
              name = 'Area',
              color='#0109F9') %>%
  layout(title = 'Distribuição dos Preços dos Imóveis',
         yaxis = list(title = 'Valor'))

# Exibir o gráfico
boxplotArea

#histPreco2 (Sem Outliers)
histPreco2 <- plot_ly(anunciosFinal, x = ~Preco, type = 'histogram') %>%
  layout(title = 'Distribuição dos Preços dos Imóveis',
         xaxis = list(title = 'Preço'),
         yaxis = list(title = 'Frequência'))

# Exibir o gráfico
histPreco2

#Gráfico de Dispersão (Scatter plot) Area x Preço

scatter_plot <- plot_ly(anunciosFinal, 
                        x= ~Area, 
                        y= ~Preco,
                        type = 'scatter',
                        mode = 'markers') %>%
  layout(title = 'Relação entre Preço x Área',
         xaxis = list(title = 'Área'),
         yaxis = list(title = 'Preço'))


#Exibir gráfico
scatter_plot

#Tratar valores ausentes (remover todos os NA do dataset)
anunciosFinal <- na.omit(anunciosFinal)

str(anunciosFinal)

#Gráfico de Dispersão (Scatter plot) Preço x Quartos

scatter_plot2 <- plot_ly(anunciosFinal, 
                         x= ~Quartos, 
                         y= ~Preco,
                         type = 'scatter',
                         mode = 'markers') %>%
  layout(title = 'Relação entre Preço x Quartos',
         xaxis = list(title = 'Quartos'),
         yaxis = list(title = 'Preço'))


#Exibir gráfico
scatter_plot2


#Gráfico de Dispersão (Scatter plot) Preço x Banheiros

scatter_plot3 <- plot_ly(anunciosFinal, 
                         x= ~Banheiros, 
                         y= ~Preco,
                         type = 'scatter',
                         mode = 'markers') %>%
  layout(title = 'Relação entre Preço x Banheiros',
         xaxis = list(title = 'Banheiros'),
         yaxis = list(title = 'Preço'))


#Exibir gráfico
scatter_plot3

#Remover dados faltantes (Anuncios sem preco)
anunciosFinal <- anunciosFinal[!is.na(anunciosFinal$Preco),]

#Teste de correlação Preço x Quartos
precoXquartos <- cor(x=anunciosFinal$Preco, y=anunciosFinal$Quartos)
paste0("O coeficiente de correlação Preço x Quartos é: ", precoXquartos)

#Teste de correlação Preço x Banheiros
precoXbanheiros <- cor(x=anunciosFinal$Preco, y=anunciosFinal$Banheiros)
paste0("O coeficiente de correlação Preço x Banheiros é: ", precoXbanheiros)

#Tabela Resumo da Análise Exploratória dos Dados
resumoAED <- anunciosFinal %>%
  summarise(Quantidade = n(),
            PrecoMedio = round(mean(Preco, na.rm = TRUE),2),
            Desvio = round(sd(Preco, na.rm = TRUE), 2),
            MediaArea = round(mean(Area), 2),
            MediaQuartos = round(mean(Quartos),2),
            MediaBanheiros = round(mean(Banheiros),2)
  )

resumoAED

#Análise localização
resumoAELoc <- anunciosFinal %>%
  group_by(Quadra) %>%
  summarise(Quantidade = n(),
            PrecoMedio = mean(Preco)) %>%
  arrange(PrecoMedio)

#Grafico de barras do preço medio por Quadra  
graficoBarra <- plot_ly(data = resumoAELoc,
                        type = 'bar', x=~Quadra, y=~PrecoMedio)

graficoBarra
