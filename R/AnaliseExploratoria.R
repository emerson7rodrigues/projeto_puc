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

# Definindo os valores mínimos esperados
min_area <- 30  # Area mínima 30m2
min_preco <- 100000  # Preco mínimo 100k


# Filtrando o dataframe com base nos valores mínimos esperados
anunciosFiltrado <- subset(anunciosFinal, 
                           Area >= min_area & 
                             Preco >= min_preco)

str(anunciosFiltrado)

#boxplotPreco
boxplotPreco2 <- plot_ly(anunciosFiltrado, 
                         type = 'box',
                         colors = c('#FF5653')) %>%
  add_boxplot(y=~Preco,
              name = 'Preço',
              color='#FF5653') %>%
  layout(title = 'Distribuição da variável Preco',
         yaxis = list(title = 'Valor'))

# Exibir o gráfico
boxplotPreco2

#boxplotArea
boxplotArea <- plot_ly(anunciosFiltrado, 
                       type = 'box',
                       colors = c('#0109F9')) %>%
  add_boxplot(y=~Area,
              name = 'Area',
              color='#0109F9') %>%
  layout(title = 'Distribuição da variável Area',
         yaxis = list(title = 'Valor'))

# Exibir o gráfico
boxplotArea

#histPreco2 (Sem Outliers)
histPreco2 <- plot_ly(anunciosFiltrado, x = ~Preco, type = 'histogram') %>%
  layout(title = 'Distribuição dos Preços dos Imóveis',
         xaxis = list(title = 'Preço'),
         yaxis = list(title = 'Frequência'))

# Exibir o gráfico
histPreco2

#Gráfico de Dispersão (Scatter plot) Area x Preço

scatter_plot <- plot_ly(anunciosFiltrado, 
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
anunciosFiltrado <- na.omit(anunciosFiltrado)

# Verificando registros nulos nas colunas do dataframe
nulos <- colSums(is.na(anunciosFinal))
nulos

str(anunciosFiltrado)

#Gráfico de Dispersão (Scatter plot) Preço x Quartos

scatter_plot2 <- plot_ly(anunciosFiltrado, 
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

scatter_plot3 <- plot_ly(anunciosFiltrado, 
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
anunciosFiltrado <- anunciosFiltrado[!is.na(anunciosFiltrado$Preco),]

#Teste de correlação Preço x Quartos
corPrecoQuarto <- cor.test(anunciosFiltrado$Preco, anunciosFiltrado$Quartos, method = "pearson")
paste0("O coeficiente de correlação Preco x Quartos é: ",corPrecoQuarto)


#Teste de correlação Preço x Banheiros
corPrecoBanheiros <- cor.test(anunciosFiltrado$Preco, anunciosFiltrado$Banheiros, method = "pearson")
paste0("O coeficiente de correlação Preco x Banheiros é: ",corPrecoBanheiros)

#Tabela Resumo da Análise Exploratória dos Dados
resumoAED <- anunciosFiltrado %>%
  summarise(Quantidade = n(),
            PrecoMedio = round(mean(Preco, na.rm = TRUE),2),
            Desvio = round(sd(Preco, na.rm = TRUE), 2),
            MediaArea = round(mean(Area), 2),
            MediaQuartos = round(mean(Quartos),2),
            MediaBanheiros = round(mean(Banheiros),2)
  )

resumoAED

#Análise localização
resumoAELoc <- anunciosFiltrado %>%
  group_by(Quadra) %>%
  summarise(Quantidade = n(),
            PrecoMedio = mean(Preco)) %>%
  arrange(PrecoMedio)

#Grafico de barras do preço medio por Quadra  
graficoBarra <- plot_ly(data = resumoAELoc,
                        type = 'bar', x=~Quadra, y=~PrecoMedio)

graficoBarra
