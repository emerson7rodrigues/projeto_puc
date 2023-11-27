# Carregando as bibliotecas
library(dplyr)
library(stringr)
library(tidyverse)

# Lendo o arquivo CSV
anuncios_df <- read.csv('dados/DFImoveis.csv', encoding = 'latin')

str(anuncios_df)

# Visualizando as primeiras linhas do dataframe
head(anuncios_df, 10)

# Removendo caracteres indesejados, quebras de linha e espaços em excesso
anuncios_df <- anuncios_df %>%
  mutate(
    Anuncio = gsub("\n|\\s{2,}", " ", Anuncio, perl = TRUE)
  )

# Visualizando o dataframe limpo
head(anuncios_df, 10)

# APLICAÇAO DE EXPRESSOES REGULARES PARA EXTRAIR AS INFORMAÇOES DESEJADAS

# [QUADRA] Extraindo as Quadras dos anuncios

# Extraindo informaçoes com base em padrões das Quadras seguido de numeros
padrao_Quadra <- "(?i)(SQS|CLS|EQS|SGAS|SEPS|SHIGS)\\s\\d+"
anuncios_df$Quadra <- str_extract(anuncios_df$Anuncio, padrao_Quadra)

# Apicando letras maíusculas no campo
anuncios_df$Quadra <- toupper(anuncios_df$Quadra)

# [AREA] Extraindo padrões de números seguidos de "m²"
anuncios_df$Area <- str_extract(anuncios_df$Anuncio, "\\d+\\s*m²")

# Apenas valores
anuncios_df$Area <- parse_number(
  x=anuncios_df$Area, 
  locale=locale(decimal_mark=',', grouping_mark='.'))

# [PRECO] Extraindo o preço do imovel
anuncios_df$Preco <- str_extract(anuncios_df$Anuncio, "R\\$\\s[0-9.,]+")

# Apenas valores
anuncios_df$Preco <- parse_number(
  x=anuncios_df$Preco, 
  locale=locale(decimal_mark=',', grouping_mark='.'))

# [QUARTOS] Extraindo a quantidade de quartos

# Extraindo informaçoes com base em padrões da palavra "quartos"
padrao_Quartos <- "\\b\\d+\\s*(quarto|quartos|Quarto|Quartos|QUARTO|QUARTOS|quarto)\\b"
anuncios_df$Quartos <- str_extract(anuncios_df$Anuncio, padrao_Quartos)

# Removendo a palavra "quartos" da coluna
anuncios_df$Quartos <- gsub("\\bquartos|QUARTOS\\b", "", anuncios_df$Quartos, ignore.case = TRUE)

# Substituindo a palavra "quarto" por um 1
anuncios_df$Quartos <- ifelse(grepl("\\bquarto\\b", anuncios_df$Quartos, ignore.case = TRUE), 1, anuncios_df$Quartos)

# Definindo a coluna como um inteiro
anuncios_df$Quartos <- as.integer(anuncios_df$Quartos)

# [SUITES] Extraindo a quantidade de Suites

# Extraindo informaçoes com base em padrões da palavra "Suítes"
padrao_Suites <- "\\b\\d+\\s*(Suítes|Suíte|suítes|suíte|SUÍTE|SUÍTES|Suítes)\\b"
anuncios_df$Suites <- str_extract(anuncios_df$Anuncio, padrao_Suites)

# Substituindo a palavra "suíte" por um 1
anuncios_df$Suites <- ifelse(grepl("\\bSuíte\\b", anuncios_df$Suites, ignore.case = TRUE), 1, anuncios_df$Suites)

# Removendo a palavra "suítes"
anuncios_df$Suites <- gsub("\\bSuítes\\b", "", anuncios_df$Suites, ignore.case = TRUE)

# Substituir "<NA>" em Suites por 0
anuncios_df$Suites <- ifelse(is.na(anuncios_df$Suites), 0, anuncios_df$Suites)

# [BANHEIROS] Extraindo a quantidade de banheiros

# Extraindo informaçoes com base em padrões da palavra "banheiros"
padrao_Banheiros <- "\\b\\d+\\s*(banheiro|banheiros|Banheiro|Banheiros|BANHEIRO|BANHEIROS|ban|banheiro)\\b"
anuncios_df$Banheiros <- str_extract(anuncios_df$Anuncio, padrao_Banheiros)

# Substituindo a palavra "banheiro" por um 1
anuncios_df$Banheiros <- ifelse(grepl("\\bbanheiro\\b", anuncios_df$Banheiros, ignore.case = TRUE), 1, anuncios_df$Banheiros)

# Removendo a palavra "banheiros"
anuncios_df$Banheiros <- gsub("\\bbanheiros\\b", "", anuncios_df$Banheiros, ignore.case = TRUE)

# Substituindo "<NA>" em Banheiros por 0
anuncios_df$Banheiros <- ifelse(is.na(anuncios_df$Banheiros), 0, anuncios_df$Banheiros)

# Adicionando uma nova coluna condicional
anuncios_df <- anuncios_df %>%
  mutate(Banheiro = ifelse(Suites == 0 & Banheiros == 0, 0,
                           ifelse(Suites > 0 & Banheiros == 0, Suites,
                                  ifelse(Suites == 0 & Banheiros > 0,
                                         Banheiros, Banheiros))))

# Removendo as colunas que não serao mais utilizadas
anuncios_df <- anuncios_df %>%
  select(-Suites, -Banheiros, -Anuncio)

# Renomeando a nova coluna criada
colnames(anuncios_df)[colnames(anuncios_df) == "Banheiro"] <- "Banheiros"

# Substituindo 0 em "Banheiros" por 1
anuncios_df$Banheiros <- ifelse(anuncios_df$Banheiros == 0, 1, anuncios_df$Banheiros)

# Removendo registros que não contem a coluna "Quadra" preenchida
anuncios_df <- subset(anuncios_df, Quadra != "<NA>")

# Exibindo a estrutura do dataframe após tratamentos
str(anuncios_df)

# Verificando registros nulos nas colunas do dataframe
nulos <- colSums(is.na(anuncios_df))
nulos

# Amostras do dataframe tratado
head(anuncios_df, 10)
tail(anuncios_df, 10)

# Salvando o datraframe em um arquivo .CSV
write.csv(anuncios_df, "dados/DFImoveisTratado.csv", row.names = FALSE)

