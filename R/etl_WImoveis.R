# Carregando as bibliotecas
library(dplyr)
library(stringr)
library(tidyverse)

# Lendo o arquivo CSV
anuncios_w <- read.csv('dados/WImoveis.csv', encoding = 'latin')

str(anuncios_w)

# Visualizando as primeiras linhas do dataframe
head(anuncios_w, 10)

# Removendo caracteres indesejados, quebras de linha e espaços em excesso
anuncios_w <- anuncios_w %>%
  mutate(
    Anuncio = gsub("\n|\\s{2,}", " ", Anuncio, perl = TRUE)
  )

# Visualizando o dataframe limpo
head(anuncios_w, 10)

# APLICAÇAO DE EXPRESSOES REGULARES PARA EXTRAIR AS INFORMAÇOES DESEJADAS

# [QUADRA] Extraindo as Quadras dos anúncios

# Extraindo informaçoes com base em padrões das Quadras seguido de numeros
padrao_Qd <- "(?i)(SQS|CLS|EQS|SGAS|SEPS|SHIGS)\\s\\d+"
anuncios_w$Quadra <- str_extract(anuncios_w$Anuncio, padrao_Qd)

# Aplicando letras maíusculas no campo
anuncios_w$Quadra <- toupper(anuncios_w$Quadra)

# [AREA] Extraindo padrões de números seguidos de "m²"
anuncios_w$Area <- str_extract(anuncios_w$Anuncio, "\\d+\\s*m²")  

# Apenas valores
anuncios_w$Area <- parse_number(
  x= anuncios_w$Area,
  locale = locale(decimal_mark = ',', grouping_mark = '.')
)

# [PRECO] Extraindo o preço do imovel
anuncios_w$Preco <- str_extract(anuncios_w$Anuncio, "R\\$\\s[0-9.,]+")

# Apenas valores
anuncios_w$Preco <- parse_number(
  x= anuncios_w$Preco,
  locale = locale(decimal_mark = ',', grouping_mark = '.')
)

# [QUARTOS] Extraindo a quantidade de quartos
padrao_Qt <- "\\b\\d+\\s*(quarto|quartos|Quarto|Quartos|QUARTO|QUARTOS|quarto)\\b"
anuncios_w$Quartos <- str_extract(anuncios_w$Anuncio, padrao_Qt)

# Removendo a palavra "quartos" da coluna
anuncios_w$Quartos <- gsub("\\bquartos|QUARTOS\\b", "", anuncios_w$Quartos, ignore.case = TRUE)

# Substituindo a palavra "quarto" por 1
anuncios_w$Quartos <- ifelse(grepl("\\bquarto|QUARTO\\b", anuncios_w$Quartos, ignore.case = TRUE), 1, anuncios_w$Quartos)

# Definindo a coluna como um inteiro
anuncios_w$Quartos <- as.integer(anuncios_w$Quartos)

# [SUITES] Extraindo a quantidade de Suites
padrao_St <- "\\b\\d+\\s*(Suítes|Suíte|suítes|suíte|SUÍTE|SUÍTES|Suítes)\\b"
anuncios_w$Suites <- str_extract(anuncios_w$Anuncio, padrao_St)

# Substituindo a palavra "suíte" por um 1
anuncios_w$Suites <- ifelse(grepl("\\bsuíte\\b", anuncios_w$Suites, ignore.case = TRUE), 1, anuncios_w$Suites)

# Removendo as palavras "suítes" ou "Suítes"
anuncios_w$Suites <- gsub("\\bsuítes|Suítes\\b", "", anuncios_w$Suites, ignore.case = TRUE)

# Substituir "NA" em Suites por 0
anuncios_w$Suites <- ifelse(is.na(anuncios_w$Suites), 0, anuncios_w$Suites)

# Definindo a coluna como um inteiro
anuncios_w$Suites <- as.integer(anuncios_w$Suites)

# [BANHEIROS] Extraindo a quantidade de banheiros

# Extraindo informações com base em padroes da palavra "banheiros"
padrao_Ban <- "\\b\\d+\\s*(banheiro|banheiros|Banheiro|Banheiros|BANHEIRO|BANHEIROS|ban|banheiro)\\b"
anuncios_w$Banheiros <- str_extract(anuncios_w$Anuncio, padrao_Ban)

# Remover as palavras "ban" e "banheiro"
anuncios_w$Banheiros <- gsub("\\bban|banheiro\\b", "", anuncios_w$Banheiros, ignore.case = TRUE)

# Substituir '<NA>' em Banheiros por 0
anuncios_w$Banheiros <- ifelse(is.na(anuncios_w$Banheiros), 0, anuncios_w$Banheiros)

# Adicionando uma nova coluna condicional
anuncios_w <- anuncios_w %>%
  mutate(Banheiro = ifelse(Suites == 0 & Banheiros == 0, 0,
                           ifelse(Suites > 0 & Banheiros == 0, Suites,
                                  ifelse(Suites == 0 & Banheiros > 0, Banheiros, Banheiros))))

# Removendo as colunas que não serão mais utilizadas
anuncios_w <- anuncios_w %>%
  select(-Suites, -Banheiros, -Anuncio)

# Renomeando a nova coluna criada
colnames(anuncios_w)[colnames(anuncios_w) == "Banheiro"] <- "Banheiros"

# Substituindo 0 em "Banheiros" por 1
anuncios_w$Banheiros <- ifelse(anuncios_w$Banheiros == 0, 1, anuncios_w$Banheiros)

# Remove registros quem não contem a Quadra preenchida
anuncios_w <- subset(anuncios_w, Quadra != "<NA>")

# Exibindo a estrutura do dataframe após tratamentos
str(anuncios_w)

# Verificando registros nulos nas colunas do dataframe
nulos <- colSums(is.na(anuncios_w))
nulos

# Amostra do dataframe tratado
head(anuncios_w, 10)
tail(anuncios_w, 10)

# Salvando o dataframe em um arquivo CSV
write.csv(anuncios_w, "dados/WImoveisTratado.csv", row.names = FALSE)

