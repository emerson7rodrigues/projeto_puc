# Carregar as bibliotecas
library(RSelenium)
library(rvest)

# Configurar o serviço do GeckoDriver
gecko_driver <- "C:\\R\\geckodriver-v0.33.0-win64\\geckodriver.exe"

# Configurar opções do Selenium
driver_options <- list("marionette" = TRUE)

# Configurar o cliente remoto do WebDriver
driver <- RSelenium::rsDriver(
  port = 4448L, 
  browser = c("firefox"),
  version = "latest",
  chromever = NULL,
  extraCapabilities = driver_options
)

# Obter o cliente remoto
remDr <- driver$client

# URL a ser aberta
url_base <- "https://www.dfimoveis.com.br/venda/df/brasilia/asa-sul/apartamento?pagina="

# Número de páginas a serem percorridas
num_paginas <- 50

# Lista para armazenar os resultados
resultados <- list()

# Loop de Páginas
for (pagina in 1:num_paginas) {
  # Construa a URL da página
  url <- paste0(url_base, pagina)
  
  # Abrir o navegador
  remDr$open()
  
  # Navegar para a URL
  remDr$navigate(url)
  
  # Esperar um tempo para a página carregar completamente
  Sys.sleep(20)
  
  # Requisição HTTP
  df_page <- read_html(remDr$getPageSource()[[1]])
  
  # Selecione os elementos HTML desejados (anúncios)
  anuncios_css <- df_page %>%
    html_elements(".new-info")
  
  # Renderize os elementos HTML como texto
  anuncios_df_text <- html_text(anuncios_css)
  
  # Armazene os resultados na lista
  resultados[[pagina]] <- anuncios_df_text
  
  # Combinando os resultados em um vetor
  resultados_combinados <- unlist(resultados)
  
  # Criaçao do dataframe
  df_df_resultados <- data.frame(Anuncio = resultados_combinados)
  
  # Estrutura do dataframe
  str(df_df_resultados)
  
  # Gerar .csv do dataframe
  write.csv(df_df_resultados, "dados/DFimoveis.csv", row.names = FALSE)
  
  # Fechar o navegador
  remDr$close()
}
