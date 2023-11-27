# Carregar as bibliotecas
library(RSelenium)
library(rvest)

# Configurar o serviço do GeckoDriver (certifique-se de ter o GeckoDriver instalado)
gecko_driver <- "C:\\R\\geckodriver-v0.33.0-win64\\geckodriver.exe"

# Configurar opções do Selenium
driver_options <- list("marionette" = TRUE)

# Configurar o cliente remoto do WebDriver
driver <- RSelenium::rsDriver(
  port = 4446L, 
  browser = c("firefox"),
  version = "latest",
  chromever = NULL,
  extraCapabilities = driver_options
)

# Obter o cliente remoto
remDr <- driver$client

# URL a ser aberta
url_base <- "https://www.wimoveis.com.br/apartamentos-venda-asa-sul-brasilia-pagina-"
# Número de páginas a serem percorridas
num_paginas <- 30

# Lista para armazenar os resultados
resultados <- list()

# Loop de Páginas
for (pagina in 1:num_paginas) {
  # Construa a URL da página
  url <- paste0(url_base, pagina, ".html")
  
  # Abrir o navegador
  remDr$open()
  
  # Navegar para a URL
  remDr$navigate(url)
  
  # Esperar um tempo para a página carregar completamente
  Sys.sleep(10)
  
  # Requisição HTTP
  w_page <- read_html(remDr$getPageSource()[[1]])
  
  # Selecione os elementos HTML desejados (anúncios)
  anuncios_css <- w_page %>%
    html_elements(".sc-i1odl-3")
  
  # Renderize os elementos HTML como texto
  anuncios_w_text <- html_text(anuncios_css)
  
  # Armazene os resultados na lista
  resultados[[pagina]] <- anuncios_w_text
  
  # Combinando os resultados em um vetor
  resultados_combinados <- unlist(resultados)
  
  # Criaçao do dataframe
  df_w_resultados <- data.frame(Anuncio = resultados_combinados)
  
  # Estrutura do dataframe
  str(df_w_resultados)
  
  # Gerar .csv do dataframe
  write.csv(df_w_resultados, "dados/Wimoveis.csv", row.names = FALSE)
  
  # Fechar o navegador
  remDr$close()
}
