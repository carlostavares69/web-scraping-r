##############################################################################################
## CONFIGURACOES DO PROJETO
##############################################################################################

# Endereco do site que contem os dados 
URL_site <- "https://www.sspds.ce.gov.br/estatisticas-2-2-2-2-2-2/"

# Nome padrao para todos os arquivos
nome_arquivo <- c("Homicidios_CEARA_2014-2018")

# Nomes da planilha EXCEL (.xls | .xlsx)
arquivo_csv <- nome_arquivo

# Pacotes utilizados na analise.
pacotes_analise <- c("tidyverse","tabulizer","rvest","readr","stringi","readxl","lubridate",
                     "rgdal","leaflet","htmlwidgets","ggthemes","forecast","prophet")

##############################################################################################
## CONFIGURACOES DO RSTUDIO
##############################################################################################

# Codificacao para representar qualquer caractere universal padrao Unicode.
options(encoding="UTF-8")

# Aspectos da internacionalizacao (locale) para Portugues Brasileiro.
Sys.setlocale(category = "LC_ALL", locale = "Portuguese")

##############################################################################################
## ESTRUTURA DO PROJETO
##############################################################################################

dir_auxiliares <- c("Auxiliares")
dir_arquivos <- c("Arquivos")
dir_dados <- c("Dados")
dir_graficos <- c("Graficos")
dir_scripts <- c("Scripts")
dir_docs <- c("Documentos")
sub_dir1 <- paste(dir_auxiliares, "Documentos Padronizados", sep = "/")
sub_dir2 <- paste(sub_dir1, "Docs Reformatados", sep = "/")

# Vetor contendo o camainho absoluto dos diretorios
diretorios <- c(dir_auxiliares, dir_arquivos, dir_dados, dir_graficos, dir_scripts, dir_docs)

# Cria pasta e subpasta caso nao existam
for (diretorio in diretorios) {
  if(!dir.exists(diretorio)) {
    dir.create(diretorio, recursive = TRUE)
  } 
}

##############################################################################################
## PACOTES
##############################################################################################

print("Instalando pacotes:")
print(pacotes_analise)
# Pacotes necessarios para RMarkdown.
pacotes_padrao <- c("rmarkdown","knitr","prettydoc","tinytex","installr")
if (length(setdiff(c(pacotes_padrao, pacotes_analise), rownames(installed.packages()))) > 0) {
  suppressMessages(install.packages(setdiff(c(pacotes_padrao, pacotes_analise), rownames(installed.packages())), dependencies=TRUE))
  # RMarkdown precisa de Pandoc e MiKTeX instalados. https://miktex.org/2.9/setup.
  if(!require(pacotes_padrao[length(pacotes_padrao)])){
    suppressMessages(install.packages(setdiff(pacotes_padrao[length(pacotes_padrao)], rownames(installed.packages()))))
    require(installr)  
    install.pandoc()
  }
}
pacotes <- c(pacotes_padrao, pacotes_analise)
print("Carregando bibliotecas:")
suppressPackageStartupMessages(base::sapply(pacotes, require, character.only=TRUE))
# # R Markdown
# print(suppressMessages(library(rmarkdown)))
# print(suppressMessages(library(knitr)))
# print(suppressMessages(library(prettydoc)))
# print(suppressMessages(library(tinytex)))
# # Padroes Data Scraping
# print(suppressMessages(library(tidyverse)))
# print(suppressMessages(library(tabulizer)))
# print(suppressMessages(library(rvest)))
# print(suppressMessages(library(readr)))
# print(suppressMessages(library(stringi)))
# print(suppressMessages(library(readxl)))
# print(suppressMessages(library(lubridate)))
# print(suppressMessages(library(rgdal)))
# # Graficos
# print(suppressMessages(library(leaflet)))
# print(suppressMessages(library(htmlwidgets)))
# print(suppressMessages(library(scales)))
# print(suppressMessages(library(ggplot2)))
# print(suppressMessages(library(ggthemes)))
# print(suppressMessages(library(forecast)))
# print(suppressMessages(library(prophet)))
