# -------------------------------------------------------------------------------------------#
# RASPAGEM DE DADOS (Data Scraping)
#
# Script:      Configuracoes.R - Configura o ambinte R para realizacao das analises.
# Autor:       Erivando Sena
# E-mail:      erivandosena@gmail.com 
# Data:        04/09/2019
# Atualizado:  15/12/2019
##-------------------------------------------------------------------------------------------#

##############################################################################################
## CONFIGURACOES DO PROJETO
##############################################################################################

# Endereco do site que contem os dados 
URL_site <- "https://www.sspds.ce.gov.br/estatisticas-2-2-2-2-2-2/"

# Nome padrao para todos os arquivos
nome_arquivo <- c("Crimes_Ceara_2014-2019")
# Declara como variavel global
assign('nome_arquivo', nome_arquivo, envir=.GlobalEnv)

# Nomes da planilha EXCEL (.xls | .xlsx)
arquivo_csv <- nome_arquivo

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

# Cria pasta e subpasta caso nao existam.
for (diretorio in diretorios) {
  if(!dir.exists(diretorio)) {
    dir.create(diretorio, recursive = TRUE)
  } 
}

##############################################################################################
## PACOTES
##############################################################################################
# Pacotes utilizados na analise.
pacotes_analise <- c("tidyverse","tabulizer","rvest","readr","stringi","readxl","lubridate",
                     "rgdal","leaflet","htmlwidgets","ggthemes","forecast","prophet", "mice", 
                     "tabplot", "kableExtra")

# Pacotes necessarios para RMarkdown.
pacotes_padrao <- c("rmarkdown","knitr","prettydoc","tinytex","installr","devtools","rJava")

# # Instalacao de p acotes necessarios para documentos RMarkdown e Analises.
# if (length(setdiff(c(pacotes_padrao, pacotes_analise), rownames(installed.packages()))) > 0) {
#   suppressMessages(install.packages(setdiff(c(pacotes_padrao, pacotes_analise), rownames(installed.packages())), dependencies=TRUE))
#   Sys.setenv(JAVA_HOME='C://Program Files//Java//jre1.8.0_231') # Para 64Bit, substutuir por o local atual do sistema em uso.
#   # RMarkdown precisa de Pandoc e MiKTeX instalados. https://miktex.org/2.9/setup.
#   # Instalacao Pandoc e MiKTeX. Cancele manualmente o processo de instalacao caso ja o possua instalados no sistema atual.
#   if(!require(pacotes_padrao[length(pacotes_padrao)])){
#     suppressMessages(install.packages(dplyr::setdiff(pacotes_padrao[length(pacotes_padrao)], rownames(installed.packages()))))
#     require(installr)  
#     install.pandoc()
#     install.MikTeX() 
#   }
# }

for (pkg in c(pacotes_padrao, pacotes_analise)) {
  if (!(pkg %in% rownames(installed.packages()))){ 
    print("Instalando pacotes...")
    install.packages(pkg, dependencies=TRUE)
    Sys.setenv(JAVA_HOME='C://Program Files//Java//jre1.8.0_231') # Para 64Bit, substutuir por o local atual do sistema em uso.
    # RMarkdown precisa de Pandoc e MiKTeX instalados. https://miktex.org/2.9/setup.
    # Instalacao Pandoc e MiKTeX. Cancele manualmente o processo de instalacao caso ja o possua instalados no sistema atual.
    if(!require(pacotes_padrao[length(pacotes_padrao)])){
      suppressMessages(install.packages(dplyr::setdiff(pacotes_padrao[length(pacotes_padrao)], rownames(installed.packages())), dependencies=TRUE))
      require(installr)  
      install.pandoc()
      install.MikTeX() 
    }
    
  }
  require(pkg, character.only = TRUE)
}

print("Carregando bibliotecas:")
pacotes <- c(pacotes_padrao, pacotes_analise)
print(suppressPackageStartupMessages(base::sapply(pacotes, require, character.only=TRUE)))
rm(pacotes, pkg)

# Para RMarkdown:
# knitr
# prettydoc
# tinytex

# Para Data Scraping:
# tidyverse
# tabulizer
# rvest
# readr
# stringi
# readxl
# lubridate
# rgdal

# Para Imputacao:
# mice

# Para Tabelas:
# tabplot
# kableExtra

# Para Graficos:
# leaflet
# htmlwidgets
# scales
# ggplot2
# ggthemes
# forecast
# prophet