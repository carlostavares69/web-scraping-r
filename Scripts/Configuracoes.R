##############################################################################################
## CONFIGURACOES DO PROJETO
##############################################################################################

# Nome padrao para todos os arquivos
nome_arquivo <- c("Indicadores_crimes_CE_2018")

# Nomes da planilha EXCEL (.xls | .xlsx)
arquivo_csv <- nome_arquivo

# Pacotes utilizados na analise.
pacotes_analise <- c("tidyverse","tabulizer","readr","rgdal","kableExtra")

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
  suppressMessages(install.packages(setdiff(c(pacotes_padrao, pacotes_analise), rownames(installed.packages()))))
  # RMarkdown precisa de Pandoc e MiKTeX instalados. https://miktex.org/2.9/setup.
  if(!require(pacotes_padrao[length(pacotes_padrao)])){
    suppressMessages(install.packages(setdiff(pacotes_padrao[length(pacotes_padrao)], rownames(installed.packages()))))
    require(installr)  
    install.pandoc()
  }
}
# Carregamento de pacotes padroes
print("Carregando bibliotecas:")
print(suppressMessages(library(rmarkdown)))
print(suppressMessages(library(knitr)))
print(suppressMessages(library(prettydoc)))
print(suppressMessages(library(tinytex)))
# Carregamento de pacotes analise
print(suppressMessages(library(tidyverse)))
print(suppressMessages(library(tabulizer)))
print(suppressMessages(library(readr)))
print(suppressMessages(library(rgdal)))
print(suppressMessages(library(kableExtra)))
