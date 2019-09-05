
library(readr) 
library(tidyverse)

# Local
setwd("E:/Prof Carlos e Sena/Novas Variaveis BD Sena")

##############################################################################################
## FUNCOES
##############################################################################################

dir_auxiliares <- c("Auxiliares")

# Funcao para remover acentos
remove_acentos <- function(obj_str) {
  if(!is.character(obj_str)) {
    obj_str <- as.character(obj_str)
  }
  obj_str <- stringi::stri_trans_general(str = obj_str, "latin-ascii")
  
  return(obj_str)
}

# Funcao para criar variavel mes/ano
merge_variavel_mesano <- function(df_dados) {
  
  # Cria variavel mes/ano
  df_var_mesano <- df_dados %>% 
    mutate(MES_ANO = format.Date(as.Date(DATA_HOMICIDIO, format = "%d/%m/%Y"), "%m/%Y")) 
  
  return(df_var_mesano)
}

# Funcao para merge de coluna de indice populacional 
merge_dados_populacao <- function(df_dados) {
  censo_pop <- suppressMessages(readxl::read_xls(
    file.path(".", dir_auxiliares, "Censo_Demografico_2010/total_populacao_ceara.xls"), col_names = FALSE))[2:185,][,-c(1,3,4,5,6,7)] %>% 
    rename(MUNICIPIO = "...2", POPULACAO = "...8") %>% 
    mutate(POPULACAO = as.numeric(POPULACAO)) %>% 
    mutate(MUNICIPIO = remove_acentos(toupper(MUNICIPIO)))
  
  # Converte variavel para maiusculo
  df_dados <- df_dados %>% mutate(MUNICIPIO_HOMICIDIO = toupper(MUNICIPIO_HOMICIDIO))
  
  # Verifica inconsistencia entre as colunas de merge
  #inconsistencias <- verifica_inconsistencias(df_dados$MUNICIPIO_HOMICIDIO, censo_pop$MUNICIPIO)
  
  # Merge dos data frames com populacao por municipio
  df_merge <- merge(df_dados, censo_pop, by.x=c('MUNICIPIO_HOMICIDIO'), by.y=c('MUNICIPIO'))
  
  # Restaura formato da coluna MUNICIPIO_HOMICIDIO
  df_merge <- df_merge %>% mutate(MUNICIPIO_HOMICIDIO = str_to_title(MUNICIPIO_HOMICIDIO)) %>% 
    # Reordena coluna MUNICIPIO_HOMICIDIO 
    .[,c(2,3,1,4:ncol(.))]
  
  return(df_merge)
}

# Funcao para merge de coluna do IDH
merge_dados_idhm <- function(df_dados) {
  idhm <- suppressMessages(readxl::read_xlsx(
    file.path(".", dir_auxiliares, "Atlas_Desenvolvimento_Humano_Brasil_2010/AtlasBrasil_Consulta.xlsx"), col_names = FALSE))[3:186,][,c(2:3)] %>% 
    rename(MUNICIPIO = "...2", IDHM = "...3") %>% 
    mutate(IDHM = as.numeric(IDHM)) %>% 
    mutate(MUNICIPIO = remove_acentos(toupper(MUNICIPIO)))
  
  # Converte variavel para maiusculo
  df_dados <- df_dados %>% mutate(MUNICIPIO_HOMICIDIO = toupper(MUNICIPIO_HOMICIDIO))
  
  # Verifica inconsistencia entre as colunas de merge
  #inconsistencias <- verifica_inconsistencias(df_dados$MUNICIPIO_HOMICIDIO, idhm$MUNICIPIO)
  
  # Merge dos data frames com IDH por municipio
  df_merge <- merge(df_dados, idhm, by.x=c('MUNICIPIO_HOMICIDIO'), by.y=c('MUNICIPIO'))
  
  # Restaura formato da coluna MUNICIPIO_HOMICIDIO
  df_merge <- df_merge %>% mutate(MUNICIPIO_HOMICIDIO = str_to_title(MUNICIPIO_HOMICIDIO)) %>% 
    # Reordena coluna MUNICIPIO_HOMICIDIO 
    .[,c(2,3,1,4:ncol(.))]
  
  return(df_merge)
}

# Funcao para merge de coluna do PIB
merge_dados_pib <- function(df_dados) {
  pib_percapita <- suppressMessages(readxl::read_xls(
    file.path(".", dir_auxiliares, "Pib_Municipios_2010/PibMunicipal2006_2010.xls"), col_names = FALSE))[913:1096,][,-c(2:6)] %>% 
    rename(MUNICIPIO = "...1", PIB_PERCAPITA = "...7") %>% 
    mutate(PIB_PERCAPITA = as.numeric(PIB_PERCAPITA)) %>% 
    mutate(MUNICIPIO = remove_acentos(toupper(MUNICIPIO)))
  
  # Converte variavel para maiusculo
  df_dados <- df_dados %>% mutate(MUNICIPIO_HOMICIDIO = toupper(MUNICIPIO_HOMICIDIO))
  
  # Verifica inconsistencia entre as colunas de merge
  #inconsistencias <- verifica_inconsistencias(df_dados$MUNICIPIO_HOMICIDIO, pib_percapita$MUNICIPIO)
  
  # Merge dos data frames com PIB por municipio
  df_merge <- merge(df_dados, pib_percapita, by.x=c('MUNICIPIO_HOMICIDIO'), by.y=c('MUNICIPIO'))
  
  # Restaura formato da coluna MUNICIPIO_HOMICIDIO
  df_merge <- df_merge %>% mutate(MUNICIPIO_HOMICIDIO = str_to_title(MUNICIPIO_HOMICIDIO)) %>% 
    # Reordena coluna MUNICIPIO_HOMICIDIO 
    .[,c(2,3,1,4:ncol(.))]
  
  return(df_merge)
}

# Funcao para criar variavel incidencia de homicidio
merge_variavel_incidencia_homicidio <- function(df_dados) {
  
  # Converte variavel para maiusculo
  df_dados <- df_dados %>% mutate(MUNICIPIO_HOMICIDIO = toupper(MUNICIPIO_HOMICIDIO))
  
  # obtem o numero de incidencia
  df_incidencia <- df_dados %>%
    group_by(MUNICIPIO_HOMICIDIO) %>%
    summarise(INCIDENCIA_HOMICIDIO = n()) %>%
    distinct()
  
  df_merge <- merge(df_dados, df_incidencia, by.x=c('MUNICIPIO_HOMICIDIO'), by.y=c('MUNICIPIO_HOMICIDIO'))
  
  # Restaura formato da coluna MUNICIPIO_HOMICIDIO e restaura formato da variavel
  df_merge <- df_merge %>% mutate(MUNICIPIO_HOMICIDIO = str_to_title(MUNICIPIO_HOMICIDIO)) %>% 
    # Reordena coluna MUNICIPIO_HOMICIDIO 
    .[,c(2,3,1,4:ncol(.))]
  
  return(df_merge)
}

# Funcao para criar variavel incidencia de homicidio por 100 mil
merge_var_incidencia_homicidio_porcemmil <- function(df_dados) {
  # obtem o calculo do numero de incidencia por cem mil
  df_var_incidencia_cemmil <- df_dados %>%
    group_by(MUNICIPIO_HOMICIDIO) %>%
    mutate(INCIDENCIA_HOMICIDIO_PORCEMMIL = (INCIDENCIA_HOMICIDIO / POPULACAO) * 100000 ) 
  
  return(df_var_incidencia_cemmil)
}

# Funcao para criar variavel incidencia de homicidio por ano
merge_dados_incidencia_homicidio_porano <- function(df_dados) {
  
  # obtem total incidencia por ano
  df_incidencia_homicidio_ano <- df_dados %>% mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format = "%d/%m/%Y")) %>% 
    mutate(ano = lubridate::year(DATA_HOMICIDIO)) %>%
    group_by(ano) %>%
    #arrange(ano) %>%
    mutate(INCIDENCIA_HOMICIDIO_POR_ANO = n()) %>% 
    .[,-c(ncol(.)-1)]
  
  return(df_incidencia_homicidio_ano)
}

# Funcao para criar variavel incidencia de homicidio por ano/sexo
merge_dados_incidencia_homicidio_poranosexo <- function(df_dados) {
  
  # obtem total incidencia por ano/sexo
  df_incidencia_homicidio_anosexo <- df_dados %>% mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format = "%d/%m/%Y")) %>% 
    mutate(ano = lubridate::year(DATA_HOMICIDIO)) %>%
    group_by(ano, SEXO) %>%
    arrange(SEXO) %>%
    mutate(INCID_HOMI_POR_ANOSEXO = n()) %>% 
    .[,-c(ncol(.)-1)]
  
  return(df_incidencia_homicidio_anosexo)
}

# Funcao para criar variavel incidencia de homicidio por ano/sexo/municipio
merge_dados_incidencia_homicidio_poranosexomunicipio <- function(df_dados) {
  
  # obtem total incidencia por ano/sexo/municipio
  df_incidencia_homicidio_anosexomunicipio <- df_dados %>% mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format = "%d/%m/%Y")) %>% 
    mutate(ano = lubridate::year(DATA_HOMICIDIO)) %>%
    group_by(ano, SEXO, MUNICIPIO_HOMICIDIO) %>%
    arrange(ano, SEXO, MUNICIPIO_HOMICIDIO) %>%
    mutate(INCID_HOMI_POR_ANOSEXOMUNICIPIO = n()) %>% 
    .[,-c(ncol(.)-1)]
  
  return(df_incidencia_homicidio_anosexomunicipio)
}

# Funcao para criar variavel faixa etaria
merge_var_faixaetaria <- function(df_dados) {
  
  # Obtem as faixa etarias
  idadebreaks <- c(0,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,150)
  idadelabels <- c("0-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99","+ de 100")
  data.table::setDT(df_dados)[,FAIXAETARIA:= cut(IDADE, breaks = idadebreaks, labels = idadelabels, right = FALSE)]
  
  df_faixa_etarias <- df_dados %>% 
    mutate(FAIXAETARIA = forcats::fct_explicit_na(FAIXAETARIA)) %>% 
    group_by(IDADE, FAIXAETARIA) %>%
    arrange(IDADE, FAIXAETARIA) %>%
    mutate(INCID_FAIXAETARIA = n())
  
  return(df_faixa_etarias)
}

# Funcao que obtem dados do arquivo CSV
importa_csv <- function(arquivo_csv) {
  # classes dos tipos de colunas
  classes_colunas <- cols(
    ID = col_number(), 
    AIS = col_character(), 
    MUNICIPIO_HOMICIDIO = col_character(), 
    NATUREZA_HOMICIDIO = col_character(), 
    ARMA_UTILIZADA = col_character(), 
    DATA_HOMICIDIO = col_character(), 
    NOME_VITIMA = col_character(), 
    GUIA_CADAVERICA = col_character(), 
    SEXO = col_character(), 
    IDADE = col_number()
  )
  # Importanto CSV
  dados_csv <- readr::read_delim(file = arquivo_csv, 
                                 delim = ";", 
                                 na = "NA",
                                 col_names = TRUE,
                                 col_types = classes_colunas, 
                                 locale = locale(date_names = "pt", encoding = "UTF-8", decimal_mark = ".", date_format = "%d/%m/%Y"),
                                 progress = show_progress())
  return(dados_csv)
}

# Funcao GENERICA que convert data frame em arquivo CSV tabulacao Excel
exporta_csv <- function(df_limpo, nome_csv=NA) {
  
  # classes dos tipos de colunas
  classes_colunas <- base::sapply(df_limpo, class)
  
  # salvar dados em arquivo CSV
  if(is.na(nome_csv)) {
    nome_arquivo_csv <- base::file.path(".", dir_dados, paste0(nome_arquivo, ".csv"))
  } else {
    nome_arquivo_csv <- base::file.path(getwd(), paste0(nome_csv))
  }
  
  # Escrever arquivo CSV
  readr::write_excel_csv2(x = df_limpo, path = nome_arquivo_csv, na = "NA", col_names = TRUE, delim = ";")
}

##############################################################################################
## IMPORTACAO DOS DADOS
##############################################################################################

# Importa dados do arquivo CSV dos anos 2014-2018
# Importacao banco de dados pos raspagem sem merges
homicidio_ceara <- readr::read_delim(file = file.path(".", dir_dados, "Homicidios_CEARA_2014-2018_Original-Sem-Merges.csv"), 
                                      delim = ";", 
                                      na = "NA",
                                      col_names = TRUE,
                                      #col_types = classes_colunas, 
                                      locale = locale(date_names = "pt", encoding = "UTF-8", decimal_mark = ",", grouping_mark = ".", date_format = "%d/%m/%Y")
)

# Merges de novas variaveis
homicidio_ceara <- homicidio_ceara %>% # banco limpo
  merge_variavel_mesano(.) %>%  
  merge_dados_idhm(.) %>%  
  merge_dados_pib(.) %>%  
  merge_dados_populacao(.) %>%  
  merge_variavel_incidencia_homicidio(.) %>%  
  merge_var_incidencia_homicidio_porcemmil(.) %>% 
  merge_dados_incidencia_homicidio_porano(.) %>% 
  merge_dados_incidencia_homicidio_poranosexo(.) %>% 
  merge_dados_incidencia_homicidio_poranosexomunicipio(.) %>% 
  merge_var_faixaetaria(.)

##############################################################################################
## EXPOTACAO DOS DADOS
##############################################################################################

exporta_csv(homicidio_ceara, "Homicidios_CEARA_2014-2018_artigo-sena.csv")

# Classificacao dos tipos de classe das colunas
classes_colunas <- cols(
  ID = col_number(), 
  AIS = col_character(), 
  MUNICIPIO_HOMICIDIO = col_character(), 
  NATUREZA_HOMICIDIO = col_character(), 
  ARMA_UTILIZADA = col_character(), 
  DATA_HOMICIDIO = col_character(), 
  NOME_VITIMA = col_character(), 
  GUIA_CADAVERICA = col_character(), 
  SEXO = col_character(), 
  IDADE = col_number(), 
  LONGITUDE = col_number(), 
  LATITUDE = col_number(),
  MES_ANO = col_character(),
  IDHM = col_number(),
  PIB_PERCAPITA = col_number(),
  POPULACAO = col_number(),
  INCIDENCIA_HOMICIDIO = col_number(),
  INCIDENCIA_HOMICIDIO_PORCEMMIL = col_number(),
  INCIDENCIA_HOMICIDIO_POR_ANO = col_number(),
  INCID_HOMI_POR_ANOSEXO = col_number(),
  INCID_HOMI_POR_ANOSEXOMUNICIPIO = col_number(),
  FAIXAETARIA = col_factor(ordered=TRUE),
  INCID_FAIXAETARIA = col_number()
)

# Importacao Homicidios_CEARA_2014-2018_artigo-sena.csv
Homicidios_CEARA <- readr::read_delim(file = "Homicidios_CEARA_2014-2018_artigo-sena.csv", 
                                      delim = ";", 
                                      na = "NA",
                                      col_names = TRUE,
                                      col_types = classes_colunas, 
                                      locale = locale(date_names = "pt", encoding = "UTF-8", decimal_mark = ",", grouping_mark = ".", date_format = "%d/%m/%Y")
)


view(Homicidios_CEARA)
str(Homicidios_CEARA)

Homicidios_CEARA %>%
  group_by(MUNICIPIO_HOMICIDIO) %>%
  summarize(mean(INCIDENCIA_HOMICIDIO_PORCEMMIL),
            median(INCIDENCIA_HOMICIDIO_PORCEMMIL))



df <- Homicidios_CEARA %>% 
  group_by(
    MUNICIPIO_HOMICIDIO,INCIDENCIA_HOMICIDIO_PORCEMMIL
  ) %>%
  summarise(total = n())
View(df)


Homicidios_CEARA %>%
  group_by(MUNICIPIO_HOMICIDIO,INCIDENCIA_HOMICIDIO) %>%
  summarise(INCIDENCIA_HOMICIDIO * 1 ) 

df_var_incidencia_cemmil <- Homicidios_CEARA %>%
  group_by(MUNICIPIO_HOMICIDIO) %>%
  arrange(MUNICIPIO_HOMICIDIO)%>%
  mutate(INCIDENCIA_HOMICIDIO_PORCEMMIL = (INCIDENCIA_HOMICIDIO / POPULACAO) * 100000, NUM_INCIDENCIA_HOMICIDIO_PORCEMMIL = n() ) 

10489 / 11 
11/ 10489 * 100000
0.001048718 * 100000

# replacing with standard missing value type, NA
df <- df %>%
  mutate(TotalCharges = replace(TotalCharges, TotalCharges == "na", NA)) %>%
  mutate(TotalCharges = replace(TotalCharges, TotalCharges == "N/A", NA))