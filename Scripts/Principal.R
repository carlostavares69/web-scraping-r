# -------------------------------------------------------------------------------------------#
# RASPAGEM DE DADOS (Data Scraping)
# CRIMES VIOLENTOS LETAIS E INTENCIONAIS – CVLI (SSPDS/CE) (2014 a 2019)
#
# Script:      Principal.R - Inicia scripts dependentes, configuracoes, funcoes e analises.
# Autor:       Erivando Sena
# E-mail:      erivandosena@gmail.com 
# Data:        03/08/2019
# Atualizado: 16/01/2020
##-------------------------------------------------------------------------------------------#

##############################################################################################
## MEMORIA
##############################################################################################
# Limpa objetos globais
rm(list = ls(all.names = TRUE))

# Limpa historico
loadhistory(".blank")

# Limpa console
shell("cls") 

##############################################################################################
## INCLUDES
##############################################################################################

# Includes das configuracoes da analise
source(file = file.path(".", "Scripts/Configuracoes.R"), encoding = "UTF-8") 

# Includes das funcoes utilizadas na analise
source(file = file.path(".", "Scripts/Funcoes.R"), encoding = "UTF-8") 

# Includes dos parametros da imputacao de dados
source(file = file.path(".", "Scripts/Imputacao.R"), encoding = "UTF-8") 

# Includes das funcoes utilizadas nas limpezas dos dados
source(file = file.path(".", "Scripts/Funcao_limpa_dados_2019.R"), encoding = "UTF-8") 
source(file = file.path(".", "Scripts/Funcao_limpa_dados_2018.R"), encoding = "UTF-8") 
source(file = file.path(".", "Scripts/Funcao_limpa_dados_2017.R"), encoding = "UTF-8") 
source(file = file.path(".", "Scripts/Funcao_limpa_dados_2016.R"), encoding = "UTF-8") 
source(file = file.path(".", "Scripts/Funcao_limpa_dados_2015.R"), encoding = "UTF-8") 
source(file = file.path(".", "Scripts/Funcao_limpa_dados_2014.R"), encoding = "UTF-8") 

##############################################################################################
## COLETA
##############################################################################################
# Executa a funcao que realiza o download do(s) documento(s) dos anos de 2014 a 2018
obtem_arquivos()

##############################################################################################
## LIMPEZA
##############################################################################################
# Executa a funcao de limpeza dos dados e obtem um data frame
df_dados_limpos_2014_2019 <- realiza_limpeza_dados()
exporta_csv(df_dados_limpos_2014_2019, paste0(nome_arquivo,"_Original",".csv")) 

##############################################################################################
## TRATAMENTO
##############################################################################################
# Padroniza dados
# Exporta data frame sem merge para CSV
df_crime_ce_2014_2019 <- exporta_csv(padroniza_dados(df_dados_limpos_2014_2019), paste0(nome_arquivo,"_Original_Limpo",".csv"))

##############################################################################################
## IMPUTACAO
##############################################################################################
# Da RASPAGEM
# Prepara o conjunto de dados para imputacao
df_crime_ce_2014_2019_missing <- prepara_dados_imputacao(df_crime_ce_2014_2019)

# OU

# Do Arquivo CSV
# Importa data frame sem merge do CSV
df_crime_ce_2014_2019_missing <- prepara_dados_imputacao(importa_csv("original",paste0(nome_arquivo,"_Original_Limpo",".csv")))

# Faz IMPUTACAO do conjunto de dados
df_crime_ce_2014_2019_imputado <- realiza_imputacao_dados(df_crime_ce_2014_2019_missing)

# Resultados da imputacao
df_tab_orig1
df_tab_pred1

df_tab_orig2
df_tab_pred2

df_tab_orig3
df_tab_pred3

df_tab_orig4
df_tab_pred4

# Graficos 
gera_grafico_comparacao_imputacao(df_crime_ce_2014_2019_missing, df_crime_ce_2014_2019_imputado)

##############################################################################################
## MERGES
##############################################################################################
# Adiciona novas variaveis ao conjunto de dados de MUNICIPIOS do Ceara
df_crime_ce_2014_2019_merges <- df_municipios_executa_merges(df_crime_ce_2014_2019_imputado)

# Adiciona novas variaveis ao conjunto de dados de BAIRROS de Fortaleza
df_crime_ce_2014_2019_merges_bairros <- df_bairros_executa_merges(df_crime_ce_2014_2019_imputado)

##############################################################################################
## EXPORTACAO
##############################################################################################

# Exporta data frames para CSV
exporta_csv(obtem_dados_por_ano(df_crime_ce_2014_2019_merges, 2019, 2019), "Homicidios_Ceara_2019_Original_Limpo_Imputado_Merges.csv")
exporta_csv(obtem_dados_por_ano(df_crime_ce_2014_2019_merges, 2018, 2018), "Homicidios_Ceara_2018_Original_Limpo_Imputado_Merges.csv")
exporta_csv(obtem_dados_por_ano(df_crime_ce_2014_2019_merges, 2017, 2017), "Homicidios_Ceara_2017_Original_Limpo_Imputado_Merges.csv")
exporta_csv(obtem_dados_por_ano(df_crime_ce_2014_2019_merges, 2016, 2016), "Homicidios_Ceara_2016_Original_Limpo_Imputado_Merges.csv")
exporta_csv(obtem_dados_por_ano(df_crime_ce_2014_2019_merges, 2015, 2015), "Homicidios_Ceara_2015_Original_Limpo_Imputado_Merges.csv")
exporta_csv(obtem_dados_por_ano(df_crime_ce_2014_2019_merges, 2014, 2014), "Homicidios_Ceara_2014_Original_Limpo_Imputado_Merges.csv")

##############################################################################################
## IMPORTACAO
##############################################################################################

# Importa dados do arquivo CSV dos anos 2014-2019
crime_ceara <- importa_csv("personalizado", paste0(nome_arquivo,"_Original_Limpo_Imputado_Merges",".csv"))

# Seleciona apenas colunas relevantes para analise
cols_relevantes <- c("ID","MUNICIPIO_HOMICIDIO","NATUREZA_HOMICIDIO","ARMA_UTILIZADA","DATA_HOMICIDIO","SEXO","IDADE","INCIDENCIA_HOMICIDIO","MES_ANO","FAIXA_ETARIA","GRUPO_MUNICIPIO","GRUPO_AIS","ARMA_DE_FOGO","POPULACAO","IDHM","PIB_PERCAPITA","LATITUDE","LONGITUDE")
crime_ceara <- dplyr::select(crime_ceara, cols_relevantes)

##############################################################################################
## ANALISE
##############################################################################################

lapply(crime_ceara, function(x) sum(is.na(x))) # Verifando os valores ausentes
head(crime_ceara)
str(crime_ceara)
summary(crime_ceara)

##############################################################################################
## VISUALIZAÇÃO
##############################################################################################

#install.packages("plotly")
library(plotly)
library(sf)

mapboxToken <- paste(readLines("../.mapbox_token"), collapse="")    # You need your own token

mapboxToken <- c("pk.eyJ1IjoiZXJpdmFuZG9yYW1vcyIsImEiOiJjazVobXkzNXIwNWhmM3JudjJqMHR1MDdlIn0.LFq36FW2CPZ8n5Yw16fQ9w")
Sys.setenv("MAPBOX_TOKEN" = mapboxToken) # for Orca

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

# Importando o arquivo Shapefile (Fonte IBGE) com informacoes geograficas e coordenadas GPS
if(!exists("dados_shp")) {
  dados_shp <- rgdal::readOGR(dsn = file.path(".", dir_auxiliares, "Shapefile_SHP/BR_Localidades_2010_v1.shp"), layer = "BR_Localidades_2010_v1", verbose = FALSE)
  # Cria variavel global
  assign('dados_shp', dados_shp, envir=.GlobalEnv)
}
# Filtro por municipios do Ceara
df_geo <- dados_shp[which(as.numeric(dados_shp$CD_NIVEL) == 1 & as.character(dados_shp$NM_UF) == "CEARÁ"),] %>% 
  as.data.frame(.) %>% 
  select(., LAT, LONG, NM_MUNICIP) %>%
  rename(LATITUDE = LAT) %>%
  rename(LONGITUDE = LONG) %>% 
  mutate(LATITUDE = format(LATITUDE, trim = TRUE, digits = 7), LONGITUDE = format(LONGITUDE, trim = TRUE, digits = 7), NM_MUNICIP = as.character(NM_MUNICIP)) %>% 
  mutate(NM_MUNICIP = remove_acentos(toupper(NM_MUNICIP)))

view(df_geo@data)


library(plotly)

mapboxToken <- paste(readLines("../.mapbox_token"), collapse=mapboxToken)    # You need your own token
Sys.setenv("MAPBOX_TOKEN" = mapboxToken) # for Orca

crime_ceara %>%
 # group_by(MUNICIPIO_HOMICIDIO) %>%
  plot_mapbox(x = ~LONGITUDE, y = ~LATITUDE, color = ~INCIDENCIA_HOMICIDIO, colors = c('#ffeda0','#f03b20'),
              text = ~NATUREZA_HOMICIDIO, hoverinfo = 'text', showlegend = FALSE) %>%
  add_polygons(
    line = list(width = 0.4)
  ) %>%
  add_polygons(fillcolor = 'transparent',
               line = list(color = 'black', width = 0.5),
               showlegend = FALSE, hoverinfo = 'none'
  ) %>%
  layout(
    xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
    yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
    mapbox = list(
      style = 'light',
      zoom = 4,
      center = list(lat = ~median(LATITUDE), lon = ~median(LONGITUDE))),
    margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0)
  ) %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))
