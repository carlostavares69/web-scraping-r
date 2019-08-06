##############################################################################################
## RASPAGEM DE DADOS (Data Scraping)
##
## Autor:   Erivando Sena
## E-mail:  erivandosena@gmail.com 
## Data:    03/08/2019
##
##############################################################################################
## LIMPEZA DA MEMORIA
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

# Includes das funcoes utilizadas nas limpezas dos dados
source(file = file.path(".", "Scripts/Funcao_limpa_dados_2018.R"), encoding = "UTF-8") 
source(file = file.path(".", "Scripts/Funcao_limpa_dados_2017.R"), encoding = "UTF-8") 
source(file = file.path(".", "Scripts/Funcao_limpa_dados_2016.R"), encoding = "UTF-8") 
source(file = file.path(".", "Scripts/Funcao_limpa_dados_2015.R"), encoding = "UTF-8") 
source(file = file.path(".", "Scripts/Funcao_limpa_dados_2014.R"), encoding = "UTF-8") 

##############################################################################################
## COLETA DOS DADOS
##############################################################################################

# Executa a funcao que realiza o download do(s) documento(s) dos anos de 2014 a 2018
obtem_arquivos()

##############################################################################################
## LIMPEZA DOS DADOS
##############################################################################################

# Executa a funcao de limpeza dos dados e obtem um data frame
df_dados_limpos_2014_2018 <- realiza_limpeza_dados()

##############################################################################################
## TRATAMENTO DOS DADOS
##############################################################################################

# Padroniza dados
df_crime_ce_2014_2018 <- padroniza_dados(df_dados_limpos_2014_2018, NULL)
df_crime_ce_2018 <- padroniza_dados(dados_crime_ce_2018, NULL)
df_crime_ce_2017 <- padroniza_dados(dados_crime_ce_2017, NULL)
df_crime_ce_2016 <- padroniza_dados(dados_crime_ce_2016, NULL)
df_crime_ce_2015 <- padroniza_dados(dados_crime_ce_2015, NULL)
df_crime_ce_2014 <- padroniza_dados(dados_crime_ce_2014, NULL)

# Merge com dados geoespaciais
df_crime_ce_2014_2018 <- merge_dados_geo(df_crime_ce_2014_2018)
df_crime_ce_2018 <- merge_dados_geo(df_crime_ce_2018)
df_crime_ce_2017 <- merge_dados_geo(df_crime_ce_2017)
df_crime_ce_2016 <- merge_dados_geo(df_crime_ce_2016)
df_crime_ce_2015 <- merge_dados_geo(df_crime_ce_2015)
df_crime_ce_2014 <- merge_dados_geo(df_crime_ce_2014)

# Merge com incidencia de crime
df_crime_ce_2014_2018 <- merge_dados_incidencia_crime(df_crime_ce_2014_2018)
df_crime_ce_2018 <- merge_dados_incidencia_crime(df_crime_ce_2018)
df_crime_ce_2017 <- merge_dados_incidencia_crime(df_crime_ce_2017)
df_crime_ce_2016 <- merge_dados_incidencia_crime(df_crime_ce_2016)
df_crime_ce_2015 <- merge_dados_incidencia_crime(df_crime_ce_2015)
df_crime_ce_2014 <- merge_dados_incidencia_crime(df_crime_ce_2014)

# Merge com censo populacional
df_crime_ce_2014_2018 <- merge_dados_populacao(df_crime_ce_2014_2018)
df_crime_ce_2018 <- merge_dados_populacao(df_crime_ce_2018)
df_crime_ce_2017 <- merge_dados_populacao(df_crime_ce_2017)
df_crime_ce_2016 <- merge_dados_populacao(df_crime_ce_2016)
df_crime_ce_2015 <- merge_dados_populacao(df_crime_ce_2015)
df_crime_ce_2014 <- merge_dados_populacao(df_crime_ce_2014)

# Merge com dados IDH-M
df_crime_ce_2014_2018 <- merge_dados_idhm(df_crime_ce_2014_2018)
df_crime_ce_2018 <- merge_dados_idhm(df_crime_ce_2018)
df_crime_ce_2017 <- merge_dados_idhm(df_crime_ce_2017)
df_crime_ce_2016 <- merge_dados_idhm(df_crime_ce_2016)
df_crime_ce_2015 <- merge_dados_idhm(df_crime_ce_2015)
df_crime_ce_2014 <- merge_dados_idhm(df_crime_ce_2014)

# Merge com dados PIB percapita
df_crime_ce_2014_2018 <- merge_dados_pib(df_crime_ce_2014_2018)
df_crime_ce_2018 <- merge_dados_pib(df_crime_ce_2018)
df_crime_ce_2017 <- merge_dados_pib(df_crime_ce_2017)
df_crime_ce_2016 <- merge_dados_pib(df_crime_ce_2016)
df_crime_ce_2015 <- merge_dados_pib(df_crime_ce_2015)
df_crime_ce_2014 <- merge_dados_pib(df_crime_ce_2014)

# Exporta data frames para CSV
exporta_csv(df_crime_ce_2014_2018)
exporta_csv(df_crime_ce_2018, "Indicadores_Crimes_CE_2018.csv")
exporta_csv(df_crime_ce_2017, "Indicadores_Crimes_CE_2017.csv")
exporta_csv(df_crime_ce_2016, "Indicadores_Crimes_CE_2016.csv")
exporta_csv(df_crime_ce_2015, "Indicadores_Crimes_CE_2015.csv")
exporta_csv(df_crime_ce_2014, "Indicadores_Crimes_CE_2014.csv")

##############################################################################################
## IMPORTACAO DOS DADOS
##############################################################################################

# Importa dados do arquivo CSV anos 2014-2018
df_crime_ce <- importa_csv(file.path(".", dir_dados, paste0(nome_arquivo,".csv")))
cols_relevantes_analise <- c("MUNICIPIO_CRIME","NATUREZA_CRIME","ARMA_UTILIZADA","DATA_MORTE","SEXO","IDADE",
                             "MES_ANO","LATITUDE","LONGITUDE","INCIDENCIA_CRIME","POPULACAO","IDHM")
crime_ceara <- padroniza_dados(df_crime_ce, cols_relevantes_analise)

##############################################################################################
## ANALISE DOS DADOS
##############################################################################################
names(df_crime_ce_2014_2018)
# Data frame geral
glimpse(crime_ceara)
str(df_crime_ce)
View(crime_ceara)

# Codigo TEMP - TESTES EM BUSCA DE Inconformidades -----------------------------------------
# names(crime_ceara)
# for (linha in 1:nrow(crime_ceara)) {
#   if (!is.na(crime_ceara[linha,5]) & crime_ceara[linha,5] == "I") {
#     print(paste(crime_ceara[linha,5],crime_ceara[linha,4],sep = " "))
#   }
# }
# unique(df_crime_ce$NATUREZA_CRIME)
# Codigo TEMP - TESTES EM BUSCA DE Inconformidades -----------------------------------------

##############################################################################################
## VISUALIZAÇÃO DOS DADOS
##############################################################################################

# Mapas de clusters do crime no Ceara 2014/2018
agrupamento_crime_ce <- crime_ceara %>% dplyr::select(MUNICIPIO_CRIME,NATUREZA_CRIME,ARMA_UTILIZADA,LONGITUDE,LATITUDE,INCIDENCIA_CRIME) %>%
  mutate(LONGITUDE = as.numeric(LONGITUDE), LATITUDE = as.numeric(LATITUDE))

labels <- paste0("<strong>Cidade: </strong>", stringi::stri_trans_general(agrupamento_crime_ce$MUNICIPIO_CRIME, "latin-ascii"),
                 "<br><strong>Arma: </strong>", stringi::stri_trans_general(agrupamento_crime_ce$ARMA_UTILIZADA, "latin-ascii"),
                 "<br><strong>Crime: </strong>", stringi::stri_trans_general(agrupamento_crime_ce$NATUREZA_CRIME, "latin-ascii"),
                 "<br><strong>Incidencia: </strong>", stringi::stri_trans_general(agrupamento_crime_ce$INCIDENCIA_CRIME, "latin-ascii")) %>% lapply(htmltools::HTML)

leaflet::leaflet(agrupamento_crime_ce) %>%
  setView(lng=-38.5, lat=-3.7, zoom=7) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron", group="Mapa claro") %>%
  addProviderTiles("Esri.NatGeoWorldMap", group= "Mapa verde") %>%
  addScaleBar %>%
  addMarkers(~LONGITUDE, ~LATITUDE,label = labels,  clusterOptions = markerClusterOptions()) %>%
  addLayersControl(baseGroups = c("Mapa verde", "Mapa claro"), options = layersControlOptions(collapsed = FALSE)) %>% 
  clearBounds() %>%
  
  # Salva html do grafico dinamico (Requer caminho absoluto do diretorio destino.)
  htmlwidgets::saveWidget(., file = file.path(getwd(), dir_graficos, "clusters_crime_ce_2014_2018.html"))
