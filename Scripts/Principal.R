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
## EXPORTACAO DOS DADOS PRIMARIOS
##############################################################################################

# Executa funcao de exportacao para o formato CSV
exporta_csv(dados_crime_ce_2018, "Indicadores_Crimes_CE_2018.csv")
exporta_csv(dados_crime_ce_2017, "Indicadores_Crimes_CE_2017.csv")
exporta_csv(dados_crime_ce_2016, "Indicadores_Crimes_CE_2016.csv")
exporta_csv(dados_crime_ce_2015, "Indicadores_Crimes_CE_2015.csv")
exporta_csv(dados_crime_ce_2014, "Indicadores_Crimes_CE_2014.csv")
exporta_csv(df_dados_limpos_2014_2018)

##############################################################################################
## FORMATACAO DOS DADOS
##############################################################################################

# Padroniza dados
df_crime_ce <- padroniza_dados(df_dados_limpos_2014_2018)

##############################################################################################
## MERGE DE DADOS
##############################################################################################

# Merge com dados geoespaciais
df_crime_ce <- merge_dados_geo(df_crime_ce)

# Merge com incidencia de crime
df_crime_ce <- merge_dados_incidencia_crime(df_crime_ce)

# Merge com censo populacional
df_crime_ce <- merge_dados_populacao(df_crime_ce)

# Merge com dados IDH-M
df_crime_ce <- merge_dados_idhm(df_crime_ce)

# Merge com dados PIB percapita
df_crime_ce <- merge_dados_pib(df_crime_ce)

# Exporta para CSV
exporta_csv(df_crime_ce, "Indicadores_Crimes_CE_2014-2018_Merges.csv")

##############################################################################################
## IMPORTACAO DOS DADOS
##############################################################################################

# Importa dados do arquivo CSV anos 2014-2018
df_crime_ce <- importa_csv( file.path(".", dir_dados, c("Indicadores_Crimes_CE_2014-2018_Merges.csv")) )

##############################################################################################
## CONJUNTO DE DADOS PARA ANALISES
##############################################################################################

# Data frame geral
glimpse(df_crime_ce)

str(df_crime_ce)

View(df_crime_ce)

# TESTES EM BUSCA DE ERROS
for (linha in 1:nrow(df_crime_ce)) {
  if (!is.na(df_crime_ce[linha,5]) & df_crime_ce[linha,5] == "I") {
    print(paste(df_crime_ce[linha,5],df_crime_ce[linha,4],sep = " "))
  }
}
names(df_crime_ce)
unique(df_crime_ce$IDADE)

##############################################################################################
## GRAFICOS
##############################################################################################

# Mapas de clusters do crime no Ceara 2014/2018
agrupamento_crime_ce <- df_crime_ce %>% dplyr::select(MUNICIPIO_CRIME,NATUREZA_CRIME,ARMA_UTILIZADA,LONGITUDE,LATITUDE,INCIDENCIA_CRIME)

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
