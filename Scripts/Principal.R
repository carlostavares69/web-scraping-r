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
# Exporta data frame sem merge para CSV
df_homicidio_ce_2014_2018 <- exporta_csv(padroniza_dados(df_dados_limpos_2014_2018), paste0(nome_arquivo,"_Original-Sem-Merges",".csv"))
df_homicidio_ce_2018 <- padroniza_dados(dados_homicidio_ce_2018)
df_homicidio_ce_2017 <- padroniza_dados(dados_homicidio_ce_2017)
df_homicidio_ce_2016 <- padroniza_dados(dados_homicidio_ce_2016)
df_homicidio_ce_2015 <- padroniza_dados(dados_homicidio_ce_2015)
df_homicidio_ce_2014 <- padroniza_dados(dados_homicidio_ce_2014)

# Merge com dados geoespaciais
df_homicidio_ce_2014_2018 <- merge_dados_geo(df_homicidio_ce_2014_2018)
df_homicidio_ce_2018 <- merge_dados_geo(df_homicidio_ce_2018)
df_homicidio_ce_2017 <- merge_dados_geo(df_homicidio_ce_2017)
df_homicidio_ce_2016 <- merge_dados_geo(df_homicidio_ce_2016)
df_homicidio_ce_2015 <- merge_dados_geo(df_homicidio_ce_2015)
df_homicidio_ce_2014 <- merge_dados_geo(df_homicidio_ce_2014)

# Merge com censo populacional
df_homicidio_ce_2014_2018 <- merge_dados_populacao(df_homicidio_ce_2014_2018)
df_homicidio_ce_2018 <- merge_dados_populacao(df_homicidio_ce_2018)
df_homicidio_ce_2017 <- merge_dados_populacao(df_homicidio_ce_2017)
df_homicidio_ce_2016 <- merge_dados_populacao(df_homicidio_ce_2016)
df_homicidio_ce_2015 <- merge_dados_populacao(df_homicidio_ce_2015)
df_homicidio_ce_2014 <- merge_dados_populacao(df_homicidio_ce_2014)

# Merge com dados IDH-M
df_homicidio_ce_2014_2018 <- merge_dados_idhm(df_homicidio_ce_2014_2018)
df_homicidio_ce_2018 <- merge_dados_idhm(df_homicidio_ce_2018)
df_homicidio_ce_2017 <- merge_dados_idhm(df_homicidio_ce_2017)
df_homicidio_ce_2016 <- merge_dados_idhm(df_homicidio_ce_2016)
df_homicidio_ce_2015 <- merge_dados_idhm(df_homicidio_ce_2015)
df_homicidio_ce_2014 <- merge_dados_idhm(df_homicidio_ce_2014)

# Merge com dados PIB percapita
df_homicidio_ce_2014_2018 <- merge_dados_pib(df_homicidio_ce_2014_2018)
df_homicidio_ce_2018 <- merge_dados_pib(df_homicidio_ce_2018)
df_homicidio_ce_2017 <- merge_dados_pib(df_homicidio_ce_2017)
df_homicidio_ce_2016 <- merge_dados_pib(df_homicidio_ce_2016)
df_homicidio_ce_2015 <- merge_dados_pib(df_homicidio_ce_2015)
df_homicidio_ce_2014 <- merge_dados_pib(df_homicidio_ce_2014)

# Merge com variavel mes/ano
df_homicidio_ce_2014_2018 <- merge_variavel_mesano(df_homicidio_ce_2014_2018)
df_homicidio_ce_2018 <- merge_variavel_mesano(df_homicidio_ce_2018)
df_homicidio_ce_2017 <- merge_variavel_mesano(df_homicidio_ce_2017)
df_homicidio_ce_2016 <- merge_variavel_mesano(df_homicidio_ce_2016)
df_homicidio_ce_2015 <- merge_variavel_mesano(df_homicidio_ce_2015)
df_homicidio_ce_2014 <- merge_variavel_mesano(df_homicidio_ce_2014)

# Merge com variavel incidencia de homicidio
df_homicidio_ce_2014_2018 <- merge_variavel_incidencia_homicidio(df_homicidio_ce_2014_2018)
df_homicidio_ce_2018 <- merge_variavel_incidencia_homicidio(df_homicidio_ce_2018)
df_homicidio_ce_2017 <- merge_variavel_incidencia_homicidio(df_homicidio_ce_2017)
df_homicidio_ce_2016 <- merge_variavel_incidencia_homicidio(df_homicidio_ce_2016)
df_homicidio_ce_2015 <- merge_variavel_incidencia_homicidio(df_homicidio_ce_2015)
df_homicidio_ce_2014 <- merge_variavel_incidencia_homicidio(df_homicidio_ce_2014)

# Exporta data frames para CSV
exporta_csv(df_homicidio_ce_2014_2018)
exporta_csv(df_homicidio_ce_2018, "Indicadores_Crimes_CE_2018.csv")
exporta_csv(df_homicidio_ce_2017, "Indicadores_Crimes_CE_2017.csv")
exporta_csv(df_homicidio_ce_2016, "Indicadores_Crimes_CE_2016.csv")
exporta_csv(df_homicidio_ce_2015, "Indicadores_Crimes_CE_2015.csv")
exporta_csv(df_homicidio_ce_2014, "Indicadores_Crimes_CE_2014.csv")
##############################################################################################
## IMPORTACAO DOS DADOS
##############################################################################################

# Importa dados do arquivo CSV dos anos 2014-2018
homicidio_ceara <- importa_csv(file.path(".", dir_dados, paste0(nome_arquivo,".csv")))

# Importa dados do arquivo CSV do ano 2018
#homicidio_ceara_2018 <- importa_csv(file.path(".", dir_dados, "Indicadores_Crimes_CE_2018.csv"))

# Seleciona apenas colunas relevantes para analise
cols_relevantes<- c("ID","MUNICIPIO_HOMICIDIO","NATUREZA_HOMICIDIO","ARMA_UTILIZADA","DATA_HOMICIDIO","SEXO","IDADE","LATITUDE","LONGITUDE","POPULACAO","IDHM","PIB_PERCAPITA","MES_ANO","INCIDENCIA_HOMICIDIO")
homicidio_ceara <- dplyr::select(homicidio_ceara, cols_relevantes)

##############################################################################################
## ANALISE DOS DADOS
##############################################################################################

head(homicidio_ceara)
str(homicidio_ceara)
summary(homicidio_ceara)
# Verifando os valores ausentes
lapply(homicidio_ceara, function(x) sum(is.na(x)))
View(df_homicidio_ce_2014)

##############################################################################################
## VISUALIZAÇÃO DOS DADOS
##############################################################################################

# EXEMPLO:
# Mapas de clusters do homicidio no Ceara 2014/2018
agrupamento_homicidio_ce <- homicidio_ceara %>% dplyr::select(MUNICIPIO_HOMICIDIO,NATUREZA_HOMICIDIO,ARMA_UTILIZADA,LONGITUDE,LATITUDE,INCIDENCIA_HOMICIDIO)

labels <- paste0("<strong>Cidade: </strong>", stringi::stri_trans_general(agrupamento_homicidio_ce$MUNICIPIO_HOMICIDIO, "latin-ascii"),
                 "<br><strong>Arma: </strong>", stringi::stri_trans_general(agrupamento_homicidio_ce$ARMA_UTILIZADA, "latin-ascii"),
                 "<br><strong>Crime: </strong>", stringi::stri_trans_general(agrupamento_homicidio_ce$NATUREZA_HOMICIDIO, "latin-ascii"),
                 "<br><strong>Incidencia: </strong>", stringi::stri_trans_general(agrupamento_homicidio_ce$INCIDENCIA_HOMICIDIO, "latin-ascii")) %>% lapply(htmltools::HTML)

mapa_agrupamento <- leaflet::leaflet(agrupamento_homicidio_ce) %>%
  setView(lng=-38.5, lat=-3.7, zoom=7) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron", group="Mapa claro") %>%
  addProviderTiles("Esri.NatGeoWorldMap", group= "Mapa verde") %>%
  addScaleBar %>%
  addMarkers(~LONGITUDE, ~LATITUDE,label = labels,  clusterOptions = markerClusterOptions()) %>%
  addLayersControl(baseGroups = c("Mapa verde", "Mapa claro"), options = layersControlOptions(collapsed = FALSE)) %>% 
  clearBounds();mapa_agrupamento

# Salva html do grafico dinamico (Requer caminho absoluto do diretorio destino.)
htmlwidgets::saveWidget(widget = mapa_agrupamento, file = file.path(getwd(), dir_graficos, "clusters_homicidio_ce_2014_2018.html"))
