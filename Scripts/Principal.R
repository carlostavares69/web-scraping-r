# -------------------------------------------------------------------------------------------#
# RASPAGEM DE DADOS (Data Scraping)
# CRIMES VIOLENTOS LETAIS E INTENCIONAIS – CVLI (SSPDS/CE) (2014 a 2019)
#
# Script:      Principal.R - Inicia scripts dependentes, configuracoes, funcoes e analises.
# Autor:       Erivando Sena
# E-mail:      erivandosena@gmail.com 
# Data:        03/08/2019
# Atualizado:  05/02/2020
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
# Exporta data frame sem merges para CSV
df_crime_ce_2014_2019 <- exporta_csv(padroniza_dados(df_dados_limpos_2014_2019), paste0(nome_arquivo,"_Original_Limpo",".csv"))

##############################################################################################
## IMPUTACAO
##############################################################################################
# Da RASPAGEM
# Prepara o conjunto de dados para imputacao
df_crime_ce_2014_2019_missing <- prepara_dados_imputacao(df_crime_ce_2014_2019)
# OU
# Do CSV
# Importa data frame sem merge do CSV
df_crime_ce_2014_2019_missing <- prepara_dados_imputacao(importa_csv("original",paste0(nome_arquivo,"_Original_Limpo",".csv")))

# Faz IMPUTACAO de NA's do conjunto de dados
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

df_tab_orig5
df_tab_pred5

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
cols_relevantes <- c("ID","AIS","MUNICIPIO_HOMICIDIO","NATUREZA_HOMICIDIO","ARMA_UTILIZADA","DATA_HOMICIDIO","HORA_HOMICIDIO","SEXO","IDADE","INCIDENCIA_HOMICIDIO","MES_ANO","FAIXA_ETARIA","GRUPO_NATUREZA_HOMICIDIO","GRUPO_MUNICIPIO","ARMA_DE_FOGO","POPULACAO","IDHM","PIB_PERCAPITA","LATITUDE","LONGITUDE")
crime_ceara <- dplyr::select(crime_ceara, cols_relevantes)

# Importa dados do arquivo CSV dos anos 2014-2019 (Bairros de Fortaleza)
crime_Fortaleza <- importa_csv("fortaleza", paste0(nome_arquivo,"_Original_Limpo_Imputado_Merges_Bairros",".csv"))
cols_relevantes <- c("ID","AIS","NATUREZA_HOMICIDIO","ARMA_UTILIZADA","DATA_HOMICIDIO","HORA_HOMICIDIO","SEXO","IDADE","INCIDENCIA_HOMICIDIO","MES_ANO","FAIXA_ETARIA","GRUPO_NATUREZA_HOMICIDIO","ARMA_DE_FOGO","BAIRRO","LATITUDE","LONGITUDE")
crime_Fortaleza <- dplyr::select(crime_Fortaleza, cols_relevantes)


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

# ----------------------------------------- SCRIPT TEMPORARIO -----------------------------------------#
#install.packages("plotly")
library(plotly)
library(sf)

mapboxToken <- paste(readLines("../.mapbox_token"), collapse="")    # You need your own token
mapboxToken <- c("pk.eyJ1IjoiZXJpdmFuZG9yYW1vcyIsImEiOiJjazVobXkzNXIwNWhmM3JudjJqMHR1MDdlIn0.LFq36FW2CPZ8n5Yw16fQ9w")
Sys.setenv("MAPBOX_TOKEN" = mapboxToken) # for Orca


# Fonte: https://www.littlemissdata.com/blog/maps

# # install.packages("lubridate")
# # install.packages("ggplot2")
# # install.packages("data.table")
#   install.packages("ggrepel")
# # install.packages("dplyr")
# # install.packages("data.table")
# # install.packages("tidyverse")
# if(!requireNamespace("devtools")) {
#   install.packages("devtools")
#   devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)
# } 

#Load the library
library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(ggrepel)
library(tidyverse)
library(ggmap)

# ATIVAR O SERVIÇO DE MAPA ESTÁTICO DO GOOGLE
#Habilitar API Google para utlizar ggmap
suppressMessages(ggmap::register_google(key = "AIzaSyAxnGSaU3mFZanbLuTOQ2vk405Gjp_u6v4", account_type = "standard", client = "weighty-flux-244912", second_limit = 50, day_limit = 2500, write = TRUE))
suppressMessages(ggmap::get_googlemap(language = "pt-BR"))
suppressMessages(ggmap_show_api_key())
print("Google API")
showing_key()
suppressMessages(ggmap_hide_api_key())

coordenadas_lonlat_ce <- c(lon = -39.320624, lat = -5.4983977) 
mapa_ce <- get_googlemap(center = coordenadas_lonlat_ce, zoom = 7, maptype ='terrain', color = 'color', scale = 2, source = 'google')
#mapa_ce <- get_map(location='ceara', zoom=7, maptype = 'terrain', color = 'color', scale = 2, source = 'google')

df_crime_ce <- crime_ceara %>% 
  select(MUNICIPIO_HOMICIDIO, INCIDENCIA_HOMICIDIO, NATUREZA_HOMICIDIO, GRUPO_AIS, LATITUDE, LONGITUDE) %>% 
  mutate(LATITUDE = as.numeric(LATITUDE), LONGITUDE = as.numeric(LONGITUDE)) # %>%
#filter(GRUPO_AIS  == "Interior Sul")
#crime_ceara[stats::complete.cases(crime_ceara),]
#--------------------------------------------------------------------------------------------------#


p <- ggmap(mapa_ce)
p + geom_point(data = df_crime_ce, aes(x = LONGITUDE, y = LATITUDE,  colour = factor(NATUREZA_HOMICIDIO)), size = 3) + theme(legend.position="bottom")

#--------------------------------------------------------------------------------------------------#

p <- ggmap(mapa_ce)
p + geom_point(data=df_crime_ce, aes(x = LONGITUDE, y = LATITUDE), color="red", size=4, alpha=0.5)

#--------------------------------------------------------------------------------------------------#

ggmap(mapa_ce) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, colour = INCIDENCIA_HOMICIDIO), size=4, data=df_crime_ce) + scale_color_gradient(low='blue', high='red')

#--------------------------------------------------------------------------------------------------#

library(RColorBrewer)
cores <- colorRampPalette(brewer.pal(8, "Set2"))(length(unique(df_crime_ce$GRUPO_AIS)))
p <- ggmap(mapa_ce) +
  geom_point(data=df_crime_ce, aes(x = LONGITUDE, y = LATITUDE, color=factor(GRUPO_AIS)), alpha=0.05) +
  guides(colour = guide_legend(override.aes = list(alpha=1.0, size=5.0), title="Type of Crime")) +
  #scale_colour_brewer(type="qual", palette="Paired") + 
  scale_fill_manual(values = cores) +
  ggtitle("Top Crimes in San Francisco") +
  theme_light(base_size=10) +
  theme(legend.position="bottom") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
p
ggsave("sf_top_crimes_map.png", p, width=14, height=10, units="in")

#--------------------------------------------------------------------------------------------------#

ggmap(mapa_ce, extent="device") + 
  geom_point(aes(x = LONGITUDE, y = LATITUDE, fill = INCIDENCIA_HOMICIDIO, size = INCIDENCIA_HOMICIDIO), shape = 21, alpha = 0.7, data = df_crime_ce) + 
  scale_fill_gradient(high = "red",low = "green") + 
  labs(fill = "Number of \nMigrants", title = "Some Migration") + 
  guides(size = FALSE)


#--------------------------------------------------------------------------------------------------#

library(dplyr)
# 10 MUNICICPIOS COM MAIOR NUMERO DE HOMICIDIO
crime_ce_top10 <- crime_ceara %>%
  group_by(MUNICIPIO_HOMICIDIO, LATITUDE, LONGITUDE) %>%
  summarise(INCIDENCIA_MEDIA = mean(INCIDENCIA_HOMICIDIO, na.rm = TRUE)) %>%
  arrange(desc(INCIDENCIA_MEDIA)) %>% 
  ungroup() %>%
  top_n(10) %>%
  mutate(CLASSIFICACAO = rank(desc(INCIDENCIA_MEDIA)))


tipo_homicidio <- filter(crime_ceara, NATUREZA_HOMICIDIO %in% c(unique(GRUPO_NATUREZA_HOMICIDIO)))

p <- ggmap(mapa_ce)

p + stat_density2d(
  aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = 0.25),
  size = 0.1, bins = 5, data = tipo_homicidio,
  geom = "polygon"
) +
  geom_density2d(data = crime_ceara, 
                 aes(x = LONGITUDE, y = LATITUDE), size = 0.3) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, stroke = 2), colour=c("#CC0000"), data = crime_ce_top10, size =1.5)+ 
  geom_label_repel(
    aes(x = LONGITUDE, y = LATITUDE, label = paste(crime_ce_top10$CLASSIFICACAO, crime_ce_top10$MUNICIPIO_HOMICIDIO, sep="-")),
    data=crime_ce_top10,
    #family = 'Arial', 
    size = 3, 
    box.padding = 0.2, point.padding = 0.3,
    segment.color = 'grey50') 

#--------------------------------------------------------------------------------------------------#

# %>% mutate(MUNICIPIO_HOMICIDIO = ifelse(is.na(MUNICIPIO_HOMICIDIO), median(MUNICIPIO_HOMICIDIO, na.rm=TRUE), MUNICIPIO_HOMICIDIO))

library(dplyr)
# 10 BAIRROS COM MAIOR NUMERO DE HOMICIDIO
crime_for_top10 <- crime_Fortaleza %>%
  select(AIS, BAIRRO, LATITUDE, LONGITUDE, INCIDENCIA_HOMICIDIO_BAIRRO) %>%
  group_by(AIS) %>%
  slice(n()) %>%
  arrange(desc(INCIDENCIA_HOMICIDIO_BAIRRO)) %>% 
  ungroup() %>%
  mutate(CLASSIFICACAO = rank(desc(INCIDENCIA_HOMICIDIO_BAIRRO))) %>%
  top_n(10) 

tipo_homicidio <- filter(crime_Fortaleza, NATUREZA_HOMICIDIO %in% c(unique(GRUPO_NATUREZA_HOMICIDIO)))

coordenadas_lonlat_for <- c(lon = -38.58993, lat = -3.723805) 
mapa_fortaleza <- get_googlemap(center = coordenadas_lonlat_for, zoom = 12, maptype ='terrain', color = 'color', scale = 2, source = 'google')

library(ggrepel)

p <- ggmap(mapa_fortaleza)

p + stat_density2d(
  aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = 0.25),
  size = 0.1, bins = 5, data = tipo_homicidio,
  geom = "polygon"
) +
  geom_density2d(data = crime_Fortaleza, 
                 aes(x = LONGITUDE, y = LATITUDE), size = 0.3) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, stroke = 2), colour=c("#CC0000"), data = crime_for_top10, size =1.5)+ 
  geom_label_repel(
    aes(x = LONGITUDE, y = LATITUDE, label = paste(crime_for_top10$CLASSIFICACAO, crime_for_top10$BAIRRO, sep="-")),
    data=crime_for_top10,
    #family = 'Arial', 
    size = 3, 
    box.padding = 0.2, point.padding = 0.3,
    segment.color = 'grey50') 

# ----------------------------------------- SCRIPT TEMPORARIO -----------------------------------------#

# library(lubridate)
# SSPDSCE_2014_2019_ORIGINAL <- suppressMessages(readxl::read_xlsx(file.path(".", dir_auxiliares, "CSAI da SSPDS-CE/CVLI_2014-2019_CSAI-SSPDS-CE.xlsx"), col_names = TRUE, sheet = "CVLI"))
# 
# SSPDSCE_2014_2019_ORIGINAL <- SSPDSCE_2014_2019_ORIGINAL %>%
#   mutate(MUNICÍPIO = replace(MUNICÍPIO, MUNICÍPIO == "ITAPAJE", "ITAPAGE")) %>%
#   mutate(MUNICÍPIO = replace(MUNICÍPIO, MUNICÍPIO == "NOVA JAGUARIBARA", "JAGUARIBARA")) %>%
#   mutate(MUNICÍPIO = replace(MUNICÍPIO, MUNICÍPIO == "IRAPUAN PINHEIRO", "DEPUTADO IRAPUAN PINHEIRO")) 
# 
# setdiff(str_to_upper(SSPDSCE_2014_2019_ORIGINAL$MUNICÍPIO), str_to_upper(crime_ceara$MUNICIPIO_HOMICIDIO))
# 
# # str(SSPDSCE_2014_2019_ORIGINAL)
# # names(SSPDSCE_2014_2019_ORIGINAL)
# 
# # Data frame A
# crime_ceara_imputado <- crime_ceara %>% select(2:7,9:10)
# 
# names(SSPDSCE_2014_2019_ORIGINAL)
# 
# # Data frame B
# SSPDSCE_2014_2019_Formatado <- SSPDSCE_2014_2019_ORIGINAL %>% 
#   #mutate(DATA = format.Date(as.POSIXct(paste(DATA, strftime(HORA, format='%H:%M:%S', tz = "GMT"), sep = " "), tz = "GMT"), format="%d/%m/%Y %H:%M:%S")) %>% 
#   mutate(DATA = format.Date(DATA, format="%d/%m/%Y")) %>% 
#   mutate(HORA = strftime(HORA, format='%H:%M:%S', tz = "GMT")) %>% 
#   arrange(as.Date(DATA, format="%d/%m/%Y")) %>% 
#   mutate_if(is.character, str_to_title) %>% 
#   mutate("MUNICÍPIO" = remove_acentos(MUNICÍPIO), `NOME DA VÍTIMA` = remove_acentos(`NOME DA VÍTIMA`)) %>% 
#   .[,c(1,4,3,2,5,6,8,7,9)] #%>%
#   #select("MUNICÍPIO",DATA)
# names(SSPDSCE_2014_2019_Formatado) <- c(names(crime_ceara_imputado[1:5]),"HORA_HOMICIDIO",names(crime_ceara_imputado[6:8]))
# 
# 
# # Funcao que preparado o conjunto de dados para imputacao
# prepara_dados_imputacao <- function(df_dados) {
#   
#   print("Preparando o conjunto de dados para imputacao.")
#   
#   # obtem os nomes das colunas
#   nomes_colunas_df <- names(df_dados)
#   
#   # cria data variavel global
#   assign('nomes_colunas_df', nomes_colunas_df, envir=.GlobalEnv)
#   
#   # Obter apenas as colunas irrelevantes
#   cols_irrelevantes<- c("AIS","MUNICIPIO_HOMICIDIO","NATUREZA_HOMICIDIO","ARMA_UTILIZADA")
#   crime_ce_reservado <- dplyr::select(df_dados, cols_irrelevantes)
#   
#   # Obter apenas as colunas relevantes
#   cols_relevantes <- c("DATA_HOMICIDIO","NOME_VITIMA","SEXO","IDADE")
#   df_dados <- dplyr::select(df_dados, cols_relevantes)
#   
#   # cria data frame global
#   assign('cols_irrelevantes', cols_irrelevantes, envir=.GlobalEnv)
#   assign('cols_relevantes', cols_relevantes, envir=.GlobalEnv)
#   assign('crime_ce_reservado', crime_ce_reservado, envir=.GlobalEnv)
#   
#   # Transformar os tipos das variáveis em fatores ou numéricos.
#   df_dados <- df_dados %>% dplyr::mutate(IDADE = as.numeric(IDADE))
#   
#   # Duplicar data frame original para avaliar a precisão da imputação posteriormente.
#   df_dados_original <- na.omit(df_dados)
#   
#   # cria data frame global
#   assign('df_dados_original', df_dados_original, envir=.GlobalEnv)
#   
#   # Verificando os tipos de cada preditor
#   tipos <- sapply(df_dados, class)
#   print(tipos)
#   
#   # Verifica os dados para valores ausentes.
#   missing <- sapply(df_dados, function(x) sum(is.na(x)))
#   
#   print("Quantitativo de dados missing nas variaveis:")
#   print(missing)
#   
#   # Retorna em data frame pronto para imputacao
#   return(df_dados)
# }
# 
# # Funcao que realiza imputacao dos dados faltantes
# realiza_imputacao_dados <- function(df_dados) {
#   
#   print("Iniciando imputação multivariada com o algoritmo Mice.")
#   
#   # Iniciando o pacote mice. 
#   init <- mice(df_dados, maxit=0) 
#   meth <- init$method
#   predM <- init$predictorMatrix
# 
#   # Metodos padroes: norm, logreg, polyreg, polr, pmm.
#   # meth[c("NATUREZA_HOMICIDIO","ARMA_UTILIZADA")] <- "polyreg" 
#   meth[c("DATA_HOMICIDIO","NOME_VITIMA")] <- "polyreg"
#   meth[c("IDADE")] <- "pmm" 
#   meth[c("SEXO")] <- "logreg" 
#   
#   # Executar a imputação múltipla (m=5).
#   set.seed(103)
#   df_dados_imputado <- mice(df_dados, method=meth, predictorMatrix=predM, m=5)
#   
#   # Faz imputacao de NA em MUNICIPIO_HOMICIDIO pela mediana de todos os MUNICIPIO_HOMICIDIO
#   crime_ce_reservado <- crime_ce_reservado %>% mutate(MUNICIPIO_HOMICIDIO = ifelse(is.na(MUNICIPIO_HOMICIDIO), median(MUNICIPIO_HOMICIDIO, na.rm=TRUE), MUNICIPIO_HOMICIDIO))
#   
#   # Cria um conjunto de dados apos imputacao.
#   df_dados_imputado <- complete(df_dados_imputado)
#   
#   # Verificar se existe missing no conjunto de dados imputado.
#   status_missing <- sapply(df_dados_imputado, function(x) sum(is.na(x)))
#   
#   print("Exibindo resultados da IMPUTACAO:")
#   print(status_missing)
#   
#   # DATA_HOMICIDIO
#   col1_original <- df_dados_original$DATA_HOMICIDIO[is.na(df_dados$DATA_HOMICIDIO)]#;print(table(col1_original))
#   col1_preditado <- df_dados_imputado$DATA_HOMICIDIO[is.na(df_dados$DATA_HOMICIDIO)]#;print(table(col1_preditado))
# 
#   # NOME_VITIMA
#   col2_original <- df_dados_original$NOME_VITIMA[is.na(df_dados$NOME_VITIMA)]#;table(col2_original)
#   col2_preditado  <- df_dados_imputado$NOME_VITIMA[is.na(df_dados$NOME_VITIMA)]#;table(col2_preditado)
# 
#   # SEXO
#   col3_original <- df_dados_original$SEXO[is.na(df_dados$SEXO)]#;table(col3_original)
#   col3_preditado <- df_dados_imputado$SEXO[is.na(df_dados$SEXO)]#;table(col3_preditado)
#   
#   # IDADE
#   col4_original <- df_dados_original$IDADE[is.na(df_dados$IDADE)]#;mean(col4_original, na.rm=TRUE) #mean(col4_original) 
#   col4_preditado <- df_dados_imputado$IDADE[is.na(df_dados$IDADE)]#;mean(col4_preditado)
#   
#   # Cria data frame com acesso global
#   df_tab_orig1 <- table(col1_original)
#   df_tab_pred1 <- table(col1_preditado)
#   assign('df_tab_orig1', df_tab_orig1, envir=.GlobalEnv)
#   assign('df_tab_pred1', df_tab_pred1, envir=.GlobalEnv)
# 
#   df_tab_orig2 <- table(col2_original)
#   df_tab_pred2 <- table(col2_preditado)
#   assign('df_tab_orig2', df_tab_orig2, envir=.GlobalEnv)
#   assign('df_tab_pred2', df_tab_pred2, envir=.GlobalEnv)
# 
#   df_tab_orig3 <- table(col3_original)
#   df_tab_pred3 <- table(col3_preditado)
#   assign('df_tab_orig3', df_tab_orig3, envir=.GlobalEnv)
#   assign('df_tab_pred3', df_tab_pred3, envir=.GlobalEnv)
#   
#   df_tab_orig4 <- mean(col4_original, na.rm=TRUE)
#   df_tab_pred4 <- mean(col4_preditado)
#   assign('df_tab_orig4', df_tab_orig4, envir=.GlobalEnv)
#   assign('df_tab_pred4', df_tab_pred4, envir=.GlobalEnv)
#   
#   # Faz uniao de data frames para manter estrutura anterior do conjunto de dados.
#   df_dados_imputado <- cbind(crime_ce_reservado, df_dados_imputado)
#   
#   # Reordena colunas 
#   df_dados_imputado <- df_dados_imputado[, c(1:4, 5:8)]
#   
#   # Renomeia titulos das colunas
#   names(df_dados_imputado) <- nomes_colunas_df
#   
#   print("Realizando exportacao para arquivo CSV:")
#   exporta_csv(df_dados_imputado, c("CVLI_2014-2019_CSAI-SSPDS-CE_Imputado.csv"))
#   
#   # Retorna data frame imputado
#   return(df_dados_imputado)
#   
# }
# 
# # Prepara o conjunto de dados para imputacao
# #SSPDSCE_2014_2019_Formatado_missing <- prepara_dados_imputacao(SSPDSCE_2014_2019_Formatado)
# 
# # Faz IMPUTACAO do conjunto de dados
# #SSPDSCE_2014_2019_Formatado_imputado <- realiza_imputacao_dados(SSPDSCE_2014_2019_Formatado_missing)
# 
# # SSPDSCE_2014_2019_Formatado_imputado<- SSPDSCE_2014_2019_ORIGINAL %>% 
# #   mutate(DATA = as.Date(DATA, format="%d/%m/%Y")) %>%
# #   mutate(HORA = strftime(HORA, format='%H:%M:%S', tz = "GMT")) %>%
# #   mutate(DATA_HORA = as.POSIXct(paste(DATA, HORA, sep = " "), tz = "GMT"))
# 
# str(crime_ceara_imputado)
# str(SSPDSCE_2014_2019_Formatado_imputado)
# 
# # 2014
# df_A_2014 <- crime_ceara_imputado  %>% 
#   mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
#   filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2014)
# 
# df_B_2014 <- SSPDSCE_2014_2019_Formatado_imputado %>% 
#   mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
#   filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2014)
# 
# # 2015
# df_A_2015 <- crime_ceara_imputado  %>% 
#   mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
#   filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2015)
# 
# df_B_2015 <- SSPDSCE_2014_2019_Formatado_imputado %>% 
#   mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
#   filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2015)
# 
# # 2016
# df_A_2016 <- crime_ceara_imputado  %>% 
#   mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
#   filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2016)
# 
# df_B_2016 <- SSPDSCE_2014_2019_Formatado_imputado %>% 
#   mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
#   filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2016)
# 
# # 2017
# df_A_2017 <- crime_ceara_imputado %>% 
#   mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
#   filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2017)
# 
# df_B_2017 <- SSPDSCE_2014_2019_Formatado_imputado %>% 
#   mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
#   filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2017)
# 
# # 2018
# df_A_2018 <- crime_ceara_imputado  %>% 
#   mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
#   filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2018)
# 
# df_B_2018 <- SSPDSCE_2014_2019_Formatado_imputado  %>% 
#   mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
#   filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2018)
#   
# # 2019
# df_A_2019 <- crime_ceara_imputado  %>% 
#   mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
#   filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2019)
# 
# df_B_2019 <- SSPDSCE_2014_2019_Formatado_imputado  %>% 
#   mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
#   filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2019)
# 
# 
# df_A_2014n <- nrow(df_A_2014)
# df_B_2014n <- nrow(df_B_2014)
# df_A_2015n <- nrow(df_A_2015)
# df_B_2015n <- nrow(df_B_2015)
# df_A_2016n <- nrow(df_A_2016)
# df_B_2016n <- nrow(df_B_2016)
# df_A_2017n <- nrow(df_A_2017)
# df_B_2017n <- nrow(df_B_2017)
# df_A_2018n <- nrow(df_A_2018)
# df_B_2018n <- nrow(df_B_2018)
# df_A_2019n <- nrow(df_A_2019)
# df_B_2019n <- nrow(df_B_2019)
# 
# print(paste0("DF A 2014: ", df_A_2014n))
# print(paste0("DF B 2014: ", df_B_2014n))
# print(paste0("Inconsistência : ", df_B_2014n-df_A_2014n))
# 
# print(paste0("DF A 2015: ", df_A_2015n))
# print(paste0("DF B 2015: ", df_B_2015n))
# print(paste0("Inconsistência : ", df_B_2015n-df_A_2015n))
# 
# print(paste0("DF A 2016: ", df_A_2016n))
# print(paste0("DF B 2016: ", df_B_2016n))
# print(paste0("Inconsistência : ", df_B_2016n-df_A_2016n))
# 
# print(paste0("DF A 2017: ", df_A_2017n))
# print(paste0("DF B 2017: ", df_B_2017n))
# print(paste0("Inconsistência : ", df_B_2017n-df_A_2017n))
# 
# print(paste0("DF A 2018: ", df_A_2018n))
# print(paste0("DF B 2018: ", df_B_2018n))
# print(paste0("Inconsistência : ", df_B_2018n-df_A_2018n))
# 
# print(paste0("DF A 2019: ", df_A_2019n))
# print(paste0("DF B 2019: ", df_B_2019n))
# print(paste0("Inconsistência : ", df_B_2019n-df_A_2019n))
# 
# print(paste0("Inconsistência total: ", (44+28+50+37+49)-3)) 
# 
# str(crime_ceara_imputado)
# str(SSPDSCE_2014_2019_Formatado)
# 
# # Seleciona mes
# crime_ceara_imputado_data <- crime_ceara_imputado %>% mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) 
# crime_ceara_imputado_data$DIAS_MES <- lubridate::days_in_month(crime_ceara_imputado_data$DATA_HOMICIDIO)
# 
# SSPDSCE_2014_2019_Formatado_data <- SSPDSCE_2014_2019_Formatado_imputado %>% mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) 
# SSPDSCE_2014_2019_Formatado_data$DIAS_MES <- lubridate::days_in_month(SSPDSCE_2014_2019_Formatado_data$DATA_HOMICIDIO)
# 
# crime_ceara_imputado_m <- crime_ceara_imputado_data %>% 
#   filter(between(month(DATA_HOMICIDIO), 1, max(DIAS_MES)) & month(DATA_HOMICIDIO) == 1)
# 
# SSPDSCE_2014_2019_Formatado_m <- SSPDSCE_2014_2019_Formatado_data %>% 
#   filter(between(month(DATA_HOMICIDIO), 1, max(DIAS_MES)) & month(DATA_HOMICIDIO) == 1)
# 
# 
# summary(crime_ceara_imputado_m)
# summary(SSPDSCE_2014_2019_Formatado_m)
# 
# which(crime_ceara_imputado$NOME_VITIMA == "Yanka Vieira Do Nascimento", arr.ind = TRUE)
# 
# crime_ceara_hora <- crime_ceara_imputado
# crime_ceara_hora$HORA_HOMICIDIO <- NA
# crime_ceara_hora <- crime_ceara_hora[,c(1:5,9,6:8)]


# ----------------------------------------- SCRIPT TEMPORARIO -----------------------------------------#
# DEFINICAO DO ALGORITMO PARA CRIACAO DE FUNCAO

# Importa dados do arquivo CSV dos anos 2014-2019
df_raspagem <- importa_csv("original", paste0(nome_arquivo,"_Original_Limpo",".csv")) %>% 
  mutate(NOME_VITIMA = stringr::str_squish(NOME_VITIMA)) 

SSPDS_2014_2019_Ceara_Transparente <- suppressMessages(readxl::read_xlsx(file.path(".", dir_auxiliares, "CSAI da SSPDS-CE/CVLI_2014-2019_CSAI-SSPDS-CE.xlsx"), col_names = TRUE, sheet = "CVLI"))

library(stringr)

df_CSAI_SSPDS <- SSPDS_2014_2019_Ceara_Transparente %>%
  mutate(DATA = format.Date(DATA, format="%d/%m/%Y")) %>% 
  mutate(HORA = strftime(HORA, format='%H:%M:%S', tz = "GMT")) %>% 
  arrange(as.Date(DATA, format="%d/%m/%Y")) %>% 
  mutate_if(is.character, str_to_title) %>% 
  mutate(`NOME DA VÍTIMA` = stringr::str_squish(remove_acentos(`NOME DA VÍTIMA`))) %>% 
  .[,c(1,4,3,2,5,6,8,7,9)]

names(df_CSAI_SSPDS) <- c(names(df_raspagem[2:6]),"HORA_HOMICIDIO",names(df_raspagem[c(7,9:10)]))

######## apenas para verificar a consistencia do numero de observacoes ######## 
# 2014
df_A_2014 <- df_raspagem  %>% 
  mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
  filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2014)

df_B_2014 <- df_CSAI_SSPDS %>% 
  mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
  filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2014)

# 2015
df_A_2015 <- df_raspagem  %>% 
  mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
  filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2015)

df_B_2015 <- df_CSAI_SSPDS %>% 
  mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
  filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2015)

# 2016
df_A_2016 <- df_raspagem  %>% 
  mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
  filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2016)

df_B_2016 <- df_CSAI_SSPDS %>% 
  mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
  filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2016)

# 2017
df_A_2017 <- df_raspagem %>% 
  mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
  filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2017)

df_B_2017 <- df_CSAI_SSPDS %>% 
  mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
  filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2017)

# 2018
df_A_2018 <- df_raspagem  %>% 
  mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
  filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2018)

df_B_2018 <- df_CSAI_SSPDS  %>% 
  mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
  filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2018)

# 2019
df_A_2019 <- df_raspagem  %>% 
  mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
  filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2019)

df_B_2019 <- df_CSAI_SSPDS  %>% 
  mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
  filter(between(month(DATA_HOMICIDIO), 1, 12) & year(DATA_HOMICIDIO) == 2019)

df_A_2014n <- nrow(df_A_2014)
df_B_2014n <- nrow(df_B_2014)
df_A_2015n <- nrow(df_A_2015)
df_B_2015n <- nrow(df_B_2015)
df_A_2016n <- nrow(df_A_2016)
df_B_2016n <- nrow(df_B_2016)
df_A_2017n <- nrow(df_A_2017)
df_B_2017n <- nrow(df_B_2017)
df_A_2018n <- nrow(df_A_2018)
df_B_2018n <- nrow(df_B_2018)
df_A_2019n <- nrow(df_A_2019)
df_B_2019n <- nrow(df_B_2019)

print(paste0("DF A 2014: ", df_A_2014n))
print(paste0("DF B 2014: ", df_B_2014n))
print(paste0("Inconsistência : ", df_B_2014n-df_A_2014n))

print(paste0("DF A 2015: ", df_A_2015n))
print(paste0("DF B 2015: ", df_B_2015n))
print(paste0("Inconsistência : ", df_B_2015n-df_A_2015n))

print(paste0("DF A 2016: ", df_A_2016n))
print(paste0("DF B 2016: ", df_B_2016n))
print(paste0("Inconsistência : ", df_B_2016n-df_A_2016n))

print(paste0("DF A 2017: ", df_A_2017n))
print(paste0("DF B 2017: ", df_B_2017n))
print(paste0("Inconsistência : ", df_B_2017n-df_A_2017n))

print(paste0("DF A 2018: ", df_A_2018n))
print(paste0("DF B 2018: ", df_B_2018n))
print(paste0("Inconsistência : ", df_B_2018n-df_A_2018n))

print(paste0("DF A 2019: ", df_A_2019n))
print(paste0("DF B 2019: ", df_B_2019n))
print(paste0("Inconsistência : ", df_B_2019n-df_A_2019n))

print(paste0("Inconsistência total: ", (44+28+50+37+49)-3)) 
######## apenas para verificar a consistencia do numero de observacoes ######## 

verifica_inconsistencias(df_CSAI_SSPDS$NOME_VITIMA, df_raspagem$NOME_VITIMA)

# Verifica missing
sapply(df_CSAI_SSPDS, function(df){
  sum(is.na(df) == TRUE)/length(df);
})

df_raspagem$HORA_HOMICIDIO <- NA
df_raspagem <- df_raspagem[,c(1:6,11,7:10)]

# # [1] "NOVA Linha para -> Francisco Carlito Oliveira De Sousa Filho" -> 	Francisco Carlito Oliveira De S.filho
# # [1] "NOVA Linha para -> Luzimeire Da Silva Costa" -> 					          Luzimeire Da Silvacosta
# # [1] "NOVA Linha para -> Manoel Mendes Da Silva" -> 						          Manoel Mendes Dasilva
# # [1] "NOVA Linha para -> Francisco Cristiano De Aguiar Do Nascimento" -> Francisco Cristiano De Aguiar Do Nasci
# # [1] "NOVA Linha para -> Francisco Francicleudo Rodrigues De Moura" ->   Francisco Francicleudo Rodrigues De Mo
# # [1] "NOVA Linha para -> Francisco Xavier Pereira Guimaraes Junior" ->   Francisco Xavier Pereira Guimaraes Jun
# # [1] "NOVA Linha para -> Francisco Antonio Yago Da Silva Bezerra" ->     Francisco Antonio Yago Da Silva Bezerr
# # [1] "NOVA Linha para -> Robson Helder Torres Dos Santos" -> 			      Robsonhelder Torres Dos Santos

# Substitui valores inconsistentes da variavel NOME_VITIMA
df_raspagem <- df_raspagem %>%
  mutate(NOME_VITIMA = replace(NOME_VITIMA, NOME_VITIMA == "Francisco Carlito Oliveira De S.filho", "Francisco Carlito Oliveira De Sousa Filho")) %>%
  mutate(NOME_VITIMA = replace(NOME_VITIMA, NOME_VITIMA == "Luzimeire Da Silvacosta", "Luzimeire Da Silva Costa")) %>%
  mutate(NOME_VITIMA = replace(NOME_VITIMA, NOME_VITIMA == "Manoel Mendes Dasilva", "Manoel Mendes Da Silva"))  %>%
  mutate(NOME_VITIMA = replace(NOME_VITIMA, NOME_VITIMA == "Francisco Cristiano De Aguiar Do Nasci", "Francisco Cristiano De Aguiar Do Nascimento")) %>%
  mutate(NOME_VITIMA = replace(NOME_VITIMA, NOME_VITIMA == "Francisco Francicleudo Rodrigues De Mo", "Francisco Francicleudo Rodrigues De Moura")) %>%
  mutate(NOME_VITIMA = replace(NOME_VITIMA, NOME_VITIMA == "Francisco Xavier Pereira Guimaraes Jun", "Francisco Xavier Pereira Guimaraes Junior"))  %>%
  mutate(NOME_VITIMA = replace(NOME_VITIMA, NOME_VITIMA == "Francisco Antonio Yago Da Silva Bezerr", "Francisco Antonio Yago Da Silva Bezerra")) %>%
  mutate(NOME_VITIMA = replace(NOME_VITIMA, NOME_VITIMA == "Robsonhelder Torres Dos Santos", "Robson Helder Torres Dos Santos")) 

inexistente <- NULL

# Estrutura de repeticao 
for(i in 1:nrow(df_CSAI_SSPDS)) {

  indice_linha <- which(df_raspagem$NOME_VITIMA == df_CSAI_SSPDS[i,7]$NOME_VITIMA, arr.ind = TRUE)
  
  # merge variavel HORA e atualizacao das variaveis MUNICIPIO_HOMICIDIO,NATUREZA_HOMICIDIO,ARMA_UTILIZADA,HORA_HOMICIDIO
  if(!is.na(indice_linha[1])) {
      if(length(indice_linha[1]) > 0 ) {
      
        print(paste("Incluindo variavel HORA atraves da variavel NOME_VITIMA:", "Indice pesquisado",indice_linha[1], "Indice atualizado",i, sep = " "))
        print(paste("->", df_raspagem[indice_linha[1],8]$NOME_VITIMA, sep = " "))
        
        # Atualiza variavel MUNICIPIO_HOMICIDIO
        df_raspagem[indice_linha[1], 3] <- df_CSAI_SSPDS[i, 2]$MUNICIPIO_HOMICIDIO

        # Atualiza variavel NATUREZA_HOMICIDIO
        df_raspagem[indice_linha[1], 4] <- df_CSAI_SSPDS[i, 3]$NATUREZA_HOMICIDIO

        # Atualiza variavel ARMA_UTILIZADA
        df_raspagem[indice_linha[1], 5] <- df_CSAI_SSPDS[i, 4]$ARMA_UTILIZADA

        # Atualiza variavel HORA_HOMICIDIO
        df_raspagem[indice_linha[1], 7] <- df_CSAI_SSPDS[i, 6]$HORA_HOMICIDIO
        
      } 
  }  else {
      inexistente <- rbind(inexistente, df_CSAI_SSPDS[i, 7]$NOME_VITIMA) 
  }
  
  if(df_CSAI_SSPDS[i, 7]$NOME_VITIMA %in% inexistente[,1]) {

    print(paste("NOVA LINHA referente ->", df_CSAI_SSPDS[i,7]$NOME_VITIMA, sep = " "))
    df_raspagem <- rbind(dplyr::data_frame(

        ID = NA,
        AIS = df_CSAI_SSPDS[i, 1]$AIS,
        MUNICIPIO_HOMICIDIO = df_CSAI_SSPDS[i, 2]$MUNICIPIO_HOMICIDIO,
        NATUREZA_HOMICIDIO = df_CSAI_SSPDS[i, 3]$NATUREZA_HOMICIDIO,
        ARMA_UTILIZADA = df_CSAI_SSPDS[i, 4]$ARMA_UTILIZADA,
        DATA_HOMICIDIO = df_CSAI_SSPDS[i, 5]$DATA_HOMICIDIO,
        HORA_HOMICIDIO = df_CSAI_SSPDS[i, 6]$HORA_HOMICIDIO,
        NOME_VITIMA = df_CSAI_SSPDS[i, 7]$NOME_VITIMA,
        GUIA_CADAVERICA = NA,
        SEXO = df_CSAI_SSPDS[i, 8]$SEXO,
        IDADE = df_CSAI_SSPDS[i, 9]$IDADE

      ), df_raspagem) %>% arrange(as.Date(DATA_HOMICIDIO, format="%d/%m/%Y"))

  }
  
}

