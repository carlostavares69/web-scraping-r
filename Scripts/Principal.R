# -------------------------------------------------------------------------------------------#
# RASPAGEM DE DADOS (Data Scraping)
# CRIMES VIOLENTOS LETAIS E INTENCIONAIS – CVLI (SSPDS/CE)
#
# Script:      Principal.R - Inicia scripts dependentes, configuracoes, funcoes e analises.
# Autor:       Erivando Sena
# E-mail:      erivandosena@gmail.com 
# Data:        03/08/2019
# Atualizado:  24/12/2019
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
# Adiciona novas variaveis ao Dataset
df_crime_ce_2014_2019_merges <- executa_merges(df_crime_ce_2014_2019_imputado)

##############################################################################################
## EXPORTACAO
##############################################################################################

# Exporta data frames para CSV
exporta_csv(obtem_dados_por_ano(df_crime_ce_2014_2019_merges, 2019, 2019), "Crimes_Ceara_Limpo_Imputado_Merges_2019",".csv")
exporta_csv(obtem_dados_por_ano(df_crime_ce_2014_2019_merges, 2018, 2018), "Crimes_Ceara_Limpo_Imputado_Merges_2018",".csv")
exporta_csv(obtem_dados_por_ano(df_crime_ce_2014_2019_merges, 2017, 2017), "Crimes_Ceara_Limpo_Imputado_Merges_2017",".csv")
exporta_csv(obtem_dados_por_ano(df_crime_ce_2014_2019_merges, 2016, 2016), "Crimes_Ceara_Limpo_Imputado_Merges_2016",".csv")
exporta_csv(obtem_dados_por_ano(df_crime_ce_2014_2019_merges, 2015, 2015), "Crimes_Ceara_Limpo_Imputado_Merges_2015",".csv")
exporta_csv(obtem_dados_por_ano(df_crime_ce_2014_2019_merges, 2014, 2014), "Crimes_Ceara_Limpo_Imputado_Merges_2014",".csv")

##############################################################################################
## IMPORTACAO
##############################################################################################

# Importa dados do arquivo CSV dos anos 2014-2019
crime_ceara <- importa_csv("personalizado", paste0(nome_arquivo,"_Original_Limpo_Imputado_Merges",".csv"))

# Seleciona apenas colunas relevantes para analise
cols_relevantes <- c("ID","MUNICIPIO_HOMICIDIO","NATUREZA_HOMICIDIO","ARMA_UTILIZADA","DATA_HOMICIDIO","SEXO","IDADE","INCIDENCIA_HOMICIDIO","MES_ANO","FAIXA_ETARIA","GRUPO_MUNICIPIO","ARMA_DE_FOGO","POPULACAO","IDHM","PIB_PERCAPITA","LATITUDE","LONGITUDE")
crime_ceara <- dplyr::select(crime_ceara, cols_relevantes)

##############################################################################################
## ANALISE
##############################################################################################

head(crime_ceara)
str(crime_ceara)
summary(crime_ceara)

# Verifando os valores ausentes
lapply(crime_ceara, function(x) sum(is.na(x)))

##############################################################################################
## VISUALIZAÇÃO
##############################################################################################
