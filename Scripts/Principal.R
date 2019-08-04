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
## EXPORTACAO DOS DADOS
##############################################################################################

# Executa funcao de exportacao para o formato CSV
exporta_csv(dados_crime_ce_2018, "Indicadores_Crimes_CE_2018.csv")
exporta_csv(dados_crime_ce_2017, "Indicadores_Crimes_CE_2017.csv")
exporta_csv(dados_crime_ce_2016, "Indicadores_Crimes_CE_2016.csv")
exporta_csv(dados_crime_ce_2015, "Indicadores_Crimes_CE_2015.csv")
exporta_csv(dados_crime_ce_2014, "Indicadores_Crimes_CE_2014.csv")
exporta_csv(df_dados_limpos_2014_2018)

##############################################################################################
## IMPORTACAO DOS DADOS
##############################################################################################

# Importa dados do arquivo CSV anos 2014-2018
df_crime_ce_2014_2018 <- importa_csv( file.path(".", dir_dados, paste0(nome_arquivo, ".csv")) )

##############################################################################################
## FORMATACAO DOS DADOS
##############################################################################################

# Padroniza dados
df_crime_ce_2014_2018 <- padroniza_dados(df_crime_ce_2014_2018)

##############################################################################################
## MERGE DE DADOS
##############################################################################################

# Merge com dados geoespaciais
df_crime_ce_2014_2018 <- merge_dados_geo(df_crime_ce_2014_2018)

##############################################################################################
## CONJUNTO DE DADOS PARA ANALISES
##############################################################################################

# Data frame geral
glimpse(df_crime_ce_2014_2018)
View(df_crime_ce_2014_2018)
