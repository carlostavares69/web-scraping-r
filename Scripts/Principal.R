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
source(file = file.path(".", "Scripts/Funcao_limpa_dados_2017.R"), encoding = "UTF-8") 

##############################################################################################
## COLETA DOS DADOS
##############################################################################################

# Executa a funcao que obtem um data frame com urls e nomes dos documentos de 2018
df_listas_2018 = extrai_lista_documentos(URL_2018)

# Executa a funcao que obtem um data frame com urls e nomes dos documentos de 2017
df_listas_2017 = extrai_lista_documentos(URL_2017)

# Executa a funcao que obtem um data frame com urls e nomes dos documentos de 2016
df_listas_2016 = extrai_lista_documentos(URL_2016)

# Executa a funcao que obtem um data frame com urls e nomes dos documentos de 2015
df_listas_2015 = extrai_lista_documentos(URL_2015)

# Executa a funcao que obtem um data frame com urls e nomes dos documentos de 2014
df_listas_2014 = extrai_lista_documentos(URL_2014)

# Executa a funcao que realiza o download do(s) documento(s) de 2018
download_documentos(df_listas_2018)

# Executa a funcao que realiza o download do(s) documento(s) de 2017
download_documentos(df_listas_2017)

# Executa a funcao que realiza o download do(s) documento(s) de 2016
download_documentos(df_listas_2016)

# Executa a funcao que realiza o download do(s) documento(s) de 2015
download_documentos(df_listas_2015)

# Executa a funcao que realiza o download do(s) documento(s) de 2014
download_documentos(df_listas_2014)

# Executa a funcao que realiza o download do(s) documento(s) por ano
obtem_arquivos()

##############################################################################################
## LIMPEZA DOS DADOS
##############################################################################################

# Executa a funcao de limpeza dos dados e obtem um data frame com dados para analises com "tabulizer"
df_dados_limpos <- limpa_dados(df_listas)

# Merge com dados geoespaciais
df_dados_merge <- merge_dados_geo(df_dados_limpos)


##############################################################################################
## IMPORTACAO DE DADOS
##############################################################################################

# Execua a funcao de exportacao para o formato CSV e obtem um data frame do CSV salvo
df_violencia_ce_2018 <- exporta_importa_csv(df_dados_merge)

##############################################################################################
## CONJUNTO DE DADOS
##############################################################################################

# Data frame principal
glimpse(df_violencia_ce_2018)
