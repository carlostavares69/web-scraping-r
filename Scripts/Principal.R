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

##############################################################################################
## COLETA DOS DADOS
##############################################################################################

# Executa a funcao que realiza o download do(s) documento(s) dos anos de 2014 a 2018
obtem_arquivos()

##############################################################################################
## LIMPEZA DOS DADOS
##############################################################################################

# Executa a funcao de limpeza dos dados e obtem um data frame com dados para analises com "tabulizer"
df_dados_limpos <- realiza_limpeza_dados()
View(df_dados_limpos)

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
