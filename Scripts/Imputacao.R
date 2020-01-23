# -------------------------------------------------------------------------------------------#
# RASPAGEM DE DADOS (Data Scraping)
#
# Script:      Imputacao.R - Realiza imputação múltipla de dados faltantes (missing data).
# Autor:       Erivando Sena
# E-mail:      erivandosena@gmail.com
# Data:        14/12/2019
# Atualizado:  15/12/2019
##-------------------------------------------------------------------------------------------#

##############################################################################################
## PREPARACAO DOS DADOS
##############################################################################################
# Funcao que preparado o conjunto de dados para imputacao
prepara_dados_imputacao <- function(df_dados_limpos) {

  print("Preparando o conjunto de dados para imputacao.")
  
  # Obter o bando de dados
  df_pronto <- df_dados_limpos
  
  # obtem os nomes das colunas
  nomes_colunas_df <- names(df_pronto)
  
  # cria data variavel global
  assign('nomes_colunas_df', nomes_colunas_df, envir=.GlobalEnv)
  
  # Obter apenas as colunas irrelevantes
  cols_irrelevantes<- c("ID","AIS","MUNICIPIO_HOMICIDIO","DATA_HOMICIDIO","NOME_VITIMA","GUIA_CADAVERICA")
  crime_ce_reservado <- dplyr::select(df_pronto, cols_irrelevantes)
  
  # Obter apenas as colunas relevantes
  cols_relevantes <- c("NATUREZA_HOMICIDIO","ARMA_UTILIZADA","SEXO","IDADE")
  df_pronto <- dplyr::select(df_pronto, cols_relevantes)
  
  # cria data frame global
  assign('cols_irrelevantes', cols_irrelevantes, envir=.GlobalEnv)
  assign('cols_relevantes', cols_relevantes, envir=.GlobalEnv)
  assign('crime_ce_reservado', crime_ce_reservado, envir=.GlobalEnv)

  # Transformar os tipos das variáveis em fatores ou numéricos.
  df_pronto <- df_pronto %>%
    dplyr::mutate(
      NATUREZA_HOMICIDIO = as.factor(NATUREZA_HOMICIDIO),
      ARMA_UTILIZADA = as.factor(ARMA_UTILIZADA),
      SEXO = as.factor(SEXO),
      IDADE = as.numeric(IDADE)
    )
  
  # Duplicar data frame original para avaliar a precisão da imputação posteriormente.
  df_pronto_original <- na.omit(df_pronto)
  
  # cria data frame global
  assign('df_pronto_original', df_pronto_original, envir=.GlobalEnv)
  
  # Verificando os tipos de cada preditor
  tipos <- sapply(df_pronto, class)
  print(tipos)
  
  # Verifica os dados para valores ausentes.
  missing <- sapply(df_pronto, function(x) sum(is.na(x)))
  
  print("Quantitativo de dados missing nas variaveis:")
  print(missing)
  
  # Retorna em data frame pronto para imputacao
  return(df_pronto)
}

##############################################################################################
## IMPUTACAO E PRECISAO
##############################################################################################
realiza_imputacao_dados <- function(df_pronto) {
    
  print("Iniciando imputação multivariada com o algoritmo Mice.")
  
  # Iniciando o pacote mice. 
  init <- mice(df_pronto, maxit=0) 
  meth <- init$method
  predM <- init$predictorMatrix
  
  # Remove variável da predicao na imputacao.
  #predM[, c("DATA_HOMICIDIO")] <- 0
  
  # Remove variavel da imputacao mantendo-se na predicao.
  #meth[c("MUNICIPIO_HOMICIDIO")]=""
  
  # Especificar os metodos para imputar os valores ausentes.
  # Existem métodos específicos para variáveis contínuas, binárias e ordinais.
  # Metodos padroes: norm, logreg, polyreg, polr, pmm.
  meth[c("NATUREZA_HOMICIDIO","ARMA_UTILIZADA")] <- "polyreg" 
  meth[c("IDADE")] <- "pmm" 
  meth[c("SEXO")] <- "logreg" 
  
  # Executar a imputação múltipla (m=5).
  set.seed(103)
  df_pronto_imputado <- mice(df_pronto, method=meth, predictorMatrix=predM, m=5)
  
  # Faz imputacao de NA em MUNICIPIO_HOMICIDIO pela mediana de todos os MUNICIPIO_HOMICIDIO
  crime_ce_reservado <- crime_ce_reservado %>% mutate(MUNICIPIO_HOMICIDIO = ifelse(is.na(MUNICIPIO_HOMICIDIO), median(MUNICIPIO_HOMICIDIO, na.rm=TRUE), MUNICIPIO_HOMICIDIO))
  
  # Cria um conjunto de dados apos imputacao.
  df_pronto_imputado <- complete(df_pronto_imputado)
  
  # Verificar se existe missing no conjunto de dados imputado.
  status_missing <- sapply(df_pronto_imputado, function(x) sum(is.na(x)))
  
  print("Exibindo resultados da IMPUTACAO:")
  print(status_missing)
  
  # NATUREZA_HOMICIDIO
  col1_original <- df_pronto_original$NATUREZA_HOMICIDIO[is.na(df_pronto$NATUREZA_HOMICIDIO)]#;print(table(col1_original))
  col1_preditado <- df_pronto_imputado$NATUREZA_HOMICIDIO[is.na(df_pronto$NATUREZA_HOMICIDIO)]#;print(table(col1_preditado))
  # table(col1_original)
  # table(col1_preditado)
  
  # ARMA_UTILIZADA
  col2_original <- df_pronto_original$ARMA_UTILIZADA[is.na(df_pronto$ARMA_UTILIZADA)]#;table(col2_original)
  col2_preditado  <- df_pronto_imputado$ARMA_UTILIZADA[is.na(df_pronto$ARMA_UTILIZADA)]#;table(col2_preditado)
  # table(col2_original)
  # table(col2_preditado)
  
  # SEXO
  col3_original <- df_pronto_original$SEXO[is.na(df_pronto$SEXO)]#;table(col3_original)
  col3_preditado <- df_pronto_imputado$SEXO[is.na(df_pronto$SEXO)]#;table(col3_preditado)
  # table(col3_original)
  # table(col3_preditado)
  
  # IDADE
  col4_original <- df_pronto_original$IDADE[is.na(df_pronto$IDADE)]#;mean(col4_original, na.rm=TRUE) #mean(col4_original) 
  col4_preditado <- df_pronto_imputado$IDADE[is.na(df_pronto$IDADE)]#;mean(col4_preditado)
  # mean(col4_original, na.rm=TRUE)
  # mean(col4_preditado)
  
  # Cria data frame com acesso global
  df_tab_orig1 <- table(col1_original)
  df_tab_pred1 <- table(col1_preditado)
  assign('df_tab_orig1', df_tab_orig1, envir=.GlobalEnv)
  assign('df_tab_pred1', df_tab_pred1, envir=.GlobalEnv)
  
  df_tab_orig2 <- table(col2_original)
  df_tab_pred2 <- table(col2_preditado)
  assign('df_tab_orig2', df_tab_orig2, envir=.GlobalEnv)
  assign('df_tab_pred2', df_tab_pred2, envir=.GlobalEnv)
  
  df_tab_orig3 <- table(col3_original)
  df_tab_pred3 <- table(col3_preditado)
  assign('df_tab_orig3', df_tab_orig3, envir=.GlobalEnv)
  assign('df_tab_pred3', df_tab_pred3, envir=.GlobalEnv)
  
  df_tab_orig4 <- mean(col4_original, na.rm=TRUE)
  df_tab_pred4 <- mean(col4_preditado)
  assign('df_tab_orig4', df_tab_orig4, envir=.GlobalEnv)
  assign('df_tab_pred4', df_tab_pred4, envir=.GlobalEnv)
  
  # Faz uniao de data frames para manter estrutura anterior do conjunto de dados.
  df_pronto_imputado <- cbind(crime_ce_reservado, df_pronto_imputado)
  
  # cols_irrelevantes<- c("ID","AIS","MUNICIPIO_HOMICIDIO","DATA_HOMICIDIO","NOME_VITIMA","GUIA_CADAVERICA")
  # cols_relevantes<- c("NATUREZA_HOMICIDIO","ARMA_UTILIZADA","SEXO","IDADE")

  # Reordena colunas 
  df_pronto_imputado <- df_pronto_imputado[, c(1:3,7:8,4,5,6,9,10)]
  
  # Renomeia titulos das colunas
  names(df_pronto_imputado) <- nomes_colunas_df
  
  print("Realizando exportacao para arquivo CSV:")
  exporta_csv(df_pronto_imputado, paste0(nome_arquivo,"_Original_Limpo_Imputado",".csv"))
  
  # Retorna data frame imputado
  return(df_pronto_imputado)

}

##############################################################################################
## GRAFICO DE COMPARACAO
##############################################################################################

# Fucao que gera grafico pera verificacao da imputacao
gera_grafico_comparacao_imputacao <- function(df_limpo, df_imputado) {

  # Gráficos
  tableplot(df_limpo)
  tableplot(df_imputado)
  
  # # Um exemplo para tabelas
  # table(col1_original) %>%
  #   kable(col.names = c("Natureza do Homcídio","Freq")) %>%
  #   kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
  # 
  # 
  # t1 <- table(col1_original) %>%
  #   kable(col.names = c("Natureza do Homcídio","Freq")) %>%
  #   kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
  # 
  # 
  # t2 <- table(col2_preditado) %>%
  #   kable(col.names = c("Natureza do Homcídio","Freq")) %>%
  #   kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
  # 
  # 
  # t1 <- table(col1_original)
  # t2 <- table(col1_preditado)
  # knitr::kable(mat)  %>%
  #   kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
  
  
  # kable(
  #   list(
  #     t1,
  #     t2
  #   ),
  #   caption = 'A Tale of Two Tables.', booktabs = TRUE
  # ) %>%
  #   kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
  
}