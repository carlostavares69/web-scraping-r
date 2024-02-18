# -------------------------------------------------------------------------------------------#
# RASPAGEM DE DADOS (Data Scraping)
#
# Script:      Imputacao.R - Realiza imputação múltipla de dados faltantes (missing data).
# Autor:       Erivando Sena
# E-mail:      erivandosena@gmail.com
# Data:        14/12/2019
# Atualizado:  05/02/2019
##-------------------------------------------------------------------------------------------#

##############################################################################################
## PREPARACAO DOS DADOS
##############################################################################################

# Funcao que converte variavel data/hora categorica contínua para numerica discreta de segundos 
converte_datahora_para_segundos <- function(var_data_hora_padrao) {
  datahora_segundos <- (as.numeric(as.POSIXct(var_data_hora_padrao, format = "%H:%M:%OS"))*1000)/1000
  return(datahora_segundos)
}

# Funcao que converte variavel numerica discreta de segundos para categorica contínua data/hora
converte_segundos_para_datahora <- function(var_datahora_segundos) {
  data_hora_GMT3 <- as.POSIXct(var_datahora_segundos, origin="1970-01-01", tz="America/Fortaleza")
  return(strftime(as.POSIXct(data_hora_GMT3, format = "%d-%m-%Y  %H:%M:%OS"), format='%H:%M:%S'))
}

# Funcao que preparado o conjunto de dados para imputacao
prepara_dados_imputacao <- function(df_dados_limpos) {
  
  print("Preparando o conjunto de dados para imputacao.")
  
  # obtem os nomes das colunas
  nomes_colunas_df <- names(df_dados_limpos)
  
  # cria data variavel global
  assign('nomes_colunas_df', nomes_colunas_df, envir=.GlobalEnv)
  
  # Transformar os tipos das variáveis em fatores ou numéricos.
  df_dados_limpos <- df_dados_limpos %>%
    dplyr::mutate(
      ARMA_UTILIZADA = as.factor(ARMA_UTILIZADA),
      HORA_HOMICIDIO = as.factor(HORA_HOMICIDIO),
      SEXO = as.factor(SEXO),
      IDADE = as.numeric(IDADE)
    )
  
  # Obter apenas as colunas irrelevantes
  cols_irrelevantes<- c("ID","AIS","MUNICIPIO_HOMICIDIO","NATUREZA_HOMICIDIO","DATA_HOMICIDIO","NOME_VITIMA","GUIA_CADAVERICA")
  crime_ce_reservado <- dplyr::select(df_dados_limpos, cols_irrelevantes)
  
  # Obter apenas as colunas relevantes
  cols_relevantes <- c("ARMA_UTILIZADA","HORA_HOMICIDIO","SEXO","IDADE")
  df_dados_limpos <- dplyr::select(df_dados_limpos, cols_relevantes)
  
  # cria data frame global
  assign('cols_irrelevantes', cols_irrelevantes, envir=.GlobalEnv)
  assign('cols_relevantes', cols_relevantes, envir=.GlobalEnv)
  assign('crime_ce_reservado', crime_ce_reservado, envir=.GlobalEnv)
  
  # Duplicar data frame original para avaliar a precisão da imputação posteriormente.
  df_dados_limpos_original <- na.omit(df_dados_limpos)
  
  # cria data frame global
  assign('df_dados_limpos_original', df_dados_limpos_original, envir=.GlobalEnv)
  
  # Converte hora H:M:S para segundos numerico
  df_dados_limpos$HORA_HOMICIDIO <- converte_datahora_para_segundos(df_dados_limpos$HORA_HOMICIDIO)
  
  # Verificando os tipos de cada preditor
  tipos <- sapply(df_dados_limpos, class)
  print(tipos)
  
  # Verifica os dados para valores ausentes.
  missing <- sapply(df_dados_limpos, function(x) sum(is.na(x)))
  
  print("Quantitativo de dados missing nas variaveis:")
  print(missing)
  
  # Retorna em data frame pronto para imputacao
  return(df_dados_limpos)
}

##############################################################################################
## IMPUTACAO E PRECISAO
##############################################################################################

# Funcao que faz imputacao usando mice() por arvores de classificacao e regressao
realiza_imputacao_dados <- function(df_pronto) {
  
  print("Iniciando imputação multivariada com o algoritmo Mice.")
  
  #---- teste conversao hora
  
  #df_pronto <- df_dados_limpos #%>% dplyr::select(-MUNICIPIO_HOMICIDIO)
  
  #df_pronto_teste <- df_dados_limpos 
  
  # Sys.time()
  # print(as.numeric(Sys.time())*1000, digits=15)
  # 
  # # Imprimir milissegundos da hora atual
  # # Vejo? Strptime para detalhes, especificamente
  # # a opção de formatação% OSn, em que 0 <= n <= 6
  # as.numeric(format(Sys.time(), "%OS3")) * 1000
  # 
  # #Para obter o tempo atual da época (em segundo):
  # as.numeric(Sys.time())
  # 
  # 
  # # Fonte: https://masterr.org/r/how-to-convert-millisecond-to-date-in-r/
  # ms_to_date = function(ms, t0="1970-01-01", timezone) {
  #   ## @ms: um vetor numérico de milissegundos (números inteiros grandes de 13 dígitos)
  #   ## @t0: uma sequência do formato "aaaa-mm-dd", especificando a data que corresponde a 0 milissegundo
  #   ## @timezone: uma sequência que especifica um fuso horário que pode ser reconhecido por R
  #   ## return: um vetor POSIXct representando datas e horários do calendário       
  #   sec = ms / 1000
  #   as.POSIXct(sec, origin=t0, tz=timezone)
  # }
  # 
  # segundos_para_data = function(ms, t0="1970-01-01", timezone) {
  #   ## @ms: um vetor numérico de milissegundos (números inteiros grandes de 13 dígitos)
  #   ## @t0: uma sequência do formato "aaaa-mm-dd", especificando a data que corresponde a 0 milissegundo
  #   ## @timezone: uma sequência que especifica um fuso horário que pode ser reconhecido por R
  #   ## return: um vetor POSIXct representando datas e horários do calendário       
  #   sec = ms / 1000
  #   as.POSIXct(sec, origin="1970-01-01", tz="America/Fortaleza")
  # }
  # 
  # 
  # df_temp <- df_pronto
  # 
  # df_temp$datahora_segundos <- (as.numeric(as.POSIXct(df_temp$HORA_HOMICIDIO, format = "%H:%M:%OS"))*1000)/1000
  # 
  # df_temp$datahora_milisegundos <- as.numeric(as.POSIXct(df_temp$HORA_HOMICIDIO, format = "%H:%M:%OS"))*1000
  # df_temp$datahora_segundos <- as.POSIXct((datahora_milisegundos)/1000, origin = "1970-01-01")
  # df_temp$datahora_segundos <- as.POSIXct((datahora_milisegundos)/1000, origin = "1970-01-01")
  # df_temp$datahora_padrao <- ms_to_date(df_temp$datahora_milisegundos*1000, timezone="America/Fortaleza")
  # 
  # df_temp$datahora_padrao <- as.POSIXct(df_temp$datahora_segundos, origin="1970-01-01", tz="America/Fortaleza")
  # 
  # 
  # df_temp$hora_padrao <- strftime(as.POSIXct(df_temp$datahora_padrao, format = "%d-%m-%Y  %H:%M:%OS"), format='%H:%M:%S')
  #
  # converte_datahora_para_milisegundos <- function(var_data_hora_padrao) {
  #   return(as.numeric(as.POSIXct(var_data_hora_padrao, format = "%H:%M:%OS"))*1000)
  # }
  # 
  # converte_milisegundos_para_datahora <- function(var_datahora_milisegundos) {
  #   data_hora_GMT3 <- ms_to_date(var_datahora_milisegundos, timezone="America/Fortaleza")
  #   return(strftime(as.POSIXct(data_hora_GMT3, format = "%d-%m-%Y  %H:%M:%OS"), format='%H:%M:%S'))
  # }
  #
  # milisegundos <- converte_datahora_para_milisegundos(df_temp$HORA_HOMICIDIO) / 1000
  # converte_milisegundos_para_datahora(milisegundos)
  # 
  # segundos <- converte_datahora_para_segundos(df_temp$HORA_HOMICIDIO) 
  # converte_segundos_para_datahora(segundos)
  #df_pronto$HORA_IMPUTADA <- converte_segundos_para_datahora(df_pronto_imputado$HORA_HOMICIDIO)
  #---- teste conversao hora
  
  # Iniciando o pacote mice. 
  init <- mice(df_pronto, maxit=0) 
  #meth <- init$method # Utilziando eegressão linear padrao
  meth <- "cart" # Não precisa fazer inversão da matriz X
  predM <- init$predictorMatrix
  print(init) # Visualizar detalhes 
  
  # Remove variável da predicao na imputacao.
  #predM[, c("DATA_HOMICIDIO")] <- 0
  
  # Remove variavel da imputacao mantendo-se na predicao.
  #meth[c("MUNICIPIO_HOMICIDIO")]=""
  
  # Especificar os metodos para imputar os valores ausentes.
  # Existem métodos específicos para variáveis contínuas, binárias e ordinais.
  # Metodos padroes: norm, logreg, polyreg, polr, pmm.
  # meth[c("ARMA_UTILIZADA")] <- "polyreg" 
  # meth[c("HORA_HOMICIDIO")] <- "cart"
  # meth[c("IDADE")] <- "pmm" 
  # meth[c("SEXO")] <- "logreg" 
  
  # Executar a imputação múltipla (m=5).
  #set.seed(1234)
  #df_pronto_imputado <- mice(df_pronto, method=meth, predictorMatrix=predM, m=5, maxit = 10, MaxNWts=4000, seed = 2345, printFlag = TRUE)
  # Gerou erro abaixo:
  # Error in solve.default(xtx + diag(pen)) : 
  #   sistema é computacionalmente singular: condição recíproca número = 5.23382e-24
  
  # Como os métodos de imputação padrão envolvem regressão linear, isso resulta em uma matriz X que não pode ser invertida.
  # Uma solução é alterar o método de imputação padrão para um que não seja estocástico. 
  # Usando o "cart" (árvores de classificação e regressão) como método de imputação, R não precisa fazer inversão da matriz X
  # Fonte: https://academic.oup.com/aje/article/172/9/1070/148540
  # Outra fonte: https://www.kaggle.com/c/house-prices-advanced-regression-techniques/discussion/24586
  df_pronto_imputado <- mice(data = df_pronto, meth=meth , predictorMatrix=predM, m=5, maxit = 10, printFlag = TRUE)
  
  # #install.packages("parallel")
  # #library(parallel)
  # 
  # # obter o numero de processadores logicos do PC
  # nucleos <- parallel::detectCores(all.tests = FALSE, logical = TRUE)
  # 
  # # Acelerar tempo da imputação com processamento paralelo
  # df_pronto_imputado <- parlmice(data = df_pronto, method=meth, predictorMatrix=predM, m=5, nnet.MaxNWts = 5000, maxit = 30, aggregate_automatically = FALSE, n.core = nucleos-1, n.imp.core = 5, cl.type = "PSOCK")
  #df_pronto_imputado <- parlmice(data = df_pronto, method=meth, predictorMatrix=predM, m=5, maxit = 10)
  
  # Faz imputacao de NA em MUNICIPIO_HOMICIDIO pela mediana de todos os MUNICIPIO_HOMICIDIO
  #crime_ce_reservado <- crime_ce_reservado %>% mutate(MUNICIPIO_HOMICIDIO = ifelse(is.na(MUNICIPIO_HOMICIDIO), median(MUNICIPIO_HOMICIDIO, na.rm=TRUE), MUNICIPIO_HOMICIDIO))
  crime_ce_reservado <- crime_ce_reservado %>% mutate(MUNICIPIO_HOMICIDIO = ifelse(is.na(MUNICIPIO_HOMICIDIO), median(sample(MUNICIPIO_HOMICIDIO, nrow(.)-1), na.rm=TRUE), MUNICIPIO_HOMICIDIO))
  
  # # Testes
  # amostra <- c("Aquiraz","Aquiraz","Aquiraz","Ico", "Fortaleza","Camocim","Morada Nova")
  # amostra = sample(crime_ce_reservado$MUNICIPIO_HOMICIDIO, nrow(crime_ce_reservado)-3)
  # length(amostra)
  # median(amostra, na.rm=TRUE)
  
  # Cria um conjunto de dados apos imputacao.
  df_pronto_imputado <- complete(df_pronto_imputado)
  
  # Converte segundos numerico para hora H:M:S character
  df_pronto_imputado$HORA_HOMICIDIO <- converte_segundos_para_datahora(df_pronto_imputado$HORA_HOMICIDIO)
  
  # Verificar se existe missing no conjunto de dados imputado.
  status_missing <- sapply(df_pronto_imputado, function(x) sum(is.na(x)))
  
  print("Exibindo resultados da IMPUTACAO:")
  print(status_missing)
  
  # ARMA_UTILIZADA
  col2_original <- df_dados_limpos_original$ARMA_UTILIZADA[is.na(df_pronto$ARMA_UTILIZADA)]#;table(col2_original)
  col2_preditado  <- df_pronto_imputado$ARMA_UTILIZADA[is.na(df_pronto$ARMA_UTILIZADA)]#;table(col2_preditado)
  # table(col2_original)
  # table(col2_preditado)
  
  # HORA_HOMICIDIO
  col5_original <- df_dados_limpos_original$HORA_HOMICIDIO[is.na(df_pronto$HORA_HOMICIDIO)]#;table(col5_original)
  col5_preditado  <- df_pronto_imputado$HORA_HOMICIDIO[is.na(df_pronto$HORA_HOMICIDIO)]#;table(col5_preditado)
  # table(col5_original)
  # table(col5_preditado)
  
  # SEXO
  col3_original <- df_dados_limpos_original$SEXO[is.na(df_pronto$SEXO)]#;table(col3_original)
  col3_preditado <- df_pronto_imputado$SEXO[is.na(df_pronto$SEXO)]#;table(col3_preditado)
  # table(col3_original)
  # table(col3_preditado)
  
  # IDADE
  col4_original <- df_dados_limpos_original$IDADE[is.na(df_pronto$IDADE)]#;mean(col4_original, na.rm=TRUE) #mean(col4_original) 
  col4_preditado <- df_pronto_imputado$IDADE[is.na(df_pronto$IDADE)]#;mean(col4_preditado)
  # mean(col4_original, na.rm=TRUE)
  # mean(col4_preditado)
  
  # Cria data frame com acesso global
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
  
  df_tab_orig5 <- table(col5_original)
  df_tab_pred5 <- table(col5_preditado)
  assign('df_tab_orig5', df_tab_orig5, envir=.GlobalEnv)
  assign('df_tab_pred5', df_tab_pred5, envir=.GlobalEnv)
  
  # Faz uniao de data frames para resturar estrutura original do conjunto de dados.
  df_pronto_imputado <- cbind(crime_ce_reservado, df_pronto_imputado)
  
  # Reordena colunas para estrutura original do conjunto de dados.
  df_pronto_imputado <- df_pronto_imputado %>% dplyr::select(nomes_colunas_df)
  
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
}