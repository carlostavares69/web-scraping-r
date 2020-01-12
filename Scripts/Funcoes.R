# -------------------------------------------------------------------------------------------#
# RASPAGEM DE DADOS (Data Scraping)
#
# Script:      Funcoes.R - Declara todas a funcoes necessarias nas analises.
# Autor:       Erivando Sena
# E-mail:      erivandosena@gmail.com 
# Data:        20/08/2019
# Atualizado:  12/01/2020
##-------------------------------------------------------------------------------------------#

##############################################################################################
# COLETA                                      
##############################################################################################
# Funcao que extrai informacoes de ano do codigo html da pagina
extrai_lista_anos <- function(url_site) {
  pagina_html <- xml2::read_html(url_site)
  
  df_lista_urls <- pagina_html %>% #obtem codigo html da pagina
    rvest::html_nodes('.grid a.box') %>% # Filtro das tags "a" (link) desejadas
    purrr::map(xml2::xml_attrs) %>% # # Mapeia os nos html e cria lista com tags
    purrr::map_df(~as.list(.)) %>% # Converte o mapa de tags de lista em data frame 
    na.omit(.) %>% # exclui missings
    .[-c(nrow(.)),] # remove ultima linha
  
    # # Incluir ano 2019 antes do final do ano de 2018 (Linha de codigo - TEMPORÁRIO)
    # ####################################################################################################
    # ano_2019 <- matrix(data = c("box","https://www.sspds.ce.gov.br/estatisticas-2/"),ncol=2, byrow=TRUE)
    # colnames(ano_2019) <- c("class","href")
    # df_lista_urls <- rbind(ano_2019, df_lista_urls)
    # df_lista_urls$href <- as.character(df_lista_urls$href)
    # ####################################################################################################

  df_lista_titulos <- pagina_html %>% #obtem codigo html da pagina
    rvest::html_nodes('.grid p') %>% # Filtro das tags "h3" (nomes dos meses de cada link)desejadas
    rvest::html_text() %>% # Converte a relacao de nomes em texto
    #gsub(is.character, "", .) %>% # Substitui barras por travessao com espaco 
    gsub(" ", "", .) %>% # Substitui espaco com travessao por travessao sem espaco
    substr(., nchar(.)-3, nchar(.)) %>% # obtem apenas os anos
    .[-length(.)] %>% # remove ano 2013
    as.data.frame(.) %>% # Converte a lista de titulos em data frame
    setNames(., "ano") # O "." significa trazer o data frame anterior e e atribui o nome "ano" da coluna do dataframe

    # # Incluir ano 2019 antes do final do ano de 2018 (Linha de codigo - TEMPORÁRIO)
    # ####################################################################################################
    # ano_2019 <- matrix(data = c("2019"),ncol=1, byrow=TRUE)
    # colnames(ano_2019) <- c("ano")
    # df_lista_titulos <- rbind(ano_2019, df_lista_titulos)
    # ####################################################################################################
    
  # Juntar o data frame df_lista_urls e o data frame df_lista_titulos
  df_urls_anos <- data.frame(df_lista_urls, df_lista_titulos) 
  
  excluir <- c("class", "target") # Lista de colunas a serem excluidos"
  df_urls_anos <- df_urls_anos[,!(names(df_urls_anos) %in% excluir)] # Excluir as colunas "class" e/ou "target" do data frame
  df_urls_anos[,1][df_urls_anos[, 1] == "#"] <- NA # substituii cerquilha,"#", em missing

  return(df_urls_anos)
}

# Funcao que extrai informacoes de docs do codigo html da pagina
extrai_lista_documentos <- function(url_pagina) {
  pagina_html <- xml2::read_html(url_pagina)
  
  df_lista_urls <- pagina_html %>% #obtem codigo html da pagina
    rvest::html_nodes('.-Verde a.box') %>% # Filtro das tags "a" (link) desejadas
    purrr::map(xml2::xml_attrs) %>% # # Mapeia os nos html e cria lista com tags
    purrr::map_df(~as.list(.)) # Converte o mapa de tags de lista em data frame
  
  df_lista_titulos <- pagina_html %>% #obtem codigo html da pagina
    rvest::html_nodes('.-Verde h3') %>% # Filtro das tags "h3" (nomes dos meses de cada link)desejadas
    rvest::html_text() %>% # Converte a relacao de nomes em texto
    remove_acentos(.) %>% # Remove acentos
    gsub("–", " - ", .) %>% # Substitui travessao longo por ifen com espaco 
    gsub("/", " - ", .) %>% # Substitui barras por travessao com espaco 
    gsub(" ", "", .) %>% # Substitui espaco com travessao por travessao sem espaco
    as.data.frame(.) %>% # Converte a lista de titulos em data frame
    setNames(., "mes") # O "." significa trazer o data frame anterior e e atribui o nome "mes" da coluna do dataframe 
    
  
  # Juntar o data frame df_lista_urls e o data frame df_lista_titulos
  df_urls_nomes <- data.frame(df_lista_urls, df_lista_titulos) 
  
  excluir <- c("class", "target") # Lista de colunas a serem excluidos"
  df_urls_nomes <- df_urls_nomes[,!(names(df_urls_nomes) %in% excluir)] # Excluir as colunas "class" e "target" do data frame
  df_urls_nomes[,1][df_urls_nomes[, 1] == "#"] <- NA # substituii cerquilha,"#", em missing
  
  return(df_urls_nomes)
}

# Funcao que formata o nome dos arquivos
obtem_nome_arquivos <- function(dir_superior, nome_relacao) {
  nome_arquivo <- nome_relacao %>% gsub("[/. ,]","_", .)
  path <- file.path(".", dir_arquivos, dir_superior, paste0(nome_arquivo, ".pdf"))
  
  return(path)
}

# Funcao que baixa os arquivos da web para uma pasta local
download_documentos <- function(d_frame_lista) {
  print("Executa download dos documentos da pagina Web.")
  
  anos <- extrai_lista_anos(URL_site)
  for (titulo in d_frame_lista$mes) { # Em um laco percorre todas as linhas da coluna "nome" do data frame e cria variavel titulo 
    df_url <- select(filter(d_frame_lista, d_frame_lista$mes == titulo), "href") # seleciona e faz filtro da coluna mes onde mes for igual a variavel titulo & href
    df_nome <- select(filter(d_frame_lista, d_frame_lista$mes == titulo), "mes") # seleciona e faz filtro da coluna mes onde mes for igual a variavel titulo & mes
    
    for(ano in anos$ano) {
      
      if(ano == substr(df_nome$mes, nchar(as.character(df_nome$mes))-3, nchar(as.character(df_nome$mes)))) {
        
        diretorio <- file.path(".", dir_arquivos, ano)
        if(!dir.exists(diretorio)) {
          dir.create(diretorio, recursive = TRUE)
        } 
        
        nome_arquivo_completo <- obtem_nome_arquivos(ano, df_nome$mes) # Atribui o nome do arquivo pdf a ser baixado
        if(file.exists(nome_arquivo_completo)) { # Verificar se o arquivo existe
          print(paste0("Arquivo baixado: ", df_nome$mes)) # se ja existe printa o aviso, Arquivo ja baixado
        } else { # Ou
          if (!is.na(df_url$href)) { # Se nao existe faz o download do aqruivo pdf sem missing
            download.file(df_url$href, destfile = nome_arquivo_completo, mode = "wb", quiet=FALSE)
          } else { # OU
            print(paste("Arquivo indisponivel: ", df_nome$mes, sep = " ")) # se existir dados missing print Arquivo indisponivel
          }
        }
      
      }
    
    }
    
  }
}

# Funcao que obtem matrix de tabelas
extrai_tabela <- function(caminho_completo_arquivo, areas_tabela) {
  return(extract_tables(file = caminho_completo_arquivo, area = areas_tabela, guess = FALSE, encoding = "UTF-8"))
}

# Funcao que baixa lista de arquivos por ano 
obtem_arquivos <- function() {
  anos <- extrai_lista_anos(URL_site)

  for(indice in 1:nrow(anos)) {
    print(paste("Lendo codigo HTML da pagina Web:", anos[indice,1], "-", anos[indice,2], sep = " "))
    # Executa a funcao que obtem um data frame com urls e nomes dos documentos
    df_listas <- extrai_lista_documentos(anos[indice,1])
    Sys.sleep(1)
    # Executa a funcao que realiza o download do(s) documento(s)
    download_documentos(df_listas)
    Sys.sleep(10)
  }
}

# Funcao que convert data frame em arquivo CSV
exporta_csv <- function(df_limpo, nome_csv=NA) {
  # classes dos tipos de colunas
  classes_colunas <- sapply(df_limpo, class)
  
  # Salvar dados em arquivo CSV
  if(is.na(nome_csv)) {
    nome_arquivo_csv <- file.path(".", dir_dados, paste0(nome_csv, ".csv"))
  } else {
    nome_arquivo_csv <- file.path(".", dir_dados, paste0(nome_csv))
  }
  print(nome_arquivo_csv)
  
  # Escrever arquivo CSV
  readr::write_excel_csv2(x = df_limpo, path = nome_arquivo_csv, na = "NA", col_names = TRUE, delim = ";")
}

# Funcao que obtem dados do arquivo CSV
importa_csv <- function(status_classes, nome_csv, local_arquivo_csv=NA) {
  
  # status: original | merges | personalizado
  
  # Abrir dados em arquivo CSV
  if(is.na(local_arquivo_csv)) {
    nome_arquivo_csv <- base::file.path(".", dir_dados, nome_csv)
  } else {
    nome_arquivo_csv <- base::file.path(str_replace(paste(local_arquivo_csv,"/",nome_csv)," / ","/"))
  }
  print(nome_arquivo_csv)
  
  # classes dos tipos de colunas
  if(status_classes == "original") {
    classes_colunas <- cols(
      ID = col_number(), 
      AIS = col_character(), 
      MUNICIPIO_HOMICIDIO = col_character(), 
      NATUREZA_HOMICIDIO = col_character(), 
      ARMA_UTILIZADA = col_character(), 
      DATA_HOMICIDIO = col_character(), 
      NOME_VITIMA = col_character(), 
      GUIA_CADAVERICA = col_character(), 
      SEXO = col_character(), 
      IDADE = col_number()
    )
  } else if(status_classes == "merges") {
    classes_colunas <- cols(
      ID = col_number(), 
      AIS = col_character(), 
      MUNICIPIO_HOMICIDIO = col_character(), 
      NATUREZA_HOMICIDIO = col_character(), 
      ARMA_UTILIZADA = col_character(), 
      DATA_HOMICIDIO = col_character(), 
      NOME_VITIMA = col_character(), 
      GUIA_CADAVERICA = col_character(), 
      SEXO = col_character(), 
      IDADE = col_number(), 
      INCIDENCIA_HOMICIDIO = col_number(),
      MES_ANO = col_character(),
      FAIXA_ETARIA = col_character(),
      GRUPO_MUNICIPIO = col_character(),
      ARMA_DE_FOGO = col_character(),
      GRUPO_ARMA_UTILIZADA = col_character(),
      POPULACAO = col_number(),
      IDHM = col_number(),
      PIB_PERCAPITA = col_number(),
      LATITUDE = col_number(),
      LONGITUDE = col_number()
    )
  } else if(status_classes == "personalizado") {
    classes_colunas <- cols(
      ID = col_number(), 
      #AIS = col_character(), 
      MUNICIPIO_HOMICIDIO = col_character(), 
      NATUREZA_HOMICIDIO = col_character(), 
      ARMA_UTILIZADA = col_character(), 
      DATA_HOMICIDIO = col_character(), 
      #NOME_VITIMA = col_character(), 
      #GUIA_CADAVERICA = col_character(), 
      SEXO = col_character(), 
      IDADE = col_number(), 
      INCIDENCIA_HOMICIDIO = col_number(),
      MES_ANO = col_character(),
      FAIXA_ETARIA = col_character(),
      GRUPO_MUNICIPIO = col_character(),
      ARMA_DE_FOGO = col_character(),
      GRUPO_ARMA_UTILIZADA = col_character(),
      POPULACAO = col_number(),
      IDHM = col_number(),
      PIB_PERCAPITA = col_number(),
      LATITUDE = col_number(),
      LONGITUDE = col_number()
    )
  }
  
  # Importanto CSV
  dados_csv <- readr::read_delim(file = nome_arquivo_csv, 
                                delim = ";",
                                na = "NA",
                                col_names = TRUE,
                                col_types = classes_colunas,
                                #locale = locale(date_names = "pt", encoding = "UTF-8", decimal_mark = ".", date_format = "%d/%m/%Y"),
                                locale = locale(date_names = "pt", encoding = "UTF-8", decimal_mark = ",", grouping_mark = ".", date_format = "%d/%m/%Y"),
                                progress = show_progress())
  return(dados_csv)
}

##############################################################################################
# LIMPEZA                                      
##############################################################################################
# Funcao para remover acentos
remove_acentos <- function(obj_str) {
  if(!is.character(obj_str)) {
    obj_str <- as.character(obj_str)
  }
  obj_str <- stringi::stri_trans_general(str = obj_str, "latin-ascii")
  
  return(obj_str)
}

# Funcao para padronizar o formato dos dados
padroniza_dados <- function(df_dados) {
  
  # Renomeia colunas
  df_dados <- df_dados %>% 
    rename(MUNICIPIO_HOMICIDIO = MUNICIPIO) %>% 
    rename(NATUREZA_HOMICIDIO = NATUREZA_DO_FATO) %>% 
    rename(DATA_HOMICIDIO = DATA_MORTE) 
  
  # Converte todas as variaveis factor em character
  vars_factor <- lapply(df_dados, class) == "factor"
  df_dados[, vars_factor] <- lapply(df_dados[, vars_factor], as.character) 
  
  # Remove espacos em branco na esquerda/direita das variaveis/remove acentos
  df_dados <- df_dados %>% 
    mutate(MUNICIPIO_HOMICIDIO =  remove_acentos(str_trim(MUNICIPIO_HOMICIDIO)), 
           NATUREZA_HOMICIDIO = remove_acentos(str_trim(NATUREZA_HOMICIDIO)), 
           ARMA_UTILIZADA =  remove_acentos(str_trim(ARMA_UTILIZADA)), 
           DATA_HOMICIDIO = str_trim(DATA_HOMICIDIO), 
           NOME_VITIMA =  remove_acentos(str_trim(NOME_VITIMA)), 
           GUIA_CADAVERICA = str_trim(GUIA_CADAVERICA), 
           SEXO = remove_acentos(str_trim(SEXO)), 
           IDADE = str_trim(IDADE))
  
  # Converte caracteres da variaveis para formato titulo
  df_dados <- df_dados %>% mutate_if(is.character, str_to_title)
  
  # Atribui NA para variaveis com valores desconhecidos
  df_dados <- df_dados %>%
    mutate(ARMA_UTILIZADA = replace(ARMA_UTILIZADA, ARMA_UTILIZADA == "Nao Informado", NA)) %>%
    mutate(SEXO = replace(SEXO, SEXO == "Nao Identificado", NA)) 
  
  # Atribui ID classificado por DATA
  df_dados <- df_dados %>% 
    arrange(as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>% 
    mutate(ID = c(1:nrow(.))) # Atualiza ID
    
  return(df_dados)
}

# Funcao que obtem linhas da matrix com valores quebrados
corrige_variavel <- function(num_col, tit_coluna, tb_matrix) {
  
  #assign('linhas', linhas, envir=.GlobalEnv)
  linhas <-c()
  conta_linha <- 0
  for (nlinha in 1:nrow(tb_matrix)) {
    conta_linha <- conta_linha +1
    if(nlinha > 1) {
      if(nlinha < nrow(tb_matrix)) {
        if(!is.na(tb_matrix[nlinha+1, num_col])) {
          if(tb_matrix[nlinha+1, num_col] != "") {
            if(tb_matrix[nlinha, num_col] == "" & !(tit_coluna %in% tb_matrix[nlinha-1, num_col]) & tb_matrix[nlinha, 1] != "") {
              linhas <- c(linhas, conta_linha)
            }
          }
        }
      }
    }
  }
  
  if(!is.null(linhas)) {
    print(paste("Linhas com valores quebrados da coluna:", num_col, sep = " "))
  }
  
  return(linhas)
}

# Funcao que corrige variaveis fragmentadas na matriz
concatena_variaveis_matriz <- function(m_tabela) {
  # concatena valores fragmentados da coluna coluna 2
  coluna <- c(2)
  linhas <- corrige_variavel(coluna, c("AIS"), m_tabela)
  if(!is.null(linhas)) {
    for (num in 1:length(linhas)) {
      var_correcao <- paste(m_tabela[linhas[num]-1, coluna], m_tabela[linhas[num]+1, coluna], sep = " ")
      print(paste("Linha:", linhas[num], "Corrigindo variavel para:", var_correcao))
      m_tabela[linhas[num], coluna] <- var_correcao
    }
  }
  
  # concatena valores fragmentados da coluna coluna 3
  coluna <- c(3)
  linhas <- corrige_variavel(coluna, c("MUNICÍPIO"), m_tabela)
  if(!is.null(linhas)) {
    for (num in 1:length(linhas)) {
      var_correcao <- paste(m_tabela[linhas[num]-1, coluna], m_tabela[linhas[num]+1, coluna], sep = " ")
      print(paste("Linha:", linhas[num], "Corrigindo variavel para:", var_correcao))
      m_tabela[linhas[num], coluna] <- var_correcao
    }
  }
  
  # concatena valores fragmentados da coluna coluna 4
  coluna <- c(4)
  linhas <- corrige_variavel(coluna, c("NATUREZA DO FATO"), m_tabela)
  if(!is.null(linhas)) {
    for (num in 1:length(linhas)) {
      var_correcao <- paste(m_tabela[linhas[num]-1, coluna], m_tabela[linhas[num]+1, coluna], sep = " ")
      print(paste("Linha:", linhas[num], "Corrigindo variavel para:", var_correcao))
      m_tabela[linhas[num], coluna] <- var_correcao
    }
  }
  
  # concatena valores fragmentados da coluna coluna 7
  coluna <- c(7)
  linhas <- corrige_variavel(coluna, c("NOME DA VÍTIMA"), m_tabela)
  if(!is.null(linhas)) {
    for (num in 1:length(linhas)) {
      var_correcao <- paste(m_tabela[linhas[num]-1, coluna], m_tabela[linhas[num]+1, coluna], sep = " ")
      print(paste("Linha:", linhas[num], "Corrigindo variavel para:", var_correcao))
      m_tabela[linhas[num], coluna] <- var_correcao
    }
  }
  
  return(m_tabela)
}

# Funcao que formata variaveis da matriz de tabelas
corrige_variaveis_matriz <- function(m_tabela) {
  for (linha in rev(1:nrow(m_tabela))) {
    # Substitui hifen ou vazio por NA nas linhas da coluna 3
    if (!is.na(m_tabela[linha,3])) {
      if(m_tabela[linha,3] == "-" | nchar(m_tabela[linha,3]) == 0) { m_tabela[linha,3] <- NA }
    }
    # Substitui hifen ou vazio por NA nas linhas da coluna 5
    if (!is.na(m_tabela[linha,5])) {
      if(m_tabela[linha,5] == "-" | nchar(m_tabela[linha,5]) == 0) { m_tabela[linha,5] <- NA }
    }
    # Substitui hifen ou hifen/barra ou vazio por NA nas linhas da coluna 8
    if (!is.na(m_tabela[linha,8])) {
      if(m_tabela[linha,8] == "-" | m_tabela[linha,8] == "-/" | nchar(m_tabela[linha,8]) == 0) { m_tabela[linha,8] <- NA }
    }
    # Substitui M por Masculino nas linhas da coluna 9
    if (!is.na(m_tabela[linha,9])) {
      if (m_tabela[linha,9] == "M") { m_tabela[linha,9] <- "Masculino" } 
    }
    # Substitui F por Feminino nas linhas da coluna 9
    if (!is.na(m_tabela[linha,9])) {
      if(m_tabela[linha,9] == "F") { m_tabela[linha,9] <- "Feminino" }
    }
    # Substitui hifen ou vazio por NA nas linhas da coluna 10
    if (!is.na(m_tabela[linha,10])) {
      if(m_tabela[linha,10] == "-" | nchar(m_tabela[linha,10]) == 0) { m_tabela[linha,10] <- NA }
    }
  }
  
  return(m_tabela)
}

# Funcao para excluir linhas vazias da matriz de tabelas
exclui_linhas_matriz <- function(m_tabela) {
  # exclui linhas desnecessarias 
  for (linha in rev(1:nrow(m_tabela))) {
    if(!is.na(m_tabela[linha,1])) {
      if(nchar(m_tabela[linha,1]) == 0 | toupper(m_tabela[linha,1]) == "ID") {
        m_tabela <- m_tabela[-c(linha),]
      }
    }
  }
  
  return(m_tabela)
}

# Funcao que padroniza valores inconsistentes de colunas
padroniza_colunas_inconsistentes <- function(df_tabela) {
  
  # Padroniza valores da coluna 5, 9 e 10  
  tipo1 <- c("ARMA DE FOGO E ARMA")
  tipo2 <- c("ARMA  DE FOGO","ARAMA DE FOGO","ARMADE FOGO","ARM DE FOGO","ARMA D FOGO","ARM ADE FOGO","ARMA FOGO", "Arma de fogo")
  tipo3 <- c("ARAMA BRANCA","ARMA DE BRANCA","ARMA DE FACA","ARAMA DE BRANCA", "Arma branca")
  tipo4 <- c("Outros meios","EOUTROS","OUTRO","ARMA OUTROS","Outro","OUTRO TIPO","AMA OUTROS")
  tipo5 <- c("Meio não informado","NÃO INFORMAD","NI")
  tipo6 <- c("NÃO IDENTIFICAD-O","NI", "I", "-")
  tipo7 <- c("-","")
  tipo8 <- c("M")
  tipo9 <- c("F")
  tipo10 <- c("","-")
  tipo11 <- c("LATROCINIO","LATROCÍNIO")
  
  for (n_linha in 1:nrow(df_tabela)) {
    # Padroniza valores da coluna 4
    if(!is.na(df_tabela[n_linha,4])) {
      if(df_tabela[n_linha,4] %in% tipo11) {df_tabela[n_linha,4] <- c("ROUBO SEGUIDO DE MORTE (LATROCINIO)")}
    }
    # Padroniza valores da coluna 5
    if(!is.na(df_tabela[n_linha,5])) {
      if(df_tabela[n_linha,5] %in% tipo1) {
        df_tabela[n_linha,5] <- c("ARMA DE FOGO E BRANCA")
      }
      if(df_tabela[n_linha,5] %in% tipo2) {
        df_tabela[n_linha,5] <- c("ARMA DE FOGO")
      }
      if(df_tabela[n_linha,5] %in% tipo3) {
        df_tabela[n_linha,5] <- c("ARMA BRANCA")
      }
      if(df_tabela[n_linha,5] %in% tipo4) {
        df_tabela[n_linha,5] <- c("OUTROS")
      }
      if(df_tabela[n_linha,5] %in% tipo5) {
        df_tabela[n_linha,5] <- c("NÃO INFORMADO")
      }
    }
    # Padroniza valores da coluna 9
    if(!is.na(df_tabela[n_linha,9])) {
      if(df_tabela[n_linha,9] %in% tipo6) {
        df_tabela[n_linha,9] <- c("Não identificado")
      }
      if(df_tabela[n_linha,9] %in% tipo7) {
        df_tabela[n_linha,9] <- NA
      }
      if(df_tabela[n_linha,9] %in% tipo8) {
        df_tabela[n_linha,9] <- c("MASCULINO")
      }
      if(df_tabela[n_linha,9] %in% tipo9) {
        df_tabela[n_linha,9] <- c("FEMININO")
      }
    }
    
    # Padroniza valores da coluna 10
    if(!is.na(df_tabela[n_linha,10])) {
      if(df_tabela[n_linha,10] %in% tipo10) { df_tabela[n_linha,10] <- NA }
    }
  } 

  return(df_tabela)
}

# Funcao para criar ID
cria_id <- function(df_dados) {
  # Recria ID
  df_dados <- df_dados %>% 
    cbind(., ID = c(1:nrow(.))) %>% 
    .[,-c(1)] %>% .[,c(10, 1:9)]
  
  return(df_dados)
}

# Funcao que inclui cabecalho com nomes das colunas
adiciona_cabecalho <- function(df_dados) {
  # Atribui cabecalho ao data frame
  m_tabela_cabecalho <- c("ID","AIS","MUNICIPIO","NATUREZA_DO_FATO","ARMA_UTILIZADA","DATA_MORTE","NOME_VITIMA","GUIA_CADAVERICA","SEXO","IDADE")
  names(df_dados) <- m_tabela_cabecalho
  
  return(df_dados)
}

# Funcao para limpeza dos dados
realiza_limpeza_dados <- function() {
  
  # Data frames necessarios
  lista_anos <- extrai_lista_anos(URL_site)
  vetor_anos <- lista_anos$ano %>% as.vector(.) %>% as.numeric(.)
  
  
  # Obtem data frames por ano com dados limpos
  if(2019 %in% vetor_anos) {
    print(paste("Limpando dados de", lista_anos[1, 2], sep = " "))
    df_lista_meses <- extrai_lista_documentos(lista_anos[1, 1])
    dados_homicidio_ce_2019 <- limpa_dados_2019(lista_anos[1, 2], df_lista_meses)  
  }
  if(2018 %in% vetor_anos) {
    print(paste("Limpando dados de", lista_anos[2, 2], sep = " "))
    df_lista_meses <- extrai_lista_documentos(lista_anos[2, 1])
    dados_homicidio_ce_2018 <- limpa_dados_2018(lista_anos[2, 2], df_lista_meses)  
  }
  if(2017 %in% vetor_anos) {
    print(paste("Limpando dados de", lista_anos[3, 2], sep = " "))
    df_lista_meses <- extrai_lista_documentos(lista_anos[3, 1])
    dados_homicidio_ce_2017 <- limpa_dados_2017(lista_anos[3, 2], df_lista_meses) 
  }
  if(2016 %in% vetor_anos) {
    print(paste("Limpando dados de", lista_anos[4, 2], sep = " "))
    df_lista_meses <- extrai_lista_documentos(lista_anos[4, 1])
    dados_homicidio_ce_2016 <- limpa_dados_2016(lista_anos[4, 2], df_lista_meses) 
  }
  if(2015 %in% vetor_anos) {
    print(paste("Limpando dados de", lista_anos[5, 2], sep = " "))
    df_lista_meses <- extrai_lista_documentos(lista_anos[5, 1])
    dados_homicidio_ce_2015 <- limpa_dados_2015(lista_anos[5, 2], df_lista_meses) 
  }
  if(2014 %in% vetor_anos) {
    print(paste("Limpando dados de", lista_anos[6, 2], sep = " "))
    df_lista_meses <- extrai_lista_documentos(lista_anos[6, 1])
    dados_homicidio_ce_2014 <- limpa_dados_2014(lista_anos[6, 2], df_lista_meses) 
  }
  
  # Merge dos data frames, inclusao de cabecalho e reconstrucao da variavel ID
  dados_homicidio_ce_merge_anos <- rbind(dados_homicidio_ce_2019,
                                         dados_homicidio_ce_2018, 
                                         dados_homicidio_ce_2017, 
                                         dados_homicidio_ce_2016, 
                                         dados_homicidio_ce_2015,
                                         dados_homicidio_ce_2014) %>% adiciona_cabecalho(.)
  
  # # Cria variaveis globais dos data frames
  # assign('dados_homicidio_ce_2019', adiciona_cabecalho( dados_homicidio_ce_2019 ), envir=.GlobalEnv)
  # assign('dados_homicidio_ce_2018', adiciona_cabecalho( dados_homicidio_ce_2018 ), envir=.GlobalEnv)
  # assign('dados_homicidio_ce_2017', adiciona_cabecalho( dados_homicidio_ce_2017 ), envir=.GlobalEnv)
  # assign('dados_homicidio_ce_2016', adiciona_cabecalho( dados_homicidio_ce_2016 ), envir=.GlobalEnv)
  # assign('dados_homicidio_ce_2015', adiciona_cabecalho( dados_homicidio_ce_2015 ), envir=.GlobalEnv)
  # assign('dados_homicidio_ce_2014', adiciona_cabecalho( dados_homicidio_ce_2014 ), envir=.GlobalEnv)
  
  return(dados_homicidio_ce_merge_anos)
}

##############################################################################################
# TRATAMENTO
##############################################################################################
# Funcao que compara diferencas entre duas colunas e retona uma lista
verifica_inconsistencias <- function(df_col1, df_col2) {
  # verifica inconsistencia entre as colunas de merge
  inconsistencias <- lubridate::setdiff(df_col1, df_col2)
  ifelse(length(inconsistencias) > 0, print(paste("Inconsistencia tratada:", inconsistencias, sep = " ")), print("Sem inconsistencias"))
  return(inconsistencias)
}

# Funcao para geocodificar conjunto de dados
merge_dados_geo <- function(df_dados) {
  # Importando o arquivo Shapefile (Fonte IBGE) com informacoes geograficas e coordenadas GPS
  if(!exists("dados_shp")) {
    dados_shp <- rgdal::readOGR(dsn = file.path(".", dir_auxiliares, "Shapefile_SHP/BR_Localidades_2010_v1.shp"), layer = "BR_Localidades_2010_v1", verbose = FALSE)
    # Cria variavel global
    assign('dados_shp', dados_shp, envir=.GlobalEnv)
  }

  # Filtro por municipios do Ceara
  df_geo <- dados_shp[which(as.numeric(dados_shp$CD_NIVEL) == 1 & as.character(dados_shp$NM_UF) == "CEARÁ"),] %>% 
    as.data.frame(.) %>% 
    select(., LAT, LONG, NM_MUNICIP) %>%
    rename(LATITUDE = LAT) %>%
    rename(LONGITUDE = LONG) %>% 
    mutate(LATITUDE = format(LATITUDE, trim = TRUE, digits = 7), LONGITUDE = format(LONGITUDE, trim = TRUE, digits = 7), NM_MUNICIP = as.character(NM_MUNICIP)) %>% 
    mutate(NM_MUNICIP = remove_acentos(toupper(NM_MUNICIP)))

  # Converte variavel para maiusculo e remove acentuacao
  df_dados <- df_dados %>% mutate(MUNICIPIO_HOMICIDIO = toupper(MUNICIPIO_HOMICIDIO))
  
  # Verifica inconsistencia entre as colunas de merge
  inconsistencias <- verifica_inconsistencias(df_dados$MUNICIPIO_HOMICIDIO, df_geo$NM_MUNICIP)

  # Substitui valores das variaveis 
  df_dados <- df_dados %>%
    mutate(MUNICIPIO_HOMICIDIO = replace(MUNICIPIO_HOMICIDIO, MUNICIPIO_HOMICIDIO == "ITAPAJE", "ITAPAGE")) %>%
    mutate(MUNICIPIO_HOMICIDIO = replace(MUNICIPIO_HOMICIDIO, MUNICIPIO_HOMICIDIO == "NOVA JAGUARIBARA", "JAGUARIBARA")) %>%
    mutate(MUNICIPIO_HOMICIDIO = replace(MUNICIPIO_HOMICIDIO, MUNICIPIO_HOMICIDIO == "DEP. IRAPUAN PINHEIRO", "DEPUTADO IRAPUAN PINHEIRO")) 
  
  # Merge dos data frames com geolocalizacao por municipio
  df_merge <- merge(df_dados, df_geo, by.x=c('MUNICIPIO_HOMICIDIO'), by.y=c('NM_MUNICIP'))
  
  # Restaura formato da coluna MUNICIPIO_HOMICIDIO
  df_merge <- df_merge %>% mutate(MUNICIPIO_HOMICIDIO = str_to_title(MUNICIPIO_HOMICIDIO)) %>% 
    arrange(ID) %>%
    # Reordena coluna MUNICIPIO_HOMICIDIO 
    .[,c(2,3,1,4:ncol(.))]
  
  # retorna dadta frame
  return(df_merge) 
}

# Funcao para merge grupo municipios
merge_grupo_municipios <- function(df_dados) {
  # 19 Municipios da Região Metropolitana de Fortaleza
  municipios_RMF <- c("Aquiraz","Cascavel","Caucaia","Chorozinho","Eusebio","Fortaleza","Guaiuba",
                      "Horizonte","Itaitinga","Maracanau","Maranguape","Pacajus","Pacatuba","Pindoretama",
                      "Sao Goncalo Do Amarante","Sao Luis Do Curu","Paraipaba","Paracuru","Trairi")
  
  # Nega o TRUE de %in%
  `%notin%` <- Negate(`%in%`) 
  
  # Cria nova variavel com grupo de municipios
  df_dados <- df_dados %>%
    mutate('GRUPO_MUNICIPIO' = case_when(MUNICIPIO_HOMICIDIO == 'Fortaleza' ~ "Capital", 
                                         MUNICIPIO_HOMICIDIO %in% municipios_RMF ~ "Regiao Metropolitana", 
                                         MUNICIPIO_HOMICIDIO %notin% municipios_RMF ~ "Interior"))
  return(df_dados)
}

# Funcao para merge grupo idades
merge_grupo_idades <- function(df_dados){
  # Cria nova variavel para grupo de idades.
  df_dados <- df_dados %>%
    mutate(
      #Idade = ifelse(is.na(IDADE), mean(full$IDADE, na.rm=TRUE), IDADE), # Media das idades (Substitui idade NA pela media das idades)
      'FAIXA_ETARIA' = case_when(IDADE < 13 ~ "0 a 12 anos", 
                                 IDADE >= 13 & IDADE < 18 ~ "13 a 17 anos", 
                                 IDADE >= 18 & IDADE < 60 ~ "18 a 59 anos", 
                                 IDADE >= 60 ~ "60 anos a cima"))
  return(df_dados)
}

# Funcao para merge do tipo de arma de fogo
merge_grupo_arma_fogo <- function(df_dados) {
  # Cria nova variavel com teste de Bernoulli para o tipo de Arma De Fogo
  df_dados <- df_dados %>% 
    mutate(ARMA_DE_FOGO = case_when(ARMA_UTILIZADA == "Arma De Fogo" ~ "Sim", ARMA_UTILIZADA != "Arma De Fogo" ~ "Nao")) 
    # %>% arrange(desc(ARMA_DE_FOGO))
  return(df_dados)
}

# Funcao para merge grupo natureza do homicidio
merge_grupo_natureza_homicidio <- function(df_dados){
  df_dados <- df_dados %>%
    # Cria nova variavel para grupo de natureza homicidio.
    mutate('GRUPO_NATUREZA_HOMICIDIO' = case_when(
      NATUREZA_HOMICIDIO == "Homicidio Doloso" | NATUREZA_HOMICIDIO == "Roubo Seguido De Morte (Latrocinio)" | NATUREZA_HOMICIDIO == "Lesao Corporal Seguida De Morte" | NATUREZA_HOMICIDIO == "Morte Suspeita" ~ "Homicidio", 
      NATUREZA_HOMICIDIO == "Feminicidio" ~ "Feminicidio")
    )
  return(df_dados)
}

# # Funcao para merge do tipo de arma utilizada
# merge_grupo_arma_utilizada <- function(df_dados) {
#   # Cria nova variavel para grupo de arma utilizada
#   df_dados <- df_dados %>%
#     mutate('GRUPO_ARMA_UTILIZADA' = case_when(ARMA_UTILIZADA == "Arma De Fogo" ~ "Arma de Fogo", ARMA_UTILIZADA != "Arma De Fogo" ~ "Outros"))
# 
#   return(df_dados)
# }
  
# Funcao para criar variavel mes/ano
merge_variavel_mesano <- function(df_dados) {
  
  # Cria variavel mes/ano
  df_var_mesano <- df_dados %>% 
  mutate(MES_ANO = format.Date(as.Date(DATA_HOMICIDIO, format = "%d/%m/%Y"), "%m/%Y")) 
  
  return(df_var_mesano)
}

# Funcao para merge de coluna de indice populacional 
merge_dados_populacao <- function(df_dados) {
  censo_pop <- suppressMessages(readxl::read_xls(
    file.path(".", dir_auxiliares, "Censo_Demografico_2010/total_populacao_ceara.xls"), col_names = FALSE))[2:185,][,-c(1,3,4,5,6,7)] %>% 
    rename(MUNICIPIO = "...2", POPULACAO = "...8") %>% 
    mutate(POPULACAO = as.numeric(POPULACAO)) %>% 
    mutate(MUNICIPIO = remove_acentos(toupper(MUNICIPIO)))
  
  # Converte variavel para maiusculo
  df_dados <- df_dados %>% mutate(MUNICIPIO_HOMICIDIO = toupper(MUNICIPIO_HOMICIDIO))
  
  # Verifica inconsistencia entre as colunas de merge
  inconsistencias <- verifica_inconsistencias(df_dados$MUNICIPIO_HOMICIDIO, censo_pop$MUNICIPIO)
  
  # Substitui valores das variaveis 
  df_dados <- df_dados %>%
    mutate(MUNICIPIO_HOMICIDIO = replace(MUNICIPIO_HOMICIDIO, MUNICIPIO_HOMICIDIO == "ITAPAJE", "ITAPAGE")) %>%
    mutate(MUNICIPIO_HOMICIDIO = replace(MUNICIPIO_HOMICIDIO, MUNICIPIO_HOMICIDIO == "NOVA JAGUARIBARA", "JAGUARIBARA")) %>%
    mutate(MUNICIPIO_HOMICIDIO = replace(MUNICIPIO_HOMICIDIO, MUNICIPIO_HOMICIDIO == "DEP. IRAPUAN PINHEIRO", "DEPUTADO IRAPUAN PINHEIRO")) 
  
  # Merge dos data frames com populacao por municipio
  df_merge <- merge(df_dados, censo_pop, by.x=c('MUNICIPIO_HOMICIDIO'), by.y=c('MUNICIPIO'))
  
  # Restaura formato da coluna MUNICIPIO_HOMICIDIO
  df_merge <- df_merge %>% mutate(MUNICIPIO_HOMICIDIO = str_to_title(MUNICIPIO_HOMICIDIO)) %>% 
    arrange(ID) %>%
    # Reordena coluna MUNICIPIO_HOMICIDIO 
    .[,c(2,3,1,4:ncol(.))]
  
  return(df_merge)
}

# Funcao para merge de coluna do IDH
merge_dados_idhm <- function(df_dados) {
  idhm <- suppressMessages(readxl::read_xlsx(
    file.path(".", dir_auxiliares, "Atlas_Desenvolvimento_Humano_Brasil_2010/AtlasBrasil_Consulta.xlsx"), col_names = FALSE))[3:186,][,c(2:3)] %>% 
    rename(MUNICIPIO = "...2", IDHM = "...3") %>% 
    mutate(IDHM = as.numeric(IDHM)) %>% 
    mutate(MUNICIPIO = remove_acentos(toupper(MUNICIPIO)))
  
  # Converte variavel para maiusculo
  df_dados <- df_dados %>% mutate(MUNICIPIO_HOMICIDIO = toupper(MUNICIPIO_HOMICIDIO))
  
  # Verifica inconsistencia entre as colunas de merge
  inconsistencias <- verifica_inconsistencias(df_dados$MUNICIPIO_HOMICIDIO, idhm$MUNICIPIO)
  
  # Substitui valores das variaveis 
  df_dados <- df_dados %>%
    mutate(MUNICIPIO_HOMICIDIO = replace(MUNICIPIO_HOMICIDIO, MUNICIPIO_HOMICIDIO == "ITAPAJE", "ITAPAGE")) %>%
    mutate(MUNICIPIO_HOMICIDIO = replace(MUNICIPIO_HOMICIDIO, MUNICIPIO_HOMICIDIO == "NOVA JAGUARIBARA", "JAGUARIBARA")) %>%
    mutate(MUNICIPIO_HOMICIDIO = replace(MUNICIPIO_HOMICIDIO, MUNICIPIO_HOMICIDIO == "DEP. IRAPUAN PINHEIRO", "DEPUTADO IRAPUAN PINHEIRO")) 
  
  # Merge dos data frames com IDH por municipio
  df_merge <- merge(df_dados, idhm, by.x=c('MUNICIPIO_HOMICIDIO'), by.y=c('MUNICIPIO'))
  
  # Restaura formato da coluna MUNICIPIO_HOMICIDIO
  df_merge <- df_merge %>% mutate(MUNICIPIO_HOMICIDIO = str_to_title(MUNICIPIO_HOMICIDIO)) %>% 
    arrange(ID) %>%
    # Reordena coluna MUNICIPIO_HOMICIDIO 
    .[,c(2,3,1,4:ncol(.))]
  
  return(df_merge)
}

# Funcao para merge de coluna do PIB
merge_dados_pib <- function(df_dados) {
  pib_percapita <- suppressMessages(readxl::read_xls(
    file.path(".", dir_auxiliares, "Pib_Municipios_2010/PibMunicipal2006_2010.xls"), col_names = FALSE))[913:1096,][,-c(2:6)] %>% 
    rename(MUNICIPIO = "...1", PIB_PERCAPITA = "...7") %>% 
    mutate(PIB_PERCAPITA = as.numeric(PIB_PERCAPITA)) %>% 
    mutate(MUNICIPIO = remove_acentos(toupper(MUNICIPIO)))

  # Converte variavel para maiusculo
  df_dados <- df_dados %>% mutate(MUNICIPIO_HOMICIDIO = toupper(MUNICIPIO_HOMICIDIO))
  
  # Verifica inconsistencia entre as colunas de merge
  inconsistencias <- verifica_inconsistencias(df_dados$MUNICIPIO_HOMICIDIO, pib_percapita$MUNICIPIO)
  
  # Substitui valores das variaveis 
  df_dados <- df_dados %>%
    mutate(MUNICIPIO_HOMICIDIO = replace(MUNICIPIO_HOMICIDIO, MUNICIPIO_HOMICIDIO == "ITAPAJE", "ITAPAGE")) %>%
    mutate(MUNICIPIO_HOMICIDIO = replace(MUNICIPIO_HOMICIDIO, MUNICIPIO_HOMICIDIO == "NOVA JAGUARIBARA", "JAGUARIBARA")) %>%
    mutate(MUNICIPIO_HOMICIDIO = replace(MUNICIPIO_HOMICIDIO, MUNICIPIO_HOMICIDIO == "DEP. IRAPUAN PINHEIRO", "DEPUTADO IRAPUAN PINHEIRO")) 
  
  # Merge dos data frames com PIB por municipio
  df_merge <- merge(df_dados, pib_percapita, by.x=c('MUNICIPIO_HOMICIDIO'), by.y=c('MUNICIPIO'))
  
  # Restaura formato da coluna MUNICIPIO_HOMICIDIO
  df_merge <- df_merge %>% mutate(MUNICIPIO_HOMICIDIO = str_to_title(MUNICIPIO_HOMICIDIO)) %>% 
    arrange(ID) %>%
    # Reordena coluna MUNICIPIO_HOMICIDIO 
    .[,c(2,3,1,4:ncol(.))]
  
  return(df_merge)
}

# Funcao para criar variavel incidencia de homicidio
merge_variavel_incidencia_homicidio <- function(df_dados) {

  # Converte para maiusculo
  df_dados <- df_dados %>% mutate(MUNICIPIO_HOMICIDIO = toupper(MUNICIPIO_HOMICIDIO))
  
  # obtem o numero de incidencia
  df_incidencia <- df_dados %>%
    group_by(MUNICIPIO_HOMICIDIO) %>%
    summarise(INCIDENCIA_HOMICIDIO = n()) %>%
    distinct()
  
  df_merge <- merge(df_dados, df_incidencia, by.x=c('MUNICIPIO_HOMICIDIO'), by.y=c('MUNICIPIO_HOMICIDIO'))
  
  # Restaura formato da coluna MUNICIPIO_HOMICIDIO e restaura formato da variavel
  df_merge <- df_merge %>% mutate(MUNICIPIO_HOMICIDIO = str_to_title(MUNICIPIO_HOMICIDIO)) %>% 
    arrange(ID) %>%
    # Reordena coluna MUNICIPIO_HOMICIDIO 
    .[,c(2,3,1,4:ncol(.))] 
  
  return(df_merge)
}

# Funcao para criar variavel incidencia de homicidio por 100 mil
merge_var_incidencia_homicidio_porcemmil <- function(df_dados) {
  # obtem o calculo do numero de incidencia por cem mil
  df_var_incidencia_cemmil <- df_dados %>%
    group_by(MUNICIPIO_HOMICIDIO) %>%
    mutate(INCIDENCIA_HOMICIDIO_PORCEMMIL = (INCIDENCIA_HOMICIDIO / POPULACAO) * 100000 ) 
  
  return(df_var_incidencia_cemmil)
}

# Funcao para criar variavel incidencia de homicidio por ano
merge_dados_incidencia_homicidio_porano <- function(df_dados) {
  
  # obtem total incidencia por ano
  df_incidencia_homicidio_ano <- df_dados %>% mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format = "%d/%m/%Y")) %>% 
    mutate(ano = lubridate::year(DATA_HOMICIDIO)) %>%
    group_by(ano) %>%
    #arrange(ano) %>%
    mutate(INCIDENCIA_HOMICIDIO_POR_ANO = n()) %>% 
    .[,-c(ncol(.)-1)]
  
  return(df_incidencia_homicidio_ano)
}

# Funcao para criar variavel incidencia de homicidio por ano/sexo
merge_dados_incidencia_homicidio_poranosexo <- function(df_dados) {
  
  # obtem total incidencia por ano/sexo
  df_incidencia_homicidio_anosexo <- df_dados %>% mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format = "%d/%m/%Y")) %>% 
    mutate(ano = lubridate::year(DATA_HOMICIDIO)) %>%
    group_by(ano, SEXO) %>%
    #arrange(SEXO) %>%
    mutate(INCID_HOMI_POR_ANOSEXO = n()) %>% 
    select(-ano) 
    #.[,-c(ncol(.)-1)]
  
  return(df_incidencia_homicidio_anosexo)
}

# Funcao para criar variavel incidencia de homicidio por ano/sexo/municipio
merge_dados_incidencia_homicidio_poranosexomunicipio <- function(df_dados) {
  
  # obtem total incidencia por ano/sexo/municipio
  df_incidencia_homicidio_anosexomunicipio <- df_dados %>% mutate(DATA_HOMICIDIO = as.Date(DATA_HOMICIDIO, format = "%d/%m/%Y")) %>% 
    mutate(ano = lubridate::year(DATA_HOMICIDIO)) %>%
    group_by(ano, SEXO, MUNICIPIO_HOMICIDIO) %>%
    arrange(ano, SEXO, MUNICIPIO_HOMICIDIO) %>%
    mutate(INCID_HOMI_POR_ANOSEXOMUNICIPIO = n()) %>% 
    select(-ano) 
    #.[,-c(ncol(.)-1)]
  
  return(df_incidencia_homicidio_anosexomunicipio)
}

# Funcao que faz os merges no data frame limpo
executa_merges <- function(df_dados_imputado) {
  # INCIDENCIA_HOMICIDIO
  df_dados_merges <- merge_variavel_incidencia_homicidio(df_dados_imputado)
  
  # MES_ANO
  df_dados_merges <- merge_variavel_mesano(df_dados_merges)
  
  # FAIXA_ETARIA
  df_dados_merges <- merge_grupo_idades(df_dados_merges)
  
  # GRUPO_NATUREZA_HOMICIDIO
  df_dados_merges <- merge_grupo_natureza_homicidio(df_dados_merges)
  
  # GRUPO_MUNICIPIO
  df_dados_merges <- merge_grupo_municipios(df_dados_merges)
  
  # ARMA_DE_FOGO
  df_dados_merges <- merge_grupo_arma_fogo(df_dados_merges)
  
  # GRUPO_ARMA_UTILIZADA
  #df_dados_merges <- merge_grupo_arma_utilizada(df_dados_merges)
  
  # POPULACAO
  df_dados_merges <- merge_dados_populacao(df_dados_merges)
  
  # IDHM
  df_dados_merges <- merge_dados_idhm(df_dados_merges)
  
  # PIB_PERCAPITA
  df_dados_merges <- merge_dados_pib(df_dados_merges)
  
  # LATITUDE/LONGITUDE
  df_dados_merges <- merge_dados_geo(df_dados_merges)
  
  print("Realizando exportacao para arquivo CSV:")
  exporta_csv(df_dados_merges, paste0(nome_arquivo,"_Original_Limpo_Imputado_Merges",".csv"))
  
  return(df_dados_merges)
}

# Funcao para selecionar dataset por periodos de ano
obtem_dados_por_ano <- function(df_dados, ano_inicial, ano_final) {

  df_dados_anual <- df_dados %>% filter(between(year(as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")), ano_inicial, ano_final)) %>% 
    arrange(as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>% 
    mutate(ID = c(1:nrow(.))) # Atualiza ID
  
  return(df_dados_anual)
}
  