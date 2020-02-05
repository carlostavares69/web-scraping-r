# -------------------------------------------------------------------------------------------#
# RASPAGEM DE DADOS (Data Scraping)
#
# Script:      Funcoes.R - Declara todas a funcoes necessarias nas analises.
# Autor:       Erivando Sena
# E-mail:      erivandosena@gmail.com 
# Data:        20/08/2019
# Atualizado:  05/02/2020
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
      HORA_HOMICIDIO = col_character(), 
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
      HORA_HOMICIDIO = col_character(), 
      NOME_VITIMA = col_character(), 
      GUIA_CADAVERICA = col_character(), 
      SEXO = col_character(), 
      IDADE = col_number(), 
      INCIDENCIA_HOMICIDIO = col_number(),
      MES_ANO = col_character(),
      FAIXA_ETARIA = col_character(),
      GRUPO_MUNICIPIO = col_character(),
      GRUPO_AIS = col_character(),
      ARMA_DE_FOGO = col_character(),
      POPULACAO = col_number(),
      IDHM = col_number(),
      PIB_PERCAPITA = col_number(),
      LATITUDE = col_character(),
      LONGITUDE = col_character()
    )
  } else if(status_classes == "personalizado") {
    classes_colunas <- cols(
      ID = col_number(), 
      AIS = col_character(), 
      MUNICIPIO_HOMICIDIO = col_character(), 
      NATUREZA_HOMICIDIO = col_character(), 
      ARMA_UTILIZADA = col_character(), 
      DATA_HOMICIDIO = col_character(), 
      HORA_HOMICIDIO = col_character(), 
      #NOME_VITIMA = col_character(), 
      #GUIA_CADAVERICA = col_character(), 
      SEXO = col_character(), 
      IDADE = col_number(), 
      INCIDENCIA_HOMICIDIO = col_number(),
      MES_ANO = col_character(),
      FAIXA_ETARIA = col_character(),
      GRUPO_MUNICIPIO = col_character(),
      GRUPO_AIS = col_character(),
      ARMA_DE_FOGO = col_character(),
      POPULACAO = col_number(),
      IDHM = col_number(),
      PIB_PERCAPITA = col_number(),
      LATITUDE = col_number(),
      LONGITUDE = col_number()
    )
  } else if(status_classes == "fortaleza") {
    classes_colunas <- cols(
      ID = col_number(), 
      AIS = col_character(), 
      MUNICIPIO_HOMICIDIO = col_character(), 
      NATUREZA_HOMICIDIO = col_character(), 
      ARMA_UTILIZADA = col_character(), 
      DATA_HOMICIDIO = col_character(), 
      HORA_HOMICIDIO = col_character(), 
      #NOME_VITIMA = col_character(), 
      #GUIA_CADAVERICA = col_character(), 
      SEXO = col_character(), 
      IDADE = col_number(), 
      INCIDENCIA_HOMICIDIO = col_number(),
      MES_ANO = col_character(),
      FAIXA_ETARIA = col_character(),
      ARMA_DE_FOGO = col_character(),
      BAIRRO = col_character(),
      # POPULACAO = col_number(),
      # IDHM = col_number(),
      # PIB_PERCAPITA = col_number(),
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
  
  # Remove remove acentos e espacos em branco na esquerda/direita das variaveis
  df_dados <- df_dados %>% 
    mutate(AIS = str_trim(AIS), 
           MUNICIPIO_HOMICIDIO =  remove_acentos(str_trim(MUNICIPIO_HOMICIDIO)), 
           NATUREZA_HOMICIDIO = remove_acentos(str_trim(NATUREZA_HOMICIDIO)), 
           ARMA_UTILIZADA =  remove_acentos(str_trim(ARMA_UTILIZADA)), 
           DATA_HOMICIDIO = str_trim(DATA_HOMICIDIO), 
           NOME_VITIMA =  stringr::str_squish(remove_acentos(str_trim(NOME_VITIMA))), 
           GUIA_CADAVERICA = str_trim(GUIA_CADAVERICA), 
           SEXO = remove_acentos(str_trim(SEXO)), 
           IDADE = str_trim(IDADE))
  
  # Converte caracteres da variaveis para formato titulo
  df_dados <- df_dados %>% mutate_if(is.character, str_to_title)
  
  # Atribui NA para variaveis com valores desconhecidos
  df_dados <- df_dados %>%
    mutate(ARMA_UTILIZADA = replace(ARMA_UTILIZADA, ARMA_UTILIZADA == "Nao Informado", NA)) %>%
    mutate(SEXO = replace(SEXO, SEXO == "Nao Identificado", NA)) 
  
  # Organiza as observacoes da coluna 2 (AIS)
  df_dados <- corrige_grupo_ais_municipios(df_dados)
  
  # Merge com variavel HORA
  df_dados <- merge_hora(df_dados)
  
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
  # concatena valores fragmentados da coluna coluna 2 (V2=AIS)
  coluna <- c(2)
  linhas <- corrige_variavel(coluna, c("V2"), m_tabela)
  if(!is.null(linhas)) {
    for (num in 1:length(linhas)) {
      var_correcao <- paste(m_tabela[linhas[num]-1, coluna], m_tabela[linhas[num]+1, coluna], sep = " ")
      print(paste("Linha:", linhas[num], "Corrigindo variavel para:", var_correcao))
      m_tabela[linhas[num], coluna] <- var_correcao
    }
  }
  
  # concatena valores fragmentados da coluna coluna 3 (V3=MUNICIPIO)
  coluna <- c(3)
  linhas <- corrige_variavel(coluna, c("V3"), m_tabela)
  if(!is.null(linhas)) {
    for (num in 1:length(linhas)) {
      var_correcao <- paste(m_tabela[linhas[num]-1, coluna], m_tabela[linhas[num]+1, coluna], sep = " ")
      print(paste("Linha:", linhas[num], "Corrigindo variavel para:", var_correcao))
      m_tabela[linhas[num], coluna] <- var_correcao
    }
  }
  
  # concatena valores fragmentados da coluna coluna 4 (V4=NATUREZA_DO_FATO)
  coluna <- c(4)
  linhas <- corrige_variavel(coluna, c("V4"), m_tabela)
  if(!is.null(linhas)) {
    for (num in 1:length(linhas)) {
      var_correcao <- paste(m_tabela[linhas[num]-1, coluna], m_tabela[linhas[num]+1, coluna], sep = " ")
      print(paste("Linha:", linhas[num], "Corrigindo variavel para:", var_correcao))
      m_tabela[linhas[num], coluna] <- var_correcao
    }
  }
  
  # concatena valores fragmentados da coluna coluna 7 (V7=NOME_DA_VITIMA)
  coluna <- c(7)
  linhas <- corrige_variavel(coluna, c("V7"), m_tabela)
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
  
  #unique(df_dados_limpos_2014_2019$AIS)
  
  #df_tabela <- df_dados_limpos_2014_2019
  
  #unique(df_tabela$V2)
  
  # Padroniza valores da coluna 2 
  aisUP <- c("UNIDADE PRISIONAL", "Unidade Prisional", "UNIDADE PRISONAL", "39PRISIONA", "34PRISIONA", "UNIDADE JUAZEIRO DO PRISIONAL NORTE",
             "dade 193 40Prisional", "dade 139 31PrisionalMULUNGU", "dade 217 36Prisional", "GONÇALO DO 233 21Prisional Unidade",
             "dade 140 26PrisionalMULUNGU") 
  aisNA <- c("AIS NÃO DEFINIDA", "99", "Acidente de Trânsito","")
  ais1 <- c("AIS 1 F", "AO CORPORAL 358 AIS 1", "AIS 1 FORTALEZA")
  ais2 <- c("AIS 2 F")
  #ais3 <- c()
  ais4 <- c("AIS 4 F", "BO SEGUIDO DE 230 AIS 4")
  ais5 <- c("AIS 5 F", "BO SEGUIDO DE 154 AIS 5")
  ais6 <- c("AIS 6 F", "AO CORPORAL 161 AIS 6")
  ais7 <- c("AIS 7 C", "GONÇALO DO 194 AIS 7", "GONÇALO DO 241 AIS 7")
  #ais8 <- c()
  ais9 <- c("AIS 9 C", "AIS 9 H", "AIS 9 E")
  ais10 <- c("A 303 AIS 10", "ULEIRO DO 300 AIS 10")
  
  # Padroniza valores da coluna 2
  df_tabela <- df_tabela %>%
    mutate(V2 = str_trim(V2)) %>% 
    mutate(V2 = replace(V2, V2 %in% aisUP, "Unidade Prisional")) %>%  
    mutate(V2 = replace(V2, V2 %in% ais1, "Ais 1")) %>% 
    mutate(V2 = replace(V2, V2 %in% ais2, "Ais 2")) %>% 
    mutate(V2 = replace(V2, V2 %in% ais4, "Ais 4")) %>% 
    mutate(V2 = replace(V2, V2 %in% ais5, "Ais 5")) %>% 
    mutate(V2 = replace(V2, V2 %in% ais6, "Ais 6")) %>% 
    mutate(V2 = replace(V2, V2 %in% ais7, "Ais 7")) %>% 
    mutate(V2 = replace(V2, V2 %in% ais9, "Ais 9")) %>% 
    mutate(V2 = replace(V2, V2 %in% ais10, "Ais 10")) %>% 
    mutate(V2 = replace(V2, V2 %in% aisNA, NA)) 
  
  # Padroniza valores da coluna 4, 5, 7, 9 e 10  
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
  
  # Adicionado posteriormente
  tipo12 <- c("DESCONHECIDA DO SEXO FEMININO","DESCONHECIDO DO SEXO FEMININO")
  tipo13 <- c("DESCONHECIDO  DO SEXO MASCULINO","DESCONHECIDO DO SEXO MASCULINO","DESCONHECIDO DO SEXO MASCULINO (CIOPS: ALISSON)","DESCONHECIDO MASCULINO","DESCONHECIDO SEXO MASCULINO")
  tipo14 <- c("DESCONHECIDO","DESCONHECIDO - SEXO IDENTERMINADO - CORPO CARBONIZADO","DESCONHECIDO DO SEXO INDEFINIDO","DESCONHECIDO INDEFINIDO","DESCONHECIDO SEXO NAO IDENTIFICADO")
  
  # Padroniza valores da coluna 7
  df_tabela <- df_tabela %>%
    mutate(V7 = str_trim(V7)) %>% 
    mutate(V7 = replace(V7, V7 %in% tipo12, "DESCONHECIDO DO SEXO FEMININO")) %>%  
    mutate(V7 = replace(V7, V7 %in% tipo13, "DESCONHECIDO DO SEXO MASCULINO")) %>% 
    mutate(V7 = replace(V7, V7 %in% tipo14, "DESCONHECIDO DO SEXO INDEFINIDO"))

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
    data_frame_meses <- extrai_lista_documentos(lista_anos[1, 1])
    dados_homicidio_ce_2019 <- limpa_dados_2019(lista_anos[1, 2], data_frame_meses)  
  }
  if(2018 %in% vetor_anos) {
    print(paste("Limpando dados de", lista_anos[2, 2], sep = " "))
    data_frame_meses <- extrai_lista_documentos(lista_anos[2, 1])
    dados_homicidio_ce_2018 <- limpa_dados_2018(lista_anos[2, 2], data_frame_meses)  
  }
  if(2017 %in% vetor_anos) {
    print(paste("Limpando dados de", lista_anos[3, 2], sep = " "))
    data_frame_meses <- extrai_lista_documentos(lista_anos[3, 1])
    dados_homicidio_ce_2017 <- limpa_dados_2017(lista_anos[3, 2], data_frame_meses) 
  }
  if(2016 %in% vetor_anos) {
    print(paste("Limpando dados de", lista_anos[4, 2], sep = " "))
    data_frame_meses <- extrai_lista_documentos(lista_anos[4, 1])
    dados_homicidio_ce_2016 <- limpa_dados_2016(lista_anos[4, 2], data_frame_meses) 
  }
  if(2015 %in% vetor_anos) {
    print(paste("Limpando dados de", lista_anos[5, 2], sep = " "))
    data_frame_meses <- extrai_lista_documentos(lista_anos[5, 1])
    dados_homicidio_ce_2015 <- limpa_dados_2015(lista_anos[5, 2], data_frame_meses) 
  }
  if(2014 %in% vetor_anos) {
    print(paste("Limpando dados de", lista_anos[6, 2], sep = " "))
    data_frame_meses <- extrai_lista_documentos(lista_anos[6, 1])
    dados_homicidio_ce_2014 <- limpa_dados_2014(lista_anos[6, 2], data_frame_meses) 
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

# Funcao para geocodificar conjunto de dados por municipio do Ceara
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
    #mutate(LATITUDE = format(LATITUDE, trim = TRUE, digits = 7), LONGITUDE = format(LONGITUDE, trim = TRUE, digits = 7), NM_MUNICIP = as.character(NM_MUNICIP)) %>% 
    mutate(LATITUDE = as.numeric(LATITUDE), LONGITUDE = as.numeric(LONGITUDE), NM_MUNICIP = as.character(NM_MUNICIP)) %>% 
    mutate(NM_MUNICIP = remove_acentos(toupper(NM_MUNICIP)))
  str(df_geo)
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
  # municipios_RMF <- c("Aquiraz","Cascavel","Caucaia","Chorozinho","Eusebio","Fortaleza","Guaiuba",
  #                     "Horizonte","Itaitinga","Maracanau","Maranguape","Pacajus","Pacatuba","Pindoretama",
  #                     "Sao Goncalo Do Amarante","Sao Luis Do Curu","Paraipaba","Paracuru","Trairi")
  # 
  # # Nega o TRUE de %in%
  # `%notin%` <- Negate(`%in%`) 
  
  # Cria nova variavel com grupo de municipios
  df_dados <- df_dados %>%
    # mutate('GRUPO_MUNICIPIO' = case_when(MUNICIPIO_HOMICIDIO == 'Fortaleza' ~ "Capital", 
    #                                      MUNICIPIO_HOMICIDIO %in% municipios_RMF ~ "Regiao Metropolitana", 
    #                                      MUNICIPIO_HOMICIDIO %notin% municipios_RMF ~ "Interior"))
    mutate('GRUPO_MUNICIPIO' = case_when(MUNICIPIO_HOMICIDIO == 'Fortaleza' ~ "Capital", 
                                         MUNICIPIO_HOMICIDIO != 'Fortaleza' ~ "Interior"))
  return(df_dados)
}

# Funcao para merge grupo idades
merge_grupo_idades <- function(df_dados){
  # Cria nova variavel para grupo de idades.
  df_dados <- df_dados %>%
    # mutate('FAIXA_ETARIA' = case_when(IDADE < 13 ~ "0 a 12 anos", 
    #                              IDADE >= 13 & IDADE < 18 ~ "13 a 17 anos", 
    #                              IDADE >= 18 & IDADE < 60 ~ "18 a 59 anos", 
    #                              IDADE >= 60 ~ "60 anos a cima"))
    mutate('FAIXA_ETARIA' = case_when(IDADE < 15 ~ "0 a 14 anos", 
                                      IDADE >= 15 & IDADE <= 19 ~ "15 a 19 anos", # nova faixa incluida
                                      IDADE >= 20 & IDADE < 60 ~ "20 a 59 anos", 
                                      IDADE >= 60 ~ "60 anos a cima"))
  return(df_dados)
}

# Funcao para merge do tipo de arma de fogo
merge_grupo_arma_fogo <- function(df_dados) {
  # Cria nova variavel com teste de Bernoulli para o tipo de Arma De Fogo
  df_dados <- df_dados %>% 
    mutate(ARMA_DE_FOGO = case_when(ARMA_UTILIZADA == "Arma De Fogo" ~ "Sim", ARMA_UTILIZADA != "Arma De Fogo" ~ "Nao")) 
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

# Funcao para criar variavel incidencia de homicidio por municipio
merge_variavel_incidencia_homicidio <- function(df_dados) {
  
  # Converte para maiusculo
  df_dados <- df_dados %>% mutate(MUNICIPIO_HOMICIDIO = toupper(MUNICIPIO_HOMICIDIO))
  
  # obtem o numero de incidencia
  df_incidencia <- df_dados %>%
    group_by(MUNICIPIO_HOMICIDIO) %>%
    summarise(INCIDENCIA_HOMICIDIO = n()) %>%
    distinct()
  
  # faz merge com o data frame origem df_dados
  df_merge <- merge(df_dados, df_incidencia, by.x=c('MUNICIPIO_HOMICIDIO'), by.y=c('MUNICIPIO_HOMICIDIO'))
  
  # Restaura formato da coluna MUNICIPIO_HOMICIDIO e restaura formato da variavel
  df_merge <- df_merge %>% mutate(MUNICIPIO_HOMICIDIO = str_to_title(MUNICIPIO_HOMICIDIO)) %>% 
    arrange(ID) %>%
    # Reordena coluna MUNICIPIO_HOMICIDIO 
    .[,c(2,3,1,4:ncol(.))] 
  
  return(df_merge)
}

# Funcao para criar variavel incidencia de homicidio por 100 mil de habitantes
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


# Funcao para selecionar dataset por periodos de ano
obtem_dados_por_ano <- function(df_dados, ano_inicial, ano_final) {
  
  df_dados_anual <- df_dados %>% filter(between(year(as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")), ano_inicial, ano_final)) %>% 
    arrange(as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>% 
    mutate(ID = c(1:nrow(.))) # Atualiza ID
  
  return(df_dados_anual)
}


# Funcao para merge areas integradas
corrige_grupo_ais_municipios <- function(df_dados) {
  # Faixas de AIS
  # Fonte: https://www.sspds.ce.gov.br/ais/
  
  # Cria vetor com nomes das ais
  Ais_fortaleza <- c(paste0("Ais ", 1:10))
  
  Ais_11 <- c("Caucaia", "Paracuru", "Paraipaba", "Sao Goncalo Do Amarante", "Sao Luis Do Curu", "Trairi")
  Ais_12 <- c("Guaiuba", "Itaitinga", "Maracanau", "Maranguape", "Pacatuba")
  Ais_13 <- c("Aquiraz", "Cascavel", "Chorozinho", "Eusebio", "Horizonte", "Pacajus", "Pindoretama")
  Ais_14 <- c("Alcantaras", "Barroquinha", "Camocim", "Carire", "Carnaubal", "Chaval", "Coreau", "Croata", "Forquilha", "Frecheirinha", "Graca", 
              "Granja", "Groairas", "Guaraciaba Do Norte", "Ibiapina", "Martinopole", "Massape", "Meruoca", "Moraujo", "Mucambo", "Pacuja", 
              "Santana Do Acarau", "Sao Benedito", "Senador Sa", "Sobral", "Tiangua", "Ubajara", "Uruoca", "Vicosa Do Ceara")
  Ais_15 <- c("Acarape", "Aracoiaba", "Aratuba", "Barreira", "Baturite", "Boa Viagem", "Caninde", "Capistrano", "Caridade", "Guaramiranga", 
              "Itapiuna", "Itatira", "Madalena", "Mulungu", "Ocara", "Pacoti", "Palmacia", "Paramoti", "Redencao")
  Ais_16 <- c("Ararenda", "Catunda", "Crateus", "Hidrolandia", "Independencia", "Ipaporanga", "Ipu", "Ipueiras", "Monsenhor Tabosa", 
              "Nova Russas", "Novo Oriente", "Pires Ferreira", "Poranga", "Reriutaba", "Santa Quiteria", "Tamboril", "Varjota")
  Ais_17 <- c("Acarau", "Amontada", "Apuiares", "Bela Cruz", "Cruz", "General Sampaio", "Iraucuba", "Itapaje", "Itapipoca", "Itarema", 
              "Jijoca De Jericoacoara", "Marco", "Miraima", "Morrinhos", "Pentecoste", "Tejucuoca", "Tururu", "Umirim", "Uruburetama")
  Ais_18 <- c("Alto Santo", "Aracati", "Beberibe", "Erere", "Fortim", "Icapui", "Iracema", "Itaicaba", "Jaguaribe", "Jaguaruana", "Limoeiro Do Norte", 
              "Nova Jaguaribara", "Palhano", "Pereiro", "Potiretama", "Quixere", "Russas", "Sao Joao Do Jaguaribe", "Tabuleiro Do Norte")
  Ais_19 <- c("Abaiara", "Altaneira", "Antonina Do Norte", "Araripe", "Assare", "Aurora", "Barbalha", "Barro", "Brejo Santo", "Campos Sales", 
              "Caririacu", "Crato", "Farias Brito", "Jardim", "Jati", "Juazeiro Do Norte", "Mauriti", "Milagres", "Missao Velha", "Nova Olinda", 
              "Penaforte", "Porteiras", "Potengi", "Salitre", "Santana Do Cariri")
  Ais_20 <- c("Banabuiu", "Choro", "Dep. Irapuan Pinheiro", "Ibaretama", "Ibicuitinga", "Jaguaretama", "Milha", "Morada Nova", "Pedra Branca", 
              "Quixada", "Quixeramobim", "Senador Pompeu", "Solonopole")
  Ais_21 <- c("Acopiara", "Baixio", "Carius", "Cedro", "Granjeiro", "Ico", "Iguatu", "Ipaumirim", "Jucas", "Lavras Da Mangabeira", "Oros", 
              "Quixelo", "Saboeiro", "Tarrafas", "Umari", "Varzea Alegre")
  Ais_22 <- c("Aiuaba", "Arneiroz", "Catarina", "Mombaca", "Parambu", "Piquet Carneiro", "Quiterianopolis", "Taua")
  
  # Atualiza variavel AIS
  dados_ais <- df_dados %>%
    # Remove acentos
    mutate(MUNICIPIO_HOMICIDIO = remove_acentos(str_trim(MUNICIPIO_HOMICIDIO))) %>%  
    mutate(MUNICIPIO_HOMICIDIO = str_to_title(MUNICIPIO_HOMICIDIO, locale = "pt_BR")) %>%  
    # Resolve inconsistencias
    mutate(MUNICIPIO_HOMICIDIO = replace(MUNICIPIO_HOMICIDIO, MUNICIPIO_HOMICIDIO == "Itapage", "Itapaje")) %>%
    mutate(MUNICIPIO_HOMICIDIO = replace(MUNICIPIO_HOMICIDIO, MUNICIPIO_HOMICIDIO == "Jaguaribara", "Nova Jaguaribara")) %>%
    mutate(MUNICIPIO_HOMICIDIO = replace(MUNICIPIO_HOMICIDIO, MUNICIPIO_HOMICIDIO == "Deputado Irapuan Pinheiro", "Dep. Irapuan Pinheiro")) %>%
    # Corrige variavel AIS
    mutate(AIS = replace(AIS, (MUNICIPIO_HOMICIDIO %in% Ais_11 & AIS != "Unidade Prisional" | is.na(AIS) & MUNICIPIO_HOMICIDIO != "Fortaleza"), "Ais 11")) %>%  
    mutate(AIS = replace(AIS, (MUNICIPIO_HOMICIDIO %in% Ais_12 & AIS != "Unidade Prisional" | is.na(AIS) & MUNICIPIO_HOMICIDIO != "Fortaleza"), "Ais 12")) %>% 
    mutate(AIS = replace(AIS, (MUNICIPIO_HOMICIDIO %in% Ais_13 & AIS != "Unidade Prisional" | is.na(AIS) & MUNICIPIO_HOMICIDIO != "Fortaleza"), "Ais 13")) %>% 
    mutate(AIS = replace(AIS, (MUNICIPIO_HOMICIDIO %in% Ais_14 & AIS != "Unidade Prisional" | is.na(AIS) & MUNICIPIO_HOMICIDIO != "Fortaleza"), "Ais 14")) %>% 
    mutate(AIS = replace(AIS, (MUNICIPIO_HOMICIDIO %in% Ais_15 & AIS != "Unidade Prisional" | is.na(AIS) & MUNICIPIO_HOMICIDIO != "Fortaleza"), "Ais 15")) %>% 
    mutate(AIS = replace(AIS, (MUNICIPIO_HOMICIDIO %in% Ais_16 & AIS != "Unidade Prisional" | is.na(AIS) & MUNICIPIO_HOMICIDIO != "Fortaleza"), "Ais 16")) %>% 
    mutate(AIS = replace(AIS, (MUNICIPIO_HOMICIDIO %in% Ais_17 & AIS != "Unidade Prisional" | is.na(AIS) & MUNICIPIO_HOMICIDIO != "Fortaleza"), "Ais 17")) %>% 
    mutate(AIS = replace(AIS, (MUNICIPIO_HOMICIDIO %in% Ais_18 & AIS != "Unidade Prisional" | is.na(AIS) & MUNICIPIO_HOMICIDIO != "Fortaleza"), "Ais 18")) %>% 
    mutate(AIS = replace(AIS, (MUNICIPIO_HOMICIDIO %in% Ais_19 & AIS != "Unidade Prisional" | is.na(AIS) & MUNICIPIO_HOMICIDIO != "Fortaleza"), "Ais 19")) %>% 
    mutate(AIS = replace(AIS, (MUNICIPIO_HOMICIDIO %in% Ais_20 & AIS != "Unidade Prisional" | is.na(AIS) & MUNICIPIO_HOMICIDIO != "Fortaleza"), "Ais 20")) %>% 
    mutate(AIS = replace(AIS, (MUNICIPIO_HOMICIDIO %in% Ais_21 & AIS != "Unidade Prisional" | is.na(AIS) & MUNICIPIO_HOMICIDIO != "Fortaleza"), "Ais 21")) %>% 
    mutate(AIS = replace(AIS, (MUNICIPIO_HOMICIDIO %in% Ais_22 & AIS != "Unidade Prisional" | is.na(AIS) & MUNICIPIO_HOMICIDIO != "Fortaleza"), "Ais 22")) %>%
    mutate(AIS = replace(AIS, is.na(AIS), sample(Ais_fortaleza, 1)))

  return(dados_ais)
}

# Funcao para merge do grupo de AIS
merge_grupo_ais <- function(df_dados) {
  
  capital <- c("Ais 1","Ais 2","Ais 3","Ais 4","Ais 5","Ais 6","Ais 7","Ais 8","Ais 9","Ais 10")
  rmf <- c("Ais 11","Ais 12","Ais 13")
  interior_norte <- c("Ais 14","Ais 15","Ais 16","Ais 17")
  interior_sul <- c("Ais 18","Ais 19","Ais 20","Ais 21","Ais 22")
  
  # Cria nova variavel com grupo de municipios
  df_dados_ais <- df_dados %>%
    mutate(GRUPO_AIS = case_when(AIS %in% capital ~ "Capital", 
                                 AIS %in% rmf ~ "Regiao Metropolitana", 
                                 AIS %in% interior_norte ~ "Interior Norte", 
                                 AIS %in% interior_sul ~ "Interior Sul",
                                 AIS == "Unidade Prisional" ~ paste0("Unidade Prisional"," de ",MUNICIPIO_HOMICIDIO)))
  return(df_dados_ais)
}

#--------------

# Funcao para geocodificar conjunto de dados por bairro de Fortaleza
merge_dados_bairro_geo <- function(df_dados) {
  # Faixas de AIS
  # Fonte: https://www.sspds.ce.gov.br/ais/
  # OBS: Não incluso o Bairro Olavo Oliveira (Latitude IBGE: -3.7298802 Longitude IBGE: -38.589347)
  
  # Bairros_Ais1 <- c("Aldeota", "Cais do Porto", "Meireles", "Mucuripe", "Praia de Iracema", "Varjota", "Vicente Pinzon")
  # Bairros_Ais2 <- c("Bom Jardim", "Conjunto Ceará I", "Conjunto Ceará II", "Genibaú", "Granja Lisboa", "Granja Portugal", "Siqueira")
  # Bairros_Ais3 <- c("Ancuri", "Barroso", "Coaçu", "Conjunto Palmeiras", "Curió", "Guajeru", "Jangurussu", "Lagoa Redonda", "Messejana", "Parque Santa Maria", "Paupina", "Pedras", "São Bento")
  # Bairros_Ais4 <- c("Álvaro Weyne", "Carlito Pamplona", "Centro", "Farias Brito", "Jacarecanga", "Monte Castelo", "Moura Brasil", "São Gerardo", "Vila Ellery")
  # Bairros_Ais5 <- c("Aeroporto", "Benfica", "Bom Futuro", "Couto Fernandes", "Damas", "Demócrito Rocha", "Dendê", "Fátima", "Itaoca", "Itaperi", "Jardim América", "José Bonifácio", "Montese", "Pan Americano", "Parangaba", "Parreão", "Serrinha", "Vila Pery", "Vila União")
  # Bairros_Ais6 <- c("Amadeu Furtado", "Antônio Bezerra", "Autran Nunes", "Bela Vista", "Bonsucesso", "Dom Lustosa", "Henrique Jorge", "João XXIII", "Jóquei Clube", "Padre Andrade", "Parque Araxá", "Parquelândia", "Pici", "Presidente Kennedy", "Quintino Cunha", "Rodolfo Teófilo")
  # Bairros_Ais7 <- c("Aerolândia", "Alto da Balança", "Boa Vista", "Cajazeiras", "Cambeba", "Cidade dos Funcionários", "Dias Macedo", "Edson Queiroz", "Jardim das Oliveiras", "José de Alencar", "Parque Dois Irmãos", "Parque Iracema", "Parque Manibura", "Passaré", "Sabiaguaba", "Sapiranga")
  # Bairros_Ais8 <- c("Barra do Ceará", "Cristo Redentor", "Floresta", "Jardim Guanabara", "Jardim Iracema", "Pirambu", "Vila Velha")
  # Bairros_Ais9 <- c("Canindezinho", "Conjunto Esperança", "Conjunto José Walter", "Jardim Cearense", "Maraponga", "Mondubim", "Parque Santa Rosa", "Parque São José", "Planalto Ayrton Senna", "Parque Presidente Vargas", "Vila Manuel Sátiro")
  # Bairros_Ais10 <- c("Cidade 2000", "Cocó", "Dionísio Torres", "Engenheiro Luciano Cavalcante", "Guararapes", "Joaquim Távora", "De Lourdes", "Manuel Dias Branco", "Papicu", "Praia do Futuro I", "Praia do Futuro II", "Salinas", "São João do Tauape")
  
  # Bairros_Ais1 <- c("Aldeota", "Cais do Porto", "Meireles", "Mucuripe", "Praia de Iracema", "Varjota", "Vicente Pinzon")
  # Bairros_Ais2 <- c("Bom Jardim", "Conjunto Ceará I", "Conjunto Ceará II", "Genibaú", "Granja Lisboa", "Granja Portugal", "Siqueira")
  # Bairros_Ais3 <- c("Ancuri", "Barroso", "Coaçu", "Conjunto Palmeiras", "Curió", "Guajeru", "Jangurussu", "Lagoa Redonda", "Messejana", "Parque Santa Maria", "Paupina", "Pedras", "São Bento")
  # Bairros_Ais4 <- c("Álvaro Weyne", "Carlito Pamplona", "Centro", "Farias Brito", "Jacarecanga", "Monte Castelo", "Moura Brasil", "São Gerardo", "Vila Ellery")
  # Bairros_Ais5 <- c("Aeroporto", "Benfica", "Bom Futuro", "Couto Fernandes", "Damas", "Demócrito Rocha", "Dendê", "Fátima", "Itaoca", "Itaperi", "Jardim América", "José Bonifácio", "Montese", "Pan Americano", "Parangaba", "Parreão", "Serrinha", "Vila Pery", "Vila União")
  # Bairros_Ais6 <- c("Amadeu Furtado", "Antônio Bezerra", "Autran Nunes", "Bela Vista", "Bonsucesso", "Dom Lustosa", "Henrique Jorge", "João XXIII", "Jóquei Clube", "Padre Andrade", "Parque Araxá", "Parquelândia", "Pici", "Presidente Kennedy", "Quintino Cunha", "Rodolfo Teófilo")
  # Bairros_Ais7 <- c("Aerolândia", "Alto da Balança", "Boa Vista", "Cajazeiras", "Cambeba", "Cidade dos Funcionários", "Dias Macedo", "Edson Queiroz", "Jardim das Oliveiras", "José de Alencar", "Parque Dois Irmãos", "Parque Iracema", "Parque Manibura", "Passaré", "Sabiaguaba", "Sapiranga")
  # Bairros_Ais8 <- c("Barra do Ceará", "Cristo Redentor", "Floresta", "Jardim Guanabara", "Jardim Iracema", "Pirambu", "Vila Velha")
  # Bairros_Ais9 <- c("Canindezinho", "Conjunto Esperança", "Conjunto José Walter", "Jardim Cearense", "Maraponga", "Mondubim", "Parque Santa Rosa", "Parque São José", "Planalto Ayrton Senna", "Parque Presidente Vargas", "Vila Manuel Sátiro")
  # Bairros_Ais10 <- c("Cidade 2000", "Cocó", "Dionísio Torres", "Engenheiro Luciano Cavalcante", "Guararapes", "Joaquim Távora", "De Lourdes", "Manuel Dias Branco", "Papicu", "Praia do Futuro I", "Praia do Futuro II", "Salinas", "São João do Tauape")
  
  # Bairros_Ais1 <- c("Aldeota", "Cais Do Porto", "Meireles", "Mucuripe", "Praia De Iracema", "Varjota", "Vicente Pinzon",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  # Bairros_Ais2 <- c("Bom Jardim", "Conjunto Ceara I", "Conjunto Ceara II", "Genibau", "Granja Lisboa", "Granja Portugal", "Siqueira",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  # Bairros_Ais3 <- c("Ancuri", "Barroso", "Coacu", "Conjunto Palmeiras", "Curio", "Guajeru", "Jangurussu", "Lagoa Redonda", "Messejana", "Parque Santa Maria", "Paupina", "Pedras", "Sao Bento",NA,NA,NA,NA,NA,NA)
  # Bairros_Ais4 <- c("Alvaro Weyne", "Carlito Pamplona", "Centro", "Farias Brito", "Jacarecanga", "Monte Castelo", "Moura Brasil", "Sao Gerardo", "Vila Ellery",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  # Bairros_Ais5 <- c("Aeroporto", "Benfica", "Bom Futuro", "Couto Fernandes", "Damas", "Democrito Rocha", "Dende", "Fatima", "Itaoca", "Itaperi", "Jardim America", "Jose Bonifacio", "Montese", "Pan Americano", "Parangaba", "Parreao", "Serrinha", "Vila Pery", "Vila Uniao")
  # Bairros_Ais6 <- c("Amadeu Furtado", "Antonio Bezerra", "Autran Nunes", "Bela Vista", "Bonsucesso", "Dom Lustosa", "Henrique Jorge", "Joao XXIII", "Joquei Clube", "Padre Andrade", "Parque Araxa", "Parquelandia", "Pici", "Presidente Kennedy", "Quintino Cunha", "Rodolfo Teofilo",NA,NA,NA)
  # Bairros_Ais7 <- c("Aerolandia", "Alto da Balanca", "Boa Vista", "Cajazeiras", "Cambeba", "Cidade dos Funcionarios", "Dias Macedo", "Edson Queiroz", "Jardim das Oliveiras", "Jose De Alencar", "Parque Dois Irmaos", "Parque Iracema", "Parque Manibura", "Passare", "Sabiaguaba", "Sapiranga",NA,NA,NA)
  # Bairros_Ais8 <- c("Barra Do Ceara", "Cristo Redentor", "Floresta", "Jardim Guanabara", "Jardim Iracema", "Pirambu", "Vila Velha",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  # Bairros_Ais9 <- c("Canindezinho", "Conjunto Esperanca", "Conjunto Jose Walter", "Jardim Cearense", "Maraponga", "Mondubim", "Parque Santa Rosa", "Parque Sao Jose", "Planalto Ayrton Senna", "Parque Presidente Vargas", "Vila Manuel Satiro",NA,NA,NA,NA,NA,NA,NA,NA)
  # Bairros_Ais10 <- c("Cidade 2000", "Coco", "Dionisio Torres", "Engenheiro Luciano Cavalcante", "Guararapes", "Joaquim Tavora", "De Lourdes", "Manuel Dias Branco", "Papicu", "Praia Do Futuro I", "Praia Do Futuro II", "Salinas", "Sao Joao Do Tauape",NA,NA,NA,NA,NA,NA)
  
  # Cria vetor com nomes dos grupos de bairros por ais
  Bairros_Ais1 <- c("Aldeota", "Cais Do Porto", "Meireles", "Mucuripe", "Praia De Iracema", "Varjota", "Vicente Pinzon")
  Bairros_Ais2 <- c("Bom Jardim", "Conjunto Ceara I", "Conjunto Ceara II", "Genibau", "Granja Lisboa", "Granja Portugal", "Siqueira")
  Bairros_Ais3 <- c("Ancuri", "Barroso", "Coacu", "Conjunto Palmeiras", "Curio", "Guajeru", "Jangurussu", "Lagoa Redonda", "Messejana", "Parque Santa Maria", "Paupina", "Pedras", "Sao Bento")
  Bairros_Ais4 <- c("Alvaro Weyne", "Carlito Pamplona", "Centro", "Farias Brito", "Jacarecanga", "Monte Castelo", "Moura Brasil", "Sao Gerardo", "Vila Ellery")
  Bairros_Ais5 <- c("Aeroporto", "Benfica", "Bom Futuro", "Couto Fernandes", "Damas", "Democrito Rocha", "Dende", "Fatima", "Itaoca", "Itaperi", "Jardim America", "Jose Bonifacio", "Montese", "Pan Americano", "Parangaba", "Parreao", "Serrinha", "Vila Pery", "Vila Uniao")
  Bairros_Ais6 <- c("Amadeu Furtado", "Antonio Bezerra", "Autran Nunes", "Bela Vista", "Bonsucesso", "Dom Lustosa", "Henrique Jorge", "Joao XXIII", "Joquei Clube", "Padre Andrade", "Parque Araxa", "Parquelandia", "Pici", "Presidente Kennedy", "Quintino Cunha", "Rodolfo Teofilo")
  Bairros_Ais7 <- c("Aerolandia", "Alto da Balanca", "Boa Vista", "Cajazeiras", "Cambeba", "Cidade dos Funcionarios", "Dias Macedo", "Edson Queiroz", "Jardim das Oliveiras", "Jose De Alencar", "Parque Dois Irmaos", "Parque Iracema", "Parque Manibura", "Passare", "Sabiaguaba", "Sapiranga")
  Bairros_Ais8 <- c("Barra Do Ceara", "Cristo Redentor", "Floresta", "Jardim Guanabara", "Jardim Iracema", "Pirambu", "Vila Velha")
  Bairros_Ais9 <- c("Canindezinho", "Conjunto Esperanca", "Conjunto Jose Walter", "Jardim Cearense", "Maraponga", "Mondubim", "Parque Santa Rosa", "Parque Sao Jose", "Planalto Ayrton Senna", "Parque Presidente Vargas", "Vila Manuel Satiro")
  Bairros_Ais10 <- c("Cidade 2000", "Coco", "Dionisio Torres", "Engenheiro Luciano Cavalcante", "Guararapes", "Joaquim Tavora", "De Lourdes", "Manuel Dias Branco", "Papicu", "Praia Do Futuro I", "Praia Do Futuro II", "Salinas", "Sao Joao Do Tauape")
  
  # Cria vetor com nomes das ais
  Ais_fortaleza <- c(paste0("Ais ", 1:10))
  
  # Cria vetor com nomes das ais em lista sequencial
  Coluna_Ais <- c(rep("Ais 1", length.out= length(Bairros_Ais1)),
                  rep("Ais 2", length.out= length(Bairros_Ais2)),
                  rep("Ais 3", length.out= length(Bairros_Ais3)),
                  rep("Ais 4", length.out= length(Bairros_Ais4)),
                  rep("Ais 5", length.out= length(Bairros_Ais5)),
                  rep("Ais 6", length.out= length(Bairros_Ais6)),
                  rep("Ais 7", length.out= length(Bairros_Ais7)),
                  rep("Ais 8", length.out= length(Bairros_Ais8)),
                  rep("Ais 9", length.out= length(Bairros_Ais9)),
                  rep("Ais 10", length.out= length(Bairros_Ais10)))
  
  # Cria vetor com nomes dos bairros em lista sequencial
  Coluna_Bairros <- c(Bairros_Ais1, Bairros_Ais2, Bairros_Ais3, Bairros_Ais4, Bairros_Ais5, Bairros_Ais6, Bairros_Ais7, Bairros_Ais8, Bairros_Ais9, Bairros_Ais10)
  
  df_ais_fortaleza <- data.frame(BAIRRO = Coluna_Bairros, AIS = Coluna_Ais)
  
  # Geocodificacao
  # Criando lista de bairros geolocalizados com ggmap e API Google (Depois farei testes com API Yahoo)
  if(!exists("df_bairros_geo")) {
    #Habilitar API Google para utlizar ggmap
    suppressMessages(register_google(key = "AIzaSyAxnGSaU3mFZanbLuTOQ2vk405Gjp_u6v4", 
                                     account_type = "standard", 
                                     client = "weighty-flux-244912", 
                                     second_limit = 50, 
                                     day_limit = 2500, 
                                     write = TRUE))
    suppressMessages(ggmap_show_api_key())
    print("Google API")
    showing_key()
    suppressMessages(ggmap_hide_api_key())
    
    print("Geocodificando bairros de Fortaleza atraves do Google")
    # Geocodifica bairros
    df_bairros_geo <- suppressMessages(geocode(location = paste(
      df_ais_fortaleza$BAIRRO, "fortaleza", "ceara", sep = ", "), output = "latlona", source = "google", force = FALSE) %>% as.data.frame(.))
    
    # cria data frame como variavel global
    assign('df_bairros_geo', df_bairros_geo, envir=.GlobalEnv)
    df_bairros_geo <- df_bairros_geo[,c(2,1,3)] # reordena coluna 
    
    # Altera cabecalho do data frame
    names(df_bairros_geo) <- c("LATITUDE", "LONGITUDE", "BAIRRO")
  }
  
  # Cria data frame dos bairros com ais correspondente e geolocalizacao
  df_ais_bairros_geo <- data.frame(df_ais_fortaleza, df_bairros_geo[, 1:2])
  
  # Faz merge dos bairros no data frame df_dados
  df_merge_ais <- merge(df_dados, df_ais_bairros_geo, by.x=c('AIS'), by.y=c('AIS')) %>% 
    # Classifica por data
    arrange(as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>% 
    # Atualiza ID
    mutate(ID = c(1:nrow(.))) %>%
    # Reordena titulos do cabecalho 
    dplyr::select(names(df_dados), BAIRRO, LATITUDE, LONGITUDE)


  return(df_merge_ais)
}

# Funcao para criar variavel incidencia de homicidio por bairro
merge_dados_incidencia_homicidio_bairro <- function(df_dados) {
  
  # obtem o numero de incidencia
  df_merge_incidencia <- df_dados %>%
    group_by(BAIRRO) %>%
    tally(name = "INCIDENCIA_HOMICIDIO")
  
  # faz merge com o data frame origem df_dados
  df_merge_geo_incidencia <- merge(df_dados, df_merge_incidencia, by.x=c('BAIRRO'), by.y=c('BAIRRO')) %>%
    # Classifica por data
    arrange(as.Date(DATA_HOMICIDIO, format="%d/%m/%Y")) %>%
    # Atualiza ID
    mutate(ID = c(1:nrow(.))) %>%
    # reordena titulos do cabecalho
    dplyr::select(names(df_dados), INCIDENCIA_HOMICIDIO) 
  
  return(df_merge_geo_incidencia)
}

# Funcao para merge da variavel HORA e atualizaca de outras variaveis
merge_hora <- function(df_raspagem_original_limpo) {
  
  # Importa dados do arquivo CSV
  # df_raspagem <- importa_csv("original", paste0(nome_arquivo,"_Original_Limpo",".csv")) %>% 
  df_raspagem <- df_raspagem_original_limpo %>%
    mutate(NOME_VITIMA = stringr::str_squish(NOME_VITIMA)) 
  
  # Importa dados da planilha EXCEL dos anos 2014-2019 fornecido pelo CSAI da SSPDS-CE em 21/01/2020
  SSPDS_2014_2019_Ceara_Transparente <- suppressMessages(readxl::read_xlsx(file.path(".", dir_auxiliares, "CSAI da SSPDS-CE/CVLI_2014-2019_CSAI-SSPDS-CE.xlsx"), col_names = TRUE, sheet = "CVLI"))
  
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
  
  verifica_inconsistencias(df_CSAI_SSPDS$NOME_VITIMA, df_raspagem$NOME_VITIMA)
  
  inexistente <- NULL
  
  # Estrutura de repeticao 
  for(i in 1:nrow(df_CSAI_SSPDS)) {
    
    indice_linha <- which(df_raspagem$NOME_VITIMA == df_CSAI_SSPDS[i,7]$NOME_VITIMA, arr.ind = TRUE)
    
    # merge variavel HORA e atualizacao das variaveis MUNICIPIO_HOMICIDIO,NATUREZA_HOMICIDIO,ARMA_UTILIZADA,HORA_HOMICIDIO
    if(!is.na(indice_linha[1])) {
      if(length(indice_linha[1]) > 0 ) {
        
        print(paste("Incluindo variavel HORA atraves da variavel NOME_VITIMA:", "Indice pesquisado",indice_linha[1], "Indice atualizado",i, sep = " "))
        print(paste("->", df_raspagem[indice_linha[1],8], sep = " "))
        
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
      df_raspagem <- rbind(dplyr::tibble(
        
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
        
      ), df_raspagem) #%>% arrange(as.Date(DATA_HOMICIDIO, format="%d/%m/%Y"))
      
    }
    
  }
  
  df_raspagem <- df_raspagem %>%
    mutate(MUNICIPIO_HOMICIDIO = replace(MUNICIPIO_HOMICIDIO, MUNICIPIO_HOMICIDIO == str_to_title("ITAPAJE"), str_to_title("ITAPAGE"))) %>%
    mutate(MUNICIPIO_HOMICIDIO = replace(MUNICIPIO_HOMICIDIO, MUNICIPIO_HOMICIDIO == str_to_title("NOVA JAGUARIBARA"), str_to_title("JAGUARIBARA"))) %>%
    mutate(MUNICIPIO_HOMICIDIO = replace(MUNICIPIO_HOMICIDIO, MUNICIPIO_HOMICIDIO == str_to_title("DEP. IRAPUAN PINHEIRO"), str_to_title("DEPUTADO IRAPUAN PINHEIRO"))) %>%
    mutate(MUNICIPIO_HOMICIDIO = replace(MUNICIPIO_HOMICIDIO, MUNICIPIO_HOMICIDIO == str_to_title("IRAPUAN PINHEIRO"), str_to_title("DEPUTADO IRAPUAN PINHEIRO")))
  
  return(df_raspagem)
}

# Funcao que faz os merges no data frame com municipios
df_municipios_executa_merges <- function(df_dados_imputado) {
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
  
  # GRUPO_AIS
  df_dados_merges <- merge_grupo_ais(df_dados_merges)
  
  # ARMA_DE_FOGO
  df_dados_merges <- merge_grupo_arma_fogo(df_dados_merges)
  
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

# Funcao que faz os merges no data frame com bairros
df_bairros_executa_merges <- function(df_dados_imputado) {
  
  # LATITUDE/LONGITUDE
  df_dados_merges <- merge_dados_bairro_geo(df_dados_imputado)  
  
  # INCIDENCIA_HOMICIDIO
  df_dados_merges <- merge_dados_incidencia_homicidio_bairro(df_dados_merges)
  
  # MES_ANO
  df_dados_merges <- merge_variavel_mesano(df_dados_merges)
  
  # FAIXA_ETARIA
  df_dados_merges <- merge_grupo_idades(df_dados_merges)
  
  # GRUPO_NATUREZA_HOMICIDIO
  df_dados_merges <- merge_grupo_natureza_homicidio(df_dados_merges)
  
  # ARMA_DE_FOGO
  df_dados_merges <- merge_grupo_arma_fogo(df_dados_merges)
  
  # POPULACAO
  #df_dados_merges <- merge_dados_populacao(df_dados_merges)
  
  # IDHM
  #df_dados_merges <- merge_dados_idhm(df_dados_merges)
  
  # PIB_PERCAPITA
  #df_dados_merges <- merge_dados_pib(df_dados_merges)
  
  # reordena titulos do cabecalho
  df_dados_merges <- df_dados_merges %>% 
    dplyr::select(names(df_dados_imputado),BAIRRO,INCIDENCIA_HOMICIDIO,MES_ANO,FAIXA_ETARIA,GRUPO_NATUREZA_HOMICIDIO,ARMA_DE_FOGO,LATITUDE,LONGITUDE)
  
  print("Realizando exportacao para arquivo CSV:")
  exporta_csv(df_dados_merges, paste0(nome_arquivo,"_Original_Limpo_Imputado_Merges_Bairros",".csv"))
  
  return(df_dados_merges)
}


