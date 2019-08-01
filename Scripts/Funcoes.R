#===========================================================================================
#                                   COLETA                                      
#-------------------------------------------------------------------------------------------

# Funcao que extrai informacoes de ano do codigo html da pagina
extrai_lista_anos <- function(url_site) {
  pagina_html <- xml2::read_html(url_site)
  
  df_lista_urls <- pagina_html %>% #obtem codigo html da pagina
    rvest::html_nodes('.grid a.box') %>% # Filtro das tags "a" (link) desejadas
    purrr::map(xml2::xml_attrs) %>% # # Mapeia os nos html e cria lista com tags
    purrr::map_df(~as.list(.)) %>% # Converte o mapa de tags de lista em data frame 
    na.omit(.) %>% # exclui missings
    .[-c(nrow(.)),] # remove ultima linha
  
  df_lista_titulos <- pagina_html %>% #obtem codigo html da pagina
    rvest::html_nodes('.grid p') %>% # Filtro das tags "h3" (nomes dos meses de cada link)desejadas
    rvest::html_text() %>% # Converte a relacao de nomes em texto
    #gsub(is.character, "", .) %>% # Substitui barras por travessao com espaco 
    gsub(" ", "", .) %>% # Substitui espaco com travessao por travessao sem espaco
    substr(., nchar(.)-3, nchar(.)) %>% # obtem apenas os anos
    .[-length(.)] %>% # remove ano 2013
    as.data.frame(.) %>% # Converte a lista de titulos em data frame
    setNames(., "ano") # O "." significa trazer o data frame anterior e e atribui o nome "ano" da coluna do dataframe
    
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

#===========================================================================================
#                             TRATAMENTO
#-------------------------------------------------------------------------------------------
# funcao para geocodificar conjunto de dados
merge_dados_geo <- function(df_dados) {
  # ftp://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/localidades/Shapefile_SHP/
  # Importando o arquivo Shapefile com informacoes geograficas e coordenadas GPS
  dados_shp <- rgdal::readOGR(dsn = file.path(".", dir_auxiliares, "Shapefile_SHP/BR_Localidades_2010_v1.shp"), layer = "BR_Localidades_2010_v1", verbose = FALSE)
  
  #colunas utilizadas
  colunas <- c('LONG', 'LAT', 'NM_MUNICIP')
  
  # Filtro por municipios do Ceara
  dados_shp[which(dados_shp$CD_NIVEL == 1 & as.character(dados_shp$NM_UF) == "CEARÁ"),] %>% 
    as.data.frame(.) %>% # cria data frame
    select(., colunas) %>% # obtem variaveis relevantes 
    mutate_if(is.factor, as.character) -> df_geo
  
  # obtem nomes das colunas
  cabecalho <- names(df_dados)
  
  # converte as linhas da coluna municipio para maiusculos e cria data frame
  df_dados <- df_dados %>% mutate(municipio = toupper(municipio)) %>% as.data.frame(.)
  
  # Merge dos data frames com geolocalizacao por municipio
  df_merge <- merge(df_dados, df_geo, by.x=c('municipio'), by.y=c('NM_MUNICIP'))  %>% 
    .[,c(c(2:3),1,c(4:ncol(.)))] %>% # reordena colunas 
    mutate(municipio = str_to_title(municipio)) # Restaura formato da coluna
  
  # Inclui cabecalhos
  names(df_merge) <- tolower(c(cabecalho, colunas[(1:2)]))
  
  # Renomea colunas coordenadas
  df_merge <- df_merge %>% rename(longitude = long) %>% rename(latitude = lat) 
  
  # Padroniza linhas
  df_merge %>% mutate_if(is.character, str_to_title) -> df_merge
  
  # retorna dadta frame
  return(df_merge) 
}

# Funcao que cria um arquivo CSV e obtem um data frame 
exporta_importa_csv <- function(df_limpo) {
  # classes dos tipos de colunas
  classes_colunas <- sapply(df_limpo, class)
  
  # salvar dados em arquivo CSV
  nome_arquivo_csv <- file.path(".", dir_dados, paste0(nome_arquivo, ".csv"))
  
  # Escrever arquivo CSV
  readr::write_csv(x = df_limpo, path = nome_arquivo_csv, na = "NA", col_names = TRUE)
  
  # Importanto do CSV criado
  df_importado <- utils::read.table(file = nome_arquivo_csv, 
                                    header = TRUE, 
                                    na.strings = "NA", 
                                    sep = ",", 
                                    fileEncoding = "UTF-8", 
                                    encoding = "ISO-8859-1", 
                                    colClasses = classes_colunas)
  
  return(df_importado)
}

#===========================================================================================
#                                 LIMPEZA                                      
#-------------------------------------------------------------------------------------------
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

# Funcao para remover acentos
remove_acentos <- function(nm_coluna) {
  if(!is.character(nm_coluna))
    nm_coluna <- as.character(nm_coluna)
  col_texto <- toupper(stringi::stri_trans_general(nm_coluna, "latin-ascii"))
  return(col_texto)
}

# Funcao que obtem matrix de tabelas
extrai_tabela <- function(caminho_completo_arquivo, areas_tabela) {
  return(extract_tables(file = caminho_completo_arquivo, area = areas_tabela, guess = FALSE, encoding = "UTF-8"))
  #return(extract_tables(file = caminho_completo_arquivo, method = "decide", encoding = "UTF-8"))
}

realiza_limpeza_dados <- function() {
  
  lista_anos <- extrai_lista_anos(URL_site)
  vetor_anos <- lista_anos$ano %>% as.vector(.)
  
  if(2018 %in% vetor_anos) {
    print(paste("Limpando dados de", lista_anos[1, 2], sep = " "))
    df_lista_meses <- extrai_lista_documentos(lista_anos[1, 1])
    dados_crime_ce_2018 <- limpa_dados_2018(lista_anos[1, 2], df_lista_meses)
  }
  
  if(2017 %in% vetor_anos) {
    print(paste("Limpando dados de", lista_anos[2, 2], sep = " "))
    df_lista_meses <- extrai_lista_documentos(lista_anos[2, 1])
    dados_crime_ce_2017 <- limpa_dados_2017(lista_anos[2, 2], df_lista_meses)
  }

  if(2016 %in% vetor_anos) {
    print(paste("Limpando dados de", lista_anos[3, 2], sep = " "))
    df_lista_meses <- extrai_lista_documentos(lista_anos[3, 1])
    dados_crime_ce_2016 <- limpa_dados_2016(lista_anos[3, 2], df_lista_meses)
  }
  
  if(2015 %in% vetor_anos) {
    print(paste("Limpando dados de", lista_anos[4, 2], sep = " "))
    df_lista_meses <- extrai_lista_documentos(lista_anos[4, 1])
    dados_crime_ce_2015 <- limpa_dados_2015(lista_anos[4, 2], df_lista_meses)
  }
  
  if(2014 %in% vetor_anos) {
    print(paste("Limpando dados de", lista_anos[5, 2], sep = " "))
    df_lista_meses <- extrai_lista_documentos(lista_anos[5, 1])
    dados_crime_ce_2014 <- limpa_dados_2014(lista_anos[5, 2], df_lista_meses)
  }
  
  dados_crime_ce_merge_anos <- rbind(dados_crime_ce_2018, 
                                     dados_crime_ce_2017, 
                                     dados_crime_ce_2016, 
                                     dados_crime_ce_2015,
                                     dados_crime_ce_2014)
  
  # Merge com dados geoespaciais
  #df_dados_merge_geo <- merge_dados_geo(df_merges)
  
  #return(df_dados_merge_geo)
  return(dados_crime_ce_merge_anos)

}
