# -------------------------------------------------------------------------------------------#
# RASPAGEM DE DADOS (Data Scraping)
#
# Script:      Funcao_limpa_dados_2019.R - Funcoes necessarias para limpeza dos dados de 2019.
# Autor:       Erivando Sena
# E-mail:      erivandosena@gmail.com 
# Data:        22/12/2019
# Atualizado:  12/01/2020
##-------------------------------------------------------------------------------------------#

# Funcao de limpeza dos dados da matriz por documento baixado e retorna um data frame
limpa_dados_2019 <- function(ano, data_frame_meses) {
  
  # ano <- "2019"
  
  print(paste("Definindo estrutura de limpeza para", nrow(data_frame_meses[2]), "meses.", sep = " "))
  
  df_nome_docs <- rbind(data_frame_meses[2])
  total_pgs <- 0
  
  for (num_doc in 1:nrow(df_nome_docs)) {
    nome <- as.character(data_frame_meses[num_doc,][2]$mes)
    
    arquivo <- file.path(".", dir_arquivos, ano, paste0(nome, ".pdf"))
    
    if(file.exists(arquivo)) {
      if (num_doc == 1) {
        # Usar: locate_areas(file = arquivo, pages = 4, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        #areas_tabela <- list(c("top" = 207.68668, "left" = 93.55451, "bottom" = 751.36193, "right" = 1034.39511))
        areas_tabela <- list(c("top" = 241.22509, "left" = 93.55451, "bottom" = 746.06639, "right" = 1034.39511))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        # Remove hifen,hifen-barra,vazio,F,M das colunas 3, 5, 8, 9 e 10
        m_tabela <- corrige_variaveis_matriz(m_tabela)
        
        # Exclui linhas desnecessarias 
        m_tabela <- exclui_linhas_matriz(m_tabela)
        
        # converte matriz em data frame.
        df_tabela <- as.data.frame(m_tabela)
        
        print(paste("Concluido com",nrow(m_tabela), "Linhas", sep = " "))
      }
      
      if (num_doc == 2) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 241.22509, "left" = 97.08487, "bottom" = 746.06639, "right" = 1034.39511))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)

        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        # Remove hifen,hifen-barra,vazio,F,M das colunas 3, 5, 8, 9 e 10
        m_tabela <- corrige_variaveis_matriz(m_tabela)
        
        # Exclui linhas desnecessarias 
        m_tabela <- exclui_linhas_matriz(m_tabela)

        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
        
        print(paste("Concluido com",nrow(m_tabela), "Linhas", sep = " "))
      }
      
      if (num_doc == 3) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 242.9083, "left" = 103.8571, "bottom" = 749.6594, "right" = 1043.9429))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        for (n_linha in 194:nrow(m_tabela)) {
          if (n_linha == 203) {
            NATUREZA_FATO <- substring(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-10, nchar(m_tabela[n_linha,3]))
            m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3])-nchar(NATUREZA_FATO))
            m_tabela[n_linha,4] <- NATUREZA_FATO
          } else {
            NATUREZA_FATO <- substring(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-16, nchar(m_tabela[n_linha,3]))
            m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3])-nchar(NATUREZA_FATO))
            m_tabela[n_linha,4] <- NATUREZA_FATO
          }
        }
        
        # Remove hifen,hifen-barra,vazio,F,M das colunas 3, 5, 8, 9 e 10
        m_tabela <- corrige_variaveis_matriz(m_tabela)
        
        # Exclui linhas desnecessarias 
        m_tabela <- exclui_linhas_matriz(m_tabela)
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
        
        print(paste("Concluido com",nrow(m_tabela), "Linhas", sep = " "))
      }
      
      if (num_doc == 4) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 241.1177, "left" = 105.6477, "bottom" = 749.6594, "right" = 1042.1523))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        # Remove hifen,hifen-barra,vazio,F,M das colunas 3, 5, 8, 9 e 10
        m_tabela <- corrige_variaveis_matriz(m_tabela)
        
        # Exclui linhas desnecessarias 
        m_tabela <- exclui_linhas_matriz(m_tabela)
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
        
        print(paste("Concluido com",nrow(m_tabela), "Linhas", sep = " "))
      }
      
      if (num_doc == 5) {
        # Usar locate_areas(file = arquivo, pages = 4, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 249.9405, "left" =  122.6038, "bottom" = 809.8921, "right" = 1059.5162))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)

        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        # Remove hifen,hifen-barra,vazio,F,M das colunas 3, 5, 8, 9 e 10
        m_tabela <- corrige_variaveis_matriz(m_tabela)
        
        # Exclui linhas desnecessarias 
        m_tabela <- exclui_linhas_matriz(m_tabela)
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
        
        print(paste("Concluido com",nrow(m_tabela), "Linhas", sep = " "))
      }
      
      if (num_doc == 6) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 248.1106, "left" = 122.6038, "bottom" = 808.0622, "right" = 1055.8564))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        # Remove hifen,hifen-barra,vazio,F,M das colunas 3, 5, 8, 9 e 10
        m_tabela <- corrige_variaveis_matriz(m_tabela)
        
        # Exclui linhas desnecessarias 
        m_tabela <- exclui_linhas_matriz(m_tabela)
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
        
        print(paste("Concluido com",nrow(m_tabela), "Linhas", sep = " "))
      }
      
      if (num_doc == 7) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 222.51604, "left" = 44.01288, "bottom" = 694.08260, "right" = 977.71468))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        # Remove hifen,hifen-barra,vazio,F,M das colunas 3, 5, 8, 9 e 10
        m_tabela <- corrige_variaveis_matriz(m_tabela)
        
        # Exclui linhas desnecessarias 
        m_tabela <- exclui_linhas_matriz(m_tabela)

        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
        
        print(paste("Concluido com",nrow(m_tabela), "Linhas", sep = " "))
      }
      
      if (num_doc == 8) {
        # Usar locate_areas(file = arquivo, pages = 3, resolution = 100L, widget = c("native"), )
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 332.0621, "left" = 389.1284, "bottom" = 1147.6326, "right" = 1335.2968))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        # Remove hifen,hifen-barra,vazio,F,M das colunas 3, 5, 8, 9 e 10
        m_tabela <- corrige_variaveis_matriz(m_tabela)
        
        # Exclui linhas desnecessarias 
        m_tabela <- exclui_linhas_matriz(m_tabela)
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
        
        print(paste("Concluido com",nrow(m_tabela), "Linhas", sep = " "))
      }
      
      if (num_doc == 9) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 179.75668, "left" = 41.40446, "bottom" = 647.47371, "right" = 950.76905))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        # Corrige variaveis com valores quebrados nas colunas 3 e 4
        for (n_linha in 189:nrow(m_tabela)) {
          NATUREZA_FATO <- substring(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-16, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3])-nchar(NATUREZA_FATO))
          m_tabela[n_linha,4] <- NATUREZA_FATO
        }
        
        # Remove hifen,hifen-barra,vazio,F,M das colunas 3, 5, 8, 9 e 10
        m_tabela <- corrige_variaveis_matriz(m_tabela)
        
        # Exclui linhas desnecessarias 
        m_tabela <- exclui_linhas_matriz(m_tabela)

        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
        
        print(paste("Concluido com",nrow(m_tabela), "Linhas", sep = " "))
      }
      
      if (num_doc == 10) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 262.2043, "left" = 174.0111, "bottom" = 839.6049, "right" = 1111.2985))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        # Corrige variaveis com valores quebrados nas colunas 3 e 4
        for (n_linha in 178:nrow(m_tabela)) {
          NATUREZA_FATO <- substring(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-16, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3])-nchar(NATUREZA_FATO))
          m_tabela[n_linha,4] <- NATUREZA_FATO
        }
        
        # Remove hifen,hifen-barra,vazio,F,M das colunas 3, 5, 8, 9 e 10
        m_tabela <- corrige_variaveis_matriz(m_tabela)
        
        # Exclui linhas desnecessarias 
        m_tabela <- exclui_linhas_matriz(m_tabela)
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
        
        print(paste("Concluido com",nrow(m_tabela), "Linhas", sep = " "))
      }
      
      if (num_doc == 11) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 248.1106, "left" = 124.4337, "bottom" = 809.8921, "right" = 1059.5162))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)

        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        # Remove hifen,hifen-barra,vazio,F,M das colunas 3, 5, 8, 9 e 10
        m_tabela <- corrige_variaveis_matriz(m_tabela)
        
        # Exclui linhas desnecessarias 
        m_tabela <- exclui_linhas_matriz(m_tabela)   

        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
        
        print(paste("Concluido com",nrow(m_tabela), "Linhas", sep = " "))
      }
      
      if (num_doc == 12) {
        # Usar locate_areas(file = arquivo, pages = 2, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 141.22849, "left" = 78.18576, "bottom" = 537.36966, "right" = 762.31113))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        # Remove hifen,hifen-barra,vazio,F,M das colunas 3, 5, 8, 9 e 10
        m_tabela <- corrige_variaveis_matriz(m_tabela)
        
        # Exclui linhas desnecessarias 
        m_tabela <- exclui_linhas_matriz(m_tabela)
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
        
        print(paste("Concluido com",nrow(m_tabela), "Linhas", sep = " "))
      } 
    }
  }
  
  # Padroniza valores da coluna 2, 4, 5, 9 e 10  
  df_tabela <- df_tabela %>% mutate(V2 = as.character(V2),V3 = as.character(V3), V4 = as.character(V4), V5 = as.character(V5), V9 = as.character(V9))
  df_tabela <- padroniza_colunas_inconsistentes(df_tabela)
  
  print(paste("Finalizado", total_pgs, "pagina(s).", sep = " ")) 
  print(paste(nrow(df_tabela), "Linhas", sep = " ")) 
  
  return(df_tabela)
}
