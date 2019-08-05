#===========================================================================================
#                                 LIMPEZA DADOS 2015                                      
#-------------------------------------------------------------------------------------------

# Funcao de limpeza dos dados da matriz por documento baixado e retorna um data frame
limpa_dados_2015 <- function(ano, data_frame_meses) {
  
  print(paste("Definindo estrutura de limpeza para", nrow(data_frame_meses[2]), "meses.", sep = " "))
  
  df_nome_docs <- rbind(data_frame_meses[2])
  total_pgs <- 0
  
  # ######################### temp
  # ano <- lista_anos[4, 2]
  # data_frame_meses <- df_lista_meses
  # nome <- "OUTUBRO-2015"
  # glimpse(m_tabelas)
  # View(m_tabela)
  # ######################### temp
  
  for (num_doc in 1:nrow(df_nome_docs)) {
    nome <- as.character(data_frame_meses[num_doc,][2]$mes)
    arquivo <- file.path(".", dir_arquivos, ano, paste0(nome, ".pdf"))
    
    if(file.exists(arquivo)) {
      if (num_doc == 1) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 141.7228, "left" = 27.82115, "bottom" = 560.285057, "right" = 813.19157))
        
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
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 141.7228, "left" = 27.82115, "bottom" = 560.285057, "right" = 813.19157))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
      
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        # Corrige variaveis de valores inconsistentes
        for (n_linha in 1:nrow(m_tabela)) {
          if(n_linha %in% c(308,315)) {
            m_tabela[n_linha,4] <- c("HOMICIDIO DOLOSO")
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
      
      if (num_doc == 3) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 141.7228, "left" = 27.82115, "bottom" = 560.285057, "right" = 813.19157))
        
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
      
      if (num_doc == 4) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 141.7228, "left" = 27.82115, "bottom" = 560.285057, "right" = 813.19157))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)

        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)

        # Corrige variaveis com valores quebrados
        for (n_linha in 416:nrow(m_tabela)) {
          NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-15, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,4] <- NATUREZA_FATO
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
        }
        
        # Remove hifen,hifen-barra,vazio,F,M das colunas 3, 5, 8, 9 e 10
        m_tabela <- corrige_variaveis_matriz(m_tabela)
        
        # Exclui linhas desnecessarias 
        m_tabela <- exclui_linhas_matriz(m_tabela)
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
        
        print(paste("Concluido com",nrow(m_tabela), "Linhas", sep = " "))
      }
      
      if (num_doc == 5) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 141.7228, "left" = 27.82115, "bottom" = 560.285057, "right" = 813.19157))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        # Corrige variaveis de valores duplicados
        for (n_linha in 1:nrow(m_tabela)) {
          if(n_linha %in% c(241,243,246,328,340)) {
            m_tabela[n_linha,4] <- c("HOMICIDIO DOLOSO")
          }
        }
        # Corrige variaveis de valores vazios com NA
        for (n_linha in 1:nrow(m_tabela)) {
          if(m_tabela[n_linha,4] == "") {
            m_tabela[n_linha,4] <- NA
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
      
      if (num_doc == 6) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 141.7228, "left" = 27.82115, "bottom" = 560.285057, "right" = 813.19157))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)

        # Corrige variaveis com valores quebrados
        for (n_linha in 46:62) {
          NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-15, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,4] <- NATUREZA_FATO
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
        }
        
        # Remove hifen,hifen-barra,vazio,F,M das colunas 3, 5, 8, 9 e 10
        m_tabela <- corrige_variaveis_matriz(m_tabela)
        
        # Exclui linhas desnecessarias 
        m_tabela <- exclui_linhas_matriz(m_tabela)
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
        
        print(paste("Concluido com",nrow(m_tabela), "Linhas", sep = " "))
      }
      
      if (num_doc == 7) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 155.24408, "left" = 33.52566, "bottom" = 622.15538, "right" = 934.33433))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)

        # Corrige variaveis com valores quebrados
        m_tabela[327,4] <- paste(m_tabela[326,3], m_tabela[328,3], sep = " ")
        for (n_linha in 329:nrow(m_tabela)) {
          NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-15, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,4] <- NATUREZA_FATO
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
        }
        
        # Remove hifen,hifen-barra,vazio,F,M das colunas 3, 5, 8, 9 e 10
        m_tabela <- corrige_variaveis_matriz(m_tabela)
        
        # Exclui linhas desnecessarias 
        m_tabela <- exclui_linhas_matriz(m_tabela)
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
        
        print(paste("Concluido com",nrow(m_tabela), "Linhas", sep = " "))
      }
      
      if (num_doc == 8) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 155.24408, "left" = 33.52566, "bottom" = 622.15538, "right" = 934.33433))
        
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
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 155.24408, "left" = 33.52566, "bottom" = 622.15538, "right" = 934.33433))
        
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
      
      if (num_doc == 10) {
        # Usar locate_areas(file = arquivo, pages = 22, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 155.24408, "left" = 33.52566, "bottom" = 622.15538, "right" = 934.33433))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        # Corrige variaveis com valores quebrados
        m_tabela[441,4] <- m_tabela[339,4]
        m_tabela[441,5] <- m_tabela[404,5]
        for (n_linha in 433:nrow(m_tabela)) {
          if(n_linha %in% c(433:438,444)) {
            ARMA_UTILZADA <- substr(m_tabela[n_linha,4], nchar(m_tabela[n_linha,4])-11, nchar(m_tabela[n_linha,4]))
            m_tabela[n_linha,5] <- ARMA_UTILZADA
            m_tabela[n_linha,4] <- substr(m_tabela[n_linha,4], 0, nchar(m_tabela[n_linha,4]) - nchar(ARMA_UTILZADA)-1 )
          }
          if(n_linha %in% c(447,453,456)) {
            ARMA_UTILZADA <- substr(m_tabela[n_linha,4], nchar(m_tabela[n_linha,4])-5, nchar(m_tabela[n_linha,4]))
            m_tabela[n_linha,5] <- ARMA_UTILZADA
            m_tabela[n_linha,4] <- substr(m_tabela[n_linha,4], 0, nchar(m_tabela[n_linha,4]) - nchar(ARMA_UTILZADA)-1 )
          }
          if(n_linha == 450) {
            ARMA_UTILZADA <- substr(m_tabela[n_linha,4], nchar(m_tabela[n_linha,4])-1, nchar(m_tabela[n_linha,4]))
            m_tabela[n_linha,5] <- ARMA_UTILZADA
            m_tabela[n_linha,4] <- substr(m_tabela[n_linha,4], 0, nchar(m_tabela[n_linha,4]) - nchar(ARMA_UTILZADA)-1 )
          }
        }
        m_tabela[453,4] <- c("HOMICIDIO DOLOSO")
        
        # Remove hifen,hifen-barra,vazio,F,M das colunas 3, 5, 8, 9 e 10
        m_tabela <- corrige_variaveis_matriz(m_tabela)
        
        # Exclui linhas desnecessarias 
        m_tabela <- exclui_linhas_matriz(m_tabela)

        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
        
        print(paste("Concluido com",nrow(m_tabela), "Linhas", sep = " "))
      }
      
      if (num_doc == 11) {
        # Usar locate_areas(file = arquivo, pages = 22, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 155.24408, "left" = 33.52566, "bottom" = 622.15538, "right" = 934.33433))
        
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
        # Usar locate_areas(file = arquivo, pages = 22, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 155.24408, "left" = 33.52566, "bottom" = 622.15538, "right" = 934.33433))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)

        # Corrige variaveis com valores quebrados
        m_tabela[104,4] <- m_tabela[104,3]
        m_tabela[104,3] <- paste(m_tabela[103,3], m_tabela[105,3], sep = " ")
        m_tabela[108,4] <- paste(m_tabela[107,3], m_tabela[109,3], sep = " ")
        m_tabela[116,4] <- m_tabela[116,3]
        m_tabela[116,3] <- paste(m_tabela[115,3], m_tabela[117,3], sep = " ")
        for (n_linha in 103:124) {
          if(n_linha %in% c(106,110:114,118:124)) {
            NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-15, nchar(m_tabela[n_linha,3]))
            m_tabela[n_linha,4] <- NATUREZA_FATO
            m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
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
    }
  }

  # Padroniza o formato da data
  df_tabela[,6] <- df_tabela[,6] %>% 
    gsub("-jan-", "/01/", .) %>% 
    gsub("-fev-", "/02/", .) %>% 
    gsub("-mar-", "/03/", .) %>% 
    gsub("-Apr-", "/04/", .) %>% 
    gsub("-mai-15", "/05/2015", .) %>% 
    gsub("-Aug-", "/08/", .) %>% 
    gsub("-Sep-", "/09/", .) %>% 
    gsub("-Oct-", "/10/", .) %>% 
    gsub("-Dec-", "/12/", .)
  
  # Padroniza valores da coluna 5, 9 e 10  
  df_tabela <- padroniza_colunas_inconsistentes(df_tabela)
  
  print(paste("Finalizado", total_pgs, "pagina(s).", sep = " ")) 
  print(paste(nrow(df_tabela), "Linhas", sep = " ")) 

  
  return(df_tabela)
}