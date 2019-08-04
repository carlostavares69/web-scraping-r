#===========================================================================================
#                                 LIMPEZA DADOS 2017                                      
#-------------------------------------------------------------------------------------------

# Funcao de limpeza dos dados da matriz por documento baixado e retorna um data frame
limpa_dados_2017 <- function(ano, data_frame_meses) {
  
  print(paste("Definindo estrutura de limpeza para", nrow(data_frame_meses[2]), "meses.", sep = " "))
  
  df_nome_docs <- rbind(data_frame_meses[2])
  total_pgs <- 0
  
  for (num_doc in 1:nrow(df_nome_docs)) {
    nome <- as.character(data_frame_meses[num_doc,][2]$mes)
    arquivo <- file.path(".", dir_arquivos, ano, paste0(nome, ".pdf"))
    
    if(file.exists(arquivo)) {
      if (num_doc == 1) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 183.36284, "left" = 61.59118, "bottom" = 672.24285, "right" = 923.86773))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        # Remove ifen,barra,vazio,F,M das variaveis 5, 8, 9 e 10
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
        areas_tabela <- list(c("top" = 183.36284, "left" = 61.59118, "bottom" = 672.24285, "right" = 923.86773))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        # Remove ifen,barra,vazio,F,M das variaveis 5, 8, 9 e 10
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
        areas_tabela <- list(c("top" = 199.1861, "left" = 104.6478, "bottom" = 713.8824, "right" = 993.0863))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        # Remove ifen,barra,vazio,F,M das variaveis 5, 8, 9 e 10
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
        areas_tabela <- list(c("top" = 204.8017, "left" = 149.3216, "bottom" = 685.7051, "right" = 970.5903))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Inclui coluna AIS faltante com variaveis NA para manter padrao e reordena 
        m_tabela <- cbind(m_tabela, c(NA)) %>% .[, c(1,10,2,3,4,5,6,7,8,9)]
        # Atribui titulo da coluna
        m_tabela[2,2] <- c("AIS") 
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        # Remove ifen,barra,vazio,F,M das variaveis 5, 8, 9 e 10
        m_tabela <- corrige_variaveis_matriz(m_tabela)
        
        # Exclui linhas desnecessarias 
        m_tabela <- exclui_linhas_matriz(m_tabela)
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
        
        print(paste("Concluido com",nrow(m_tabela), "Linhas", sep = " "))
      }
      
      if (num_doc == 5) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 207.7458, "left" = 131.4120, "bottom" = 688.8475, "right" = 1002.2953))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        # Remove ifen,barra,vazio,F,M das variaveis 5, 8, 9 e 10
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
        areas_tabela <- list(c("top" = 195.96850, "left" = 95.62205, "bottom" = 678.23622, "right" = 964.53543))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        for (n_linha in 615:nrow(m_tabela)) {
          NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-15, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,4] <- NATUREZA_FATO
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
        }
        
        # Remove ifen,barra,vazio,F,M das variaveis 5, 8, 9 e 10
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
        areas_tabela <- list(c("top" = 181.16481, "left" = 47.53346, "bottom" = 658.40080, "right" = 920.24788))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)

        # Corrige variaveis com valores quebrados entre colunas 3 e 4
        for (n_linha in 598:nrow(m_tabela)) {
          NATUREZA_FATO <- substring(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-16, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3])-nchar(NATUREZA_FATO))
          m_tabela[n_linha,4] <- NATUREZA_FATO
        }
        
        # Remove ifen,barra,vazio,F,M das variaveis 5, 8, 9 e 10
        m_tabela <- corrige_variaveis_matriz(m_tabela)
        
        # Exclui linhas desnecessarias 
        m_tabela <- exclui_linhas_matriz(m_tabela)
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
        
        print(paste("Concluido com",nrow(m_tabela), "Linhas", sep = " "))
      }
      
      if (num_doc == 8) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 183.0662, "left" = 49.4348, "bottom" = 646.9928, "right" = 918.3465))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)

        # Corrige variaveis com valores quebrados entre colunas 3 e 4
        m_tabela[589, 4] <- m_tabela[589,3]
        m_tabela[592, 4] <- m_tabela[592,3]
        m_tabela[595, 4] <- m_tabela[595,3]
        m_tabela[589,3] <- paste(m_tabela[588,3], m_tabela[590,3], sep = " ")
        m_tabela[592,3] <- paste(m_tabela[591,3], m_tabela[593,3], sep = " ")
        m_tabela[595,3] <- paste(m_tabela[594,3], m_tabela[596,3], sep = " ")
        
        for (n_linha in 598:nrow(m_tabela)) {
          NATUREZA_FATO <- substring(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-16, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3])-nchar(NATUREZA_FATO))
          m_tabela[n_linha,4] <- NATUREZA_FATO
        }
        
        # Remove ifen,barra,vazio,F,M das variaveis 5, 8, 9 e 10
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
        areas_tabela <- list(c("top" = 188.20286, "left" = 65.26998, "bottom" = 668.52298, "right" = 942.23077))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Inclui colunas faltantes para manter padrao e reordena colunas
        m_tabelas[[28]] <- cbind(m_tabelas[[28]], c(NA)) %>% .[,c(1,2,3,4,5,6,7,8,10,9)]
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)

        # Corrige variaveis com valores quebrados entre colunas 3 e 4

        m_tabela[493, 9] <- c("MASCULINO")
        m_tabela[495, 9] <- c("FEMININO")
        m_tabela[600, 9] <- m_tabela[493, 9]
        m_tabela[493, 8] <- NA
        m_tabela[495, 8] <- NA
        m_tabela[603, 10] <- c(39)
        m_tabela[605, 10] <- c(34)
        
        for (n_linha in 600:nrow(m_tabela)) {
          NATUREZA_FATO <- substring(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-16, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3])-nchar(NATUREZA_FATO))
          m_tabela[n_linha,4] <- NATUREZA_FATO
          
        }

        for (n_linha in c(488:506,603,605)) {
          if(!(n_linha %in% c(493, 495, 600))) {
            SEXO <- substring(m_tabela[n_linha,8], nchar(m_tabela[n_linha,8])-8, nchar(m_tabela[n_linha,8]))
            m_tabela[n_linha,8] <- substr(m_tabela[n_linha,8], 0, nchar(m_tabela[n_linha,8])-nchar(SEXO))
            m_tabela[n_linha,9] <- SEXO
          }
        }

        # Remove ifen,barra,vazio,F,M das variaveis 5, 8, 9 e 10
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
        areas_tabela <- list(c("top" = 215.9543, "left" = 153.9322 , "bottom" = 831.6829, "right" = 1038.0553))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)

        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)

        for (n_linha in 638:nrow(m_tabela)) {
          NATUREZA_FATO <- substring(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-16, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3])-nchar(NATUREZA_FATO))
          m_tabela[n_linha,4] <- NATUREZA_FATO
        }
        
        # Remove ifen,barra,vazio,F,M das variaveis 5, 8, 9 e 10
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
        areas_tabela <- list(c("top" = 215.9543, "left" = 153.9322 , "bottom" = 831.6829, "right" = 1038.0553))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)

        for (n_linha in 582:nrow(m_tabela)) {
          NATUREZA_FATO <- substring(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-16, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3])-nchar(NATUREZA_FATO))
          m_tabela[n_linha,4] <- NATUREZA_FATO
        }
        
        # Remove ifen,barra,vazio,F,M das variaveis 5, 8, 9 e 10
        m_tabela <- corrige_variaveis_matriz(m_tabela)
        
        # Exclui linhas desnecessarias 
        m_tabela <- exclui_linhas_matriz(m_tabela)
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
        
        print(paste("Concluido com",nrow(m_tabela), "Linhas", sep = " "))
      }
      
      if (num_doc == 12) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 184.53898, "left" = 49.98711, "bottom" = 668.28522, "right" = 922.34284))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)

        m_tabela[575,4] <- paste(m_tabela[574,3], m_tabela[576,3], sep = " ")
        for (n_linha in 572:nrow(m_tabela)) {
          if(n_linha != 575) {
            NATUREZA_FATO <- substring(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-16, nchar(m_tabela[n_linha,3]))
            m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3])-nchar(NATUREZA_FATO))
            m_tabela[n_linha,4] <- NATUREZA_FATO
          }
        }
        
        # Remove ifen,barra,vazio,F,M das variaveis 5, 8, 9 e 10
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
  df_tabela[,6] <- df_tabela[,6] %>% gsub("-Apr-", "/04/", .)
  
  print(paste("Finalizado", total_pgs, "pagina(s).", sep = " ")) 
  print(paste(nrow(df_tabela), "Linhas", sep = " ")) 
  
  return(df_tabela)
}