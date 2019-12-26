# -------------------------------------------------------------------------------------------#
# RASPAGEM DE DADOS (Data Scraping)
#
# Script:      Funcao_limpa_dados_2018.R - Funcoes necessarias para limpeza dos dados de 2018.
# Autor:       Erivando Sena
# E-mail:      erivandosena@gmail.com 
# Data:        04/09/2019
# Atualizado:  22/12/2019
##-------------------------------------------------------------------------------------------#

# Funcao de limpeza dos dados da matriz por documento baixado e retorna um data frame
limpa_dados_2018 <- function(ano, data_frame_meses) {
  
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
        areas_tabela <- list(c("top" = 182.92649, "left" = 41.92467, "bottom" = 627.97304, "right" = 932.01777))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Excluir coluna excedente
        m_tabelas[[2]] <- m_tabelas[[2]][,-c(4)]
        m_tabelas[[3]] <- m_tabelas[[3]][,-c(4)]
        m_tabelas[[9]] <- m_tabelas[[9]][,-c(4)]
        m_tabelas[[19]] <- m_tabelas[[19]][,-c(4)]
        m_tabelas[[27]] <- m_tabelas[[27]][,-c(4)]
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        m_tabela[627,4] <- m_tabela[627,3]
        m_tabela[627,3] <- paste(m_tabela[626,3], m_tabela[628,3], sep = " ")
        m_tabela[663,2] <- m_tabela[660,2]
        for (n_linha in 620:nrow(m_tabela)) {
          if((!n_linha %in% c(626:628))) {
            NATUREZA_FATO <- substring(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-16, nchar(m_tabela[n_linha,3]))
            m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3])-nchar(NATUREZA_FATO))
            m_tabela[n_linha,4] <- NATUREZA_FATO
          }
        }

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
        areas_tabela <- list(c("top" = 200.00253, "left" = 99.61703, "bottom" = 687.22038, "right" = 992.54789))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Excluir coluna excedente
        m_tabelas[[14]] <- m_tabelas[[14]][,-c(4)]
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)

        for (n_linha in 449:nrow(m_tabela)) {
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
      
      if (num_doc == 3) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 182.92649, "left" = 45.14965, "bottom" = 665.06025, "right" = 927.18031))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Excluir coluna excedente
        m_tabelas[[6]] <- m_tabelas[[6]][,-c(4)]
        m_tabelas[[10]] <- m_tabelas[[10]][,-c(4)]
        m_tabelas[[11]] <- m_tabelas[[11]][,-c(4)]
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)

        for (n_linha in 521:nrow(m_tabela)) {
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
      
      if (num_doc == 4) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 196.44741, "left" = 88.14691, "bottom" = 679.49249, "right" = 973.14190))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Excluir coluna excedente
        m_tabelas[[5]] <- m_tabelas[[5]][,-c(4)]
        m_tabelas[[7]] <- m_tabelas[[7]][,-c(4)]
        m_tabelas[[10]] <- m_tabelas[[10]][,-c(4)]
        m_tabelas[[13]] <- m_tabelas[[13]][,-c(4)]
        m_tabelas[[15]] <- m_tabelas[[15]][,-c(4)]
        m_tabelas[[23]] <- m_tabelas[[23]][,-c(4)]
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
 
        for (n_linha in 434:nrow(m_tabela)) {
          m_tabela[n_linha,4] <- m_tabela[430,4]
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
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 196.44741, "left" = 88.14691, "bottom" = 679.49249, "right" = 976.66778))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Excluir coluna excedente
        m_tabelas[[4]] <- m_tabelas[[4]][,-c(4)]
        m_tabelas[[7]] <- m_tabelas[[7]][,-c(4)]
        m_tabelas[[8]] <- m_tabelas[[8]][,-c(4)]
        m_tabelas[[13]] <- m_tabelas[[13]][,-c(4)]
        m_tabelas[[17]] <- m_tabelas[[17]][,-c(4)]
        m_tabelas[[19]] <- m_tabelas[[19]][,-c(4)]
        m_tabelas[[22]] <- m_tabelas[[22]][,-c(4)]
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)

        # Corrige variaveis com valores quebrados nas colunas 3 e 4
        for (n_linha in 463:nrow(m_tabela)) {
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
      
      if (num_doc == 6) {
        # Usar locate_areas(file = arquivo, pages = 4, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 182.08880, "left" = 51.50438, "bottom" = 666.01848, "right" = 924.21826))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Inclui colunas faltantes para manter padrao
        m_tabelas[[25]] <- cbind(m_tabelas[[25]], c(NA))
 
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        # Corrige variaveis com valores quebrados nas colunas 3 e 4
        for (n_linha in 473:nrow(m_tabela)) {
          NATUREZA_FATO <- substring(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-16, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3])-nchar(NATUREZA_FATO))
          m_tabela[n_linha,4] <- NATUREZA_FATO
        }
        for (n_linha in 473:nrow(m_tabela)) {
          if(n_linha != 485) {
            IDADE <- substring(m_tabela[n_linha,9], nchar(m_tabela[n_linha,9])-1, nchar(m_tabela[n_linha,9]))
            m_tabela[n_linha,9] <- substr(m_tabela[n_linha,9], 0, nchar(m_tabela[n_linha,9])-nchar(IDADE))
            m_tabela[n_linha,10] <- IDADE
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
      
      if (num_doc == 7) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 205.56020, "left" = 97.23656, "bottom" = 781.90781, "right" = 1044.85104))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)
        
        # Corrige variaveis com valores quebrados nas colunas 3 e 4
        for (n_linha in 445:nrow(m_tabela)) {
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
      
      if (num_doc == 8) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 205.56020, "left" = 97.23656, "bottom" = 781.90781, "right" = 1044.85104))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Excluir coluna excedente
        m_tabelas[[4]] <- m_tabelas[[4]][,-c(4)]
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)

        # Corrige variaveis com valores quebrados nas colunas 3 e 4
        for (n_linha in 439:nrow(m_tabela)) {
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
      
      if (num_doc == 9) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 210.3131, "left" = 120.1603, "bottom" = 777.0394, "right" = 1036.6069))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Excluir coluna excedente
        m_tabelas[[16]] <- m_tabelas[[16]][,-c(4)]
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)

        # Corrige variaveis com valores quebrados nas colunas 3 e 4
        for (n_linha in 475:nrow(m_tabela)) {
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
        areas_tabela <- list(c("top" = 241.6826, "left" = 215.4898, "bottom" = 915.8884, "right" = 1158.5244))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)

        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)

        # Corrige variaveis com valores quebrados nas colunas 3 e 4
        m_tabela[447,4] <- m_tabela[447,3]
        m_tabela[447,3] <- paste(m_tabela[446,3], m_tabela[448,3], sep = " ")
        for (n_linha in 414:444) {
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
        areas_tabela <- list(c("top" = 200.48081, "left" = 79.67382, "bottom" = 761.58789, "right" = 1013.72217))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Excluir coluna excedente
        m_tabelas[[14]] <- m_tabelas[[14]][,-c(4)]
        m_tabelas[[15]] <- m_tabelas[[15]][,-c(4)]
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)

        # Corrige variaveis com valores quebrados nas colunas 3 e 4
        for (n_linha in 400:nrow(m_tabela)) {
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
      
      if (num_doc == 12) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 200.48081, "left" = 72.89307, "bottom" = 744.63602, "right" = 1017.11254))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Excluir coluna excedente
        m_tabelas[[11]] <- m_tabelas[[11]][,-c(4)]
        m_tabelas[[14]] <- m_tabelas[[14]][,-c(4)]
        
        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Concatena valores fragmentados das colunas 2, 3, 4 e 7
        m_tabela <- concatena_variaveis_matriz(m_tabela)

        # Corrige variaveis com valores quebrados nas colunas 3 e 4
        m_tabela[436,4] <- m_tabela[436,3]
        m_tabela[436,3] <- paste(m_tabela[435,3], m_tabela[437,3], sep = " ")
        m_tabela[439,4] <- m_tabela[439,3]
        m_tabela[439,3] <- paste(m_tabela[438,3], m_tabela[440,3], sep = " ")
        
        for (n_linha in 418:nrow(m_tabela)) {
          if(n_linha == 423) {
            NATUREZA_FATO <- substring(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-10, nchar(m_tabela[n_linha,3]))
            m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3])-nchar(NATUREZA_FATO))
            m_tabela[n_linha,4] <- NATUREZA_FATO
          }
          if(!(n_linha %in% c(423,435:441))) {
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
    }
  }

  # Padroniza o formato da data
  df_tabela[,6] <- df_tabela[,6] %>% 
    gsub("-Apr-", "/04/", .) %>% 
    gsub("-May-", "/05/", .)
  
  # Padroniza valores da coluna 4, 5, 9 e 10  
  df_tabela <- df_tabela %>% mutate(V3 = as.character(V3),V4 = as.character(V4),V5 = as.character(V5),V9 = as.character(V9))
  df_tabela <- padroniza_colunas_inconsistentes(df_tabela)
  
  print(paste("Finalizado", total_pgs, "pagina(s).", sep = " ")) 
  print(paste(nrow(df_tabela), "Linhas", sep = " ")) 
  
  return(df_tabela)
}

