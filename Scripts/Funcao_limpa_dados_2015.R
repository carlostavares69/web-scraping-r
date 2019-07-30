#===========================================================================================
#                                 LIMPEZA DADOS 2015                                      
#-------------------------------------------------------------------------------------------

# Funcao de limpeza dos dados da matriz por documento baixado e retorna um data frame
limpa_dados_2015 <- function(ano, data_frame_meses) {
  
  print(paste("Definindo estrutura de limpeza para", nrow(data_frame_meses[2]), "meses.", sep = " "))
  
  df_nome_docs <- rbind(data_frame_meses[2])
  m_tabelas <- m_tabela <- matrix()
  
  # ######################### temp
  # ano <- lista_anos[4, 2]
  # data_frame_meses <- df_lista_meses
  # nome <- "JANEIRO-2015"
  # glimpse(m_tabelas)
  # View(m_tabela)
  # ######################### temp
  
  total_pgs <- 0
  
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
        
        # concatena o titulo fragmentado da coluna 8 e padroniza
        m_tabela[2,8] <- paste0(m_tabela[1,8], m_tabela[3,8])
        
        # limpa caracteres indesejados e padroniza titulos para obter cabecalho
        m_tabela_cabecalho <- m_tabela[2,]  %>% 
          gsub("-/", "_", .) %>% 
          gsub("-", "_", .) %>% 
          gsub(" ", "_", .) %>% 
          gsub("^\\s+|\\s+$", "", .) %>%  remove_acentos(.)

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
        
        # Substitui o ifen barra por NA nas linhas da coluna 8
        m_tabela[,8] <- m_tabela[,8] %>% gsub("-/", NA, .)
        # Substitui o ifen por NA nas linhas da coluna 10
        m_tabela[,10] <- m_tabela[,10] %>% gsub("-", NA, .)
        # Substitui o F ou M por Masculino ou Feminino nas linhas da coluna 9
        m_tabela[,9] <- m_tabela[,9] %>% gsub("M", "Masculino", .) %>% gsub("F", "Feminino", .)

        # exclui linhas desnecessarias 
        for (linha in rev(1:length(m_tabela[,1]))) {
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }

        # converte matriz em data frame.
        df_tabela <- as.data.frame(m_tabela)
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
        
        # concatena o titulo fragmentado da coluna 8 e padroniza
        m_tabela[2,8] <- paste0(m_tabela[1,8], m_tabela[3,8])
      
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
        
        # Substitui o ifen barra por NA nas linhas da coluna 8
        m_tabela[,8] <- m_tabela[,8] %>% gsub("-/", NA, .)
        # Substitui o ifen por NA nas linhas da coluna 10
        m_tabela[,10] <- m_tabela[,10] %>% gsub("-", NA, .)
        # Substitui o F ou M por Masculino ou Feminino nas linhas da coluna 9
        m_tabela[,9] <- m_tabela[,9] %>% gsub("M", "Masculino", .) %>% gsub("F", "Feminino", .)

        # exclui linhas desnecessarias 
        for (linha in rev(1:length(m_tabela[,1]))) {
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
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
        
        # concatena o titulo fragmentado da coluna 8 e padroniza
        m_tabela[2,8] <- paste0(m_tabela[1,8], m_tabela[3,8])
        
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
        
        # Substitui o ifen barra por NA nas linhas da coluna 8
        m_tabela[,8] <- m_tabela[,8] %>% gsub("-/", NA, .)
        # Substitui o ifen por NA nas linhas da coluna 10
        m_tabela[,10] <- m_tabela[,10] %>% gsub("-", NA, .)
        # Substitui o F ou M por Masculino ou Feminino nas linhas da coluna 9
        m_tabela[,9] <- m_tabela[,9] %>% gsub("M", "Masculino", .) %>% gsub("F", "Feminino", .)

        # exclui linhas desnecessarias 
        for (linha in rev(1:length(m_tabela[,1]))) {
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
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
        
        # concatena o titulo fragmentado da coluna 8 e padroniza
        m_tabela[2,8] <- paste0(m_tabela[1,8], m_tabela[3,8])
        
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
        
        # Substitui o ifen barra por NA nas linhas da coluna 8
        m_tabela[,8] <- m_tabela[,8] %>% gsub("-/", NA, .)
        # Substitui o ifen por NA nas linhas da coluna 10
        m_tabela[,10] <- m_tabela[,10] %>% gsub("-", NA, .)
        # Substitui o F ou M por Masculino ou Feminino nas linhas da coluna 9
        m_tabela[,9] <- m_tabela[,9] %>% gsub("M", "Masculino", .) %>% gsub("F", "Feminino", .)
        
        #Corrige variaveis de linhas com valores quebrados
        for (n_linha in 416:nrow(m_tabela)) {
          NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-15, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,4] <- NATUREZA_FATO
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
        }
        
        # exclui linhas desnecessarias 
        for (linha in rev(1:length(m_tabela[,1]))) {
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
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
        
        # concatena o titulo fragmentado da coluna 8 e padroniza
        m_tabela[2,8] <- paste0(m_tabela[1,8], m_tabela[3,8])
        
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
        
        # Substitui o ifen barra por NA nas linhas da coluna 8
        m_tabela[,8] <- m_tabela[,8] %>% gsub("-/", NA, .)
        # Substitui o ifen por NA nas linhas da coluna 10
        m_tabela[,10] <- m_tabela[,10] %>% gsub("-", NA, .)
        # Substitui o F ou M por Masculino ou Feminino nas linhas da coluna 9
        m_tabela[,9] <- m_tabela[,9] %>% gsub("M", "Masculino", .) %>% gsub("F", "Feminino", .)

        # exclui linhas desnecessarias 
        for (linha in rev(1:length(m_tabela[,1]))) {
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
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
        
        # concatena o titulo fragmentado da coluna 8 e padroniza
        m_tabela[2,8] <- paste0(m_tabela[1,8], m_tabela[3,8])
        
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
        
        # Substitui o ifen barra por NA nas linhas da coluna 8
        m_tabela[,8] <- m_tabela[,8] %>% gsub("-/", NA, .)
        # Substitui o ifen por NA nas linhas da coluna 10
        m_tabela[,10] <- m_tabela[,10] %>% gsub("-", NA, .)
        # Substitui o F ou M por Masculino ou Feminino nas linhas da coluna 9
        m_tabela[,9] <- m_tabela[,9] %>% gsub("M", "Masculino", .) %>% gsub("F", "Feminino", .)
        
        #Corrige variaveis de linhas com valores quebrados
        for (n_linha in 46:62) {
          NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-15, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,4] <- NATUREZA_FATO
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
        }
        
        # exclui linhas desnecessarias 
        for (linha in rev(1:length(m_tabela[,1]))) {
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
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
        
        # concatena o titulo fragmentado da coluna 8 e padroniza
        m_tabela[2,8] <- paste0(m_tabela[1,8], m_tabela[3,8])
        
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
        
        # Substitui o ifen barra por NA nas linhas da coluna 8
        m_tabela[,8] <- m_tabela[,8] %>% gsub("-/", NA, .)
        # Substitui o ifen por NA nas linhas da coluna 10
        m_tabela[,10] <- m_tabela[,10] %>% gsub("-", NA, .)
        # Substitui o F ou M por Masculino ou Feminino nas linhas da coluna 9
        m_tabela[,9] <- m_tabela[,9] %>% gsub("M", "Masculino", .) %>% gsub("F", "Feminino", .)
        
        # Corrige variaveis de linhas com valores quebrados
        m_tabela[327,4] <- paste(m_tabela[326,3], m_tabela[328,3], sep = " ")
        for (n_linha in 329:nrow(m_tabela)) {
          NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-15, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,4] <- NATUREZA_FATO
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
        }
        
        # exclui linhas desnecessarias 
        for (linha in rev(1:length(m_tabela[,1]))) {
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
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
        
        # concatena o titulo fragmentado da coluna 8 e padroniza
        m_tabela[2,8] <- paste0(m_tabela[1,8], m_tabela[3,8])
        
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
        
        # Substitui o ifen barra por NA nas linhas da coluna 8
        m_tabela[,8] <- m_tabela[,8] %>% gsub("-/", NA, .)
        # Substitui o ifen por NA nas linhas da coluna 10
        m_tabela[,10] <- m_tabela[,10] %>% gsub("-", NA, .)
        # Substitui o F ou M por Masculino ou Feminino nas linhas da coluna 9
        m_tabela[,9] <- m_tabela[,9] %>% gsub("M", "Masculino", .) %>% gsub("F", "Feminino", .)
        
        # exclui linhas desnecessarias 
        for (linha in rev(1:length(m_tabela[,1]))) {
          # Substitui valores vazios por NA nas linhas da coluna 5
          if(m_tabela[linha,5] == "") {
            m_tabela[linha,5] <- NA
          }
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
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
        
        # concatena o titulo fragmentado da coluna 8 e padroniza
        m_tabela[2,8] <- paste0(m_tabela[1,8], m_tabela[3,8])
        
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
        
        # Substitui o ifen barra por NA nas linhas da coluna 8
        m_tabela[,8] <- m_tabela[,8] %>% gsub("-/", NA, .)
        # Substitui o ifen por NA nas linhas da coluna 10
        m_tabela[,10] <- m_tabela[,10] %>% gsub("-", NA, .)
        # Substitui o F ou M por Masculino ou Feminino nas linhas da coluna 9
        m_tabela[,9] <- m_tabela[,9] %>% gsub("M", "Masculino", .) %>% gsub("F", "Feminino", .)
        
        # exclui linhas desnecessarias 
        for (linha in rev(1:length(m_tabela[,1]))) {
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
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
        
        # concatena o titulo fragmentado da coluna 8 e padroniza
        m_tabela[2,8] <- paste0(m_tabela[1,8], m_tabela[3,8])
        
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
        
        # Substitui o ifen barra por NA nas linhas da coluna 8
        m_tabela[,8] <- m_tabela[,8] %>% gsub("-/", NA, .)
        # Substitui o ifen por NA nas linhas da coluna 10
        m_tabela[,10] <- m_tabela[,10] %>% gsub("-", NA, .)
        # Substitui o F ou M por Masculino ou Feminino nas linhas da coluna 9
        m_tabela[,9] <- m_tabela[,9] %>% gsub("M", "Masculino", .) %>% gsub("F", "Feminino", .)
        # Substitui o ifen por NA nas linhas da coluna 3
        m_tabela[,3] <- m_tabela[,3] %>% gsub("-", NA, .)
        
        #Corrige variaveis de linhas com valores quebrados
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
        
        for (linha in rev(1:length(m_tabela[,1]))) {
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }

        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
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
        
        # concatena o titulo fragmentado da coluna 8 e padroniza
        m_tabela[2,8] <- paste0(m_tabela[1,8], m_tabela[3,8])
        
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
        
        # Substitui o ifen barra por NA nas linhas da coluna 8
        m_tabela[,8] <- m_tabela[,8] %>% gsub("-/", NA, .)
        # Substitui o ifen por NA nas linhas da coluna 10
        m_tabela[,10] <- m_tabela[,10] %>% gsub("-", NA, .)
        # Substitui o F ou M por Masculino ou Feminino nas linhas da coluna 9
        m_tabela[,9] <- m_tabela[,9] %>% gsub("M", "Masculino", .) %>% gsub("F", "Feminino", .)

        # exclui linhas desnecessarias 
        for (linha in rev(1:length(m_tabela[,1]))) {
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
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
        
        # concatena o titulo fragmentado da coluna 8 e padroniza
        m_tabela[2,8] <- paste0(m_tabela[1,8], m_tabela[3,8])
        
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
        
        # Substitui o ifen barra por NA nas linhas da coluna 8
        m_tabela[,8] <- m_tabela[,8] %>% gsub("-/", NA, .)
        # Substitui o ifen por NA nas linhas da coluna 10
        m_tabela[,10] <- m_tabela[,10] %>% gsub("-", NA, .)
        # Substitui o F ou M por Masculino ou Feminino nas linhas da coluna 9
        m_tabela[,9] <- m_tabela[,9] %>% gsub("M", "Masculino", .) %>% gsub("F", "Feminino", .)
        
        #Corrige variaveis de linhas com valores quebrados
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
        
        # exclui linhas desnecessarias 
        for (linha in rev(1:length(m_tabela[,1]))) {
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
      } 
    }
  }
  
  print(paste("Finalizado", total_pgs, "pagina(s).", sep = " ")) 

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
  
  return(df_tabela)
}