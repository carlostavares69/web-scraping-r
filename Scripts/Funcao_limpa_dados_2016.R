#===========================================================================================
#                                 LIMPEZA DADOS 2016                                      
#-------------------------------------------------------------------------------------------

# Funcao de limpeza dos dados da matriz por documento baixado e retorna um data frame
limpa_dados_2016 <- function(ano, data_frame_meses) {
  
  print(paste("Definindo estrutura de limpeza para", nrow(data_frame_meses[2]), "meses.", sep = " "))
  
  df_nome_docs <- rbind(data_frame_meses[2])
  m_tabelas <- m_tabela <- matrix()

  total_pgs <- 0
  
  for (num_doc in 1:nrow(df_nome_docs)) {
    nome <- as.character(data_frame_meses[num_doc,][2]$mes)
    arquivo <- file.path(".", dir_arquivos, ano, paste0(nome, ".pdf"))
    
    if(file.exists(arquivo)) {
      if (num_doc == 1) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 136.12198, "left" = 27.82115, "bottom" = 539.81705, "right" = 813.19157))
        
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
        
        # Corrige variaveis de linhas com valores fragmentados 
        # m_tabela[432,2] <- m_tabela[429,2]

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

        # converte matriz em data frame.
        df_tabela <- as.data.frame(m_tabela)
      }
      
      if (num_doc == 2) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 150.44530, "left" = 23.13406, "bottom" = 594.37012, "right" = 910.98369))
        
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

        # Corrige variaveis de linhas com valores fragmentados 
        m_tabela[381,4] <- paste(m_tabela[380,3], m_tabela[382,3], sep = " ")
        m_tabela[384,4] <- paste(m_tabela[383,3], m_tabela[385,3], sep = " ")
        
        # Corrige variaveis de linhas com valores quebrados 
        for (n_linha in 378:nrow(m_tabela)) {
          if (n_linha != 381 & n_linha != 384) {
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
      
      if (num_doc == 3) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 128.78207, "left" = 27.82115, "bottom" = 533.81167, "right" = 813.19157))
        
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
        areas_tabela <- list(c("top" = 193.29418, "left" = 36.94088, "bottom" = 647.66101, "right" = 927.26558))
        
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
      
      if (num_doc == 5) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 161.49687, "left" = 48.65568, "bottom" = 625.90495, "right" = 921.40818))
        
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
        for (n_linha in 348:372) {
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
      
      if (num_doc == 6) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 194.96772, "left" = 46.98214, "bottom" = 642.64038, "right" = 917.22432))
        
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
      
      if (num_doc == 7) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 154.80108, "left" = 45.07588, "bottom" = 672.85695, "right" = 914.95238))
        
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
        for (n_linha in 320:nrow(m_tabela)) {
          NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-15, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,4] <- NATUREZA_FATO
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
        }
        m_tabela[323,3] <- paste(m_tabela[322,4], m_tabela[324,4], sep = " ")
        
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
        # Usar locate_areas(file = arquivo, pages = 2, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 209.48879, "left" = 44.07175, "bottom" = 720.86099, "right" = 936.91480))
        
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
      
      if (num_doc == 9) {
        # Usar locate_areas(file = arquivo, pages = 2, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 182.41615, "left" = 47.81891, "bottom" = 649.33455, "right" = 916.38755))
        
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
        # Usar locate_areas(file = arquivo, pages = 2, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 183.25292, "left" = 46.98214, "bottom" = 644.31392, "right" = 917.22432))
        
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
      
      if (num_doc == 11) {
        # Usar locate_areas(file = arquivo, pages = 2, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 183.25292, "left" = 46.98214, "bottom" = 656.02872, "right" = 918.06109))
        
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
        m_tabela[384,4] <- m_tabela[384,3]
        for (n_linha in 376:390) {
          NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-15, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,4] <- NATUREZA_FATO
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
        }
        m_tabela[384,3] <-  m_tabela[246,3]
        m_tabela[393,4] <- paste(m_tabela[392,3], m_tabela[394,3], sep = " ")
        
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
        # Usar locate_areas(file = arquivo, pages = 2, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 184.0897, "left" = 57.0234, "bottom" = 645.9875, "right" = 911.3669))
        
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
    }
  }

  # Padroniza o formato da data
  df_tabela[,6] <- df_tabela[,6] %>% gsub("-Feb-", "/02/", .)
  
  print(paste("Finalizado", total_pgs, "pagina(s).", sep = " ")) 
  print(paste(nrow(df_tabela), "Linhas", sep = " ")) 
  
  return(df_tabela)
}