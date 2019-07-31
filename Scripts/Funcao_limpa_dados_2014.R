#===========================================================================================
#                                 LIMPEZA DADOS 2014                                      
#-------------------------------------------------------------------------------------------

# Funcao de limpeza dos dados da matriz por documento baixado e retorna um data frame
limpa_dados_2014 <- function(ano, data_frame_meses) {
  
  print(paste("Definindo estrutura de limpeza para", nrow(data_frame_meses[2]), "meses.", sep = " "))
  
  df_nome_docs <- rbind(data_frame_meses[2])
  m_tabelas <- m_tabela <- matrix()
  
  # ######################### temp
  # ano <- lista_anos[5, 2]
  # data_frame_meses <- df_lista_meses
  # nome <- "JANEIRO-2014"
  # glimpse(m_tabelas)
  # View(m_tabela)
  # ######################### temp
  
  total_pgs <- 0
  
  for (num_doc in 1:nrow(df_nome_docs)) {
    nome <- as.character(data_frame_meses[num_doc,][2]$mes)
    arquivo <- file.path(".", dir_arquivos, ano, paste0(nome, ".pdf"))
    
    if(file.exists(arquivo)) {
      if (num_doc == 1) {
        # Importanto do arquivo EXCEL previamente convertido do PDF
        arquivo <- file.path(".", sub_dir1, paste0(nome, ".xlsx"))
        m_tabela <- suppressMessages(readxl::read_xlsx(path = arquivo, col_names = FALSE, skip = 2))
        
        # Inclui coluna ID faltante com variaveis numerica e reordena 
        m_tabela <- m_tabela %>% cbind(., ...0 = c(1:nrow(.))) %>% .[, c(10,1,2,3,4,5,6,7,8,9)]
        
        # Substitui o ifen barra por NA nas linhas da coluna 8 e 10
        for (linha in rev(1:nrow(m_tabela))) {
          if(!is.na(m_tabela[linha,8])) {
            if(m_tabela[linha,8] == "-" | m_tabela[linha,8] == "-/") {
              m_tabela[linha,8] <- NA
            }
          }
          if(m_tabela[linha,10] == "-") {
            m_tabela[linha,10] <- NA
          }
          # Substitui o F ou M por Masculino ou Feminino nas linhas da coluna 9
          if(m_tabela[linha,9] == "M") {
            m_tabela[linha,9] <- "Masculino"
          } else {
            m_tabela[linha,9] <- "Feminino"
          }
        }
        
      }
      
      if (num_doc == 2) {
        # Importanto do arquivo EXCEL previamente convertido do PDF
        arquivo <- file.path(".", sub_dir1, paste0(nome, ".xlsx"))
        m_tabela <- suppressMessages(readxl::read_xlsx(path = arquivo, col_names = FALSE, skip = 2))
        
        # Corrige variaveis de linhas com valores quebrados
        m_tabela[116,3] <- paste(m_tabela[116,3], m_tabela[117,3], sep = " ")
        
        # exclui linha irrelevante
        m_tabela <- m_tabela[-c(117),]
        
        # Substitui o ifen barra por NA nas linhas da coluna 8 e 10
        for (linha in rev(1:nrow(m_tabela))) {
          if(!is.na(m_tabela[linha,8])) {
            if(m_tabela[linha,8] == "-" | m_tabela[linha,8] == "-/") {
              m_tabela[linha,8] <- NA
            }
          }
          if(m_tabela[linha,10] == "-") {
            m_tabela[linha,10] <- NA
          }
          # Substitui o F ou M por Masculino ou Feminino nas linhas da coluna 9
          if(m_tabela[linha,9] == "M") {
            m_tabela[linha,9] <- "Masculino"
          } else {
            m_tabela[linha,9] <- "Feminino"
          }
        }

        # faz merge de linhas do data frame anterior.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
      }
      
      if (num_doc == 3) {
        # Importanto do arquivo EXCEL previamente convertido do PDF
        arquivo <- file.path(".", sub_dir1, paste0(nome, ".xlsx"))
        m_tabela <- suppressMessages(readxl::read_xlsx(path = arquivo, col_names = FALSE, skip = 2))
        
        # Corrige variaveis de linhas com valores quebrados
        m_tabela[116,3] <- paste(m_tabela[116,3], m_tabela[117,3], sep = " ")

        # Substitui o ifen barra por NA nas linhas da coluna 8 e 10
        for (linha in rev(1:nrow(m_tabela))) {
          if(!is.na(m_tabela[linha,8])) {
            if(m_tabela[linha,8] == "-" | m_tabela[linha,8] == "-/") {
              m_tabela[linha,8] <- NA
            }
          }
          if(m_tabela[linha,10] == "-") {
            m_tabela[linha,10] <- NA
          }
          # Substitui o F ou M por Masculino ou Feminino nas linhas da coluna 9
          if(m_tabela[linha,9] == "M") {
            m_tabela[linha,9] <- "Masculino"
          } else {
            m_tabela[linha,9] <- "Feminino"
          }
        }
        
        # faz merge de linhas do data frame anterior.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
      }
      
      if (num_doc == 4) {
        # Importanto do arquivo EXCEL previamente convertido do PDF
        arquivo <- file.path(".", sub_dir1, paste0(nome, ".xlsx"))
        m_tabela <- suppressMessages(readxl::read_xlsx(path = arquivo, col_names = FALSE, skip = 2))
        
        # Corrige variaveis de linhas com valores quebrados
        m_tabela[116,3] <- paste(m_tabela[116,3], m_tabela[117,3], sep = " ")
        
        # Substitui o ifen barra por NA nas linhas da coluna 5, 8 e 10
        for (linha in rev(1:nrow(m_tabela))) {
          if(!is.na(m_tabela[linha,5])) {
            if(m_tabela[linha,5] == "-") {
              m_tabela[linha,5] <- NA
            }
          }
          if(!is.na(m_tabela[linha,8])) {
            if(m_tabela[linha,8] == "-" | m_tabela[linha,8] == "-/") {
              m_tabela[linha,8] <- NA
            }
          }
          if(!is.na(m_tabela[linha,10])) {
            if(m_tabela[linha,10] == "-") {
              m_tabela[linha,10] <- NA
            }
          }
          # Substitui o F ou M por Masculino ou Feminino nas linhas da coluna 9
          if(!is.na(m_tabela[linha,9])) {
            if(m_tabela[linha,9] == "M") {
              m_tabela[linha,9] <- "Masculino"
            } else {
              m_tabela[linha,9] <- "Feminino"
            }
          }
          # exclui linha irrelevante
          if(is.na(m_tabela[linha,1])) {
            m_tabela <- m_tabela[-c(linha),]
          }
        }
        
        # faz merge de linhas do data frame anterior.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
      }
      
      if (num_doc == 5) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 140.11282, "left" = 13.02539, "bottom" = 545.20235, "right" = 815.38923))
        
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
        m_tabela[,8] <- m_tabela[,8] %>% gsub("-", NA, .) %>% gsub("-/", NA, .)
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
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 140.11282, "left" = 14.32793, "bottom" = 547.80743, "right" = 824.50700))
        
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
        
        # Substitui o ifen barra por NA nas linhas da coluna 5, 8 e 10
        for (linha in rev(1:nrow(m_tabela))) {
          if(!is.na(m_tabela[linha,5])) {
            if(m_tabela[linha,5] == "") {
              m_tabela[linha,5] <- NA
            }
          }
          if(!is.na(m_tabela[linha,8])) {
            if(m_tabela[linha,8] == "-" | m_tabela[linha,8] == "-/") {
              m_tabela[linha,8] <- NA
            }
          }
          if(!is.na(m_tabela[linha,10])) {
            if(m_tabela[linha,10] == "-") {
              m_tabela[linha,10] <- NA
            }
          }
          # Substitui o F ou M por Masculino ou Feminino nas linhas da coluna 9
          if(!is.na(m_tabela[linha,9])) {
            if(m_tabela[linha,9] == "M") {
              m_tabela[linha,9] <- "Masculino"
            } else {
              m_tabela[linha,9] <- "Feminino"
            }
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
      
      if (num_doc == 7) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 138.81028, "left" = 14.32793, "bottom" = 555.62266, "right" = 812.78415))
        
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
        
        # Substitui o ifen barra por NA nas linhas da coluna 5, 8 e 10
        for (linha in rev(1:nrow(m_tabela))) {
          if(!is.na(m_tabela[linha,5])) {
            if(m_tabela[linha,5] == "") {
              m_tabela[linha,5] <- NA
            }
          }
          if(!is.na(m_tabela[linha,8])) {
            if(m_tabela[linha,8] == "-" | m_tabela[linha,8] == "-/") {
              m_tabela[linha,8] <- NA
            }
          }
          if(!is.na(m_tabela[linha,10])) {
            if(m_tabela[linha,10] == "-") {
              m_tabela[linha,10] <- NA
            }
          }
          # Substitui o F ou M por Masculino ou Feminino nas linhas da coluna 9
          if(!is.na(m_tabela[linha,9])) {
            if(m_tabela[linha,9] == "M") {
              m_tabela[linha,9] <- "Masculino"
            } else {
              m_tabela[linha,9] <- "Feminino"
            }
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
      
      if (num_doc == 8) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 137.50774, "left" = 14.32793, "bottom" = 549.10997, "right" = 815.38923))
        
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
        
        # Substitui o ifen barra por NA nas linhas da coluna 8 e 10
        for (linha in rev(1:nrow(m_tabela))) {
          if(!is.na(m_tabela[linha,8])) {
            if(m_tabela[linha,8] == "") {
              m_tabela[linha,8] <- NA
            }
          }
          if(!is.na(m_tabela[linha,10])) {
            if(m_tabela[linha,10] == "") {
              m_tabela[linha,10] <- NA
            }
          }
          # Substitui o F ou M por Masculino ou Feminino nas linhas da coluna 9
          if(!is.na(m_tabela[linha,9])) {
            if(m_tabela[linha,9] == "M") {
              m_tabela[linha,9] <- "Masculino"
            } else {
              m_tabela[linha,9] <- "Feminino"
            }
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
      
      if (num_doc == 9) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 137.50774, "left" = 14.32793, "bottom" = 546.50489, "right" = 815.38923))
        
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
        
        # Substitui o ifen barra por NA nas linhas da coluna 8 e 10
        for (linha in rev(1:nrow(m_tabela))) {
          if(!is.na(m_tabela[linha,8])) {
            if(m_tabela[linha,8] == "-") {
              m_tabela[linha,8] <- NA
            }
          }
          if(!is.na(m_tabela[linha,10])) {
            if(m_tabela[linha,10] == "-") {
              m_tabela[linha,10] <- NA
            }
          }
          # Substitui o F ou M por Masculino ou Feminino nas linhas da coluna 9
          if(!is.na(m_tabela[linha,9])) {
            if(m_tabela[linha,9] == "M") {
              m_tabela[linha,9] <- "Masculino"
            } else {
              m_tabela[linha,9] <- "Feminino"
            }
          }
        }
        
        # Corrige variaveis de linhas com valores quebrados
        m_tabela[481,3] <- m_tabela[481,2]
        m_tabela[484,3] <- m_tabela[484,2]
        m_tabela[481,2] <- paste(m_tabela[480,2], m_tabela[482,2], sep = " ")
        m_tabela[484,2] <- paste(m_tabela[483,2], m_tabela[485,2], sep = " ")
        m_tabela[487,3] <- m_tabela[358,3]
        for (n_linha in 473:478) {
          MUNICIPIO <- substring(m_tabela[n_linha,2], 7, nchar(m_tabela[n_linha,2]))
          m_tabela[n_linha,3] <- MUNICIPIO
          m_tabela[n_linha,2] <- substr(m_tabela[n_linha,2], 0, nchar(m_tabela[n_linha,2]) - nchar(MUNICIPIO) )
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
      
      if (num_doc == 10) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 134.73623, "left" = 10.42328, "bottom" = 549.06168, "right" = 805.19851))
        
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
        
        # Substitui o ifen barra por NA nas linhas da coluna 8 e 10
        for (linha in rev(1:nrow(m_tabela))) {
          if(!is.na(m_tabela[linha,8])) {
            if(m_tabela[linha,8] == "-") {
              m_tabela[linha,8] <- NA
            }
          }
          if(!is.na(m_tabela[linha,10])) {
            if(m_tabela[linha,10] == "-") {
              m_tabela[linha,10] <- NA
            }
          }
          # Substitui o F ou M por Masculino ou Feminino nas linhas da coluna 9
          if(!is.na(m_tabela[linha,9])) {
            if(m_tabela[linha,9] == "M") {
              m_tabela[linha,9] <- "Masculino"
            } else {
              m_tabela[linha,9] <- "Feminino"
            }
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
      
      if (num_doc == 11) {
        # Usar locate_areas(file = arquivo, pages = 1, resolution = 100L, widget = c("native"))
        # para obter a lista da area padrao para o restante das paginas do documento
        #areas_tabela <- list(c("top" = 136.48209, "left" = 12.88943, "bottom" = 547.65501, "right" = 804.30064))
        areas_tabela <- list(c("top" = 156.91637, "left" = 15.68289, "bottom" = 536.76219, "right" = 788.36073))
        
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo, areas_tabela)

        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Inclui colunas faltantes para manter padrao e reordena colunas
        m_tabelas[[1]] <- cbind(m_tabelas[[1]], c(NA)) %>% .[,c(10,1,2,3,4,5,6,7,8,9)]
        m_tabelas[[2]] <- cbind(m_tabelas[[2]], c(NA)) %>% .[,c(10,1,2,3,4,5,6,7,8,9)]
        m_tabelas[[3]] <- cbind(m_tabelas[[3]], c(NA), c(NA)) %>% .[,c(9,1,2,3,10,4,5,6,7,8)]
        m_tabelas[[5]] <- cbind(m_tabelas[[5]], c(NA), c(NA)) %>% .[,c(9,1,10,2,3,4,5,6,7,8)]
        m_tabelas[[6]] <- cbind(m_tabelas[[6]], c(NA)) %>% .[, c(10,1,2,3,4,5,6,7,8,9)]
        m_tabelas[[7]] <- cbind(m_tabelas[[7]], c(NA)) %>% .[, c(10,1,2,3,4,5,6,7,8,9)]
        m_tabelas[[8]] <- cbind(m_tabelas[[8]], c(NA), c(NA)) %>% .[,c(9,1,2,10,3,4,5,6,7,8)]
        m_tabelas[[9]] <- cbind(m_tabelas[[9]], c(NA), c(NA)) %>% .[,c(9,1,10,2,3,4,5,6,7,8)]
        m_tabelas[[10]] <- cbind(m_tabelas[[10]], c(NA), c(NA)) %>% .[,c(9,1,2,3,10,4,5,6,7,8)]
        m_tabelas[[12]] <- cbind(m_tabelas[[12]], c(NA), c(NA)) %>% .[,c(9,1,10,2,3,4,5,6,7,8)]
        m_tabelas[[13]] <- cbind(m_tabelas[[13]], c(NA), c(NA)) %>% .[,c(9,1,2,3,10,4,5,6,7,8)]
        m_tabelas[[14]] <- cbind(m_tabelas[[14]], c(NA), c(NA)) %>% .[,c(9,1,2,3,10,4,5,6,7,8)]
        m_tabelas[[15]] <- cbind(m_tabelas[[15]], c(NA)) %>% .[, c(10,1,2,3,4,5,6,7,8,9)]
        m_tabelas[[18]] <- cbind(m_tabelas[[18]], c(NA), c(NA)) %>% .[,c(9,1,2,10,3,4,5,6,7,8)]
        m_tabelas[[19]] <- cbind(m_tabelas[[19]], c(NA)) %>% .[, c(10,1,2,3,4,5,6,7,8,9)]
        m_tabelas[[20]] <- cbind(m_tabelas[[20]], c(NA), c(NA)) %>% .[,c(1,9,2,10,3,4,5,6,7,8)]
        m_tabelas[[21]] <- cbind(m_tabelas[[21]], c(NA)) %>% .[, c(10,1,2,3,4,5,6,7,8,9)]
        m_tabelas[[22]] <- cbind(m_tabelas[[22]], c(NA), c(NA)) %>% .[,c(9,1,2,3,10,4,5,6,7,8)]
        m_tabelas[[23]] <- cbind(m_tabelas[[23]], c(NA), c(NA)) %>% .[,c(9,1,2,3,4,5,6,7,8,10)]

        # Une as tabelas extraídas 
        m_tabela <- do.call(rbind, m_tabelas)
        
        # Corrige variaveis com valores quebrados entre colunas 2,3,4,5
        for (n_linha in 1:51) {
          ID <- substring(m_tabela[n_linha,2], 1, 2)
          m_tabela[n_linha,1] <- ID
          m_tabela[n_linha,2] <- substr(m_tabela[n_linha,2], 3, nchar(m_tabela[n_linha,2]))
        }
        for (n_linha in 69:99) {
          ID <- substring(m_tabela[n_linha,2], 1, 2)
          m_tabela[n_linha,1] <- ID
          m_tabela[n_linha,2] <- substr(m_tabela[n_linha,2], 3, nchar(m_tabela[n_linha,2]))
        }
        for (n_linha in 100:170) {
          ID <- substring(m_tabela[n_linha,2], 1, 3)
          m_tabela[n_linha,1] <- ID
          m_tabela[n_linha,2] <- substr(m_tabela[n_linha,2], 4, nchar(m_tabela[n_linha,2]))
        }
        for (n_linha in 188:255) {
          ID <- substring(m_tabela[n_linha,2], 1, 3)
          m_tabela[n_linha,1] <- ID
          m_tabela[n_linha,2] <- substr(m_tabela[n_linha,2], 4, nchar(m_tabela[n_linha,2]))
        }
        for (n_linha in 290:323) {
          ID <- substring(m_tabela[n_linha,2], 1, 3)
          m_tabela[n_linha,1] <- ID
          m_tabela[n_linha,2] <- substr(m_tabela[n_linha,2], 4, nchar(m_tabela[n_linha,2]))
        }
        for (n_linha in 341:nrow(m_tabela)) {
          ID <- substring(m_tabela[n_linha,2], 1, 3)
          m_tabela[n_linha,1] <- ID
          m_tabela[n_linha,2] <- substr(m_tabela[n_linha,2], 4, nchar(m_tabela[n_linha,2]))
        }
        
        for (n_linha in 35:51) {
          NATUREZA_FATO <- substring(m_tabela[n_linha,4], 0, 16)
          m_tabela[n_linha,5] <- substr(m_tabela[n_linha,4], 18, nchar(m_tabela[n_linha,4]))
          m_tabela[n_linha,4] <- NATUREZA_FATO
        }
        
        for (n_linha in 69:85) {
          MUNICIPIO <- substring(m_tabela[n_linha,2], 0, 8) 
          m_tabela[n_linha,3] <- substring(m_tabela[n_linha,2], nchar(MUNICIPIO), nchar(m_tabela[n_linha,2]))
          m_tabela[n_linha,2] <- MUNICIPIO
        }
        
        for (n_linha in 120:136) {
          NATUREZA_FATO <- substring(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-16, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,4] <- NATUREZA_FATO
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3])-nchar(NATUREZA_FATO))
        }
        
        for (n_linha in 141:153) {
          MUNICIPIO <- substring(m_tabela[n_linha,2], 0, 8) 
          m_tabela[n_linha,3] <- substring(m_tabela[n_linha,2], nchar(MUNICIPIO), nchar(m_tabela[n_linha,2]))
          m_tabela[n_linha,2] <- MUNICIPIO
        }
        
        for (n_linha in 156:170) {
          ARMA_UTILIZADA <- substring(m_tabela[n_linha,4], 18, nchar(m_tabela[n_linha,4]))
          m_tabela[n_linha,5] <- ARMA_UTILIZADA
          m_tabela[n_linha,4] <- substr(m_tabela[n_linha,4], 0, nchar(m_tabela[n_linha,4])-nchar(ARMA_UTILIZADA))
        }
        
        for (n_linha in 188:204) {
          NATUREZA_FATO <- substring(m_tabela[n_linha,4], nchar(m_tabela[n_linha,4])-16, nchar(m_tabela[n_linha,4]))
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,4], 0, nchar(m_tabela[n_linha,4])-nchar(NATUREZA_FATO))
          m_tabela[n_linha,4] <- NATUREZA_FATO
        }
        
        for (n_linha in 205:238) {
          ARMA_UTILIZADA <- substring(m_tabela[n_linha,4], 18, nchar(m_tabela[n_linha,4]))
          m_tabela[n_linha,5] <- ARMA_UTILIZADA
          m_tabela[n_linha,4] <- substr(m_tabela[n_linha,4], 0, nchar(m_tabela[n_linha,4])-nchar(ARMA_UTILIZADA))
        }
        
        for (n_linha in 239:255) {
          NATUREZA_FATO <- substring(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-16, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3])-nchar(NATUREZA_FATO))
          m_tabela[n_linha,4] <- NATUREZA_FATO
        }
        
        for (n_linha in 290:306) {
          NATUREZA_FATO <- substring(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-16, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3])-nchar(NATUREZA_FATO))
          m_tabela[n_linha,4] <- NATUREZA_FATO
        }
        
        for (n_linha in 324:340) {
          NATUREZA_FATO <- substring(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-16, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3])-nchar(NATUREZA_FATO))
          m_tabela[n_linha,4] <- NATUREZA_FATO
        }
        
        for (n_linha in 359:374) {
          ARMA_UTILIZADA <- substring(m_tabela[n_linha,4], 18, nchar(m_tabela[n_linha,4]))
          m_tabela[n_linha,5] <- ARMA_UTILIZADA
          m_tabela[n_linha,4] <- substr(m_tabela[n_linha,4], 0, nchar(m_tabela[n_linha,4])-nchar(ARMA_UTILIZADA))
        }
        
        # Corrige variaveis com valores quebrados da coluna 3
        m_tabela[7,4] <- substring(m_tabela[7,3], 16, nchar(m_tabela[7,3]))
        m_tabela[7,3] <- substr(m_tabela[7,3], 0, nchar(m_tabela[7,3]) - nchar(m_tabela[7,4]))
        
        for (n_linha in 1:length(m_tabela)) {
          if(n_linha == 15) {
            m_tabela[n_linha,3] <- c("JUAZEIRO DO NORTE")
          }
          if(n_linha %in% c(72, 76, 122, 123) ) {
            m_tabela[n_linha,3] <- m_tabela[15,3]
          }
          if(n_linha %in% c(137,138) ) {
            m_tabela[n_linha,3] <- c("FORTALEZA")
          }
          if(n_linha %in% c(139,140) ) {
            m_tabela[n_linha,3] <- c("MULUNGU")
          }
          if(n_linha == 144) {
            m_tabela[n_linha,3] <- c("LIMOEIRO DO NORTE")
          }
          if(n_linha == 191) {
            m_tabela[n_linha,3] <- c("JIJOCA DE JERICOACOARA")
          }
          if(n_linha %in% c(194, 233, 241)) {
            m_tabela[n_linha,3] <- c("SÃO GONÇALO DO AMARANTE")
          }
          if(n_linha == 300) {
            m_tabela[n_linha,3] <- c("TABULEIRO DO NORTE")
          }
          if(n_linha == 303) {
            m_tabela[n_linha,3] <- c("NOVA JAGUARIBARA")
          }
          if(n_linha %in% c(308, 326)) {
            m_tabela[n_linha,3] <- m_tabela[15,3]
          }
          if(n_linha == 331) {
            m_tabela[n_linha,3] <- m_tabela[251,3]
          }
          if(n_linha == 337) {
            m_tabela[n_linha,3] <- m_tabela[191,3]
          }
          if(n_linha %in% c(332, 358)) {
            m_tabela[n_linha,3] <- m_tabela[1,3]
          }
          if(n_linha == 373) {
            m_tabela[n_linha,3] <- m_tabela[15,3]
          }
          if(n_linha == 375) {
            m_tabela[n_linha,3] <- m_tabela[71,3]
          }
        }
        
        # Corrige variaveis com valores quebrados da coluna 4
        for (n_linha in 1:length(m_tabela)) {
          if(n_linha %in% c(31, 40, 99, 154, 155, 212, 230, 331)) {
            m_tabela[n_linha,4] <- c("ROUBO SEGUIDO DE MORTE (LATROCINIO)")
          }
          if(n_linha %in% c(122, 123, 133, 191, 194, 241, 300, 303, 326, 334, 335, 337)) {
            m_tabela[n_linha,4] <- m_tabela[1,4]
          }
          if(n_linha %in% c(161, 358)) {
            m_tabela[n_linha,4] <- c("LESAO CORPORAL SEGUIDA DE MORTE")
          }
          if(n_linha == 332) {
            m_tabela[n_linha,4] <- NA
          }
        }
      
        # Corrige variaveis com valores quebrados da coluna 5
        for (n_linha in 1:length(m_tabela)) {
          if(n_linha %in% c(40, 154, 155)) {
            m_tabela[n_linha,5] <- m_tabela[1,5]
          }
          if(n_linha == 212) {
            m_tabela[n_linha,5] <- m_tabela[10,5]
          }
          if(n_linha == 230) {
            m_tabela[n_linha,5] <- m_tabela[215,5]
          }
          if(n_linha %in% c(320, 322, 323, 326)) {
            m_tabela[n_linha,5] <- NA
          }
          if(n_linha == 358) {
            m_tabela[n_linha,5] <- m_tabela[32,5]
          }
        }
        
        # Corrige variaveis com valores quebrados da coluna 10
        for (n_linha in 1:length(m_tabela)) {
          if (n_linha == 15) { m_tabela[n_linha,10] <- c(22) }
          if (n_linha == 31) { m_tabela[n_linha,10] <- c(52) }
          if (n_linha == 40) { m_tabela[n_linha,10] <- c(36) }
          if (n_linha == 45) { m_tabela[n_linha,10] <- c(28) }
          if (n_linha == 46) { m_tabela[n_linha,10] <- c(36) }
          if (n_linha == 72) { m_tabela[n_linha,10] <- c(25) }
          if (n_linha == 76) { m_tabela[n_linha,10] <- c(22) }
          if (n_linha == 99) { m_tabela[n_linha,10] <- c(34) }
          if (n_linha == 112) { m_tabela[n_linha,10] <- c(16) }
          if (n_linha == 122) { m_tabela[n_linha,10] <- c(21) }
          if (n_linha == 123) { m_tabela[n_linha,10] <- c(48) }
          if (n_linha == 133) { m_tabela[n_linha,10] <- c(18) }
          if (n_linha == 139) { m_tabela[n_linha,10] <- c(31) }
          if (n_linha == 140) { m_tabela[n_linha,10] <- c(26) }
          if (n_linha == 144) { m_tabela[n_linha,10] <- c(36) }
          if (n_linha == 154) { m_tabela[n_linha,10] <- c(30) }
          if (n_linha == 155) { m_tabela[n_linha,10] <- c(64) }
          if (n_linha == 161) { m_tabela[n_linha,10] <- c(28) }
          if (n_linha == 191) { m_tabela[n_linha,10] <- c(30) }
          if (n_linha == 193) { m_tabela[n_linha,10] <- c(40) }
          if (n_linha == 194) { m_tabela[n_linha,10] <- c(21) }
          if (n_linha == 212) { m_tabela[n_linha,10] <- c(38) }
          if (n_linha == 217) { m_tabela[n_linha,10] <- c(36) }
          if (n_linha == 230) { m_tabela[n_linha,10] <- c(14) }
          if (n_linha == 233) { m_tabela[n_linha,10] <- c(21) }
          if (n_linha == 241) { m_tabela[n_linha,10] <- c(44) }
          if (n_linha == 300) { m_tabela[n_linha,10] <- c(45) }
          if (n_linha == 303) { m_tabela[n_linha,10] <- c(31) }
          if (n_linha == 308) { m_tabela[n_linha,10] <- c(40) }
          if (n_linha == 326) { m_tabela[n_linha,10] <- c(25) }
          if (n_linha == 331) { m_tabela[n_linha,10] <- c(59) }
          if (n_linha == 334) { m_tabela[n_linha,10] <- c(17) }
          if (n_linha == 335) { m_tabela[n_linha,10] <- c(32) }
          if (n_linha == 337) { m_tabela[n_linha,10] <- c(40) }
          if (n_linha == 351) { m_tabela[n_linha,10] <- c(18) }
          if (n_linha == 358) { m_tabela[n_linha,10] <- c(45) }
          if (n_linha == 373) { m_tabela[n_linha,10] <- c(36) }
        }

        # Substitui o ifen barra por NA nas linhas da coluna 8
        m_tabela[,8] <- m_tabela[,8] %>% gsub("-/", NA, .)
        # Substitui o ifen por NA nas linhas da coluna 10
        m_tabela[,10] <- m_tabela[,10] %>% gsub("-", NA, .)
        # Substitui o F ou M por Masculino ou Feminino nas linhas da coluna 9
        m_tabela[,9] <- m_tabela[,9] %>% gsub("M", "Masculino", .) %>% gsub("F", "Feminino", .)

        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
      }
      
      if (num_doc == 12) {
        # Usar locate_areas(file = arquivo, pages = 22, resolution = 100L, widget = c("shiny"))
        # para obter a lista da area padrao para o restante das paginas do documento
        areas_tabela <- list(c("top" = 136.48209, "left" = 12.88943, "bottom" = 547.65501, "right" = 804.30064))
        
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
        
        # Substitui o ifen barra por NA nas linhas da coluna 8 e 10
        for (linha in rev(1:nrow(m_tabela))) {
          if(!is.na(m_tabela[linha,8])) {
            if(m_tabela[linha,8] == "-") {
              m_tabela[linha,8] <- NA
            }
          }
          if(!is.na(m_tabela[linha,10])) {
            if(m_tabela[linha,10] == "-") {
              m_tabela[linha,10] <- NA
            }
          }
          # Substitui o F ou M por Masculino ou Feminino nas linhas da coluna 9
          if(!is.na(m_tabela[linha,9])) {
            if(m_tabela[linha,9] == "M") {
              m_tabela[linha,9] <- "Masculino"
            } else {
              m_tabela[linha,9] <- "Feminino"
            }
          }
        }
        
        #Corrige variaveis de linhas com valores quebrados
        for (n_linha in 512:534) {
            NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-15, nchar(m_tabela[n_linha,3]))
            m_tabela[n_linha,4] <- NATUREZA_FATO
            m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
        }
        m_tabela[517,3] <- m_tabela[507,3]
        
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
    gsub("-mar-", "/03/", .) %>% 
    gsub(".04.", "/04/", .) %>% 
    gsub("-mai-14", "/05/2014", .) %>% 
    gsub("/jun/", "/07/", .) %>% 
    gsub("-jul-", "/07/", .) %>% 
    gsub("-Aug-", "/08/", .) %>% 
    gsub("-Sep-", "/09/", .) %>% 
    gsub("-out-", "/10/", .) %>% 
    gsub("-nov-", "/11/", .) %>% 
    gsub("-dez-", "/12/", .)
  
  return(df_tabela)
}