#===========================================================================================
#                                 LIMPEZA DADOS 2017                                      
#-------------------------------------------------------------------------------------------

# Funcao de limpeza dos dados da matriz por documento baixado e retorna um data frame
limpa_dados_2017 <- function(ano, data_frame_meses) {
  
  print(paste("Definindo estrutura de limpeza para", nrow(data_frame_meses[2]), "meses.", sep = " "))
  
  df_nome_docs <- rbind(data_frame_meses[2])
  m_tabelas <- m_tabela <- matrix()
  
  # ######################### temp
  # ano <- lista_anos[2, 2]
  # data_frame_meses <- df_lista_meses
  # nome <- "JANEIRO-2017"
  # glimpse(m_tabelas)
  # View(m_tabela)
  # ######################### temp
  
  total_pgs <- 0
  
  for (num_doc in 1:nrow(df_nome_docs)) {
    nome <- as.character(data_frame_meses[num_doc,][2]$mes)
    arquivo <- file.path(".", dir_arquivos, ano, paste0(nome, ".pdf"))
    
    if(file.exists(arquivo)) {
      if (num_doc == 1) {
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo)

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
        
        # Inclusao de tabela nao raspada automaticamente.
        m_tabela <- rbind(m_tabela,
                          matrix(c(2, "Unidade Prisional", "Itaitinga", "HOMICIDIO DOLOSO", "OUTROS", "07/01/2017", "FRANCISCO GILIARD ANDRE DA SILVA", NA, "Masculino", 39), nrow = 1, ncol = 10),
                          matrix(c(3, "Unidade Prisional", "Itaitinga", "HOMICIDIO DOLOSO", "OUTROS", "07/01/2017", "LEONARDO DE SOUZA MESQUISTA", NA, "Masculino", 39), nrow = 1, ncol = 10)

        )
        
        # exclui linhas desnecessarias 
        for (linha in rev(1:length(m_tabela[,1]))) {
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }
        
        # Substitui o ifen por valor anterior nas linhas da coluna 10
        for (linha in 1:length(m_tabela[,1])) {
          if(m_tabela[linha, 10] == "-") {
            m_tabela[linha, 10] <- m_tabela[linha-1, 10]
          }
        }

        # converte matriz em data frame.
        df_tabela <- as.data.frame(m_tabela)
      }
      
      if (num_doc == 2) {
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo)
        
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
        
        # Inclusao de tabela nao raspada automaticamente.
        m_tabela <- rbind(m_tabela, 
                          matrix(c(82, "AIS 2", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "07/02/2017", "JORGE MAILSON BARRETO DA SILVA", "107-484/2017", "Masculino", 28), nrow = 1, ncol = 10),
                          matrix(c(83, "AIS 5", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "07/02/2017", "JOSE OLAVO LOPES DA SILVA", "107-428/2017", "Masculino", 55), nrow = 1, ncol = 10),
                          matrix(c(84, "AIS 7", "Caucaia", "HOMICIDIO DOLOSO", "OUTROS", "07/02/2017", "MAIRTON RODRIGUES DE SOUSA", "134-457/2017", "Masculino", 34), nrow = 1, ncol = 10),
                          matrix(c(85, "AIS 17", "Paracuru", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "07/02/2017", "MARCELO DOUGLAS DE FREITAS", "110-65/2017", "Masculino", 17), nrow = 1, ncol = 10),
                          matrix(c(86, "AIS 11", "Missão Velha", "HOMICIDIO DOLOSO", "ARMA BRANCA", "07/02/2017", "MARIA LUZANIRA DE SOUZA", "488-371/2017", "Feminino", 17), nrow = 1, ncol = 10),
                          matrix(c(87, "AIS 4", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "07/02/2017", "PABLO ISRAEL DE SOUSA LOPES", "322-98/2017", "Masculino", 19), nrow = 1, ncol = 10),
                          matrix(c(88, "AIS 10", "Fortim", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "07/02/2017", "RITA MARIA FERREIRA DOS SANTOS", "512-2/2017", "Feminino", 52), nrow = 1, ncol = 10),
                          matrix(c(89, "AIS 15", "Quixadá", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "07/02/2017", "UEDSON FAUSTINO GOMES", "534-128/2017", "Masculino", 23), nrow = 1, ncol = 10),
                          matrix(c(90, "AIS 5", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "07/02/2017", "WELLINGTON CATUNDA DE SOUZA VIANA", "134-461/2017", "Masculino", 19), nrow = 1, ncol = 10),
                          matrix(c(91, "AIS 11", "Juazeiro do Norte", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "08/02/2017", "ANTONIEL LINDEMBERCK RODRIGUES DE SOUSA", "488-381/2017", "Masculino", 32), nrow = 1, ncol = 10),
                          matrix(c(92, "AIS 7", "Caucaia", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "08/02/2017", "EDSON DA SILVA BARBOSA", "118-44/2017", "Masculino", 21), nrow = 1, ncol = 10),
                          matrix(c(93, "AIS 9", "Eusébio", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "08/02/2017", "FRANCISCO LUCIANO CARDOSO LIMA JUNIOR", "206-186/2017", "Masculino", 21), nrow = 1, ncol = 10),
                          matrix(c(94, "AIS 11", "Crato", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "08/02/2017", "FRANCISCO TORQUATO PINHEIRO DA SILVA", "446-213/2017", "Masculino", 29), nrow = 1, ncol = 10),
                          matrix(c(95, "AIS 12", "Sobral", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "08/02/2017", "HAILTON DA SILVA FERREIRA", "553-348/2017", "Masculino", 27), nrow = 1, ncol = 10),
                          matrix(c(96, "AIS 4", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "08/02/2017", "IAGO PRAXEDES DA COSTA", "113-72/2017", "Masculino", 24), nrow = 1, ncol = 10)
        )
        
        # exclui linhas desnecessarias 
        for (linha in rev(1:length(m_tabela[,1]))) {
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }
        
        # Substitui o ifen por valor anterior nas linhas da coluna 10
        for (linha in 1:length(m_tabela[,1])) {
          if(m_tabela[linha, 10] == "-") {
            m_tabela[linha, 10] <- m_tabela[linha-1, 10]
          }
        }
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
      }
      
      if (num_doc == 3) {
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo)
        
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
        
        # exclui linhas desnecessarias 
        for (linha in rev(1:length(m_tabela[,1]))) {
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }
        
        # Substitui o ifen por valor anterior nas linhas da coluna 10
        for (linha in 1:length(m_tabela[,1])) {
          if(m_tabela[linha, 10] == "-") {
            m_tabela[linha, 10] <- m_tabela[linha-1, 10]
          }
        }
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
      }
      
      if (num_doc == 4) {
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Une as tabelas extraídas
        m_tabela <- do.call(rbind, m_tabelas) 
        
        # Inclui coluna AIS faltante com variaveis NA e reordena 
        m_tabela <- cbind(m_tabela, c(NA)) %>% .[, c(1,10,2,3,4,5,6,7,8,9)]
        m_tabela[2,2] <- c("AIS") # Atribui nome da coluna
        
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
        
        # exclui linhas desnecessarias 
        for (linha in rev(1:length(m_tabela[,1]))) {
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }
        
        # Substitui o ifen por valor anterior nas linhas da coluna 10
        for (linha in 1:length(m_tabela[,1])) {
          if(m_tabela[linha, 10] == "-") {
            m_tabela[linha, 10] <- m_tabela[linha-1, 10]
          }
        }
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
      }
      
      if (num_doc == 5) {
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo)
        
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
        
        # Inclusao de tabela nao raspada automaticamente.
        m_tabela <- rbind(m_tabela, 
                          matrix(c(274, "AIS 14", "Coreaú", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "19-May-2017", "MANOEL SAMPAIO DE SOUSA", "553-1441/2017", "Masculino", 39), nrow = 1, ncol = 10),
                          matrix(c(275, "AIS 15", "Barreira", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "19-May-2017", "PAULO HENRIQUE OLIVEIRA PEREIRA", "107-1537/2017", "Masculino", 29), nrow = 1, ncol = 10),
                          matrix(c(276, "AIS 6", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "19-May-2017", "SANTIAGO DA SILVA PEREIRA", "110-654/2017", "Masculino", 32), nrow = 1, ncol = 10),
                          matrix(c(277, "AIS 11", "Caucaia", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "19-May-2017", "SILVANIR MARTINS LEITÃO", "110-664/2017", "Masculino", 26), nrow = 1, ncol = 10),
                          matrix(c(278, "AIS 18", "Fortim", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "19-May-2017", "SOLONILDO ROCHA PINTO", "206-656/2017", "Masculino", 37), nrow = 1, ncol = 10),
                          matrix(c(279, "AIS 9", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "19-May-2017", "WILLAMY MORAES DA SILVA", "110-656/2017", "Masculino", 24), nrow = 1, ncol = 10),
                          matrix(c(280, "AIS 10", "Fortaleza", "LESAO CORPORAL SEGUIDA DE MORTE", "ARMA BRANCA", "20-May-2017", "ADRIANO BATISTA DOS ANJOS", "109-595/2017", "Masculino", 26), nrow = 1, ncol = 10),
                          matrix(c(281, "AIS 8", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "20-May-2017", "ALEXANDRE BRENO BARBOZA DE LIMA", "107-1558/2017", "Masculino", 21), nrow = 1, ncol = 10),
                          matrix(c(282, "AIS 7", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "20-May-2017", "ARISTENIO MACIEL CORREIA", "113-524/2017", "Masculino", 55), nrow = 1, ncol = 10),
                          matrix(c(283, "AIS 1", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "20-May-2017", "BRENO RODRIGUES DE ALMEIDA", "109-599/2017", "Masculino", 19), nrow = 1, ncol = 10),
                          matrix(c(284, "AIS 18", "Russas", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "20-May-2017", "DONATAN XAVIER MARTINS", "541-478/2017", "Masculino", 24), nrow = 1, ncol = 10),
                          matrix(c(285, "AIS 14", "Sobral", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "20-May-2017", "EDNARDO SILVA MESQUITA", "553-1448/2017", "Masculino", 24), nrow = 1, ncol = 10),
                          matrix(c(286, "AIS 20", "Senador", "Pompeu HOMICIDIO DOLOSO", "ARMA DE FOGO", "20-May-2017", "FRANCISCO DO Ó NUNES", "551-80/2017", "Masculino", 69), nrow = 1, ncol = 10),
                          matrix(c(287, "AIS 19", "Mauriti", "HOMICIDIO DOLOSO", "ARMA BRANCA", "20-May-2017", "FRANCISCO JACINTO DO NASCIMENTO", "429-489/2017", "Masculino", 45), nrow = 1, ncol = 10),
                          matrix(c(288, "AIS 5", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "20-May-2017", "JOSE BRUNO PEREIRA DOS SANTOS", "110-666/2017", "Masculino", 24), nrow = 1, ncol = 10)
        )
        
        # exclui linhas desnecessarias 
        for (linha in rev(1:length(m_tabela[,1]))) {
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }
        
        # Substitui o ifen por valor anterior nas linhas da coluna 10
        for (linha in 1:length(m_tabela[,1])) {
          if(m_tabela[linha, 10] == "-") {
            m_tabela[linha, 10] <- m_tabela[linha-1, 10]
          }
        }
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
      }
      
      if (num_doc == 6) {
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo)
        
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
        
        m_tabela[567, 2] <- m_tabela[565, 3]
        m_tabela[24, 3] <- m_tabela[283, 3]
        m_tabela[384, 4] <- m_tabela[375, 4]
        m_tabela[389, 4] <- m_tabela[375, 4]
        for (n_linha in 561:nrow(m_tabela)) {
          NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-nchar(m_tabela[557, 4])+1, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
          m_tabela[n_linha,4] <- NATUREZA_FATO
        }
    
        # Inclusao de tabela nao raspada automaticamente.
        m_tabela <- rbind(m_tabela, 
                          matrix(c(18, "AIS 7", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "02/06/2017", "LEONARDO COSTA DA SILVA", "322-685/2017", "Masculino", 32), nrow = 1, ncol = 10),
                          matrix(c(19, "AIS 16", "Monsenhor Tabosa", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "02/06/2017", "VALDEMAR PINHO CHAVES", "432-738/2017", "Masculino", 54), nrow = 1, ncol = 10),
                          matrix(c(20, "AIS 7", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "02/06/2017", "VITOR MANUEL DE LIMA VIANA", "322-687/2017", "Masculino", 18), nrow = 1, ncol = 10),
                          matrix(c(21, "AIS 19", "Crato", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "02/06/2017", "WANDSON ROBERTY TEOFILO", "446-854/2017", "Masculino", 35), nrow = 1, ncol = 10),
                          matrix(c(22, "AIS 2", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "03/06/2017", "ALISSON DO NASCIMENTO MACHADO", "110-783/2017", "Masculino", 35), nrow = 1, ncol = 10),
                          matrix(c(23, "AIS 7", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "03/06/2017", "ANTONIO DANILO DO NASCIMENTO BATISTA", "107-1725/2017", "Masculino", 33), nrow = 1, ncol = 10),
                          matrix(c(24, "AIS 20", "Quixeramobim", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "03/06/2017", "ANTÔNIO GILVAN DE SOUZA SILVA", "534-667/2017", "Masculino", 40), nrow = 1, ncol = 10),
                          matrix(c(25, "AIS 20", "Solonópole", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "03/06/2017", "ANTONIO MARCOS DA SILVA", "551-88/2017", "Masculino", 32), nrow = 1, ncol = 10),
                          matrix(c(26, "AIS 20", "Senador Pompeu", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "03/06/2017", "ANTÔNIO OLNEY PEREIRA SÍRIO", "534-666/2017", "Masculino", 22), nrow = 1, ncol = 10),
                          matrix(c(27, "AIS 18", "Aracati", "HOMICIDIO DOLOSO", "ARMA BRANCAO", "03/06/2017", "ARISANGELO DA SILVA BRITO", "412-101/2017", "Masculino", 41), nrow = 1, ncol = 10),
                          matrix(c(28, "AIS 12", "Maracanaú", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "03/06/2017", "CLEDINALDO PEREIRA DA SILVA", "111-694/2017", "Masculino", 29), nrow = 1, ncol = 10),
                          matrix(c(29, "AIS 1", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "03/06/2017", "DAMIAO DE SOUSA DAVID", "109-651/2017", "Masculino", 48), nrow = 1, ncol = 10),
                          matrix(c(30, "AIS 13", "Aquiraz", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "03/06/2017", "DAVI SARAIVA BENIGNO", "107-1728/2017", "Masculino", 23), nrow = 1, ncol = 10),
                          matrix(c(31, "AIS 13", "Aquiraz", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "03/06/2017", "EDMILSON MAGALHAES NETO", "107-1727/2017", "Masculino", 25), nrow = 1, ncol = 10),
                          matrix(c(32, "AIS 14", "Sobral", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "03/06/2017", "EVALDO FERREIRA DE ANDRADE", "553-1601/2017", "Masculino", 41), nrow = 1, ncol = 10),
                          
                          matrix(c(306, "AIS 19", "Crato", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "19/06/2017", "MARIA BELARMINO DA CRUZ SILVA", "488-1823/2017", "Feminino", 40), nrow = 1, ncol = 10),
                          matrix(c(307, "AIS 14", "Sobral", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "19/06/2017", "WELLINGTON MOREIRA DA SILVA", "553-1791/2017", "Masculino", 31), nrow = 1, ncol = 10),
                          matrix(c(308, "AIS 13", "Eusébio", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "19/06/2017", "WILSON SILVA AMARAL FILHO", "206-821/2017", "Masculino", 31), nrow = 1, ncol = 10),
                          matrix(c(309, "AIS 3", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "20/06/2017", "ANTONIO ALISSON FERREIRA", "322-777/2017", "Masculino", 25), nrow = 1, ncol = 10),
                          matrix(c(310, "AIS 1", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "20/06/2017", "BENEDITO LOURENÇO DE PAULA", "109-768/2017", "Masculino", 36), nrow = 1, ncol = 10),
                          matrix(c(311, "AIS 13", "Pacajus", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "20/06/2017", "DANIEL VALENTINO DA ROCHA", "110-928/2017", "Masculino", 18), nrow = 1, ncol = 10),
                          matrix(c(312, "AIS 1", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "20/06/2017", "EMERSON RIBEIRO DO VALE", "109-770/2017", "Masculino", 15), nrow = 1, ncol = 10),
                          matrix(c(313, "AIS 8", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "20/06/2017", "FRANCISCO OCELIANO DA SILVA FREIRE", "134-1833/2017", "Masculino", 16), nrow = 1, ncol = 10),
                          matrix(c(314, "AIS 3", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "20/06/2017", "GEOVA MARQUES SILVA", "130-1183/2017", "Masculino", 18), nrow = 1, ncol = 10),
                          matrix(c(315, "AIS 17", "Acaraú", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "20/06/2017", "IVONALDO DE SOUZA ALUCRÉCIO", "403-140/2017", "Masculino", 21), nrow = 1, ncol = 10),
                          matrix(c(316, "AIS 10", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "20/06/2017", "MANOEL SALVIANO DA COSTA", "107-1976/2017", "Masculino", 21), nrow = 1, ncol = 10),
                          matrix(c(317, "AIS 15", "Itapiúna", "HOMICIDIO DOLOSO", "ARMA BRANCA", "20/06/2017", "MARIA DE NAZARE EUFRAZIO BARBOSA", "432-810/2017", "Feminino", 40), nrow = 1, ncol = 10),
                          matrix(c(318, "AIS 4", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "20/06/2017", "RAFAEL MATOS GARCIA", "107-1977/2017", "Masculino", 31), nrow = 1, ncol = 10),
                          matrix(c(319, "AIS 14", "Massapê", "ROUBO SEGUIDO DE MORTE (LATROCINIO)", "ARMA DE FOGO", "21/06/2017", "ANTÔNIO GLEISON DO CARMO DE MARIA", NA, "Masculino", 25), nrow = 1, ncol = 10),
                          matrix(c(320, "AIS 21", "Jucás", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "21/06/2017", "ARMANDO DA SILVA ALVES", "479-1249/2017", "Masculino", 22), nrow = 1, ncol = 10),
                          
                          matrix(c(322, "AIS 19", "Juazeiro do Norte", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "21/06/2017", "CARLOS ANTONIO FERREIRA DA SILVA", "488-1851/2017", "Masculino", 55), nrow = 1, ncol = 10),
                          matrix(c(323, "AIS 12", "Itaitinga", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "21/06/2017", "CRISTIANO EVANGELISTA DA SILVA", "134-1859/2017", "Masculino", 31), nrow = 1, ncol = 10),
                          matrix(c(324, "AIS 13", "Eusébio", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "21/06/2017", "DENILSON TAVARES DE FREITAS", "206-841/2017", "Masculino", 18), nrow = 1, ncol = 10),
                          matrix(c(325, "AIS 19", "Juazeiro do Norte", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "21/06/2017", "DESCONHECIDO DO SEXO FEMININO", "488-1855/2017", "Feminino", 18), nrow = 1, ncol = 10),
                          matrix(c(326, "AIS 12", "Maranguape", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "21/06/2017", "EDUARDO RODRIGUES LÔBO", "134-1855/2017", "Masculino", 24), nrow = 1, ncol = 10),
                          matrix(c(327, "AIS 3", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "21/06/2017", "FERNANDO ALVES PAULINO", "134-1858/2017", "Masculino", 20), nrow = 1, ncol = 10),
                          matrix(c(328, "AIS 13", "Eusébio", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "21/06/2017", "JEAN WILLAME SILVA DA ROCHA", "206-834/2017", "Masculino", 22), nrow = 1, ncol = 10),
                          matrix(c(329, "AIS 15", "Redenção", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "21/06/2017", "JOAO PEDRO DE CASTRO MOURA", "134-1864/2017", "Masculino", 21), nrow = 1, ncol = 10),
                          matrix(c(330, "AIS 14", "Massapê", "ROUBO SEGUIDO DE MORTE (LATROCINIO)", "ARMA DE FOGO", "21/06/2017", "LORENA LIMA DE FARIAS", NA, "Feminino", 31), nrow = 1, ncol = 10),
                          matrix(c(331, "AIS 7", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "21/06/2017", "LUCIANO LINHARES DA SILVA FILHO", "107-2092/2017", "Masculino", 30), nrow = 1, ncol = 10),
                          matrix(c(332, "AIS 19", "Juazeiro do Norte", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "21/06/2017", "LUIS PEREIRA DE MORAIS", "488-1846/2017", "Masculino", 42), nrow = 1, ncol = 10),
                          matrix(c(333, "AIS 21", "Acopiara", "ROUBO SEGUIDO DE MORTE (LATROCINIO)", "OUTROS", "21/06/2017", "LUIZ SOARES DE SOUZA", "404-40/2017", "Masculino", 76), nrow = 1, ncol = 10),
                          matrix(c(334, "AIS 7", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "21/06/2017", "RENE BREENDOWN DA SILVA AGUIAR", "107-1985/2017", "Masculino", 21), nrow = 1, ncol = 10),
                          matrix(c(335, "AIS 19", "Brejo Santo", "HOMICIDIO DOLOSO", "OUTROS", "22/06/2017", "ANA PAULA OLIVEIRA FERNANDES", "429-589/2017", "Feminino", 21), nrow = 1, ncol = 10),
                          matrix(c(336, "AIS 16", "Crateús", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "22/06/2017", "ANTONIO FRANCISCO FILHO", "445-136/2017", "Masculino", 27), nrow = 1, ncol = 10)
        )
        
        # exclui linhas desnecessarias 
        for (linha in rev(1:length(m_tabela[,1]))) {
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }
        
        # Substitui o ifen por valor anterior nas linhas da coluna 10
        for (linha in 1:length(m_tabela[,1])) {
          if(m_tabela[linha, 10] == "-") {
            m_tabela[linha, 10] <- m_tabela[linha-1, 10]
          }
        }
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
      }
      
      if (num_doc == 7) {
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo)
        
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
        
        m_tabela[390, 4] <- m_tabela[357, 4]
        for (n_linha in 558:nrow(m_tabela)) {
          NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-nchar(m_tabela[554, 4])+1, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
          m_tabela[n_linha,4] <- NATUREZA_FATO
        }
        
        # Inclusao de tabela nao raspada automaticamente.
        m_tabela <- rbind(m_tabela,
                          matrix(c(306, "AIS 12", "Itaitinga", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "21/07/2017", "MACIEL QUEIROZ VIEIRA", "322-962/2017", "Masculino", 35), nrow = 1, ncol = 10),
                          matrix(c(307, "AIS 19", "Juazeiro do Norte", "HOMICIDIO DOLOSO", "ARMA BRANCA", "21/07/2017", "MANOEL CLAUDEMIR DOS SANTOS", "488-2277/2017", "Masculino", 32), nrow = 1, ncol = 10),
                          matrix(c(308, "AIS 19", "Juazeiro do Norte", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "21/07/2017", "VALDEMIR MARQUES DE ALBUQUERQUE", "488-2274/2017", "Masculino", 27), nrow = 1, ncol = 10),
                          matrix(c(309, "AIS 13", "Aquiraz", "HOMICIDIO DOLOSO", "NI", "21/07/2017", "ZAIRTON GASPAR DE OLIVEIRA FILHO", "134-2154/2017", "Masculino", 60), nrow = 1, ncol = 10),
                          matrix(c(310, "AIS 7", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "22/07/2017", "ANTÔNIA NAYARA NEVES DA SILVA", "206-996/2017", "Feminino", 26), nrow = 1, ncol = 10),
                          matrix(c(311, "AIS 2", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "22/07/2017", "ANTONIO ALISSON FERNANDES CRUZ", "322-964/2017", "Masculino", 23), nrow = 1, ncol = 10),
                          matrix(c(312, "AIS 8", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "22/07/2017", "ANTONIO DIEGO DO NASCIMENTO SILVA", "110-1158/2017", "Masculino", 23), nrow = 1, ncol = 10),
                          matrix(c(313, "AIS 13", "Aquiraz", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "22/07/2017", "BRUNO DA SILVA", "134-2177/2017", "Masculino", 22), nrow = 1, ncol = 10),
                          matrix(c(314, "AIS 9", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "22/07/2017", "CARLOS NASCIMENTO NUNES", "322-965/2017", "Masculino", 27), nrow = 1, ncol = 10),
                          matrix(c(315, "AIS 2", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "22/07/2017", "DANIEL DE ARAUJO BESERRA", "111-940/2017", "Masculino", 26), nrow = 1, ncol = 10),
                          matrix(c(316, "AIS 6", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "22/07/2017", "DESCONHECIDO DO SEXO MASCULINO", "322-963/2017", "Masculino", 26), nrow = 1, ncol = 10),
                          matrix(c(317, "AIS 11", "Caucaia", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "22/07/2017", "FRANCISCO ANDRE TEIXEIRA FERNANDES SILVA", "110-1159/2017", "Masculino", 29), nrow = 1, ncol = 10),
                          matrix(c(318, "AIS 16", "Ipu", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "22/07/2017", "FRANCISCO DJACI MORENO CAVALCANTE", "553-2177/2017", "Masculino", 40), nrow = 1, ncol = 10),
                          matrix(c(319, "AIS 11", "Caucaia", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "22/07/2017", "FRANCISCO PAULO MOURA SILVA", "107-2357/2017", "Masculino", 25), nrow = 1, ncol = 10),
                          matrix(c(320, "AIS 1", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "22/07/2017", "FRANCISCO RAFAEL DE SOUSA", "107-2353/2017", "Masculino", 20), nrow = 1, ncol = 10),
                          
                          matrix(c(353, "AIS 20", "Quixeramobim", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "24/07/2017", "ROBERTO BARBOZA DO NASCIMENTO", "536-206/2017", "Masculino", 24), nrow = 1, ncol = 10),
                          
                          matrix(c(434, "AIS 11", "Caucaia", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "29/07/2017", "FRANCISCO ROBERTO BARBOSA", "110-1215/2017", "Masculino", 32), nrow = 1, ncol = 10),
                          matrix(c(435, "AIS 11", "Caucaia", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "29/07/2017", "FRANCISCO VILACI FERREIRA DE SOUSA", "107-2465/2017", "Masculino", 24), nrow = 1, ncol = 10),
                          matrix(c(436, "AIS 17", "Marco", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "29/07/2017", "FRANCISCO WELLINGTON RODRIGUES", "553-2278/2017", "Masculino", 21), nrow = 1, ncol = 10),
                          matrix(c(437, "AIS 1", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "29/07/2017", "FRANCISCO WILDSON SILVA DE SOUZA", "107-2464/2017", "Masculino", 22), nrow = 1, ncol = 10),
                          matrix(c(438, "AIS 16", "Nova Russas", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "29/07/2017", "JOSE BRUNO ABREU", "445-162/2017", "Masculino", 29), nrow = 1, ncol = 10),
                          matrix(c(439, "AIS 17", "Itarema", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "29/07/2017", "JOSE VALDEMIR FERREIRA ALVES", "553-2289/2017", "Masculino", 18), nrow = 1, ncol = 10),
                          matrix(c(440, "AIS 21", "Iguatu", "HOMICIDIO DOLOSO", "ARMA BRANCA", "29/07/2017", "LUSINETE FILGUEIRAS DE SÁ", "479-1511/2017", "Feminino", 49), nrow = 1, ncol = 10),
                          matrix(c(441, "AIS 21", "Iguatu", "HOMICIDIO DOLOSO", "ARMA BRANCA", "29/07/2017", "NARA RAQUEL FERREIRA DA SILVA", "479-1512/2017", "Feminino", 33), nrow = 1, ncol = 10),
                          matrix(c(442, "AIS 8", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "29/07/2017", "RANDERSON FRANCISCO DA SILVA MATIAS", "107-2463/2017", "Masculino", 22), nrow = 1, ncol = 10),
                          matrix(c(443, "AIS 10", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "29/07/2017", "RAYANE CRISTINA SILVA PAULINO", "134-2244/2017", "Feminino", 23), nrow = 1, ncol = 10),
                          matrix(c(444, "AIS 16", "Novo Oriente", "ROUBO SEGUIDO DE MORTE (LATROCINIO)", "ARMA DE FOGO", "30/07/2017", "ANTONIA NILDA TAVARES DA SILVA", "445-164/2017", "Feminino", 51), nrow = 1, ncol = 10),
                          matrix(c(445, "AIS 15", "Itapiúna", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "30/07/2017", "ANTONIO EUFRASIO BARBOSA", "432-1003/2017", "Masculino", 46), nrow = 1, ncol = 10),
                          matrix(c(446, "AIS 16", "Ipu", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "30/07/2017", "DAYANE SENA DE SOUSA", "553-2290/2017", "Feminino", 22), nrow = 1, ncol = 10),
                          matrix(c(447, "AIS 3", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "30/07/2017", "DESCONHECIDO DO SEXO MASCULINO", "134-2249/2017", "Masculino", 22), nrow = 1, ncol = 10),
                          matrix(c(448, "AIS 8", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "30/07/2017", "ELIVELTON MARTINS DE SOUSA", "107-2476/2017", "Masculino", 40), nrow = 1, ncol = 10)
        )
        
        # exclui linhas desnecessarias 
        for (linha in rev(1:length(m_tabela[,1]))) {
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }
        
        # Substitui o ifen por valor anterior nas linhas da coluna 10
        for (linha in 1:length(m_tabela[,1])) {
          if(m_tabela[linha, 10] == "-") {
            m_tabela[linha, 10] <- m_tabela[linha-1, 10]
          }
        }
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
      }
      
      if (num_doc == 8) {
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo)
        
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
        
        m_tabela[572, 4] <- m_tabela[572, 3]
        m_tabela[575, 4] <- m_tabela[575, 3]
        m_tabela[578, 4] <- m_tabela[578, 3]
        
        m_tabela[572, 3] <- paste(m_tabela[571, 3], m_tabela[573, 3], sep = " ")
        m_tabela[575, 3] <- paste(m_tabela[574, 3], m_tabela[576, 3], sep = " ")
        m_tabela[578, 3] <- paste(m_tabela[577, 3], m_tabela[579, 3], sep = " ")
        
        m_tabela[587, 2] <- m_tabela[584, 2]
        
        for (n_linha in 581:nrow(m_tabela)) {
          NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-nchar(m_tabela[566, 4])+1, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
          m_tabela[n_linha,4] <- NATUREZA_FATO
        }
        
        # Inclusao de tabela nao raspada automaticamente.
        m_tabela <- rbind(m_tabela,
                          matrix(c(84, "AIS 4", "Fortaleza", "ROUBO SEGUIDO DE MORTE (LATROCINIO)", "ARMA DE FOGO", "05/08/2017", "JOSE ELIO RIBEIRO", "107-2777/2017", "Masculino", 61), nrow = 1, ncol = 10),
                          matrix(c(85, "AIS 12", "Maracanaú", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "05/08/2017", "JOSIEL CALAÇA", "107-2549/2017", "Masculino", 23), nrow = 1, ncol = 10),
                          matrix(c(86, "AIS 19", "Abaiara", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "05/08/2017", "LINDOMAR RODRIGUES DA SILVA", "488-2490/2017", "Masculino", 34), nrow = 1, ncol = 10),
                          matrix(c(87, "AIS 19", "Juazeiro do Norte", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "05/08/2017", "LUCIA GOMES DE FIGUEIREDO", "429-702/2017", "Feminino", 38), nrow = 1, ncol = 10),
                          matrix(c(88, "AIS 13", "Aquiraz", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "05/08/2017", "MARCIO WENDELL PONTES DOURADO", "308-118/2017", "Masculino", 28), nrow = 1, ncol = 10),
                          matrix(c(89, "AIS 13", "Aquiraz", "HOMICIDIO DOLOSO", "ARMA DEFOGO", "05/08/2017", "PAULO NUNES SILVA FILHO", "206-1131/2017", "Masculino", 14), nrow = 1, ncol = 10),
                          matrix(c(90, "AIS 20", "Senador", "Pompeu HOMICIDIO DOLOSO", "ARMA DE FOGO", "05/08/2017", "PEDRO SABINO DA SILVA", "551-135/2017", "Masculino", 49), nrow = 1, ncol = 10),
                          matrix(c(91, "AIS 17", "Itapajé", "HOMICIDIO DOLOSO", "ARMA DEFOGO", "06/08/2017", "ANTONIO JOSE DUTRA CAMELO", "553-2363/2017", "Masculino", 39), nrow = 1, ncol = 10),
                          matrix(c(92, "AIS 8", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "06/08/2017", "ARTHUR WILDYSON GASPAR RODRIGUES", "134-2343/2017", "Masculino", 15), nrow = 1, ncol = 10),
                          matrix(c(93, "AIS 13", "Horizonte", "HOMICIDIO DOLOSO", "ARMA DEFOGO", "06/08/2017", "CARLOS ALBERTO BELCHIOR DE VASCONCELOS", "461-399/2017", "Masculino", 44), nrow = 1, ncol = 10),
                          matrix(c(94, "AIS 11", "Caucaia", "HOMICIDIO DOLOSO", "OUTROS", "06/08/2017", "FRANCISCO CANDIDO DE OLIVEIRA", "110-1260/2017", "Masculino", 46), nrow = 1, ncol = 10),
                          matrix(c(95, "AIS 11", "Caucaia", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "06/08/2017", "FRANCISCO DE ASSIS SILVA", "110-1268/2017", "Masculino", 46), nrow = 1, ncol = 10),
                          matrix(c(96, "AIS 8", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "06/08/2017", "GERISSON XAVIER FELIPE", "107-2562/2017", "Masculino", 25), nrow = 1, ncol = 10)
        )
        
        # exclui linhas desnecessarias 
        for (linha in rev(1:length(m_tabela[,1]))) {
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }
        
        # Substitui o ifen por valor anterior nas linhas da coluna 10
        for (linha in 1:length(m_tabela[,1])) {
          if(m_tabela[linha, 10] == "-") {
            m_tabela[linha, 10] <- m_tabela[linha-1, 10]
          }
        }
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
      }
      
      if (num_doc == 9) {
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Excluir tabela quebrada
        m_tabelas <- m_tabelas[-c(28)]
        
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
        
        # Inclusao de tabela nao raspada automaticamente.
        m_tabela <- rbind(m_tabela,
                          matrix(c(460, "AIS 14", "SOBRAL", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "30/09/2017", "PEDRO IVES DE SOUSA DE ABREU", NA, "MASCULINO", 18), nrow = 1, ncol = 10),
                          matrix(c(1, "UNIDADE PRISIONAL", "ITAITINGA", "HOMICIDIO DOLOSO", "OUTROS", "21/09/2017", "JOSE ANTONIEL DE ALMEIDA", "107-3269/2017", "MASCULINO", 39), nrow = 1, ncol = 10),
                          matrix(c(2, "UNIDADE PRISIONAL", "ITAITINGA", "HOMICIDIO DOLOSO", "OUTROS", "23/09/2017", "FRANCISCO MARDONIO MESQUITA DA SILVA", "134-2902/2017", "MASCULINO", 34), nrow = 1, ncol = 10)
        )
        
        # exclui linhas desnecessarias 
        for (linha in rev(1:length(m_tabela[,1]))) {
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }
        
        # Substitui o ifen por valor anterior nas linhas da coluna 10
        for (linha in 1:length(m_tabela[,1])) {
          if(m_tabela[linha, 10] == "-") {
            m_tabela[linha, 10] <- m_tabela[linha-1, 10]
          }
        }
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
      }
      
      if (num_doc == 10) {
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo)
        
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
        
        for (n_linha in 638:nrow(m_tabela)) {
          NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-nchar(m_tabela[634, 4])+1, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
          m_tabela[n_linha,4] <- NATUREZA_FATO
        }
        
        # exclui linhas desnecessarias 
        for (linha in rev(1:length(m_tabela[,1]))) {
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }
        
        # Substitui o ifen por valor anterior nas linhas da coluna 10
        for (linha in 1:length(m_tabela[,1])) {
          if(m_tabela[linha, 10] == "-") {
            m_tabela[linha, 10] <- m_tabela[linha-1, 10]
          }
        }
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
      }
      
      if (num_doc == 11) {
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo)
        
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
        
        for (n_linha in 582:nrow(m_tabela)) {
          NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-nchar(m_tabela[578, 4])+1, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
          m_tabela[n_linha,4] <- NATUREZA_FATO
        }
        
        # exclui linhas desnecessarias 
        for (linha in rev(1:length(m_tabela[,1]))) {
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }
        
        # Substitui o ifen por valor anterior nas linhas da coluna 10
        for (linha in 1:length(m_tabela[,1])) {
          if(m_tabela[linha, 10] == "-") {
            m_tabela[linha, 10] <- m_tabela[linha-1, 10]
          }
        }
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
      }
      
      if (num_doc == 12) {
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo)
        
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
        
        m_tabela[575, 4] <- paste(m_tabela[574,3], m_tabela[576,3], sep = " ")
        m_tabela[588, 2] <- m_tabela[585, 2]
        
        for (n_linha in 572:nrow(m_tabela)) {
          if(n_linha != 575) {
            NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-nchar(m_tabela[568, 4])+1, nchar(m_tabela[n_linha,3]))
            m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
            m_tabela[n_linha,4] <- NATUREZA_FATO
          }
        }
        
        # exclui linhas desnecessarias 
        for (linha in rev(1:length(m_tabela[,1]))) {
          if(m_tabela[linha] == "" | m_tabela[linha] == "ID") {
            m_tabela <- m_tabela[-c(linha),]
          }
        }
        
        # Substitui o ifen por valor anterior nas linhas da coluna 10
        for (linha in 1:length(m_tabela[,1])) {
          if(m_tabela[linha, 10] == "-") {
            m_tabela[linha, 10] <- m_tabela[linha-1, 10]
          }
        }
        
        # converte matriz em data frame e faz merge de linhas.
        df_tabela <- df_tabela %>% rbind(., as.data.frame(m_tabela))
      } 
    }
  }
  
  print(paste("Finalizado", total_pgs, "pagina(s). ##########", sep = " ")) 

  # Padroniza o formato da data
  df_tabela[,6] <- df_tabela[,6] %>% gsub("-Apr-", "/04/", .) %>% gsub("-May-", "/05/", .)
  
  return(df_tabela)
}