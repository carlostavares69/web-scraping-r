#===========================================================================================
#                                 LIMPEZA DADOS 2018                                      
#-------------------------------------------------------------------------------------------

# Funcao de limpeza dos dados da matriz por documento baixado e retorna um data frame
limpa_dados_2018 <- function(ano, data_frame_meses) {
  
  print(paste("Definindo estrutura de limpeza para", nrow(data_frame_meses[2]), "meses.", sep = " "))
  
  df_nome_docs <- rbind(data_frame_meses[2])
  m_tabelas <- m_tabela <- matrix()
  
  # ######################### temp
  # ano <- lista_anos[1, 2]
  # data_frame_meses <- df_lista_meses
  # nome <- "JANEIRO-2018"
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
        
        # Excluir coluna excedente
        m_tabelas[[2]] <- m_tabelas[[2]][,-c(4)]
        m_tabelas[[3]] <- m_tabelas[[3]][,-c(4)]
        m_tabelas[[9]] <- m_tabelas[[9]][,-c(4)]
        m_tabelas[[19]] <- m_tabelas[[19]][,-c(4)]
        m_tabelas[[27]] <- m_tabelas[[27]][,-c(4)]
        
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
        
        m_tabela[95,4] <- m_tabela[33,4]
        m_tabela[340,4] <- m_tabela[296,4]
        m_tabela[457,3] <- m_tabela[186,3]
        m_tabela[593,4] <- m_tabela[593,3]
        m_tabela[593,3] <- m_tabela[457,3]
        
        for (n_linha in 586:nrow(m_tabela)) {
          if (n_linha != 593) {
            NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-15, nchar(m_tabela[n_linha,3]))
            m_tabela[n_linha,4] <- NATUREZA_FATO
            m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
          }
        }
        
        # Inclusao de tabela nao raspada automaticamente.
        m_tabela <- rbind(m_tabela, 
                          matrix(c(257, "AIS 14", "TIANGUÁ", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "17/01/2018", "CLEITON ARAUJO NASCIMENTO", "560-11/2018", "MASCULINO", 35), nrow = 1, ncol = 10),
                          matrix(c(258, "AIS 1", "FORTALEZA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "17/01/2018", "CRISTIANO CABRAL DA SILVA", "109-41/2018", "MASCULINO", 28), nrow = 1, ncol = 10),
                          matrix(c(259, "AIS 13", "CASCAVEL", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "17/01/2018", "DANIELISSON MARTINS PRAXEDES", "107-282/2018", "MASCULINO", 29), nrow = 1, ncol = 10),
                          matrix(c(260, "AIS 15", "PACOTI", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "17/01/2018", "FRANCISCO JACINTO FLOR", "432-93/2018", "MASCULINO", 63), nrow = 1, ncol = 10),
                          matrix(c(261, "AIS 6", "FORTALEZA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "17/01/2018", "FRANCISCO LUCAS DA SILVA LUZ", "110-94/2018", "MASCULINO", 20), nrow = 1, ncol = 10),
                          matrix(c(262, "AIS 16", "TAMBORIL", "HOMICIDIO DOLOSO", "ARMA BRANCA", "17/01/2018", "JOSE MARCOS CHAVES SILVA", "432-90/2018", "MASCULINO", 32), nrow = 1, ncol = 10),
                          matrix(c(263, "AIS 10", "FORTALEZA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "17/01/2018", "JOSE WELLINGTON DE SOUSA DANTAS FILHO", "107-285/2018", "MASCULINO", 26), nrow = 1, ncol = 10),
                          matrix(c(264, "AIS 15", "CANINDÉ", "FEMINICÍDIO", "ARMA DE FOGO", "17/01/2018", "TARCIANA ARAUJO SOUZA", "432-92/2018", "FEMININO", 31), nrow = 1, ncol = 10),
                          matrix(c(265, "AIS 3", "FORTALEZA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "18/01/2018", "ALISSON MOREIRA CAVALCANTE", "107-297/2018", "MASCULINO", 34), nrow = 1, ncol = 10),
                          matrix(c(266, "AIS 18", "FORTIM", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "18/01/2018", "BRUNO DA SILVA BRITO", NA, "MASCULINO", 24), nrow = 1, ncol = 10),
                          matrix(c(267, "AIS 21", "ICÓ", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "18/01/2018", "CARLOS EDUARDO SANTOS DE BRITO", "478-11/2018", "MASCULINO", 14), nrow = 1, ncol = 10),
                          matrix(c(268, "AIS 7", "FORTALEZA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "18/01/2018", "DAVI AMANCIO DA SILVA", "107-307/2018", "MASCULINO", 16), nrow = 1, ncol = 10),
                          matrix(c(269, "AIS 18", "FORTIM", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "18/01/2018", "DESCONHECIDO DO SEXO MASCULINO", NA, "MASCULINO", 16), nrow = 1, ncol = 10),
                          matrix(c(270, "AIS 13", "AQUIRAZ", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "18/01/2018", "FRANCISCO MARCIO SILVA DE SOUZA", "107-308/2018", "MASCULINO", 39), nrow = 1, ncol = 10),
                          
                          matrix(c(362, "AIS 13", "AQUIRAZ", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "24/01/2018", "FRANCISCO CELIO SILVA JUNIOR", "107-416/2018", "MASCULINO", 19), nrow = 1, ncol = 10),
                          matrix(c(363, "AIS 13", "EUSÉBIO", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "24/01/2018", "ISAAQUE RUFINO DA SILVA", "107-401/2018", "MASCULINO", 18), nrow = 1, ncol = 10),
                          matrix(c(364, "AIS 13", "AQUIRAZ", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "24/01/2018", "IVANILSON ABREU DE MENDONCA", "107-398/2018", "MASCULINO", 35), nrow = 1, ncol = 10),
                          matrix(c(365, "AIS 13", "HORIZONTE", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "24/01/2018", "MICAEL IURI LIMA ANDRADE", "461-61/2018", "MASCULINO", 14), nrow = 1, ncol = 10),
                          matrix(c(366, "AIS 6", "FORTALEZA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "24/01/2018", "PEDRO DA SILVA DIAS JUNIOR", "107-405/2018", "MASCULINO", 35), nrow = 1, ncol = 10),
                          matrix(c(367, "AIS 12", "PACATUBA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "24/01/2018", "RONALDO LIMA DE OLIVEIRA", "107-404/2018", "MASCULINO", 22), nrow = 1, ncol = 10),
                          matrix(c(368, "AIS 7", "FORTALEZA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "25/01/2018", "ANDRESON SILVA DAS CHAGAS", "111-141/2018", "MASCULINO", 26), nrow = 1, ncol = 10),
                          matrix(c(369, "AIS 13", "CASCAVEL", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "25/01/2018", "ANTONIO ALVES DOS SANTOS", "107-432/2018", "MASCULINO", 50), nrow = 1, ncol = 10),
                          matrix(c(370, "AIS 7", "FORTALEZA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "25/01/2018", "ANTONIO DANIEL ALCANTARA DA SILVA", "107-434/2018", "MASCULINO", 21), nrow = 1, ncol = 10),
                          matrix(c(371, "AIS 20", "BANABUIÚ", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "25/01/2018", "ANTONIO LINIVAL FELIX DA SILVA", "420-2/2018", "MASCULINO", 22), nrow = 1, ncol = 10),
                          matrix(c(372, "AIS 11", "CAUCAIA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "25/01/2018", "CARLOS WEVERTON DA SILVA", "322-109/2018", "MASCULINO", 15), nrow = 1, ncol = 10),
                          matrix(c(373, "AIS 12", "PACATUBA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "25/01/2018", "DANIEL GONCALVES DE SOUSA", "107-419/2018", "MASCULINO", 25), nrow = 1, ncol = 10),
                          matrix(c(374, "AIS 11", "CAUCAIA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "25/01/2018", "DJALMA PEREIRA DA SILVA", "107-406/2018", "MASCULINO", 36), nrow = 1, ncol = 10),
                          matrix(c(375, "AIS 12", "MARANGUAPE", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "25/01/2018", "EDUARDO FREDERICO DA SILVA", "107-426/2018", "MASCULINO", 32), nrow = 1, ncol = 10),
                          matrix(c(376, "AIS 4", "FORTALEZA", "LESAO CORPORAL SEGUIDA DE MORTE", "OUTROS", "25/01/2018", "FRANCISCO NAILTON ANDRADE", "134-321/2018", "MASCULINO", 29), nrow = 1, ncol = 10)
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
        
        # Excluir coluna excedente
        m_tabelas[[14]] <- m_tabelas[[14]][,-c(4)]
        
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
        
        m_tabela[186,4] <- m_tabela[46,4]
        m_tabela[403,3] <- m_tabela[417,3]
        
        for (n_linha in 428:nrow(m_tabela)) {
          NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-15, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,4] <- NATUREZA_FATO
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
        }
        
        # Inclusao de tabela nao raspada automaticamente.
        m_tabela <- rbind(m_tabela, 
                          matrix(c(114, "AIS 12", "MARACANAÚ", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "08/02/2018", "LUAN MATEUS TEIXEIRA DA SILVA", "107-623/2018", "MASCULINO", 17), nrow = 1, ncol = 10),
                          matrix(c(115, "AIS 6", "FORTALEZA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "08/02/2018", "LUCAS DE ANDRADE FELIPE", "111-217/2018", "MASCULINO", 24), nrow = 1, ncol = 10),
                          matrix(c(116, "AIS 12", "MARANGUAPE", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "08/02/2018", "RYAN PINTO BRAZ", "107-643/2018", "MASCULINO", 24), nrow = 1, ncol = 10),
                          matrix(c(117, "AIS 7", "FORTALEZA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "08/02/2018", "VALERIA SAMPAIO DA SILVA", "107-608/2018", "FEMININO", 17), nrow = 1, ncol = 10),
                          matrix(c(118, "AIS 16", "CATUNDA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "08/02/2018", "WILLDER ANDRADE DA CUNHA", "546-8/2018", "MASCULINO", 34), nrow = 1, ncol = 10),
                          matrix(c(119, "AIS 14", "TIANGUÁ", "HOMICIDIO DOLOSO", "ARMA BRANCA", "09/02/2018", "ANTONIO DO NASCIMENTO ANDRADE DE AGUIAR", "553-486/2018", "MASCULINO", 31), nrow = 1, ncol = 10),
                          matrix(c(120, "AIS 18", "TABULEIRO DO NORTE", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "09/02/2018", "CARLOS MATHEUS VIANA DA SILVA", "541-154/2018", "MASCULINO", 23), nrow = 1, ncol = 10),
                          matrix(c(121, "AIS 12", "MARACANAÚ", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "09/02/2018", "DANIEL MARTINS GONCALVES", "128-11/2018", "MASCULINO", 51), nrow = 1, ncol = 10),
                          matrix(c(122, "AIS 5", "FORTALEZA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "09/02/2018", "FRANCISCA DAIANE BARBOSA DE FREITAS", "134-525/2018", "FEMININO", 25), nrow = 1, ncol = 10),
                          matrix(c(123, "AIS 13", "PACAJUS", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "09/02/2018", "FRANCISCO GILDERLAN DOS SANTOS PEREIRA", "461-103/2018", "MASCULINO", 28), nrow = 1, ncol = 10),
                          matrix(c(124, "AIS 13", "PACAJUS", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "09/02/2018", "FRANCISCO LEONARDO MARTINS", "134-520/2018", "MASCULINO", 47), nrow = 1, ncol = 10),
                          matrix(c(125, "AIS 15", "CANINDÉ", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "09/02/2018", "FRANCISCO RAFAEL CRUZ RODRIGUES", "432-183/2018", "MASCULINO", 25), nrow = 1, ncol = 10),
                          matrix(c(126, "AIS 7", "FORTALEZA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "09/02/2018", "JACKSON DOUGLAS GERMANO DE LIMA", "322-177/2018", "MASCULINO", 33), nrow = 1, ncol = 10),
                          matrix(c(127, "AIS 5", "FORTALEZA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "09/02/2018", "JORGE LUIS RODRIGUES DA SILVA", "134-527/2018", "MASCULINO", 25), nrow = 1, ncol = 10),
                          matrix(c(128, "AIS 13", "HORIZONTE", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "09/02/2018", "MICAEL DE OLIVEIRA ALVES", "461-94/2018", "MASCULINO", 14), nrow = 1, ncol = 10)
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
        
        # Excluir coluna excedente
        m_tabelas[[6]] <- m_tabelas[[6]][,-c(4)]
        m_tabelas[[10]] <- m_tabelas[[10]][,-c(4)]
        m_tabelas[[11]] <- m_tabelas[[11]][,-c(4)]
        
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
        
        for (n_linha in 503:nrow(m_tabela)) {
          NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-15, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,4] <- NATUREZA_FATO
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
        }
        
        m_tabela[161,4] <- m_tabela[176,4]
        
        # Inclusao de tabela nao raspada automaticamente.
        m_tabela <- rbind(m_tabela, 
                          matrix(c(130, "AIS 13", "EUSÉBIO", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "09/03/2018", "LUCAS SILVA DE OLIVEIRA", "113-477/2018", "MASCULINO", 23), nrow = 1, ncol = 10),
                          matrix(c(131, "AIS 11", "CAUCAIA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "09/03/2018", "MANOEL DOS SANTOS SOUSA", "134-889/2018", "MASCULINO", 21), nrow = 1, ncol = 10),
                          matrix(c(132, "AIS 8", "FORTALEZA", "HOMICIDIO DOLOSO", "ARMA BRANCA", "09/03/2018", "NARA ALINE MOTA DE LIMA", "107-953/2018", "FEMININO", 23), nrow = 1, ncol = 10),
                          matrix(c(133, "AIS 5", "FORTALEZA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "09/03/2018", "PEDRO BRAGA BARROSO NETO", NA, "MASCULINO", 21), nrow = 1, ncol = 10),
                          matrix(c(134, "AIS 16", "VARJOTA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "09/03/2018", "RICARDO SOARES MESQUITA", "568-18/2018", "MASCULINO", 26), nrow = 1, ncol = 10),
                          matrix(c(135, "AIS 17", "ACARAÚ", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "10/03/2018", "ANTONIO FABIO RODRIGUES DIOGO", "553-862/2018", "MASCULINO", 19), nrow = 1, ncol = 10),
                          matrix(c(136, "AIS 2", "FORTALEZA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "10/03/2018", "EDILSON PEDRO VENANCIO FILHO", "204-477/2018", "MASCULINO", 26), nrow = 1, ncol = 10),
                          matrix(c(137, "AIS 19", "BREJO SANTO", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "10/03/2018", "FELIPE DA SILVA", "429-217/2018", "MASCULINO", 26), nrow = 1, ncol = 10),
                          matrix(c(138, "AIS 12", "MARACANAÚ", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "10/03/2018", "FRANCISCO DE ASSIS PINTO MORENO", "107-972/2018", "MASCULINO", 44), nrow = 1, ncol = 10),
                          matrix(c(139, "AIS 5", "FORTALEZA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "10/03/2018", "JARDEL DE ARAUJO COSTA", "201-526/2018", "MASCULINO", 21), nrow = 1, ncol = 10),
                          matrix(c(140, "AIS 20", "MORADA NOVA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "10/03/2018", "JOSE FERNANDO LIMA ANDRADE", "541-257/2018", "MASCULINO", 17), nrow = 1, ncol = 10),
                          matrix(c(141, "AIS 12", "MARANGUAPE", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "10/03/2018", "MILTON DIEGO ALVES RIBEIRO", "204-478/2018", "MASCULINO", 19), nrow = 1, ncol = 10),
                          matrix(c(142, "AIS 8", "FORTALEZA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "10/03/2018", "PEDRO HENRIQUE CICCONE DOS SANTOS CAVALCANTE", "107-974/2018", "MASCULINO", 21), nrow = 1, ncol = 10),
                          matrix(c(143, "AIS 19", "CRATO", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "11/03/2018", "CARLOS DE OLIVEIRA SANTOS FILHO", "446-454/2018", "MASCULINO", 23), nrow = 1, ncol = 10),
                          matrix(c(144, "AIS 17", "PARAIPABA", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "11/03/2018", "CAUA RIBEIRO ALVES", "134-900/2018", "MASCULINO", 19), nrow = 1, ncol = 10)
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
      
      if (num_doc == 4) {
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo)
        
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
        
        # Reordena coluna idade
        m_tabela <- m_tabela[, c(1,3,4,5,6,7,8,9,10,2)]
        
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
        
        for (n_linha in 434:nrow(m_tabela)) {
          m_tabela[n_linha, 4] <- m_tabela[430,4]
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
      
      if (num_doc == 5) {
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo)
        
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
        
        m_tabela[125,4] <- m_tabela[87,4]
        
        for (n_linha in 447:nrow(m_tabela)) {
          NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-15, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,4] <- NATUREZA_FATO
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
        }
        
        # Inclusao de tabela nao raspada automaticamente.
        m_tabela <- rbind(m_tabela, 
                          matrix(c(98, "AIS 8", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "10-May-2018", "FRANCISCO CLEITON SANTOS DE CASTRO", "107-1855/2018", "MASCULINO", 27), nrow = 1, ncol = 10),
                          matrix(c(99, "AIS 17", "Pentecoste", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "10-May-2018", "FRANCISCO OBITENES ALVES", "525-27/2018", "MASCULINO", 31), nrow = 1, ncol = 10),
                          matrix(c(100, "AIS 20", "Morada Nova", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "10-May-2018", "IGOR DA SILVA SANTOS", "541-502/2018", "MASCULINO", 22), nrow = 1, ncol = 10),
                          matrix(c(101, "AIS 12", "Maracanaú", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "10-May-2018", "JOSE ROGERIO OLIVEIRA UCHOA", "204-906/2018", "MASCULINO", 39), nrow = 1, ncol = 10),
                          matrix(c(102, "AIS 12", "Maracanaú", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "10-May-2018", "MATEUS MENDES GOMES", "107-1848/2018", "MASCULINO", 20), nrow = 1, ncol = 10),
                          matrix(c(103, "AIS 17", "Morrinhos", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "10-May-2018", "RAIMUNDO MAURICIO DA SILVA", "553-1659/2018", "MASCULINO", 71), nrow = 1, ncol = 10),
                          matrix(c(104, "AIS 13", "Eusébio", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "11-May-2018", "BRUNO VASCONCELOS ANICETO", "107-1990/2018", "MASCULINO", 23), nrow = 1, ncol = 10),
                          matrix(c(105, "AIS 6", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA BRANCA", "11-May-2018", "FERNANDO SALES FERREIRA", "107-1898/2018", "MASCULINO", 67), nrow = 1, ncol = 10),
                          matrix(c(106, "AIS 16", "Ipu", "HOMICIDIO DOLOSO", "ARMA BRANCA", "11-May-2018", "FRANCISCO DE ASSIS TOMAZ PEREIRA", "553-1665/2018", "MASCULINO", 40), nrow = 1, ncol = 10),
                          matrix(c(107, "AIS 15", "Capistrano", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "11-May-2018", "FRANCISCO EDNEY DOS SANTOS DE OLIVEIRA", "107-2012/2018", "MASCULINO", 28), nrow = 1, ncol = 10),
                          matrix(c(108, "AIS 8", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "11-May-2018", "FRANCISCO LAEL TEIXEIRA DE OLIVEIRA", "107-1968/2018", "MASCULINO", 28), nrow = 1, ncol = 10),
                          matrix(c(109, "AIS 7", "Fortaleza", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "11-May-2018", "JOAO MIGUEL NETO DA SILVA", NA, "MASCULINO", 28), nrow = 1, ncol = 10),
                          matrix(c(110, "AIS 13", "Pacajus", "HOMICIDIO DOLOSO", "ARMA DE FOGO", "11-May-2018", "JOSIEL PEREIRA DE SOUSA", "514-52/2018", "MASCULINO", 27), nrow = 1, ncol = 10),
                          matrix(c(111, "AIS 11", "Caucaia", "HOMICIDIO DOLOSO", "OUTROS", "11-May-2018", "MARIA ROSANE DA SILVA", "107-1863/2018", "FEMININO", 20), nrow = 1, ncol = 10),
                          matrix(c(112, "AIS 17", "Itapipoca", "HOMICIDIO DOLOSO", "ARMA BRANCA", "11-May-2018", "MICHEL MENEZES ALVES", "107-1861/2018", "MASCULINO", 28), nrow = 1, ncol = 10)
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
        
        m_tabela[472, 2] <- m_tabela[474, 2]
        m_tabela[477, 2] <- m_tabela[472, 2]
        
        for (n_linha in 469:472) {
          NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-15, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,4] <- NATUREZA_FATO
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
        }
        
        # Inclusao de tabela nao raspada automaticamente.
        m_tabela <- rbind(m_tabela,
                          matrix(c(4, "UNIDADE PRISIONAL", "Itaitinga", "HOMICIDIO DOLOSO","OUTROS", "13/06/2018", "DESCONHECIDO DO SEXO INDEFINIDO", NA, "NÃO IDENTIF", 20), nrow = 1, ncol = 10), 
                          matrix(c(5, "UNIDADE PRISIONAL", "Juazeiro do Norte", "HOMICIDIO DOLOSO", "OUTROS", "21/06/2018", "ANTONIO ALEX MACEDO PINHEIRO", "488-1858/2018", "MASCULINO", 24), nrow = 1, ncol = 10)
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
        
        for (n_linha in 403:nrow(m_tabela)) {
          NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-15, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,4] <- NATUREZA_FATO
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
        }
        
        # Inclusao de tabela nao raspada automaticamente.
        m_tabela <- rbind(m_tabela, 
                          matrix(c(206, "AIS 4", "Fortaleza", "HOMICIDIO DOLOSO", "Arma de fogo", "16/07/2018", "MICHEL OKA ELIAS", "102-943/2018", "Masculino", 71), nrow = 1, ncol = 10),
                          matrix(c(207, "AIS 5", "Fortaleza", "HOMICIDIO DOLOSO", "Arma de fogo", "16/07/2018", "NIVARDO CAMELO COSTA", "134-2357/2018", "Masculino", 27), nrow = 1, ncol = 10),
                          matrix(c(208, "AIS 11", "Caucaia", "HOMICIDIO DOLOSO", "Arma de fogo", "16/07/2018", "PAULO RICARDO DOS SANTOS RODRIGUES", "107-2756/2018", "Masculino", 19), nrow = 1, ncol = 10),
                          matrix(c(209, "AIS 20", "Banabuiú", "HOMICIDIO DOLOSO", "Arma de fogo", "16/07/2018", "RAIMUNDO NONATO FERREIRA PAULINO", "420-37/2018", "Masculino", 48), nrow = 1, ncol = 10),
                          matrix(c(210, "AIS 16", "Santa Quitéria", "HOMICIDIO DOLOSO", "Arma de fogo", "17/07/2018", "ANTONIO STENIO ALVES DA SILVA", "432-1051/2018", "Masculino", 22), nrow = 1, ncol = 10),
                          matrix(c(211, "AIS 2", "Fortaleza", "HOMICIDIO DOLOSO", "Arma de fogo", "17/07/2018", "ANTONIO VALDEMIR DA SILVA FERREIRA", "132-1028/2018", "Masculino", 20), nrow = 1, ncol = 10),
                          matrix(c(212, "AIS 11", "Caucaia", "HOMICIDIO DOLOSO", "Arma de fogo", "17/07/2018", "CARLIANE DA SILVA ARAUJO", "107-2774/2018", "Feminino", 19), nrow = 1, ncol = 10),
                          matrix(c(213, "AIS 13", "Pacajus", "HOMICIDIO DOLOSO", "Outros meios", "17/07/2018", "DESCONHECIDO DO SEXO MASCULINO", "514-79/2018", "Masculino", 19), nrow = 1, ncol = 10),
                          matrix(c(214, "AIS 13", "Pacajus", "HOMICIDIO DOLOSO", "Outros meios", "17/07/2018", "DESCONHECIDO DO SEXO MASCULINO", "514-80/2018", "Masculino", 19), nrow = 1, ncol = 10),
                          matrix(c(215, "AIS 2", "Fortaleza", "HOMICIDIO DOLOSO", "Arma de fogo", "17/07/2018", "GABRIEL DOS ANJOS AZEVEDO", "132-1036/2018", "Masculino", 20), nrow = 1, ncol = 10),
                          matrix(c(216, "AIS 19", "Caririaçu", "HOMICIDIO DOLOSO", "Arma de fogo", "17/07/2018", "JOSE GOMES DA SILVA", "488-2078/2018", "Masculino", 54), nrow = 1, ncol = 10),
                          matrix(c(217, "AIS 6", "Fortaleza", "HOMICIDIO DOLOSO", "Arma de fogo", "17/07/2018", "KEVIN FERREIRA RODRIGUES", "107-2769/2018", "Masculino", 23), nrow = 1, ncol = 10),
                          matrix(c(218, "AIS 2", "Fortaleza", "HOMICIDIO DOLOSO", "Arma de fogo", "17/07/2018", "LEONCIO SANTOS DE OLIVEIRA", "107-2770/2018", "Masculino", 21), nrow = 1, ncol = 10),
                          matrix(c(219, "AIS 17", "Pentecoste", "HOMICIDIO DOLOSO", "Arma de fogo", "17/07/2018", "RAIMUNDO NONATO OLIVEIRA DA SILVA", "134-2343/2018", "Masculino", 54), nrow = 1, ncol = 10),
                          matrix(c(220, "AIS 19", "Santana do Cariri", "HOMICIDIO DOLOSO", "Arma branca", "17/07/2018", "RAIMUNDO NONATO TAVEIRA", "446-1156/2018", "Masculino", 39), nrow = 1, ncol = 10),
                          matrix(c(221, "AIS 11", "Caucaia", "HOMICIDIO DOLOSO", "Arma de fogo", "17/07/2018", "RENATO JORGE ROCHA BEZERRA FILHO", "134-2347/2018", "Masculino", 35), nrow = 1, ncol = 10),
                          matrix(c(222, "AIS 8", "Fortaleza", "HOMICIDIO DOLOSO", "Arma de fogo", "18/07/2018", "ANTONIO EUDES TEIXEIRA DA SILVA", "107-2781/2018", "Masculino", 42), nrow = 1, ncol = 10),
                          matrix(c(223, "AIS 11", "Caucaia", "HOMICIDIO DOLOSO", "Arma de fogo", "18/07/2018", "CHARLISSON FERREIRA DE ALMEIDA", "322-600/2018", "Masculino", 32), nrow = 1, ncol = 10),
                          matrix(c(224, "AIS 6", "Fortaleza", "HOMICIDIO DOLOSO", "Arma de fogo", "18/07/2018", "ELILIANE MACARIO DA SILVA", "107-2783/2018", "Feminino", 48), nrow = 1, ncol = 10),
                          
                          matrix(c(266, "AIS 19", "Araripe", "HOMICIDIO DOLOSO", "Arma de fogo", "21/07/2018", "RITA ORLANDA DA SILVA BARBOSA", "446-1184/2018", "Feminino", 37), nrow = 1, ncol = 10),
                          matrix(c(267, "AIS 18", "Fortim", "HOMICIDIO DOLOSO", "Arma de fogo", "21/07/2018", "VINICIUS JOSE BARROS DA SILVA", "206-1139/2018", "Masculino", 19), nrow = 1, ncol = 10),
                          matrix(c(268, "AIS 9", "Fortaleza", "HOMICIDIO DOLOSO", "Arma de fogo", "21/07/2018", "WILTON GOMES DE SOUZA", "322-608/2018", "Masculino", 26), nrow = 1, ncol = 10),
                          matrix(c(269, "AIS 17", "Marco", "HOMICIDIO DOLOSO", "Arma de fogo", "22/07/2018", "ALISSON DA SILVA LIMA", "553-2620/2018", "Masculino", 17), nrow = 1, ncol = 10),
                          matrix(c(270, "AIS 19", "Araripe", "HOMICIDIO DOLOSO", "Arma de fogo", "22/07/2018", "ANTONIO LEONCIO DA SILVA FILHO", "446-1189/2018", "Masculino", 43), nrow = 1, ncol = 10),
                          matrix(c(271, "AIS 16", "Santa Quitéria", "HOMICIDIO DOLOSO", "Arma de fogo", "22/07/2018", "CARLOS ANTONIO MARIANO DA SILVA", "432-1083/2018", "Masculino", 25), nrow = 1, ncol = 10),
                          matrix(c(272, "AIS 4", "Fortaleza", "HOMICIDIO DOLOSO", "Arma de fogo", "22/07/2018", "CARLOS NATANAEL LIMA DE MORAIS", "134-2386/2018", "Masculino", 17), nrow = 1, ncol = 10),
                          matrix(c(273, "AIS 20", "Morada Nova", "HOMICIDIO DOLOSO", "Arma de fogo", "22/07/2018", "GISLEUDO FREITAS PEREIRA", "541-842/2018", "Masculino", 36), nrow = 1, ncol = 10),
                          matrix(c(274, "AIS 2", "Fortaleza", "HOMICIDIO DOLOSO", "Arma branca", "22/07/2018", "JEFFER SMALLY SOARES ALENCAR", "134-2389/2018", "Masculino", 33), nrow = 1, ncol = 10),
                          matrix(c(275, "AIS 17", "Tejuçuoca", "HOMICIDIO DOLOSO", "Arma de fogo", "22/07/2018", "JOAO CRISOSTIMO ALVES SOUSA FILHO", "107-2827/2018", "Masculino", 28), nrow = 1, ncol = 10),
                          matrix(c(276, "AIS 11", "São Gonçalo do Amarante", "HOMICIDIO DOLOSO", "Arma de fogo", "22/07/2018", "JOAO MOISES LIMA DA SILVA", "322-609/2018", "Masculino", 15), nrow = 1, ncol = 10),
                          matrix(c(277, "AIS 12", "Maracanaú", "HOMICIDIO DOLOSO", "Arma de fogo", "22/07/2018", "LUIZ CARLOS ANDRADE SILVA", "130-1249/2018", "Masculino", 18), nrow = 1, ncol = 10),
                          matrix(c(278, "AIS 15", "Boa Viagem", "HOMICIDIO DOLOSO", "Arma de fogo", "22/07/2018", "PAULO LUCIANO DE MESQUITA", "536-273/2018", "Masculino", 68), nrow = 1, ncol = 10),
                          matrix(c(279, "AIS 12", "Maranguape", "HOMICIDIO DOLOSO", "Arma de fogo", "22/07/2018", "PEDRO HENRIQUE DOS SANTOS", "204-1447/2018", "Masculino", 35), nrow = 1, ncol = 10),
                          matrix(c(280, "AIS 16", "Varjota", "HOMICIDIO DOLOSO", "Arma de fogo", "22/07/2018", "TONY ANDERSON GOMES AGUIAR", "553-2598/2018", "Masculino", 30), nrow = 1, ncol = 10),
                          matrix(c(281, "AIS 14", "Sobral", "HOMICIDIO DOLOSO", "Arma de fogo", "23/07/2018", "ANTONIO PEREIRA DE SOUSA", "553-2636/2018", "Masculino", 41), nrow = 1, ncol = 10),
                          matrix(c(282, "AIS 19", "Crato", "HOMICIDIO DOLOSO", "Arma de fogo", "23/07/2018", "BENONI BESERRA DA SILVA", "446-1193/2018", "Masculino", 62), nrow = 1, ncol = 10),
                          matrix(c(283, "AIS 11", "Caucaia", "HOMICIDIO DOLOSO", "Arma branca", "23/07/2018", "DAVID DA SILVA BEZERRA", "107-2839/2018", "Masculino", 18), nrow = 1, ncol = 10),
                          matrix(c(284, "AIS 18", "Tabuleiro do Norte", "HOMICIDIO DOLOSO", "Arma de fogo", "23/07/2018", "EXPEDITO DE ALMEIDA DIAS", "541-846/2018", "Masculino", 66), nrow = 1, ncol = 10)	
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
        
        # Excluir coluna excedente
        m_tabelas[[4]] <- m_tabelas[[4]][,-c(4)]
        
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
        
        m_tabela[92,4] <- m_tabela[84,4]
        
        # Inclusao de tabela nao raspada automaticamente.
        m_tabela <- rbind(m_tabela, 
                          matrix(c(67, "AIS 9", "Fortaleza" ,"HOMICIDIO DOLOSO", "ARMA DE FOGO", "08/08/2018", "JOSE STENIO DE SOUSA", "107-3039/2018", "MASCULINO", 41), nrow = 1, ncol = 10),
                          matrix(c(68, "AIS 15", "Canindé" ,"HOMICIDIO DOLOSO", "ARMA DE FOGO", "08/08/2018", "JOSE UENDEL MENDES GOMES", "432-1174/2018", "MASCULINO", 18), nrow = 1, ncol = 10),
                          matrix(c(69, "AIS 20", "Quixeramobim" ,"HOMICIDIO DOLOSO", "ARMA DE FOGO", "08/08/2018", "LEONARDO JUCA RIBEIRO", "536-306/2018", "MASCULINO", 26), nrow = 1, ncol = 10),
                          matrix(c(70, "AIS 17", "Trairi" ,"HOMICIDIO DOLOSO", "ARMA DE FOGO", "08/08/2018", "LUCAS AZEVEDO DE SOUSA", "201-1850/2018", "MASCULINO", 21), nrow = 1, ncol = 10),
                          matrix(c(71, "AIS 11", "Caucaia" ,"HOMICIDIO DOLOSO" ,"ARMA BRANCA" ,"08/08/2018", "MARCELO DA COSTA FARIAS", "201-1848/2018", "MASCULINO", 37), nrow = 1, ncol = 10),
                          matrix(c(72, "AIS 1", "Fortaleza" ,"HOMICIDIO DOLOSO", "ARMA DE FOGO", "08/08/2018", "MARCIELE DOS SANTOS DE LIMA", "322-684/2018", "FEMININO", 27), nrow = 1, ncol = 10),
                          matrix(c(73, "AIS 13", "Aquiraz" ,"HOMICIDIO DOLOSO", "ARMA DE FOGO", "08/08/2018", "ROMARIO DOS SANTOS COELHO", "111-1025/2018", "MASCULINO", 24), nrow = 1, ncol = 10),
                          matrix(c(74, "AIS 12", "Maracanaú" ,"HOMICIDIO DOLOSO", "ARMA DE FOGO", "09/08/2018", "ANDREZA LIMA TEIXEIRA DA COSTA", "107-3053/2018", "FEMININO", 16), nrow = 1, ncol = 10),
                          matrix(c(75, "AIS 17", "Pentecoste" ,"HOMICIDIO DOLOSO", "ARMA DE FOGO", "09/08/2018", "DYONNES ALVES DE PAULA", "525-40/2018", "MASCULINO", 24), nrow = 1, ncol = 10),
                          matrix(c(76, "AIS 18", "Aracati" ,"HOMICIDIO DOLOSO", "ARMA DE FOGO", "09/08/2018", "ERMESON RIBEIRO DOS SANTOS", "412-271/2018", "MASCULINO", 28), nrow = 1, ncol = 10),
                          matrix(c(77, "AIS 4", "Fortaleza" ,"HOMICIDIO DOLOSO", "ARMA DE FOGO", "09/08/2018", "FRANCISCO DE ASSIS SILVA DOS SANTOS", "134-2583/2018", "MASCULINO", 39), nrow = 1, ncol = 10),
                          matrix(c(78, "AIS 8", "Fortaleza" ,"HOMICIDIO DOLOSO", "ARMA DE FOGO", "09/08/2018", "FRANCISCO DENILSON TAVARES LOPES", "107-3043/2018", "MASCULINO", 19), nrow = 1, ncol = 10),
                          matrix(c(79, "AIS 18", "Jaguaruana" ,"HOMICIDIO DOLOSO", "ARMA DE FOGO", "09/08/2018", "FRANCISCO ELINEUDO DE MELO", "541-937/2018", "MASCULINO", 28), nrow = 1, ncol = 10),
                          matrix(c(80, "AIS 6", "Fortaleza" ,"HOMICIDIO DOLOSO", "ARMA DE FOGO", "09/08/2018", "ITALO BRUNO SALES DE LIMA", "107-3047/2018", "MASCULINO", 22), nrow = 1, ncol = 10),
                          matrix(c(81, "AIS 7", "Fortaleza" ,"HOMICIDIO DOLOSO", "ARMA DE FOGO", "09/08/2018", "JORDILENE PEREIRA DA SILVA", "134-2588/2018", "FEMININO", 42), nrow = 1, ncol = 10),
                          matrix(c(2, "UNIDADE PRISIONAL" ,"Pacatuba" ,"HOMICIDIO DOLOSO" ,"OUTROS" ,"21/08/2018" ,"RUAN MIQUEIAS MARTINS COSTA" ,"322-710/2018" ,"MASCULINO", 23), nrow = 1, ncol = 10)
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
        
        # Excluir coluna excedente
        m_tabelas[[16]] <- m_tabelas[[16]][,-c(4)]
        
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
        
        for (n_linha in 455:nrow(m_tabela)) {
          NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-15, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,4] <- NATUREZA_FATO
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
        }
        
        m_tabela[261,4] <- m_tabela[252,4]
        
        # Inclusao de tabela nao raspada automaticamente.
        m_tabela <- rbind(m_tabela, 
                          matrix(c(209, "AIS 13", "Aquiraz", "HOMICIDIO DOLOSO", "Arma de fogo", "16/09/2018", "JOSIVAN VIEIRA DOS SANTOS", "322-842/2018", "Masculino", 15), nrow = 1, ncol = 10),
                          matrix(c(210, "AIS 17", "Itarema", "HOMICIDIO DOLOSO", "Arma de fogo", "16/09/2018", "MAX DOS SANTOS OLIVEIRA", NA, "Masculino", 32), nrow = 1, ncol = 10),
                          matrix(c(211, "AIS 21", "Orós", "HOMICIDIO DOLOSO", "Arma de fogo", "16/09/2018", "ROBERIO ALMEIDA DE SOUSA", "478-137/2018", "Masculino", 33), nrow = 1, ncol = 10),
                          matrix(c(212, "AIS 13", "Cascavel", "HOMICIDIO DOLOSO", "Arma de fogo", "16/09/2018", "THIAGO FERREIRA DA SILVA", "134-2948/2018", "Masculino", 22), nrow = 1, ncol = 10),
                          matrix(c(213, "AIS 15", "Capistrano", "HOMICIDIO DOLOSO", "Arma de fogo", "17/09/2018", "ALEXSANDRO DO NASCIMENTO FEREIRA", "432-1445/2018", "Masculino", 25), nrow = 1, ncol = 10),
                          matrix(c(214, "AIS 16", "Varjota", "HOMICIDIO DOLOSO", "Outros meios", "17/09/2018", "ANTONIO RIBAMAR RODRIGUES DE SOUSA", "553-3400/2018", "Masculino", 40), nrow = 1, ncol = 10),
                          matrix(c(215, "AIS 11", "Caucaia", "HOMICIDIO DOLOSO", "Outros meios", "17/09/2018", "DESCONHECIDO DO SEXO MASCULINO", "134-2992/2018", "Masculino", 40), nrow = 1, ncol = 10),
                          matrix(c(216, "AIS 10", "Fortaleza", "HOMICIDIO DOLOSO", "Arma de fogo", "17/09/2018", "EDUARDO DA SILVA DOMINGUES", "109-936/2018", "Masculino", 19), nrow = 1, ncol = 10),
                          matrix(c(217, "AIS 13", "Aquiraz", "HOMICIDIO DOLOSO", "Arma de fogo", "17/09/2018", "ELIELTON INACIO SANCHO DOS SANTOS", "107-3589/2018", "Masculino", 23), nrow = 1, ncol = 10),
                          matrix(c(218, "AIS 14", "Santana do Acaraú", "HOMICIDIO DOLOSO", "Arma de fogo", "17/09/2018", "FRANCISCO DANIEL DE SOUZA", "544-9/2018", "Masculino", 19), nrow = 1, ncol = 10),
                          matrix(c(219, "AIS 13", "Eusébio", "HOMICIDIO DOLOSO", "Arma de fogo", "17/09/2018", "MARIA DE LOURDES LOPES DA SILVA", "107-3588/2018", "Feminino", 48), nrow = 1, ncol = 10),
                          matrix(c(220, "AIS 7", "Fortaleza", "HOMICIDIO DOLOSO", "Arma de fogo", "17/09/2018", "MARIA JOSE DE SOUSA", "322-843/2018", "Feminino", 59), nrow = 1, ncol = 10),
                          matrix(c(221, "AIS 19", "Crato", "HOMICIDIO DOLOSO", "Arma branca", "18/09/2018", "ANTONIO ALDEMIR BATISTA RODRIGUES", "446-1550/2018", "Masculino", 37), nrow = 1, ncol = 10),
                          matrix(c(222, "AIS 20", "Banabuiú", "HOMICIDIO DOLOSO", "Arma de fogo", "18/09/2018", "ANTONIO CESAR ROQUE FILHO", "534-1400/2018", "Masculino", 23), nrow = 1, ncol = 10),
                          matrix(c(223, "AIS 18", "Tabuleiro do Norte", "HOMICIDIO DOLOSO", "Arma de fogo", "18/09/2018", "BENEDITO JUSTINO SILVA", "541-1056/2018", "Masculino", 54), nrow = 1, ncol = 10),
                          matrix(c(224, "AIS 12", "Maracanaú", "HOMICIDIO DOLOSO", "Arma de fogo", "18/09/2018", "EMERSON LEON LOURENCO MONTEIRO", "130-1588/2018", "Masculino", 26), nrow = 1, ncol = 10),
                          matrix(c(225, "AIS 18", "Jaguaribe", "HOMICIDIO DOLOSO", "Arma de fogo", "18/09/2018", "FRANCISCO EDSON GOMES VICENTE", "472-114/2018", "Masculino", 43), nrow = 1, ncol = 10),
                          matrix(c(226, "AIS 19", "Milagres", "HOMICIDIO DOLOSO", "Arma de fogo", "18/09/2018", "FRANCISCO TAVARES DE OLIVEIRA", "429-778/2018", "Masculino", 52), nrow = 1, ncol = 10),
                          matrix(c(227, "AIS 2", "Fortaleza", "HOMICIDIO DOLOSO", "Arma de fogo", "18/09/2018", "FRANCISCO WILLIAM ARAUJO DA SILVA", "322-846/2018", "Masculino", 18), nrow = 1, ncol = 10)
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
        
        m_tabela[447,2] <- m_tabela[444,2]
        m_tabela[447,4] <- m_tabela[447,3]
        m_tabela[447,3] <- m_tabela[265,3]
        
        for (n_linha in 414:446) {
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
        
        # Excluir coluna excedente
        m_tabelas[[14]] <- m_tabelas[[14]][,-c(4)]
        m_tabelas[[15]] <- m_tabelas[[15]][,-c(4)]
        
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
 
        m_tabela[5,4] <- m_tabela[31,4]
        m_tabela[376,2] <- m_tabela[374,3]
        
        for (n_linha in 361:nrow(m_tabela)) {
          NATUREZA_FATO <- substr(m_tabela[n_linha,3], nchar(m_tabela[n_linha,3])-15, nchar(m_tabela[n_linha,3]))
          m_tabela[n_linha,4] <- NATUREZA_FATO
          m_tabela[n_linha,3] <- substr(m_tabela[n_linha,3], 0, nchar(m_tabela[n_linha,3]) - nchar(NATUREZA_FATO)-1 )
        }
        
        # Inclusao de tabela nao raspada automaticamente.
        m_tabela <- rbind(m_tabela, 
                          matrix(c(2, "AIS 12", "Maracanaú", "HOMICIDIO DOLOSO", "Arma de fogo", "01/11/2018" , "DAVID FERREIRA ALVES", "121-121/2018" , "Masculino", 19), nrow = 1, ncol = 10),
                          matrix(c(3, "AIS 22", "Parambu", "HOMICIDIO DOLOSO", "Arma de fogo", "01/11/2018" , "EDILSON PEREIRA MATIAS", "558-735/2018" , "Masculino", 12), nrow = 1, ncol = 10),
                          matrix(c(4, "AIS 5", "Fortaleza", "HOMICIDIO DOLOSO", "Arma de fogo", "01/11/2018" , "FRANCISCO ERMIVAN DA COSTA EUFRASIO", "107-4362/2018" , "Masculino", 38), nrow = 1, ncol = 10),
                          matrix(c(5, "AIS 2", "Fortaleza", "HOMICIDIO DOLOSO", "Arma de fogo", "01/11/2018" , "JEFFERSON HYLARY ALVES MOREIRA", "134-3371/2018" , "Masculino", 23), nrow = 1, ncol = 10),
                          matrix(c(6, "AIS 17", "Amontada", "HOMICIDIO DOLOSO", "Arma de fogo", "01/11/2018" , "JOAO MONTEIRO FILHO", "553-3937/2018" , "Masculino", 27), nrow = 1, ncol = 10),
                          matrix(c(7, "AIS 14", "Sobral", "HOMICIDIO DOLOSO", "Arma de fogo", "01/11/2018" , "JOELSON SOUZA FROTA", "553-3931/2018" , "Masculino", 27), nrow = 1, ncol = 10),
                          matrix(c(8, "AIS 3", "Fortaleza", "HOMICIDIO DOLOSO", "Arma de fogo", "01/11/2018" , "JOSE MARIA DA SILVA MESQUITA", "130-1822/2018" , "Masculino", 71), nrow = 1, ncol = 10),
                          matrix(c(9, "AIS 6", "Fortaleza", "HOMICIDIO DOLOSO", "Arma de fogo", "01/11/2018" , "LUIZ GUILHERME ALMEIDA PINHEIRO", "134-3573/2018" , "Masculino", 15), nrow = 1, ncol = 10),
                          matrix(c(10, "AIS 22", "Parambu", "HOMICIDIO DOLOSO", "Arma de fogo", "01/11/2018" , "MARLEIDE DE SOUSA PEREIRA", "558-734/2018" , "Feminino", 31), nrow = 1, ncol = 10),
                          matrix(c(11, "AIS 9", "Fortaleza", "HOMICIDIO DOLOSO", "Arma branca", "01/11/2018" , "RUAN DO NASCIMENTO SILVA SOUZA", "107-4218/2018" , "Masculino", 16), nrow = 1, ncol = 10),
                          matrix(c(12, "AIS 20", "Morada Nova", "HOMICIDIO DOLOSO", "Arma de fogo", "02/11/2018" , "ADAILTO CIRILO DA SILVA", "541-1212/2018" , "Masculino", 28), nrow = 1, ncol = 10),
                          matrix(c(13, "AIS 14", "Forquilha", "HOMICIDIO DOLOSO", "Arma de fogo", "02/11/2018" , "AIRTON DE SOUSA", "553-3940/2018" , "Masculino", 53), nrow = 1, ncol = 10),
                          matrix(c(14, "AIS 9", "Fortaleza", "HOMICIDIO DOLOSO", "Arma de fogo", "02/11/2018" , "ALESSANDRO LIMA DOS SANTOS", "134-3403/2018" , "Masculino", 36), nrow = 1, ncol = 10),
                          matrix(c(15, "AIS 15", "Caridade", "ROUBO SEGUIDO DE MORTE (LATROCINIO)", "Arma de fogo", "02/11/2018" , "ANTONIA GRACINETE MOREIRA BEZERRA", "134-3379/2018" , "Feminino", 41), nrow = 1, ncol = 10),
                          
                          matrix(c(17, "AIS 11", "São Gonçalo do Amarante", "HOMICIDIO DOLOSO", "Arma de fogo", "02/11/2018", "ELIAS ALVES PEREIRA", "201-2663/2018", "Masculino", 60), nrow = 1, ncol = 10),
                          matrix(c(18, "AIS 7", "Fortaleza", "HOMICIDIO DOLOSO", "Arma de fogo", "02/11/2018", "EVANGELO RODRIGUES PEREIRA", "107-4252/2018", "Masculino", 40), nrow = 1, ncol = 10),
                          matrix(c(19, "AIS 16", "Reriutaba", "HOMICIDIO DOLOSO", "Arma de fogo", "02/11/2018", "FRANCISCO FLAVIO DE OLIVEIRA", "553-4230/2018", "Masculino", 34), nrow = 1, ncol = 10),
                          matrix(c(20, "AIS 19", "Juazeiro do Norte", "HOMICIDIO DOLOSO", "Arma de fogo", "02/11/2018", "JOSE WILLAMS LOPES LIMA", "488-3131/2018", "Masculino", 27), nrow = 1, ncol = 10),
                          matrix(c(21, "AIS 9", "Fortaleza", "HOMICIDIO DOLOSO", "Arma de fogo", "02/11/2018", "PAULO SERGIO SANTOS MORAIS", "322-1033/2018", "Masculino", 30), nrow = 1, ncol = 10),
                          matrix(c(22, "AIS 19", "Crato", "HOMICIDIO DOLOSO", "Arma de fogo", "02/11/2018", "RAFAEL ALVES DE SOUSA", "446-1816/2018", "Masculino", 21), nrow = 1, ncol = 10),
                          matrix(c(23, "AIS 16", "Novo Oriente", "HOMICIDIO DOLOSO", "Arma de fogo", "03/11/2018", "ANTONIO FERREIRA NETO", "445-483/2018", "Masculino", 53), nrow = 1, ncol = 10),
                          matrix(c(24, "AIS 11", "Caucaia", "HOMICIDIO DOLOSO", "Arma de fogo", "03/11/2018", "CLEIDIANE ARMENIA DA CONCEICAO", "107-4250/2018", "Feminino", 33), nrow = 1, ncol = 10),
                          matrix(c(25, "AIS 14", "Viçosa do Ceará", "HOMICIDIO DOLOSO", "Arma branca", "03/11/2018", "DARIO GOMES RODRIGUES", "560-310/2018", "Masculino", 28), nrow = 1, ncol = 10),
                          matrix(c(26, "AIS 12", "Itaitinga", "HOMICIDIO DOLOSO", "Arma de fogo", "03/11/2018", "EDILSON FERREIRA COUTINHO", "107-4356/2018", "Masculino", 53), nrow = 1, ncol = 10),
                          matrix(c(27, "AIS 14", "Sobral", "ROUBO SEGUIDO DE MORTE (LATROCINIO)", "Arma de fogo", "03/11/2018", "EVERTON LIMA FREITAS", "553-3955/2018", "Masculino", 21), nrow = 1, ncol = 10),
                          matrix(c(28, "AIS 14", "Sobral", "HOMICIDIO DOLOSO", "Arma de fogo", "03/11/2018", "FRANCISCO FLAVIO PEREIRA DE MELO", "553-3958/2018", "Masculino", 32), nrow = 1, ncol = 10),
                          matrix(c(29, "AIS 11", "Caucaia", "HOMICIDIO DOLOSO", "Arma de fogo", "03/11/2018", "FRANCIVERTON LEONARDO MACIEL", "134-3392/2018", "Masculino", 29), nrow = 1, ncol = 10),
                          matrix(c(30, "AIS 11", "Caucaia", "HOMICIDIO DOLOSO", "Arma de fogo", "03/11/2018", "JOSE WILSON SOUSA DA SILVA", "107-4238/2018", "Masculino", 18), nrow = 1, ncol = 10),
                          matrix(c(31, "AIS 12", "Pacatuba", "ROUBO SEGUIDO DE MORTE (LATROCINIO)", "Arma de fogo", "03/11/2018", "LUIZ ANANIAS DO NASCIMENTO", "204-2170/2018", "Masculino", 63), nrow = 1, ncol = 10)
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
      
      if (num_doc == 12) {
        # Chama a funcao que extrai as tabelas do arquivo PDF e devolve uma matriz
        m_tabelas <- extrai_tabela(arquivo)
        
        print(paste("########## Extraindo tabelas de", nome, "contendo", length(m_tabelas), "pagina(s). ##########", sep = " "))
        total_pgs <- total_pgs + length(m_tabelas)
        
        # Excluir coluna excedente
        m_tabelas[[11]] <- m_tabelas[[11]][,-c(4)]
        m_tabelas[[14]] <- m_tabelas[[14]][,-c(4)]
        
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
        
        m_tabela[445,2] <- m_tabela[442,2]
        
        for (n_linha in 418:nrow(m_tabela)) {
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
  
  print(paste("Finalizado", total_pgs, "pagina(s).", sep = " ")) 

  # Padroniza o formato da data
  df_tabela[,6] <- df_tabela[,6] %>% gsub("-Apr-", "/04/", .) %>% gsub("-May-", "/05/", .)
  
  return(df_tabela)
}