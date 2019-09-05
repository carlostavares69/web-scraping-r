##############################################################################################
## IMPORTACAO DOS DADOS
##############################################################################################

# Importacao banco de dados pos raspagem sem merges
Homicidios_CEARA <- readr::read_delim(file = file.path(".", dir_dados, "Homicidios_CEARA_2014-2018_Original-Sem-Merges.csv"), 
                                      delim = ";", 
                                      na = "NA",
                                      col_names = TRUE,
                                      #col_types = classes_colunas, 
                                      locale = locale(date_names = "pt", encoding = "UTF-8", decimal_mark = ",", grouping_mark = ".", date_format = "%d/%m/%Y")
)

# Verifica tipos de variaveis
str(Homicidios_CEARA)
view(Homicidios_CEARA)

# Verificar distincao das variaveis qualitativas
unique(Homicidios_CEARA$NATUREZA_HOMICIDIO)
unique(Homicidios_CEARA$ARMA_UTILIZADA)
unique(Homicidios_CEARA$SEXO)

# criar Novas variaveis 
Homicidios_CEARA$DATA_HOMICIDIO_NUM <- as.numeric(as.POSIXlt(Homicidios_CEARA$DATA_HOMICIDIO, format="%m/%d/%Y")) 
Homicidios_CEARA$ANO_INT <- as.integer(format.Date(as.Date(Homicidios_CEARA$DATA_HOMICIDIO, format = "%d/%m/%Y"), "%Y"))
Homicidios_CEARA$IDADE_INT <- as.integer(Homicidios_CEARA$IDADE)

# Convert variavel numerica de volta para date 
library(anytime)
data_num_date <- anydate(Homicidios_CEARA$DATA_HOMICIDIO_NUM)

# Obter dia mes ano em inteiro da variavel DATE
DATA_HOMICIDIO <- as.POSIXlt(Homicidios_CEARA$DATA_HOMICIDIO, format="%m/%d/%Y")
# Testes
1900+DATA_HOMICIDIO$year #obtem ano
DATA_HOMICIDIO$yday/365
1900+DATA_HOMICIDIO$year+DATA_HOMICIDIO$yday/366

############################################################################################
############################################################################################

# Baseado em Field et al. (2012)
# Fonte https://rpubs.com/hauselin/reliabilityanalysis

# https://www.nuvem.erivandosena.com.br/s/QZ4FcpNp5QmbmFy - Descobrindo estatísticas usando R (Andy Field, Jeremy Miles, Zoë Field, 2012, SAGE)

# Se todos os seus itens tiverem pontuação positiva (sem pontuação inversa), faça o seguinte para fazer suas análises de confiabilidade. 
# Supõe-se que todos os itens tenham uma pontuação positiva; portanto, você não precisa especificar nada.

Homicidios_CEARA_ano_idade <- dplyr::select(Homicidios_CEARA, 12, 13) # 12 e 13 sao as ultimas colunas
psych::alpha(Homicidios_CEARA_ano_idade)

# Método teste-reteste
#
# Anastasi (1977), Richardson (1999), Bunchaft & Cavas (2002), Erthal (2003) e Silva & 
# Ribeiro Filho (2006) afirmam que o método teste-reteste avalia o aspecto da estabilidade da
# fidedignidade.

# Método de Kuder-Richardson
# 
# Para Anastasi (1977), Richardson (1999), Bunchaft & Cavas (2002) e Silva & Ribeiro Filho
# (2006), esse método é uma evolução do método anterior, portanto, também busca o aspecto da
# homogeneidade ou consistência interna da fidedignidade. O método utiliza o coeficiente
# proposto por Kuder e Richardson (1937), chamado de KR20.

# Coeficiente alpha
# 
# Cronbach (1951) afirma que o coeficiente alpha (α) é um caso especial da fórmula KR 20 e é
# a média de todos os possíveis coeficientes de correlação obtidos por meio das bipartições da
# mensuração completa. Portanto, também busca o aspecto da homogeneidade ou consistência
# interna da fidedignidade:
############################################################################################
############################################################################################

#Fonte https://www.r-bloggers.com/five-ways-to-calculate-internal-consistency/

library(corrr)

### Correlação média entre itens
Homicidios_CEARA_ano_idade %>% correlate()

# obter a correlação média de cada item com todos os outros calculando as médias de cada coluna 
inter_item <- Homicidios_CEARA_ano_idade %>% correlate() %>% select(-rowname) %>% colMeans(na.rm = TRUE)
inter_item

mean(inter_item)

library(ggplot2)

data.frame(inter_item) %>% 
  ggplot(aes(x = inter_item)) +
  geom_histogram(bins = 10, alpha = .5) +
  geom_vline(xintercept = mean(inter_item), color = "red") +
  xlab("Correlação média entre itens") +
  theme_bw()

### Correlação média item-total
Homicidios_CEARA_ano_idade$score <- rowMeans(Homicidios_CEARA_ano_idade)

item_total <- Homicidios_CEARA_ano_idade %>% correlate() %>% focus(score)
item_total

mean(item_total$score)

item_total %>% 
  ggplot(aes(x = score)) +
  geom_histogram(bins = 10, alpha = .5) +
  geom_vline(xintercept = mean(item_total$score), color = "red") +
  xlab("Correlação média item-total") +
  theme_bw()

### Alfa de Cronbach

Homicidios_CEARA_ano_idade$score <- NULL  #excluir a coluna de pontuação criada anteriormente
psych::alpha(Homicidios_CEARA_ano_idade)

#acesso ao valor de alfa
psych::alpha(Homicidios_CEARA_ano_idade)$total$std.alpha


### Fórmula de Spearman-Brown - correlação entre duas metades
# Calculando pontuação total
score_p <- rowMeans(Homicidios_CEARA_ano_idade[, c(TRUE, FALSE)])  # com itens pares
score_i <- rowMeans(Homicidios_CEARA_ano_idade[, c(FALSE, TRUE)])  # com itens ímpares

# Correlacionando pontuações de itens pares e ímpares
r <- cor(score_p, score_i)
r

# Ajustando com a fórmula de profecia de Spearman-Brown
(2 * r) / (1 + r)

### Confiabilidade composta

library(lavaan)

# Definir o modelo
items <- paste(names(Homicidios_CEARA_ano_idade), collapse = "+")
model <- paste("extraversion", items, sep = "=~")
model

# Ajuste do modelo
fit <- cfa(model, data = Homicidios_CEARA_ano_idade)

#Existem várias maneiras de obter a confiabilidade composta desse modelo. Extrairemos as cargas fatoriais padronizadas e trabalharemos com elas:

sl <- standardizedSolution(fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(Homicidios_CEARA_ano_idade)
sl  #Estas são as cargas fatoriais padronizadas para cada item

#Em seguida, obtemos a confiabilidade composta através do seguinte:
# Calcular a variação residual de cada item
re <- 1 - sl^2

# Calcular confiabilidade composta
sum(sl)^2 / (sum(sl)^2 + sum(re))
