##############################################################################################
## ANALISE EXPLORATORIA DOS DADOS
##############################################################################################

#FONTE: https://www.kaggle.com/teresadong/sf-crime-eda-r

crimes <- na.omit(crime_ceara) %>%
  select(NATUREZA_CRIME, ID) %>%
  group_by(NATUREZA_CRIME) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n)) %>%
  arrange(desc(freq)) %>%
  top_n(10) %>%
  ggplot(aes(x=reorder(NATUREZA_CRIME,-freq),y=freq,label=freq)) +
  geom_col(position = position_dodge(width=1),width=0.5)+ 
  geom_text(aes(label = scales::percent(freq), y= freq+0.05), stat= "identity", angle=90,size=2) +
  scale_y_continuous(name="Percentagem de Crimes Totais",labels=percent) +
  xlab("Descrição do crime") +
  ggtitle("Crimes letais por tipos de crime (Janeiro de 2014 a Dezembro de 2018)")+
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
crimes

# Homicido Doloso é o tipo mais comum de crime. Depois de

crimes <- na.omit(crime_ceara) %>% mutate(DATA_MORTE = as.Date(DATA_MORTE, format = "%d/%m/%Y")) %>% 
  select(DATA_MORTE, ID) %>%
  mutate(report_year=lubridate::year(DATA_MORTE),report_month=lubridate::month(DATA_MORTE),trunc_month=lubridate::floor_date(DATA_MORTE,"month")) %>%
  group_by(report_year, report_month) %>%
  summarise(n=n(),count=length(unique(trunc_month))) %>%
  mutate(avg=n/count) %>%
  ggplot(aes(x=report_month,y=avg)) +
  facet_grid(rows=vars(report_year))+
  geom_col(position = position_dodge(width=1),width=0.5)+ 
  geom_text(aes(label = scales::comma(avg),y= avg+550), stat= "identity", angle=0,size=3) +
  scale_y_continuous(name="Total de Crimes",labels=comma) +
  scale_x_continuous(name="Mês",breaks=seq(1,12))+
  xlab("Descrição do crime") +
  ggtitle("Crimes letais por mês do ano (Janeiro de 2014 a Dezembro de 2018)")+
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
crimes

crimes <- na.omit(crime_ceara) %>% mutate(DATA_MORTE = as.Date(DATA_MORTE, format = "%d/%m/%Y")) %>% 
  filter(DATA_MORTE>lubridate::ymd('2014-01-01'))%>%
  select(DATA_MORTE, ID) %>%
  mutate(report_month=lubridate::month(DATA_MORTE),trunc_month=lubridate::floor_date(DATA_MORTE,"month")) %>%
  group_by(report_month) %>%
  summarise(n=n(),count=length(unique(trunc_month))) %>%
  mutate(avg=n/count) %>%
  ggplot(aes(x=report_month,y=avg)) +
  geom_col(position = position_dodge(width=1),width=0.5)+ 
  geom_text(aes(label = scales::comma(avg),y= avg+150), stat= "identity", angle=90,size=3) +
  scale_y_continuous(name="Crimes Médios",labels=comma) +
  scale_x_continuous(name="Mês",breaks=seq(1,12))+
  xlab("Descrição do crime") +
  ggtitle("Crimes letais por mês do ano (Janeiro de 2014 a Dezembro de 2018)")+
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) 
crimes

crimes <- na.omit(crime_ceara) %>% mutate(DATA_MORTE = as.Date(DATA_MORTE, format = "%d/%m/%Y")) %>% 
  filter(DATA_MORTE>lubridate::ymd('2014-01-01'))%>%
  select(DATA_MORTE, ID) %>%
  mutate(report_dow=lubridate::wday(DATA_MORTE)) %>%
  mutate(report_dow_name=recode(report_dow, 
                                "1"="Domingo",
                                "2"="Segunda",
                                "3"="Terça",
                                "4"="Quarta",
                                "5"="Quinta",
                                "6"="Sexta",
                                "7"="Sábado"))%>%
  group_by(report_dow,report_dow_name) %>%
  summarise(n=n(),count=length(unique(DATA_MORTE))) %>%
  mutate(avg=n/count) %>%
  ggplot(aes(x=reorder(report_dow_name,report_dow),y=avg)) +
  geom_col(position = position_dodge(width=1),width=0.5)+ 
  geom_text(aes(label = scales::comma(avg), y= avg+6), stat= "identity", angle=0,size=3) +
  scale_y_continuous(name="Crimes Médios",labels=comma) +
  xlab("Dia da semana") +
  ggtitle("Crimes letais por dia da semana (Janeiro de 2014 a Dezembro de 2018)")+
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
crimes

#================================================================================================================
# Pergunta 1: Qual dia da semana teve a maior média de Crimes letais diários por tipo de crime em julho de 2018 ?
#================================================================================================================

crimes <- na.omit(crime_ceara) %>% mutate(DATA_MORTE = as.Date(DATA_MORTE, format = "%d/%m/%Y")) %>% 
  filter(lubridate::year(DATA_MORTE) == 2018 & lubridate::month(DATA_MORTE)==7) %>%
  select(DATA_MORTE, ID) %>%
  mutate(report_dow=lubridate::wday(DATA_MORTE)) %>%
  mutate(report_dow_name=recode(report_dow, 
                                "1"="Domingo",
                                "2"="Segunda",
                                "3"="Terça",
                                "4"="Quarta",
                                "5"="Quinta",
                                "6"="Sexta",
                                "7"="Sábado"))%>%
  group_by(report_dow,report_dow_name) %>%
  summarise(n=n(),count=length(unique(DATA_MORTE))) %>%
  mutate(avg=n/count) %>%
  ggplot(aes(x=reorder(report_dow_name,report_dow),y=avg)) +
  geom_col(position = position_dodge(width=1),width=0.5)+ 
  geom_text(aes(label = scales::comma(avg), y= avg+6), stat= "identity", angle=0,size=3) +
  scale_y_continuous(name="Crimes Médios",labels=comma) +
  xlab("Dia da semana") +
  ggtitle("Crimes letais por dia da semana (Julho de 2018)")+
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
crimes

# Sexta-feira e sábado possui a maior média por dias da semana

crimes <- na.omit(crime_ceara) %>% 
  select(ARMA_UTILIZADA, ID) %>%
  group_by(ARMA_UTILIZADA) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n)) %>%
  arrange(desc(freq)) %>%
  ggplot(aes(x=reorder(ARMA_UTILIZADA,-freq),y=freq,label=freq)) +
  geom_col(position = position_dodge(width=1),width=0.5)+ 
  geom_text(aes(label = scales::percent(freq), y= freq+0.15), stat= "identity", angle=0,size=2) +
  scale_y_continuous(name="Percentagem de Crimes Totais",labels=percent) +
  xlab("Tipo de arma") +
  ggtitle("Crimes letais por tipo de arma (Janeiro de 2014 a Dezembro de 2018)")+
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
crimes

crimes <- na.omit(crime_ceara) %>% 
  select(MUNICIPIO_CRIME, ID) %>%
  group_by(MUNICIPIO_CRIME) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n)) %>%
  arrange(desc(freq))%>%
  top_n(10)%>%
  ggplot(aes(x=reorder(MUNICIPIO_CRIME,-freq),y=freq,label=freq)) +
  geom_col(position = position_dodge(width=1),width=0.5)+ 
  geom_text(aes(label = scales::percent(freq),
                y= freq+0.04), stat= "identity", angle=0,size=2) +
  scale_y_continuous(name="Percentagem de Crimes Totais",labels=percent) +
  xlab("Cidade") +
  ggtitle("Crimes letais por cidade (Janeiro de 2014 a Dezembro de 2018)")+
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
crimes

# --------------------------------------------------------------------------------------------------------------
# Análise de dados exploratórios de incidentes
# --------------------------------------------------------------------------------------------------------------

# Que tipos de crimes são os mais comuns?

# Vamos fazer algum agrupamento:
# - Homicidio: Homicidio Doloso / Lesao Corporal Seguida De Morte / Feminicidio
# - Outros Crimes: Roubo Seguido De Morte (Latrocinio) / Morte Suspeita

crimes <- na.omit(crime_ceara) %>% 
  select(NATUREZA_CRIME, INCIDENCIA_CRIME) %>%
  mutate(NATUREZA_CRIME=as.character(NATUREZA_CRIME)) %>%
  mutate(NATUREZA_CRIME=case_when(NATUREZA_CRIME %in% c('Homicidio Doloso','Lesao Corporal Seguida De Morte','Feminicidio') ~ 'Homicidio',
                            NATUREZA_CRIME %in% c('Roubo Seguido De Morte (Latrocinio)','Morte Suspeita') ~ 'Outros Crimes')) %>%
  group_by(NATUREZA_CRIME) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n)) %>%
  ggplot(aes(x=reorder(NATUREZA_CRIME,-freq),y=freq,label=freq)) +
  geom_col(position = position_dodge(width=1),width=0.5)+ 
  geom_text(aes(label = scales::percent(freq),
                y= freq+0.02), stat= "identity", angle=0,size=2) +
  scale_y_continuous(name="Percentagem de Crimes Totais",labels=percent) +
  xlab("Tipo de crime") +
  ggtitle("Incidentes por tipo de crime (Janeiro de 2014 a Dezembro de 2018)") + 
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
crimes

crimes <- na.omit(crime_ceara) %>% mutate(DATA_MORTE = as.Date(DATA_MORTE, format = "%d/%m/%Y")) %>% 
  select(NATUREZA_CRIME, DATA_MORTE, INCIDENCIA_CRIME) %>%
  filter(DATA_MORTE < lubridate::ymd('2018-12-31'))%>%
  mutate(yearmo=lubridate::floor_date(DATA_MORTE,"month")) %>%
  mutate(NATUREZA_CRIME=as.character(NATUREZA_CRIME)) %>%
  mutate(newCat=case_when(NATUREZA_CRIME %in% c('Homicidio Doloso') ~ 'Homicidio Doloso',
                          NATUREZA_CRIME %in% c('Lesao Corporal Seguida De Morte') ~ 'Lesao Corporal Seguida De Morte', 
                          NATUREZA_CRIME %in% c('Roubo Seguido De Morte (Latrocinio)') ~ 'Roubo Seguido De Morte', 
                          NATUREZA_CRIME %in% c('Feminicidio') ~ 'Feminicidio', 
                          NATUREZA_CRIME %in% c("Morte Suspeita") ~NATUREZA_CRIME,
                          TRUE ~ 'Restante')) %>%
  
  group_by(yearmo, newCat) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=yearmo,y=n,fill=newCat)) +
  geom_area()  +
  labs(title="Categoria de Crimes de Janeiro de 2014 a Dezembro de 2018",y="Número de Crimes",x="Year",fill="Tipos de Crime") +
  scale_y_continuous(labels=comma) +
  scale_x_date(date_breaks='1 year') +
  theme_tufte() +
  theme(legend.position = "top") +
  guides(fill=guide_legend(nrow=2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.7), legend.text=element_text(size=8))
crimes

#================================================================================================================
# Pergunta 2: Qual foi operiodo da semana mais comum para crime letal com Roubo Seguido De Morte (Latrocinio) entre 2014/2018 ?
#================================================================================================================

crimes <- na.omit(crime_ceara) %>% mutate(DATA_MORTE = as.Date(DATA_MORTE, format = "%d/%m/%Y")) %>% 
  filter(NATUREZA_CRIME=='Roubo Seguido De Morte (Latrocinio)') %>%
  select(DATA_MORTE, INCIDENCIA_CRIME) %>% 
  mutate(hour=weekdays(DATA_MORTE)) %>% 
  mutate(hour_bkt=case_when(hour %in% c("segunda-feira","terça-feira","quarta-feira","quinta-feira","sexta-feira") ~ 'Semana', 
                            hour %in% c("sábado","domingo") ~ 'Final de Semana')) %>% 
  group_by(hour_bkt) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=hour_bkt,y=n,label=n)) + 
  geom_col(position = position_dodge(width=1),width=0.5)+ 
  scale_y_continuous(name="Percentagem de Crimes Totais",labels=comma) + 
  xlab("Incident Time of Day") + 
  ggtitle("Crimes letais por tipo de arma (Janeiro de 2014 a Dezembro de 2018)")+
  theme_tufte()
crimes

#================================================================================================================
# Pergunta 3: Analise e descreva a relação entre a arma utilizada e o volume e o tipo de incidentes
#================================================================================================================

# Usando agrupamentos de antes, mostre como os incidentes se parecem com hour_bkt + categoria de crime
crimes <- na.omit(crime_ceara) %>% 
  select(ARMA_UTILIZADA, NATUREZA_CRIME, INCIDENCIA_CRIME) %>%
  mutate(hour=ARMA_UTILIZADA) %>%
  
  mutate(hour_bkt=case_when(hour %in% c("Arma De Fogo") ~ 'Arma De Fogo',
                            hour %in% c("Arma Branca") ~ 'Arma Branca',
                            hour %in% c("Arma De Fogo E Branca") ~ 'Arma De Fogo E Branca',
                            hour %in% c("Outros","Não Informado","Acidente") ~ 'Outros meios'))%>%
  mutate(category=as.character(NATUREZA_CRIME)) %>%

  mutate(newCat=case_when(NATUREZA_CRIME %in% c('Homicidio Doloso') ~ 'Homicidio Doloso',
                          NATUREZA_CRIME %in% c('Lesao Corporal Seguida De Morte') ~ 'Lesao Corporal Seguida De Morte', 
                          NATUREZA_CRIME %in% c('Roubo Seguido De Morte (Latrocinio)') ~ 'Roubo Seguido De Morte', 
                          NATUREZA_CRIME %in% c('Feminicidio') ~ 'Feminicidio', 
                          NATUREZA_CRIME %in% c("Morte Suspeita") ~NATUREZA_CRIME,
                          TRUE ~ 'Restante')) %>%
  
  group_by(hour_bkt,newCat) %>%
  summarise(n=n()) %>%
  mutate(hour_bkt_f=factor(hour_bkt, levels=c('Arma De Fogo','Arma Branca','Arma De Fogo E Branca','Outros meios'))) %>%
  ggplot(aes(x=hour_bkt_f,y=n,fill=newCat)) +
  geom_col(position="dodge") + 
  #theme_tufte() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow=5)) +
  ggtitle('Incidência de crime letal por tipo de arma e categoria de crime (2014 a 2018)') 
crimes

#================================================================================================================
# Pergunta 4: Crie um modelo que prevê o volume semanal de crimes em 2018 por tipo de crime
#================================================================================================================

# Vamos primeiro dar uma olhada em como o histórico volume semanal de incidentes por tipo de crime

volume_semana_crimes <- na.omit(crime_ceara) %>% mutate(DATA_MORTE = as.Date(DATA_MORTE, format = "%d/%m/%Y")) %>% 
  summarise(min=min(DATA_MORTE),max=max(DATA_MORTE)) %>%
  mutate(max_dow=lubridate::wday(max),min_dow=lubridate::wday(min)) %>%
  mutate(new_max=as.Date(max)-8,new_min=as.Date(min)+5)%>%
  mutate(new_max_dow=lubridate::wday(new_max),new_min_dow=lubridate::wday(new_min))
volume_semana_crimes

# Note que não temos semanas completas para o primeiro e o fim, vamos retratar os dados. 
# Dado dias de semanas são de segunda a domingo, vamos encontrar a primeira segunda-feira após o min_dow e o último domingo antes do max_dow

(cat_weekly_incidents <- na.omit(crime_ceara) %>% mutate(DATA_MORTE = as.Date(DATA_MORTE, format = "%d/%m/%Y")) %>% 
    select(DATA_MORTE, NATUREZA_CRIME) %>%
    group_by(NATUREZA_CRIME, week=cut(DATA_MORTE, "week")) %>%
    summarise(n=n(),unique_days=n_distinct(DATA_MORTE)))

# Vamos limitar os dados entre a primeira semana completa e a última semana completa (por exemplo, vamos remover a semana de 2002-12-30 e a semana de 2018-05-14)

CAT <- c('Homicidio Doloso')

cat_weekly_subset <- cat_weekly_incidents %>%
  filter(as.Date(week) > lubridate::ymd('2017-12-04') & as.Date(week) < lubridate::ymd('2018-12-17')) %>%
  filter(NATUREZA_CRIME==CAT) %>%
  ungroup() 
cat_weekly_subset %>%
  select(week,n)%>%
  ggplot(aes(x=as.Date(week),y=n))+
  geom_line()+
  geom_vline(xintercept=lubridate::ymd('2018-01-01'), linetype=4)+
  scale_x_date(date_breaks="1 year")+
  theme_tufte()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

cat_weekly_subset <- cat_weekly_incidents %>%
  filter(as.Date(week) > lubridate::ymd('2017-01-01') & as.Date(week) < lubridate::ymd('2018-12-17')) %>%
  filter(NATUREZA_CRIME=='Homicidio Doloso') %>%
  ungroup() 
cat_weekly_subset %>%
  select(week,n)%>%
  ggplot(aes(x=as.Date(week),y=n))+
  geom_line()+
  geom_vline(xintercept=lubridate::ymd('2018-01-01'), linetype=4)+
  scale_x_date(date_breaks="1 year")+
  theme_tufte()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Este gráfico acima que é ampliado. Seria problemático tentar prever 2018 com dados de 2017 para Homicidio Doloso, como você vê, há uma certa queda nos dados após 2018

tail(cat_weekly_subset,5)

# Por alguma razão, o Homicidio Doloso varia de ~700s para ~800s o que é meio estranho. 
# No entanto, existem 7 dias de dados válidos. Se eu tivesse tempo, gostaria de verificar os dados...

cat_weekly_incidents %>%
  filter(as.Date(week) > lubridate::ymd('2018-11-01')) %>%
  filter(NATUREZA_CRIME %in% c("Homicidio Doloso","Feminicidio","Roubo Seguido De Morte (Latrocinio)","Lesao Corporal Seguida De Morte","Latrocinio"))%>%
  select(week,NATUREZA_CRIME,n) %>%
  spread(NATUREZA_CRIME,n)

# As outras categorias principais realmente não parecem ser afetadas por esse problema. Vamos olhar as contagens totais

cat_weekly_incidents %>%
  filter(as.Date(week) > lubridate::ymd('2018-11-01')) %>%
  group_by(week) %>%
  summarise(total=sum(n))

# Por outro lado 2018-12-31 parece anormalmente baixo ... mas a diferença é provável a partir dos crimes perdidos no roubo. 
# Por agora vou manter 2018-05-07 mas definitivamente remover 2018-05-07

categories <- cat_weekly_incidents %>% 
  distinct(NATUREZA_CRIME) %>% 
  ungroup()
(category_vector <- categories$NATUREZA_CRIME)

# Vamos criar os conjuntos de dados para modelagem
train <- cat_weekly_incidents %>% 
  filter(as.Date(week) > lubridate::ymd('2014-01-01') & as.Date(week) < lubridate::ymd('2018-01-01')) %>%
  filter(lubridate::year(week)<2018)%>%
  select(NATUREZA_CRIME,week,n)

test <- cat_weekly_incidents %>% 
  filter(as.Date(week) > lubridate::ymd('2014-01-01') & as.Date(week) < lubridate::ymd('2018-01-01')) %>%
  filter(lubridate::year(week)==2018)%>%
  select(NATUREZA_CRIME,week,n)

summary(train)
summary(test)

category_vector

CAT = "Homicidio Doloso"
print(CAT)

fbtrain <- train %>%
  filter(NATUREZA_CRIME == CAT) %>%
  mutate(week=lubridate::ymd(week))%>%
  ungroup() %>%
  select(ds=week,y=n)

fbtest <- test %>%
  filter(NATUREZA_CRIME == CAT) %>%
  mutate(week=lubridate::ymd(week))%>%
  ungroup() %>%
  select(ds=week,y=n)

prophet_model <- function(df,cat){
  # Aumentá-lo do padrão de 0,05 para torná-lo mais flexível, já que parece haver um ponto de mudança extremo em 2012
  m <- prophet(changepoint.prior.scale =0.1, seasonality.mode = 'multiplicative',weekly.seasonality=TRUE,daily.seasonality=TRUE) 
  m <- add_country_holidays(m, country_name = 'BR')
  m <- add_seasonality(m, name='monthly', period=30.5, fourier.order=5)
  m <- fit.prophet(m,df)
  return(m)
}

model_CAT = prophet_model(fbtrain,CAT)
future <- make_future_dataframe(model_CAT, periods = 19, freq = 'week')
forecast <- predict(model_CAT, future)

prophet_plot_components(model_CAT, forecast)

plot(model_CAT,forecast)

(forecast_info <- forecast %>%
    select(ds,yhat_lower,yhat_upper,yhat)  %>%
    mutate(ds=as.Date(ds)))

(combined <- forecast_info %>%
    left_join(fbtrain) %>%
    rename(y_actual=y) %>%
    left_join(fbtest) %>%
    mutate(actual_merged = rowSums(.[,c("y", "y_actual")], na.rm=TRUE))) 

# Traçar real versus previsto com intervalos credíveis para o período de holdout
ggplot(data=combined, aes(x=ds)) +
  geom_line(aes(y=y_actual, colour = "Atual (Train)")) +        
  geom_line(aes(y=y, colour = "Atual (Test)")) +
  geom_line(aes(y=yhat, colour = "Fitted"), linetype=2,linesize=1.2) +
  theme_tufte() + theme(legend.title = element_blank()) + ylab("") + xlab("") +
  geom_vline(xintercept=as.numeric(as.Date("2018-01-01")), linetype=2) + 
  geom_ribbon(aes(ymin=yhat_lower, ymax=yhat_upper), fill="grey", alpha=0.5) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme(legend.position = "bottom") +
  ggtitle(paste("Previsão para ",CAT)) +
  guides(fill=guide_legend(nrow=5))

# Calcular MAPE

# MAPE (erro percentual absoluto médio)
MAPE <- filter(combined, lubridate::year(ds)==2018) %>%
  select(ds,yhat,actual_merged) %>%
  mutate(APE = abs(actual_merged-yhat)/actual_merged)%>%
  select(APE)%>%
  summarise(MAPE=mean(is.finite(APE)))%>%
  .[[1]]
MAPE*100

tail(combined)

(test_combined <- filter(combined, lubridate::year(ds)==2018) %>%
    select(ds,yhat,actual_merged))

forecast::accuracy(test_combined$yhat,test_combined$actual_merged,na.rm=TRUE)

new_combined <- test_combined %>% 
  filter(actual_merged != 0) 
# Precisa remover para cálculos MAPE em caso de divisão por zero erros
forecast::accuracy(new_combined$yhat,new_combined$actual_merged,na.rm=TRUE)

# FIM


#FONTE: https://www.kaggle.com/pranav84/the-most-dangerous-places-to-work-in-the-usa

df <- crime_ceara

df %>% group_by(MUNICIPIO_CRIME) %>% 
  #filter(count_Hospitalized !=0 || count_Amputation !=0) %>% 
  summarise(count = n()) %>% arrange(desc(count)) %>% head(15) %>%
  hchart("bar", hcaes(x = MUNICIPIO_CRIME, y = count, color=-count)) %>%
  hc_legend(enabled = TRUE) %>% 
  hc_add_theme(hc_theme_economist()) %>% 
  hc_xAxis(title = list(text = "Cidades")) %>% 
  hc_yAxis(title = list(text = "Volume de crimes")) %>% 
  hc_title(text = "Top 15 Cidades com incidencia de crimes letais entre Janeiro de 2014 a Dezembro de 2018") %>%
  hc_credits(enabled = TRUE, text = "Raspagens de Dados do site da SSPDS/CE", style = list(fontSize = "10px"))


df %>% group_by(MUNICIPIO_CRIME) %>% 
  filter(INCIDENCIA_CRIME > 100) %>% 
  summarise(count = n()) %>% arrange(desc(count)) %>% 
  hchart("bar", hcaes(x = MUNICIPIO_CRIME, y = count, color=-count)) %>%
  hc_legend(enabled = TRUE) %>% 
  hc_add_theme(hc_theme_economist()) %>% 
  
  hc_xAxis(title = list(text = "Cidades")) %>% 
  hc_yAxis(title = list(text = "Volume de crimes")) %>% 
  
  hc_title(text = "Cidades com incidencia de mais de 100 crimes letais entre Janeiro de 2014 a Dezembro de 2018") %>%
  hc_credits(enabled = TRUE, text = "Raspagens de Dados do site da SSPDS/CE", style = list(fontSize = "10px"))


p1 <-df %>%
  group_by(ARMA_UTILIZADA, ID) %>%
  filter(ARMA_UTILIZADA != "Não Informado") %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  #head(10) %>%
  plot_ly(labels = ~ARMA_UTILIZADA, values = ~count) %>%
  add_pie(hole = 0.5) %>%
  layout(title = "Incidencia de Crimes letais por tipo de arma utilizada",
         showlegend = T, legend = list(orientation = 'h'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p1


dfcity <- na.omit(df) %>% group_by(MUNICIPIO_CRIME) %>% 
  #filter(count_Hospitalized != 0 || count_Amputation != 0) %>%
  summarize(count = n()) %>% arrange(desc(count)) %>% head(10)

dfstate <- na.omit(df) %>% group_by(NATUREZA_CRIME) %>% 
  #filter(count_Hospitalized != 0 || count_Amputation != 0) %>%
  summarize(count = n()) %>% arrange(desc(count)) %>% head(10)

dfemp <- na.omit(df) %>% group_by(ARMA_UTILIZADA) %>% 
  #filter(count_Hospitalized != 0 || count_Amputation != 0) %>%
  summarize(count = n()) %>% arrange(desc(count)) %>% head(10)

highchart(height = "700px") %>% 
  hc_title(text = "Top 10 most dangerous employers, cities and states for workers") %>%
  hc_subtitle(text = "Based on number of workers hospitalized and amputated from 2015 onward | Pie chart 1 for cities and Pie chart 2 is for states") %>%
  hc_credits(enabled = TRUE, text = "Sources: Occupational Safety and Health Administration aka OSHA", 
             style = list(fontSize = "10px")) %>%
  hc_add_theme(hc_theme_sandsignika()) %>%
  
  
  hc_add_series(data = dfemp, hcaes(x = ARMA_UTILIZADA, y = count), name = "Show/ hide bar chart", 
                dataLabels = list(align = "center", enabled = TRUE),
                #colors = substr(heat.colors(10), 0 , 7),
                color = hex_to_rgba("red", 0.1),
                colorByPoint = TRUE, type = "column") %>% 
  
  hc_add_series(data = dfcity, hcaes(x = MUNICIPIO_CRIME, y = count), name = "Pie chart- MUNICIPIO_CRIME", 
                dataLabels = list(align = "center", enabled = TRUE),
                #colors = substr(heat.colors(10), 0 , 7),
                color = hex_to_rgba("red", 0.1),
                colorByPoint = TRUE, type = "pie",
                innerSize= '40%', size= "30%", showInLegend=FALSE
                #size = 100,
                #center = c('37%', '30%')) %>%
                ) %>% 
  
  # hc_add_series(data = dfstate, hcaes(x = NATUREZA_CRIME, y = count), name = "Pie chart- NATUREZA_CRIME", 
  #               dataLabels = list(align = "center", enabled = TRUE),
  #               #colors = substr(heat.colors(10), 0 , 7),
  #               color = hex_to_rgba("red", 0.1),
  #               colorByPoint = TRUE, type = "pie", 
  #               innerSize= '40%', size= "30%", showInLegend=FALSE,
  #               #size = 100,
  #               center = c('81%', '30%')) %>%
  #                #) %>% 

 # hc_yAxis(title = list(text = "Total counts of injury"), labels = list(format = "{value}"), max = 415) %>% 
 hc_xAxis(categories = dfemp$ARMA_UTILIZADA, title = list(text = "ARMA_UTILIZADA name")) %>% 
  hc_legend(enabled = TRUE, align= "left", verticalAlign = "bottom") %>% 
  hc_tooltip(pointFormat = "{point.y}")


highchart(height = "700px") %>% 
  hc_title(text = "Top 10 most dangerous employers, cities and states for workers") %>%
  hc_subtitle(text = "Based on number of workers hospitalized and amputated from 2015 onward | Pie chart 1 for cities and Pie chart 2 is for states") %>%
  hc_credits(enabled = TRUE, text = "Sources: Occupational Safety and Health Administration aka OSHA", 
             style = list(fontSize = "10px")) %>%
  hc_add_theme(hc_theme_sandsignika()) %>%
  
  
  hc_add_series(data = dfemp, hcaes(x = ARMA_UTILIZADA, y = count), name = "Show/ hide bar chart", 
                dataLabels = list(align = "center", enabled = TRUE),
                #colors = substr(heat.colors(10), 0 , 7),
                color = hex_to_rgba("red", 0.1),
                colorByPoint = TRUE, type = "column") %>% 
  
  # hc_add_series(data = dfcity, hcaes(x = MUNICIPIO_CRIME, y = count), name = "Pie chart- MUNICIPIO_CRIME", 
  #               dataLabels = list(align = "center", enabled = TRUE),
  #               #colors = substr(heat.colors(10), 0 , 7),
  #               color = hex_to_rgba("red", 0.1),
  #               colorByPoint = TRUE, type = "pie",
  #               innerSize= '40%', size= "30%", showInLegend=FALSE
  #               #size = 100,
  #               #center = c('37%', '30%')) %>%
  # ) %>% 
  
  hc_add_series(data = dfstate, hcaes(x = NATUREZA_CRIME, y = count), name = "Pie chart- NATUREZA_CRIME",
                dataLabels = list(align = "center", enabled = TRUE),
                #colors = substr(heat.colors(10), 0 , 7),
                color = hex_to_rgba("red", 0.1),
                colorByPoint = TRUE, type = "pie",
                innerSize= '40%', size= "30%", showInLegend=FALSE
                #size = 100,
                #center = c('81%', '30%')) %>%
                ) %>%
  
# hc_yAxis(title = list(text = "Total counts of injury"), labels = list(format = "{value}"), max = 415) %>% 
hc_xAxis(categories = dfemp$ARMA_UTILIZADA, title = list(text = "ARMA_UTILIZADA name")) %>% 
  hc_legend(enabled = TRUE, align= "left", verticalAlign = "bottom") %>% 
  hc_tooltip(pointFormat = "{point.y}")


# OUTRA FONTE: https://www.kaggle.com/headsortails/personalised-medicine-eda-with-tidy-r
