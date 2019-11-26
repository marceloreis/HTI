---
title: "Hackathon de Carreiras - Positivo"
author: "Marcelo Reis"
output:
  html_document:
    df_print: paged
---

**Proposta de visualização e análise: Demonstre o comportamento dos poluentes particulados PM2.5 e PM10 com base das informações disponíveis.**

Com base nos dados obtidos junto ao repositório da [UCI Machine Learning](https://archive.ics.uci.edu/ml/datasets/Beijing+Multi-Site+Air-Quality+Data), que retratam o monitoramento da qualidade do ar em locais controlados na China, é possível efetuar as análises descritas neste documento.

O desenvolvimento do instrumental de programação será feito em linguagem R, com os blocos de código documentados para reprodutibilidade da análise. A opção pela linguagem R ocorre em função de sua aplicabilidade natural para problemas em estatística e visualização de dados. Para visualização, são utilizadas as bibliotecas ggplot e plotly.

Inicialmente, é feita a carga de bibliotecas (*libraries*) que auxiliarão no desenvolvimento do estudo:

```{r bibliotecas,warning=FALSE,message=FALSE}
# Carrega bibliotecas necessárias
library(tidyverse)
library(stringr)
library(plotly)
library(psych)
library(GPArotation)
library(forecast)
```

A carga de dados é feita a partir da pasta /PRSA_Data_20130301-20170228, obtida pela descompressão do arquivo fornecido. Uma vez que a estrutura dos arquivos é idêntica, é utilizado um loop que interage com os arquivos CSV disponíveis, carregando todos os dados em um único *data.frame*. O campo *station* diferencia as estações.

```{r dados,message=FALSE,warning=FALSE}
# Carrega os dados de trabalho

# Obtém uma lista com todos os arquivos da pasta.
dados_poluicao_arquivos <- list.files(pattern="*",path = "./PRSA_Data_20130301-20170228/")

# Declara o data.frame com os campos e os formatos respectivos.
dados_poluicao <- data.frame(No=integer(),year=integer(),month=integer(),day=integer(),hour=integer(),PM2.5=numeric(),PM10=numeric(),SO2=numeric(),NO2=numeric(),CO=numeric(),O3=numeric(),TEMP=numeric(),PRES=numeric(),DEWP=numeric(),RAIN=numeric(),wd=character(),WSPM=numeric(),station=character())

# Loop de interação por todos os arquivos da pasta.
for(i in 1:length(dados_poluicao_arquivos))
{
  # Lê os dados no dataframe principal...
  dados_poluicao <- rbind(dados_poluicao,read.csv(paste0("./PRSA_Data_20130301-20170228/",dados_poluicao_arquivos[i])))  
}

# Cria um campo adicional com a data em formato ISO - data_completa
dados_poluicao$data_completa <- ISOdate(dados_poluicao$year,dados_poluicao$month,dados_poluicao$day,dados_poluicao$hour)
```

# Análise geral das variáveis

Inicialmente, como trata-se de um quadro de dados com múltiplas colunas numéricas, cabe fazer algumas incursões sobre a análise multivariada, a fim de compreender a relação entre as variáveis.

O correlograma a seguir mostra a correlação para este quadro de dados. Notar que as variáveis PM2.5 e PM10 são fortemente correlacionadas. Outro valor de interesse, fortemente correlacionado a ambas, é a variável CO, que indica a concentração de monóxido de carbono na atmosfera nos sites de mensuração.

```{r variaveis,message=FALSE,warning=FALSE}
correlacao <- as.data.frame(cor(na.omit(dados_poluicao[,5:14]))) 
correlacao$indice = row.names(correlacao)

correlacao %>% gather(I,A,-indice) %>% plot_ly(x=~indice,y=~I,z=~A,type="heatmap",colors="Blues")
```

Já para a análise fatorial, utilizando-se as funções do pacote *psych*, inicialmente é estimado o número de fatores:

```{r analise_fatorial_previa,warning=FALSE,error=FALSE,message=FALSE}
fa.parallel(na.omit(dados_poluicao[,5:14]))
```

É feita então a análise fatorial com 4 fatores:

```{r analise_fatorial,warning=FALSE,error=FALSE,message=FALSE}
factor_poluicao <- fa(na.omit(dados_poluicao[,5:14]),nfactors=4,rotate="Varimax")
summary(factor_poluicao)
fa.diagram(factor_poluicao)
```

Observa-se que a raiz média quadrada dos resíduos (RMSA) é 0.01, bastante próxima a zero, o que indica a adequação do modelo. O fator MR1 é carregado pelas variáveis PM2.5, PM10, CO e SO2, que podem, de maneira genérica, ser associadas a um fator específico na variação das medições de poluição atmosférica.

Tendo em vista as conclusões da análise multivariada, será desenvolvido o estudo apenas para a variável do particulado fino (PM2.5), pois os resultados para a análise da variável PM10 replicariam quase todas as conclusões da análise da variável PM2.5.

# Evolução do particulado de 2.5 $\mu$m

O campo PM2.5 faz referência ao material particulado fino, de diâmetro menor que 2.5$\mu$m.

## Omissões e confiabilidade dos dados

Inicialmente, para teste da confiabilidade dos dados, é necessário comparar o total de medições para cada estação, horário e ano.

O total de medições, por estação, conforme o horário, é demonstrado no gráfico a seguir.

```{r qtd_hora_estacao}
dados_poluicao %>% filter(!is.na(PM2.5)) %>% group_by(hour,station) %>% tally() %>% plot_ly(x=~hour,y=~station,z=~n, type = "heatmap",colorscale="Cividis")
```

Nota-se que a variação -- de 1400 a cerca de 1450 medições por ano -- nos permite concluir que não há problemas graves de omissão de dados em horários específicos, independente da estação.

Conforme ano e estação, obtemos o gráfico seguinte:

```{r qtd_ano_estacao}
dados_poluicao %>% filter(!is.na(PM2.5)) %>% group_by(year,station) %>% tally() %>% plot_ly(x=~year,y=~station,z=~n, type = "heatmap",colorscale="Cividis")
```

Aqui a variação já é considerável, notando-se porém certa homogeneidade entre a disponibilidade de dados entre os anos 2014 e 2016. O ano de 2013 possui um número menor de observações, porém ainda dentro de um volume razoável (cerca de 7 mil, contra 8 mil dos outros anos). Para decidir se o ano de 2013 será contemplado, é aplicado um teste de análise de variância, com os anos 2013 e 2016. A variável de tratamento é o ano (*year*).

```{r anova_2013_2015}
# Aplicação de análise de variância (ANOVA simples/one-way).

# Seleciona amostra de 1500 observações dos anos 2013 e 2016:
set.seed(999)
anova_dados <- dados_poluicao %>% filter(year <= 2016) %>% mutate(year = as.factor(year)) %>% select(year,PM2.5) %>% sample_n(1500)

# Aplica o teste.
anova_modelo <- lm(PM2.5~year,data=anova_dados)
anova(anova_modelo)
```

O resultado da ANOVA mostra que os grupos não apresentam diferença significativa conforme o tratamento, de modo que o ano de 2013 apresenta uma distribuição de valores para o particulado PM2.5 que pode ser analisada.

Por cautela, para as análises seguintes, será desprezado o ano de 2017, devido ao número menor de observações.

## Dados acumulados e média

O valor acumulado do aglomerado particulado PM2.5, ano a ano, pode ser visualizado no gráfico seguinte:

```{r acumulado_pm25,warning=FALSE,message=FALSE}
dados_poluicao %>% filter(year <= 2016) %>% mutate(year = as.factor(year)) %>% group_by(station,year) %>% summarise(acumuladoPM25 = sum(PM2.5,na.rm = T)) %>% plot_ly(x=~year,y=~acumuladoPM25,color=~station,colors="Set3") %>% layout(yaxis = list(title = 'Acumulado PM2.5'), barmode = 'stack')
```

A média anual, conforme a estação, é ilustrada no gráfico seguinte:

```{r media_particulado2.5,message=FALSE,warning=FALSE}
dados_poluicao %>% filter(year <= 2016) %>% mutate(year = as.factor(year)) %>% group_by(station,year) %>% summarise(mediaPM25 = sum(PM2.5,na.rm = T)) %>% 
plot_ly(x=~year,y=~mediaPM25,color=~station,colors="Set3") %>% add_lines() %>% layout(yaxis = list(title = 'Média PM2.5'))
```

Notamos que as estações Huairou e Dingling apresentam curvas em patamares inferiores às demais, indicando locais com menor concentração, independente do ano, de particulado fino.

Ao longo dos anos 2013 a 2016, a evolução da concentração do particulado PM2.5 pode ser demonstrada conforme os gráficos:

```{r evolucao_particulado2.5_anos,message=FALSE,warning=FALSE}
ev_poluicao1 <- dados_poluicao %>% filter(year <= 2016) %>% ggplot(aes(data_completa,PM2.5)) + geom_smooth(se=FALSE) + theme_bw() + scale_color_brewer(palette="Set3")
ggplotly(ev_poluicao1)

ev_poluicao2 <- dados_poluicao %>% filter(year <= 2016) %>% mutate(year = as.factor(year)) %>% group_by(station) %>% ggplot(aes(data_completa,PM2.5,color=station)) + geom_smooth(se=FALSE) + theme_bw() + scale_color_brewer(palette="Set3")
ggplotly(ev_poluicao2)
```

A evolução pode ser interpretada como uma série temporal, conforme o gráfico a seguir. Para a projeção futura, utiliza-se a suavização de Holt-Winters:

```{r evolucao_timeseries}
evolucao_ts <- dados_poluicao %>% filter(year <= 2016) %>% group_by(year,month) %>% summarise(media_mes = mean(PM2.5,na.rm=TRUE)) %>% ungroup() %>% select(media_mes) %>% ts(frequency = 12,start = c(2013,3)) 
evolucao_forecast <- HoltWinters(evolucao_ts)
plot(evolucao_forecast)

autoplot(forecast(evolucao_forecast,h=48))
```

A concentração de poluentes apresenta comportamento cíclico, atingindo o auge no início de cada ano (exceto em 2015). O comportamento é esperado, pois os meses de dezembro a fevereiro coincidem com o inverno no hemisfério norte, estação em que tradicionalmente a poluição é menos dispersa. Para os anos seguintes, o comportamento esperado é similar ao demonstrado entre 2013 e 2016. 

A evolução da concentração do particulado fino PM2.5 pode ser observada também no gráfico horário, que mostra a concentração em sucessivas observações ao longo do dia para cada uma das estações:

```{r evolucao_particulado2.5_horario,message=FALSE,warning=FALSE}
# Opção pelo ggplot convencional.
dados_poluicao %>% filter(year <= 2016) %>% mutate(year = as.factor(year)) %>% group_by(station,hour,year) %>% summarise(mediaPM25 = mean(PM2.5,na.rm = T)) %>% ggplot(aes(hour,mediaPM25,color=year)) + geom_line(size=1.2) + scale_y_log10() + facet_wrap(station~.) + scale_color_brewer(palette="Dark2") + theme_bw() + theme(legend.position = "bottom")
```

O comportamento da concentração de material particulado fino PM2.5 apresenta um padrão em quase todas as estações, com maiores concentrações nos períodos da madrugada e noite (depois das 18h e antes das 5h). Novamente, as estações Dinling e Huairou apresentam comportamento ligeiramente diferente, com diminuição mais gradual das concentrações na manhã e menores concentrações do material.

# Conclusões

  1. Os sensores podem ser mais simples, efetuando a medição de apenas um tipo de particulado ou de concentrações de gases específicos.
  2. O material particulado apresenta comportamento sazonal e maior prevalência no inverno, quando atinge níveis preocupantes.
  3. Duas estações mostraram comportamento diferenciado da concentração do particulado fino PM2.5, devendo proceder-se investigações mais amplas acerca das características locais que impactam na menor poluição atmosférica.