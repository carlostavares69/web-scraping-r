### Web Scraping with R in VS Code and Jupyter Notebook

![R](https://scrape-it.cloud/assets/cache_image/assets/blog_img/web-scraping-in-r_1280x533_301.webp)

Data Scraping dos Indicadores Criminais a partir do [WebSite](https://www.sspds.ce.gov.br/) da Secretaria da Segurança Pública e Defesa Social do Estado do Ceará (SSPDS Ceará) com registros diários dos Crimes Violentos Letais e Intencionais - CVLI cometidos no Estado do Ceará.

#### Central Objective
Através da técnica de raspagem de dados, extrair informações dos documentos de indicadores criminais diários disponibilizados no portal da SSPDS-CE.

##### Specifics Objectives
**1.** Possibilitar a fácil compreensão das informações disponibilizadas em determinado site governamental através da visualização gráfica dos dados analisados.

**2.** Servir como modelo e fonte de aprendizado possibilitando as pessoas com interesse em explorar a técnica de Data Scraping para obter dados de páginas web.

**3.** Motivar a comunidade Open Data e contribuir com diversos outros projetos com análises de dados.

#### Phases
O processo da Raspagem de Dados é divido em cinco principais partes:
**Coleta**, **Limpeza**, **Tratamento**, **Análise** e **Visualização** dos Dados.

#### Environment
Linguagem **R**, **Jupyter Notebook**, **Visual Code** como ambiente de desenvolvimento integrado utilizando **linguagem de programação R** com computação estatística.

> *Setup Jupyter R Kernel in VS Code*
  ```cmd
  install.packages('IRkernel')
  IRkernel::installspec()
  ```
> **Carregando Kernels de diretórios inseguros**
  - No VS Code digite `Ctrl + ,`
  - Em configurações de pesquisa, filtre por `jupyter.kernels.trusted`
  - Adicionar Item `C:\ProgramData\jupyter\kernels\ir\kernel.json`

> **Indicadores criminais não inclusos**
  https://www.sspds.ce.gov.br/estatisticas-2-2-2-2-2-2
  Os indicadores criminais do ano de 2013 não foram incluídos nas raspagens por conter apenas quantitativos mensais.

##### REFERENCES
R Core Team (2019). **R: A language and environment for statistical computing.** R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

RStudio Team (2018). **RStudio: Integrated Development for R.** RStudio, Inc., Boston, MA URL http://www.rstudio.com/.

LAVOR, T. M. O. **Open Data Day Fortaleza 2019: os highlights do evento.** Disponível em: http://br.okfn.org/2019/04/02/open-data-day-fortaleza-2019-os-highlights-do-evento/. Acesso em: 05 Out. 2019.

RAMOS, E. S. et al. **Firearm death rates in Ceará, Brazil: differences between sexes**. International Journal of Development Research, v. 11, p. 6 pages, 2021. Disponível em: https://www.journalijdr.com/firearm-death-rates-cear%C3%A1-brazil-differences-between-sexes. Acesso em: 15 Fev. 2024.