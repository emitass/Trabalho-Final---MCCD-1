#1. Bibliotecas -----------------------------------------------------------------------------------------------------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

# pacotes para criar os gráficos e tabelas:
# library(dplyr)
# library(ggplot2)
# library(plotly)
# library(leaflet)
# 

# 2. Tratamento dos Dados -------------------------------------------------------------------------------------------------------------------------------------------------------

# leitura da base de dados
dados_brutos <- read.csv("save_the_bees.csv")

# padronização de tipos e criação de variáveis
dados_tratados <- dados_brutos %>%
  distinct() %>% # garante ausência de linhas duplicadas
  mutate(
    data_trimestre = as.Date(paste0(year, "-", (quarter - 1) * 3 + 1, "-01")), # data de início do trimestre
    trimestre_rotulo = factor(
      quarter,
      levels = c(1, 2, 3, 4), # renomeando os elementos da coluna "quarter"
      labels = c("T1 (Jan-Mar)", "T2 (Abr-Jun)", "T3 (Jul-Set)", "T4 (Out-Dez)") # ordenação dos trimestres
    ),
    
    # criação de variáveis derivadas
    saldo_colonias = added_colonies - lost_colonies,
    taxa_perda_calc = round((lost_colonies / max_colonies) * 100, 2),
    taxa_renovacao_calc = round((renovated_colonies / max_colonies) * 100, 2),
    
    # identificação do nível de registro (Nacional vs. Estadual vs. Outros)
    nivel_geografico = case_when(
      state == "United States" ~ "Nacional",
      state == "Other" ~ "Outros Estados",
      TRUE ~ "Estado"
    )
  )

# estados individuais (para o Mapa do país e/ou rankings estaduais)
dados_estados <- dados_tratados %>%
  filter(nivel_geografico == "Estado")

# agregado nacional dos EUA (para evolução histórica geral)
dados_eua <- dados_tratados %>%
  filter(nivel_geografico == "Nacional")

# wide para longo (para criação de gráficos)
dados_fatores <- dados_tratados %>%
  select(
    state, state_code, nivel_geografico, year, quarter,
    trimestre_rotulo, data_trimestre,
    varroa_mites, other_pests_and_parasites, diseases,
    pesticides, other, unknown
  ) %>%
  pivot_longer(
    cols = c(varroa_mites, other_pests_and_parasites, diseases, pesticides, other, unknown),
    names_to = "fator",
    values_to = "percentual_afetado"
  ) %>%
  mutate(
    fator_pt = case_when(
      fator == "varroa_mites" ~ "Ácaro Varroa",
      fator == "other_pests_and_parasites" ~ "Outras Pragas e Parasitas",
      fator == "diseases" ~ "Doenças",
      fator == "pesticides" ~ "Pesticidas",
      fator == "other" ~ "Outros Fatores",
      fator == "unknown" ~ "Causa Desconhecida"
    )
  )

# tabela renomeada em português
dados_tabela <- dados_tratados %>%
  filter(state != "Other") %>%
  select(
    Estado = state,
    Sigla = state_code,
    Ano = year,
    Trimestre = trimestre_rotulo,
    `Colônias Iniciais` = num_colonies,
    `Máx. Colônias` = max_colonies,
    `Colônias Perdidas` = lost_colonies,
    `Perda (%)` = taxa_perda_calc,
    `Colônias Adicionadas` = added_colonies,
    `Colônias Renovadas` = renovated_colonies,
    `Renovação (%)` = taxa_renovacao_calc,
    `Saldo Líquido` = saldo_colonias,
    `Ácaro Varroa (%)` = varroa_mites,
    `Outras Pragas/Parasitas (%)` = other_pests_and_parasites,
    `Doenças (%)` = diseases,
    `Pesticidas (%)` = pesticides,
    `Outros Fatores (%)` = other,
    `Causa Desconhecida (%)` = unknown
  )

#3. Interface do Usuário (UI) ---------------------------------------------------------------------------------------------------------------------------------------------------
ui <- dashboardPage(
  skin = "yellow", #cor de fundo do cabeçalho
  
  # cabeçalho
  dashboardHeader(title = "Save the Bees"),
  
  # barra lateral
  dashboardSidebar(
    br(),
    
    # imagem (tinha pensado em colocar o brasão da ufmg ou algo do dest)
    div(
      align = "center",
      img(
        src = "imagem.png", 
        width = "120px",
        height = "120px",
        style = "background-color: white; padding: 10px; border-radius: 50%; object-fit: contain;"
      ),
      p("DEST - Estatística", style = "margin-top: 8px; font-size: 12px;")
    ),
    
    hr(),
    
    # menu de navegação
    sidebarMenu(
      # os ícones são padrões do R e podem ser alterados, se quiserem
      menuItem("1. Introdução", tabName = "aba1", icon = icon("info-circle")),
      menuItem("2. Gráfico 1", tabName = "aba2", icon = icon("chart-bar")), # alterar os nomes depois
      menuItem("3. Gráfico 2", tabName = "aba3", icon = icon("chart-line")),
      menuItem("4. Mapa", tabName = "aba4", icon = icon("map-marker-alt")),
      menuItem("5. Banco de Dados Tratado", tabName = "aba5", icon = icon("table")),
      menuItem("6. Conclusões", tabName = "aba6", icon = icon("check-circle"))
    ),
    
    hr(),
    
    # rodapé da barra lateral
    div(
      style = "padding: 15px; font-size: 12px;",
      strong("Informações do Projeto"),
      br(), br(),
      "Integrantes: Diego Lima, Emilly Santos, Felipe Bicalho, Gabriela Barreto",
      br(),br(),
      "Disciplina: Métodos Computacionais para Ciência de Dados 1",
      br(),br(),
      "Semestre: 2026/2"
    )
  ),
  
  # corpo principal
  dashboardBody(
    
    # carrega o css da página
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "tema_abelha.css")
    ),
    
    tabItems(
      # 1. aba 1 - apresentação
      tabItem(
        tabName = "aba1",
        h2("Título 1"),
        fluidRow(
          box(
            title = "Apresentação e Motivação",
            width = 12,
            status = "info", #cores das bordas e fundo dos cabeçalhos
            solidHeader = TRUE,
            p("Apresentação do tema, motivação, objetivos do trabalho e descrição rápida do banco de dados.")
          )
        )
      ),
      
      # aba 2 - gráfico 1
      tabItem(
        tabName = "aba2",
        h2("Título 2"),
        fluidRow(
          box(
            title = "Barra de Filtros",
            width = 12,
            status = "info",
            p("Inputs (selectInput, sliderInput, etc.) do gráfico 1.")
          )
        ),
        fluidRow(
          box(
            title = "Nome do Gráfico 1",
            width = 12,
            status = "info",
            p("Gráfico 1.")
          )
        )
      ),
      
      # aba 3 - gráfico 2
      tabItem(
        tabName = "aba3",
        h2("Título 3"),
        fluidRow(
          box(
            title = "Barra de Filtros",
            width = 12,
            status = "info",
            p("Inputs (selectInput, sliderInput, etc.) do gráfico 2.")
          )
        ),
        fluidRow(
          box(
            title = "Nome do Gráfico 2",
            width = 12,
            status = "info",
            p("Gráfico 2.")
          )
        )
      ),
      
      # aba 4 - mapa (? dependendo do banco de dados que a gente pegar não dá pra fazer um mapa mapa, mas deve dar pra fazer outra coisa)
      tabItem(
        tabName = "aba4",
        h2("Título 4"),
        fluidRow(
          box(
            title = "Barra de Filtros",
            width = 12,
            status = "info",
            p("Inputs (selectInput, sliderInput, etc.) do mapa.")
          )
        ),
        fluidRow(
          box(
            title = "Nome do Mapa",
            width = 12,
            status = "info",
            p("Mapa.")
          )
        )
      ),
      
      # aba 5 - banco de dados (é bom pro usuário ter acesso direto a linhas específicas/número de observações filtradas)
      tabItem(
        tabName = "aba5",
        h2("Banco de Dados Tratado - Monitoramento de Abelhas (EUA)"),
        fluidRow(
          box(
            title = "Exploração Interativa dos Registros (2015–2022)",
            width = 12,
            status = "info",
            p("Use a caixa de pesquisa à direita ou os filtros logo abaixo dos nomes das colunas para filtrar por estado, ano, trimestre ou valores específicos."),
            DTOutput("tabela_dados")
          )
        )
      ),
      
      # TELA 6
      tabItem(
        tabName = "aba6",
        h2("Título 6"),
        fluidRow(
          box(
            title = "Conclusões",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            p("Conclusões tiradas a partir dos gráficos. Alguma notícia/pesquisa que confirme mais ainda a nossa conclusão etc.")
          )
        )
      )
      
    )
  )
)

#4. Lógica do Sevidor (Sever) --------------------------------------------------------------------------------------------------------------------------------------------------
server <- function(input, output) {
  
  #criação dos gráficos e tabelas (abas 2, 3, 4 e 5)
  
  #aba 5 - tabela com o banco dde dados tratado
  output$tabela_dados <- renderDT({
    datatable(
      dados_tabela,
      rownames = FALSE, # esconde a coluna extra com a numeração das linhas
      filter = "top",   # coloca uma caixinha de filtro em cima de cada coluna
      options = list(
        pageLength = 10, # exibe 10 linhas por página
        scrollX = TRUE,  # ativa rolagem horizontal para caber todas as 18 colunas 
        language = list(
          search = "Pesquisar:",
          lengthMenu = "Mostrar _MENU_ registros",
          info = "Mostrando de _START_ até _END_ de _TOTAL_ registros",
          infoFiltered = "(filtrado de _MAX_ registros no total)",
          zeroRecords = "Nenhum registro encontrado",
          paginate = list(previous = "Anterior", `next` = "Próximo")
        )
      )
    )
  })
}

#5. Rodar o aplicativo ---------------------------------------------------------------------------------------------------------------------------------------------------------
shinyApp(ui, server)

