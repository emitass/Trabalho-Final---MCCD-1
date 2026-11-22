#1. Bibliotecas -----------------------------------------------------------------------------------------------------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

#1.1 biblioteca para o mapa
library(plotly)

caminho_dataset <- file.path("..", "Dataset", "save_the_bees.csv")
dados_brutos <- read.csv(caminho_dataset)

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

# Anos disponiveis para ser usado no filtro do mapa
anos_disponiveis <- sort(unique(dados_tratados$year), decreasing = TRUE)


ui <- dashboardPage(
  skin = "yellow", #cor de fundo do cabeçalho
  
  # cabeçalho
  dashboardHeader(title = "Save the Bees"),
  
  # barra lateral
  dashboardSidebar(
    br(),
    
    div(
      align = "center",
      img(
        src = "images/imagem.png", 
        width = "120px",
        height = "120px",
        style = "background-color: white; padding: 10px; border-radius: 50%; object-fit: contain;"
      ),
      p("DEST - Estatística", style = "margin-top: 8px; font-size: 12px;")
    ),
    
    hr(),
    
    # menu de navegação
    sidebarMenu(
      menuItem("Introdução", tabName = "aba1", icon = icon("info-circle")),
      menuItem("Gráfico 1", tabName = "aba2", icon = icon("chart-bar")), # alterar os nomes depois
      menuItem("Gráfico 2", tabName = "aba3", icon = icon("chart-line")),
      menuItem("Mapa", tabName = "aba4", icon = icon("map-marker-alt")),
      menuItem("Banco de Dados Tratado", tabName = "aba5", icon = icon("table")),
      menuItem("Conclusões", tabName = "aba6", icon = icon("check-circle"))
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
      tags$link(rel = "stylesheet", type = "text/css", href = "css/tema_abelha.css")
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
      
      # ABA 4 MAPA
      tabItem(
        tabName = "aba4",
        h2("Mapa do numero de colonias por estado"),
        fluidRow(
          box(
            width = 20,
            status = "info",
            selectInput(
              inputId = "ano_selecionado",
              label = "Selecione o ano:",
              choices = anos_disponiveis,
              selected = anos_disponiveis[1],
              width = 200
            ),
            mainPanel(
              plotlyOutput("mapa_eua", width = "900px")
            )
            ),
          ),
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

server <- function(input, output) {
  
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
  
  # Grafico do mapa dos EUA
  cores_mel <- list(
    list(0.0, "#FFF7D6"),
    list(0.35, "#F7C948"),
    list(0.7, "#D9821B"),
    list(1.0, "#6E3500")
  )
  
  dados_mapa_ano <- reactive({
    req(input$ano_selecionado)
    
    dados_tratados %>%
      filter(
        state != "Other",
        year == as.numeric(input$ano_selecionado)
      ) %>%
      group_by(Estado = state, Sigla = state_code) %>%
      summarise(
        colonias_iniciais = sum(num_colonies, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  output$mapa_eua <- renderPlotly({
    dados_mapa <- dados_mapa_ano() %>%
      mutate(
        valor_cor = ifelse(colonias_iniciais > 0, log10(colonias_iniciais), NA_real_)
      )
    
    plot_geo(locationmode = "USA-states") %>%
      add_trace(
        data = dados_mapa,
        z = ~valor_cor,
        locations = ~Sigla,
        text = ~paste0(
          Estado, " (", Sigla, ")",
          "<br>Ano: ", input$ano_selecionado,
          "<br>Colônias Iniciais: ", format(colonias_iniciais, big.mark = ".", decimal.mark = ",")
        ),
        hoverinfo = "text",
        colorscale = cores_mel,
        showscale = TRUE,
        colorbar = list(
          title = "Colônias<br>(escala log)",
          # Mostra valores reais nos marcadores da barra de cores
          tickvals = pretty(range(dados_mapa$valor_cor, na.rm = TRUE)),
          ticktext = format(
            round(10^pretty(range(dados_mapa$valor_cor, na.rm = TRUE))),
            big.mark = ".",
            decimal.mark = ","
          )
        )
      ) %>%
      layout(
        geo = list(
          scope = "usa",
          projection = list(type = "albers usa"),
          showlakes = TRUE,
          lakecolor = toRGB("white")
        )
      )
  })
}

shinyApp(ui, server)

