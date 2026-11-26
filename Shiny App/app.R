#1. Bibliotecas -----------------------------------------------------------------------------------------------------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)

#2. Tratamento de Dados ----------------------------------------------------------------------------------------------------------------------------------------------------------
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

#lista dos estados para aba 2
lista_estados <- sort(unique(dados_estados$state))

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
# Para as visualizacoes da Aba 3, usamos somente estados individuais.
# Isso evita misturar os registros estaduais com o total nacional e "Other".
dados_fatores_estados <- dados_fatores %>%
  filter(
    nivel_geografico == "Estado",
    !is.na(percentual_afetado)
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

# Opcoes usadas nos filtros da Aba 3
anos_disponiveis <- sort(unique(dados_fatores_estados$year))
trimestres_disponiveis <- levels(dados_tratados$trimestre_rotulo)

opcoes_ano <- c("Todos os anos" = "Todos", as.character(anos_disponiveis))
opcoes_trimestre <- c(
  "Todos os trimestres" = "Todos",
  stats::setNames(trimestres_disponiveis, trimestres_disponiveis)
)

# Ordem padrao dos fatores nos graficos
ordem_fatores <- c(
  "Ácaro Varroa",
  "Outras Pragas e Parasitas",
  "Pesticidas",
  "Outros Fatores",
  "Causa Desconhecida",
  "Doenças"
)

#3. Layout ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
      menuItem("Sazonalidade", tabName = "aba2", icon = icon("chart-bar")), # alterar os nomes depois
      menuItem("Fatores de Estresse", tabName = "aba3", icon = icon("chart-line")),
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
        h2("Introdução"),
        fluidRow(
          box(
            title = "Apresentação e Motivação",
            width = 12,
            status = "info", #cores das bordas e fundo dos cabeçalhos
            solidHeader = TRUE,
            p("Por que ", em("honeybees"), "?"),
            
            p("As abelhas do gênero Apis, em particular a abelha-do-mel, destacam-se como os polinizadores domesticados mais comuns no mundo (IPBES,2016). Assim, tem um papel essencial na produção de alimentos e manutenção de ecossistemas: aproximadamente 35% da safra global depende de polinização, sendo que a diversidade e abundância de polinizadores contribui para o rendimento das safras.
            Segundo especialistas da FAO (Nações Unidas), um terço da produção alimentícia mundial depende de abelhas."),
            
            p("Com este trabalho, o grupo pretende verificar variações na população de abelhas, quais são as principais ameaças às colônias de abelhas onde elas se situam com dados referentes aos Estados Unidos da América."),
            
            p("A base foi divulgada pelo USDA (Departamento de Agricultura). Acompanham colônias entre 2015 e 2022, apresentando dados sobre a quantidade de colônias, sua localização e os fatores de estresse."),
            
          ),
          box(
            title = "Como as abelhas nos ajudam",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            div(
              style = "text-align: center;",
              p("Cerca de 70 entre os 100 alimentos mais produzidos são polinizados pelas abelhas."),
              img(
                src = "images/bee1.jpeg",
                width = "900px",
                style = "max-width: 100%; height: auto; margin-top: 10px;"
              ),
              p(
                style = "margin-top: 8px; font-size: 13px; color: #555;",
                "(Fonte: ",
                a(
                  href = "https://www.bbc.co.uk/teach/articles/zkf292p",
                  "BBC",
                  target = "_blank"
                ),
                ")"
              )
            )
          )
        )
      ),
      
      #aba 2: gráficos de sazonalidade
      tabItem(
        tabName = "aba2",
        h2("Evolução Sazonal das Colônias por Estado"),
        fluidRow(
          box(
            title = "Ano por Trimestres",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            fluidRow(
              column(
                width = 4,
                selectInput(
                  inputId = "estado_aba2",
                  label = "Selecione o Estado:",
                  choices = lista_estados,
                  selected = "Maine"
                )
              ),
              column(
                width = 4,
                sliderInput(
                  inputId = "ano_aba2",
                  label = "Selecione o Ano:",
                  min = min(dados_estados$year),
                  max = max(dados_estados$year),
                  value = min(dados_estados$year), 
                  step = 1,
                  sep = "",
                  animate = animationOptions(interval = 1200, loop = FALSE)
                )
              ),
              column(
                width = 4,
                radioButtons(
                  inputId = "taxa_aba2",
                  label = "Selecione a Taxa:",
                  choices = c(
                    "Taxa de Perda (%)" = "percent_lost",
                    "Taxa de Renovação (%)" = "percent_renovated"
                  ),
                  selected = "percent_lost",
                )
              )
            ),
            p(
              class = "nota-metodologica",
              "Alterne entre os estados, os anos e os tipos de taxa para observar a sazonalidade e a tendência temporal de perda ou renovação de colônias."
            ),
            plotlyOutput("grafico_linha_aba2", height = "480px")
          )
        )
      ),
      
   # ABA 3 - FATORES DE ESTRESSE 
      tabItem(
        tabName = "aba3",
        h2("Fatores de Estresse nas Colônias de Abelhas"),

        fluidRow(
          box(
            title = "Percentual médio por fator de estresse",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            fluidRow(
              column(
                width = 6,
                selectInput(
                  inputId = "ano_fatores",
                  label = "Selecione o ano:",
                  choices = opcoes_ano,
                  selected = "Todos"
                )
              ),
              column(
                width = 6,
                selectInput(
                  inputId = "trimestre_fatores",
                  label = "Selecione o trimestre:",
                  choices = opcoes_trimestre,
                  selected = "Todos"
                )
              )
            ),
            p(
              class = "nota-metodologica",
              "O gráfico apresenta a média do percentual informado pelos estados ",
              "para cada fator. A opção 'Todos' considera todos os anos e/ou ",
              "trimestres disponíveis."
            ),
            plotlyOutput("grafico_barras_fatores", height = "430px")
          )
        ),

        fluidRow(
          box(
            title = "Evolução dos fatores de estresse (2015–2022)",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            selectInput(
              inputId = "trimestre_heatmap",
              label = "Selecione o trimestre:",
              choices = opcoes_trimestre,
              selected = "Todos"
            ),
            p(
              class = "nota-metodologica",
              "O mapa de calor compara, ao longo dos anos, a média estadual ",
              "de cada fator. Passe o mouse sobre uma célula para consultar ",
              "o percentual correspondente."
            ),
            plotlyOutput("mapa_calor_fatores", height = "470px")
          )
        )
      ),

     # aba 4 - mapa
     tabItem(
       tabName = "aba4",
       h2("Visualização de dados nos Estados Unidos"),
       fluidRow(
         box(
           title = "Mapa do número de colônias por estado",
           width = 12,
           status = "info",
           div(
             style = "display: flex; justify-content: center; margin-bottom: 5px;",
             selectInput(
               inputId = "ano_selecionado",
               label = "Selecione o ano:",
               choices = anos_disponiveis,
               selected = anos_disponiveis[1],
               width = "220px"
             )
           ),
           div(
             style = "max-width: 920px; margin: 0 auto;",
             plotlyOutput("mapa_eua", width = "100%", height = "460px")
           )
         )
       ),
       fluidRow(
         box(
           title = "Mapa da porcentagem de colônias perdidas por estado",
           width = 12,
           status = "info",
           div(
             style = "display: flex; justify-content: center; margin-bottom: 5px;",
             selectInput(
               inputId = "ano_selecionado2",
               label = "Selecione o ano:",
               choices = anos_disponiveis,
               selected = anos_disponiveis[1],
               width = "220px"
             )
           ),
           div(
             style = "max-width: 920px; margin: 0 auto;",
             plotlyOutput("mapa_eua2", width = "100%", height = "460px")
           )
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
      
        # ABA 6 - CONCLUSOES 
      tabItem(
        tabName = "aba6",
        h2("Conclusões"),

        fluidRow(
          box(
            title = "Síntese dos resultados",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            uiOutput("texto_conclusoes")
          )
        ),

        fluidRow(
          box(
            title = "Relação com pesquisas sobre a saúde das abelhas",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            div(
              class = "info-analise",
              p(
                "O destaque do ácaro Varroa nos dados é coerente com a literatura ",
                "científica. O relatório do Departamento de Agricultura dos Estados ",
                "Unidos (USDA) descreve o Varroa destructor como um dos principais ",
                "fatores associados às perdas de colônias, além de apontar sua ",
                "capacidade de enfraquecer as abelhas e transmitir vírus."
              ),
              p(
                "Um estudo publicado no Journal of Apicultural Research, com dados ",
                "do Bee Informed Partnership para as temporadas de 2020–21 e 2021–22, ",
                "também recomenda que pesquisas continuem acompanhando fatores como ",
                "Varroa destructor, problemas de rainha e condições climáticas. ",
                "Esses resultados externos ajudam a contextualizar os padrões da base, ",
                "mas não são uma comparação direta, pois os períodos e métodos de ",
                "coleta são diferentes."
              ),
              tags$ul(
                tags$li(
                  tags$a(
                    "USDA - Report on the National Stakeholders Conference on Honey Bee Health",
                    href = "https://www.usda.gov/sites/default/files/documents/ReportHoneyBeeHealth.pdf",
                    target = "_blank"
                  )
                ),
                tags$li(
                  tags$a(
                    "Williams et al. (2023) - A national survey of managed honey bee colony losses in the USA",
                    href = "https://www.tandfonline.com/doi/full/10.1080/00218839.2023.2264601",
                    target = "_blank"
                  )
                )
              )
            )
          )
        ),

        fluidRow(
          box(
            title = "Cuidados na interpretação",
            width = 12,
            status = "warning",
            p(
              class = "nota-metodologica",
              "Os percentuais dos fatores representam os registros informados ",
              "na base e não devem ser interpretados isoladamente como prova de ",
              "causalidade. A média estadual atribui o mesmo peso a cada registro ",
              "estadual-trimestral, independentemente do tamanho da atividade ",
              "apícola em cada estado. As somas de colônias perdidas ao longo dos ",
              "trimestres representam totais de registros trimestrais, e não ",
              "necessariamente indivíduos únicos."
            )
          )
        )
      )
    )
  )
)

#4. Gráficos e Tabelas -----------------------------------------------------------------------------------------------------------------------------------------------------------
server <- function(input, output) {
  
  # aba 1 (se necessário) - introdução
  
  #aba 2: gráficos de sazonalidade
  dados_aba2_filtrados <- reactive({
    req(input$estado_aba2, input$ano_aba2)
    
    dados_estados %>%
      filter(
        state == input$estado_aba2,
        year == as.numeric(input$ano_aba2)
      ) %>%
      mutate(
        rotulo_periodo = factor(trimestre_rotulo)
      ) %>%
      arrange(quarter)
  })
  
  output$grafico_linha_aba2 <- renderPlotly({
    dados <- dados_aba2_filtrados()
    
    validate(
      need(nrow(dados) > 0, "Não há dados para o estado e ano selecionados.")
    )
    
    coluna_y <- input$taxa_aba2
    is_perda <- coluna_y == "percent_lost"
    
    cor_linha <- if (is_perda) "#D95F02" else "#2B8CBE"
    titulo_taxa <- if (is_perda) "Perda de Colônias" else "Renovação de Colônias"
    
    plot_ly(
      data = dados,
      x = ~rotulo_periodo,
      y = ~.data[[coluna_y]],
      type = "scatter",
      mode = "lines+markers",
      line = list(color = cor_linha, width = 3),
      marker = list(color = cor_linha, size = 8),
      text = ~paste0(
        "<b>Estado:</b> ", state, " (", state_code, ")<br>",
        "<b>Trimestre:</b> ", rotulo_periodo, "<br>",
        "<b>Ano:</b> ", year, "<br>",
        "<b>", titulo_taxa, ":</b> ", .data[[coluna_y]], "%"
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = list(text = paste0("<b>", titulo_taxa, " - ", input$estado_aba2, " (", input$ano_aba2, ")</b>")),
        xaxis = list(
          title = "Trimestre",
          tickangle = 0
        ),
        yaxis = list(
          title = paste0(titulo_taxa, " (%)"),
          range = c(0, 100),
          zeroline = TRUE
        ),
        margin = list(l = 50, r = 30, t = 60, b = 60),
        hovermode = "x unified"
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
  })
  
  #aba 3 - fatores de extresse
  # 3.1 Gráfico de barras horizontais
  dados_barras <- reactive({
    
    dados <- dados_fatores_estados
    
    # Filtro de ano
    if (!is.null(input$ano_fatores) && input$ano_fatores != "Todos") {
      dados <- dados %>%
        filter(year == as.integer(input$ano_fatores))
    }
    
    # Filtro de trimestre
    if (!is.null(input$trimestre_fatores) &&
        input$trimestre_fatores != "Todos") {
      dados <- dados %>%
        filter(as.character(trimestre_rotulo) == input$trimestre_fatores)
    }
    
    # Media estadual do percentual para cada fator
    dados %>%
      group_by(fator_pt) %>%
      summarise(
        media_percentual = mean(percentual_afetado, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(media_percentual) %>%
      mutate(
        fator_pt = factor(fator_pt, levels = fator_pt),
        texto_tooltip = paste0(
          "<b>", fator_pt, "</b>",
          "<br>Média: ",
          round(media_percentual, 2), "%"
        )
      )
  })
  
  output$grafico_barras_fatores <- renderPlotly({
    
    dados <- dados_barras()
    
    validate(
      need(nrow(dados) > 0, "Não há dados para os filtros selecionados.")
    )
    
    grafico <- ggplot(
      dados,
      aes(
        x = media_percentual,
        y = fator_pt,
        text = texto_tooltip
      )
    ) +
      geom_col(
        fill = "#D69A27",
        width = 0.7
      ) +
      geom_text(
        aes(label = paste0(round(media_percentual, 1), "%")),
        hjust = -0.15,
        size = 4
      ) +
      scale_x_continuous(
        name = "Percentual médio informado (%)",
        expand = expansion(mult = c(0, 0.18))
      ) +
      labs(
        y = NULL,
        title = "Média dos fatores de estresse"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        axis.text.y = element_text(color = "#333333"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(grafico, tooltip = "text") %>%
      layout(
        margin = list(l = 10, r = 35, t = 55, b = 55),
        xaxis = list(zeroline = FALSE)
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
  })
  
  # 3.2 Mapa de calor
  dados_heatmap <- reactive({
    
    dados <- dados_fatores_estados
    
    if (!is.null(input$trimestre_heatmap) &&
        input$trimestre_heatmap != "Todos") {
      dados <- dados %>%
        filter(as.character(trimestre_rotulo) == input$trimestre_heatmap)
    }
    
    dados %>%
      group_by(year, fator_pt) %>%
      summarise(
        media_percentual = mean(percentual_afetado, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        ano = factor(year, levels = sort(unique(year))),
        fator_pt = factor(
          fator_pt,
          levels = rev(ordem_fatores)
        ),
        texto_tooltip = paste0(
          "<b>", fator_pt, "</b>",
          "<br>Ano: ", year,
          "<br>Média: ", round(media_percentual, 2), "%"
        )
      )
  })
  
  output$mapa_calor_fatores <- renderPlotly({
    
    dados <- dados_heatmap()
    
    validate(
      need(nrow(dados) > 0, "Não há dados para o filtro selecionado.")
    )
    
    grafico <- ggplot(
      dados,
      aes(
        x = ano,
        y = fator_pt,
        fill = media_percentual,
        text = texto_tooltip
      )
    ) +
      geom_tile(
        color = "white",
        linewidth = 0.8
      ) +
      scale_fill_viridis_c(
        option = "magma",
        name = "Média (%)"
      ) +
      labs(
        x = "Ano",
        y = "Fator de estresse",
        title = "Intensidade média dos fatores por ano"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "right"
      )
    
    ggplotly(grafico, tooltip = "text") %>%
      layout(
        margin = list(l = 15, r = 20, t = 55, b = 55)
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
  })
  
  
  # aba 4 - mapa dos EUA
  # Paletas de cores
  cores_mel <- list(
    list(0.0, "#FFF7D6"),
    list(0.35, "#F7C948"),
    list(0.7, "#D9821B"),
    list(1.0, "#6E3500")
  )
  
  cores_vermelho <- list(
    list(0.0, "#fee5d9"),
    list(0.25, "#fcae91"),
    list(0.5, "#fb6a4a"),
    list(0.75, "#de2d26"),
    list(1.0, "#a50f15")
  )
  
  estados_eua <- data.frame(
    Sigla = state.abb,
    Estado_base = state.name,
    stringsAsFactors = FALSE
  )
  
  # 4.1 Mapa do número de colônias por estado
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
    
    dados_completo <- merge(estados_eua, dados_mapa, by = "Sigla", all.x = TRUE)
    
    # Usa o nome do dado se existir, ou o nome base do estado caso esteja ausente
    dados_completo$Estado_exibir <- ifelse(
      is.na(dados_completo$Estado) | dados_completo$Estado == "",
      dados_completo$Estado_base,
      dados_completo$Estado
    )
    
    dados_validos <- dados_completo[!is.na(dados_completo$colonias_iniciais), ]
    dados_sem_dado <- dados_completo[is.na(dados_completo$colonias_iniciais), ]
    
    p <- plot_geo(locationmode = "USA-states") %>%
      add_trace(
        data = dados_validos,
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
      )
    if (nrow(dados_sem_dado) > 0) {
      p <- p %>%
        add_trace(
          type = "choropleth",
          data = dados_sem_dado,
          z = rep(1, nrow(dados_sem_dado)),
          locations = ~Sigla,
          text = ~paste0(
            Estado_exibir, " (", Sigla, ")",
            "<br>Ano: ", input$ano_selecionado,
            "<br>Sem dados"
          ),
          hoverinfo = "text",
          colorscale = list(list(0, "gray"), list(1, "gray")),
          showscale = FALSE
        )
    }
    
    p %>%
      layout(
        geo = list(
          scope = "usa",
          projection = list(type = "albers usa"),
          showlakes = TRUE,
          lakecolor = toRGB("white")
        )
      )
  })
  
  # 4.2 Mapa da porcentagem de colônias perdidas por estado
  dados_mapa_ano2 <- reactive({
    req(input$ano_selecionado)
    
    dados_tratados %>%
      filter(
        state != "Other",
        year == as.numeric(input$ano_selecionado2)
      ) %>%
      group_by(Estado = state, Sigla = state_code) %>%
      summarise(
        colonias_perdidas = sum(percent_lost, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  output$mapa_eua2 <- renderPlotly({
    dados_mapa <- dados_mapa_ano2()

    dados_completo <- merge(estados_eua, dados_mapa, by = "Sigla", all.x = TRUE)
    
    dados_completo$Estado_exibir <- ifelse(
      is.na(dados_completo$Estado) | dados_completo$Estado == "",
      dados_completo$Estado_base,
      dados_completo$Estado
    )
    
    dados_validos <- dados_completo[!is.na(dados_completo$colonias_perdidas), ]
    dados_sem_dado <- dados_completo[is.na(dados_completo$colonias_perdidas), ]
    
    vals_legenda <- pretty(range(dados_validos$colonias_perdidas, na.rm = TRUE))
    
    p <- plot_geo(locationmode = "USA-states") %>%
      add_trace(
        data = dados_validos,
        z = ~colonias_perdidas,
        locations = ~Sigla,
        text = ~paste0(
          Estado, " (", Sigla, ")",
          "<br>Ano: ", input$ano_selecionado2,
          "<br>Colônias Perdidas: ", format(colonias_perdidas, big.mark = ".", decimal.mark = ","), "%"
        ),
        hoverinfo = "text",
        colorscale = cores_vermelho,
        showscale = TRUE,
        colorbar = list(
          title = "Colônias perdidas em %<br>",
          ticktext = paste0(format(vals_legenda, big.mark = ".", decimal.mark = ","), "%")
        )
      )
    if (nrow(dados_sem_dado) > 0) {
      p <- p %>%
        add_trace(
          type = "choropleth",
          data = dados_sem_dado,
          z = rep(1, nrow(dados_sem_dado)),
          locations = ~Sigla,
          text = ~paste0(
            Estado_exibir, " (", Sigla, ")",
            "<br>Ano: ", input$ano_selecionado,
            "<br>Sem dados"
          ),
          hoverinfo = "text",
          colorscale = list(list(0, "gray"), list(1, "gray")),
          showscale = FALSE
        )
    }
    
    p %>%
      layout(
        geo = list(
          scope = "usa",
          projection = list(type = "albers usa"),
          showlakes = TRUE,
          lakecolor = toRGB("white")
        )
      )
  })
  
  
  # aba 5 - banco de dados tratado
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
  
  
  # aba 6 - conclusões
  
  output$texto_conclusoes <- renderUI({
    
    # Evolucao nacional da taxa de perdas:
    resumo_nacional <- dados_eua %>%
      group_by(year) %>%
      summarise(
        colonias_perdidas = sum(lost_colonies, na.rm = TRUE),
        maximo_colonias = sum(max_colonies, na.rm = TRUE),
        taxa_perda = if_else(
          maximo_colonias > 0,
          100 * colonias_perdidas / maximo_colonias,
          NA_real_
        ),
        .groups = "drop"
      ) %>%
      arrange(year)
    
    taxa_inicio <- resumo_nacional %>%
      filter(year == min(year)) %>%
      pull(taxa_perda)
    
    taxa_fim <- resumo_nacional %>%
      filter(year == max(year)) %>%
      pull(taxa_perda)
    
    ano_inicio <- min(resumo_nacional$year, na.rm = TRUE)
    ano_fim <- max(resumo_nacional$year, na.rm = TRUE)
    diferenca_taxa <- taxa_fim - taxa_inicio
    
    # Fatores de estresse: media estadual ao longo de todos os anos/trimestres
    resumo_fatores <- dados_fatores_estados %>%
      group_by(fator_pt) %>%
      summarise(
        media = mean(percentual_afetado, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(media))
    
    principal_fator <- resumo_fatores$fator_pt[1]
    media_principal_fator <- resumo_fatores$media[1]
    
    # Estados: maior soma registrada de perdas e maior taxa agregada
    resumo_estados <- dados_estados %>%
      group_by(state) %>%
      summarise(
        perdas_registradas = sum(lost_colonies, na.rm = TRUE),
        maximo_registrado = sum(max_colonies, na.rm = TRUE),
        taxa_agregada = if_else(
          maximo_registrado > 0,
          100 * perdas_registradas / maximo_registrado,
          NA_real_
        ),
        .groups = "drop"
      )
    
    estado_maior_perda <- resumo_estados %>%
      slice_max(perdas_registradas, n = 1, with_ties = FALSE)
    
    estado_maior_taxa <- resumo_estados %>%
      slice_max(taxa_agregada, n = 1, with_ties = FALSE)
    
    # Formatacao dos valores para exibicao
    fmt_numero <- function(x) {
      format(round(x), big.mark = ".", decimal.mark = ",", scientific = FALSE)
    }
    
    fmt_percentual <- function(x) {
      paste0(
        format(round(x, 2), nsmall = 2,
               decimal.mark = ",", big.mark = "."),
        "%"
      )
    }
    
    tagList(
      div(
        class = "conclusao-card",
        h4("1. Como evoluíram as perdas de colônias?"),
        p(
          "Nos registros nacionais, a taxa agregada calculada a partir ",
          "das observações trimestrais foi de ",
          strong(fmt_percentual(taxa_inicio)),
          " em ", ano_inicio, " e ",
          strong(fmt_percentual(taxa_fim)),
          " em ", ano_fim, ". Isso representa uma ",
          if (diferenca_taxa < 0) {
            paste0(
              "redução de ",
              fmt_percentual(abs(diferenca_taxa)),
              " pontos percentuais"
            )
          } else if (diferenca_taxa > 0) {
            paste0(
              "elevação de ",
              fmt_percentual(diferenca_taxa),
              " pontos percentuais"
            )
          } else {
            "estabilidade da taxa entre os dois anos"
          },
          ". A trajetória não deve ser interpretada como uma queda contínua: ",
          "é importante observar as oscilações entre os anos e os trimestres."
        )
      ),
      
      div(
        class = "conclusao-card",
        h4("2. Quais fatores de estresse mais se destacam?"),
        p(
          "O fator com maior média nos registros estaduais foi ",
          strong(principal_fator),
          ", com média de ",
          strong(fmt_percentual(media_principal_fator)),
          ". Esse resultado também aparece nas visualizações da Aba 3. ",
          "A presença do ácaro Varroa merece atenção, pois estudos sobre ",
          "a saúde das abelhas o relacionam a perdas de colônias e à ",
          "transmissão de vírus. Ainda assim, os percentuais da base ",
          "descrevem fatores reportados e não demonstram, isoladamente, ",
          "que um fator causou determinada perda."
        )
      ),
      
      div(
        class = "conclusao-card",
        h4("3. Onde se concentram as perdas?"),
        p(
          "Considerando a soma dos registros trimestrais de colônias perdidas ",
          "no período, o estado com maior volume registrado foi ",
          strong(estado_maior_perda$state),
          ", com aproximadamente ",
          strong(fmt_numero(estado_maior_perda$perdas_registradas)),
          " colônias perdidas nos registros. Quando se considera a taxa ",
          "agregada (perdas divididas pelo máximo de colônias registrado), ",
          "o maior valor foi observado em ",
          strong(estado_maior_taxa$state),
          ", com ",
          strong(fmt_percentual(estado_maior_taxa$taxa_agregada)),
          ". Volume absoluto e taxa proporcional respondem a perguntas ",
          "diferentes: estados com maior atividade apícola podem acumular ",
          "mais perdas em números absolutos sem apresentar a maior taxa."
        )
      ),
      
      p(
        class = "nota-metodologica",
        "Nota: os indicadores estaduais e nacionais foram calculados a partir ",
        "dos registros trimestrais de 2015 a 2022. A soma de perdas trimestrais ",
        "não representa necessariamente o número de colônias únicas perdidas ",
        "no período. Os resultados das Abas 2 e 4 devem ser considerados em ",
        "conjunto com esta síntese durante a apresentação final."
      )
    )
  })
}
shinyApp(ui, server)
