# Carregar bibliotecas
library(shiny)
library(worldfootballR)
library(ggplot2)

# UI da aplicação
ui <- fluidPage(
  titlePanel("Análise Top 5 Ligas Europeias"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("league", "Escolha a Liga:",
                  choices = list(
                    "Premier League" = "ENG",
                    "La Liga" = "ESP",
                    "Serie A" = "ITA",
                    "Bundesliga" = "GER",
                    "Ligue 1" = "FRA"
                  ),
                  selected = "ENG"),
      sliderInput("season", "Escolha a Época:",
                  min = 2023, max = 2025, value = 2025, step = 1),
      actionButton("extract", "Executar"), width = 2 # Botão de executar
      
    ),
    
    mainPanel(
      div(
        # Estilo de grid para 2 colunas
        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;",
        
        # Gráficos alinhados na grid
        plotOutput("barras", height = "300px", width = "550px"),
        plotOutput("media_golos", height = "300px", width = "550px"),
        plotOutput("dispersao", height = "300px", width = "550px"),
        plotOutput("cartoes", height = "300px", width = "550px")
      )
    )
  )
)

# Server da aplicação
server <- function(input, output, session) {
  # Dados reativos com base na seleção do utilizador
  data <- eventReactive(input$extract, {
    req(input$league, input$season) # Certificar que inputs estão presentes
    
    fb_season_team_stats(
      country = input$league,
      gender = "M",
      season_end_year = input$season,
      tier = "1st",
      stat_type = "standard"
    )
  })
  
  # Filtrar os dados para Team_or_Opponent == "team" e criar coluna "Performance"
  data2 <- reactive({
    req(data()) # Certifica que data() não está vazio
    data_filtered <- data()[data()$Team_or_Opponent == "team", ] #Fitrar dados relevantes
    data_filtered$Performance <- ifelse(data_filtered$Gls >= data_filtered$xG_Expected, 
                                        "Acima do Esperado", "Abaixo do Esperado") #Coluna para cor do gráfico de dispersão
    data_filtered
  })
  
  # Gráfico de barras (Golos Totais por Equipa)
  output$barras <- renderPlot({
    req(data2()) # Certificar que os dados filtrados existem
    
    ggplot(data2(), aes(x = reorder(Squad, -Gls), y = Gls, fill = Gls)) +
      geom_bar(stat = "identity", show.legend = FALSE) + 
      scale_fill_gradient(low = "lightblue", high = "blue") +
      labs(
        title = "Total de Golos por Equipa",
        x = "Equipa",
        y = "Total de Golos"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Gráfico de dispersão (Gls vs. xG_Expected)
  output$dispersao <- renderPlot({
    req(data2()) # Certificar que os dados filtrados existem
    
    ggplot(data2(), aes(x = xG_Expected, y = Gls, label = Squad, color = Performance)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") + # Linha de referência
      geom_point(size = 3, show.legend = FALSE) +
      geom_text(hjust = 0.5, vjust = -1, size = 3) + 
      scale_color_manual(values = c("Acima do Esperado" = "blue", "Abaixo do Esperado" = "red")) + # Cores personalizadas
      labs(
        title = "Golos vs. xG",
        x = "Golos Esperados (xG)",
        y = "Golos"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Gráfico de Média de Golos por Jogo - Análise por Clube
  output$media_golos <- renderPlot({
    req(data2()) # Certificar que os dados filtrados existem
    
    data2 <- data2()
    data2$Media_Golos <- data2$Gls / data2$Mins_Per_90_Playing_Time #Calcular média
    
    ggplot(data2, aes(x = reorder(Squad, -Media_Golos), y = Media_Golos, fill = Media_Golos)) +
      geom_bar(stat = "identity", show.legend = FALSE) + 
      scale_fill_gradient(low = "lightblue", high = "blue") +
      labs(
        title = "Média de Golos por Jogo por Equipa",
        x = "Equipa",
        y = "Média de Golos por Jogo"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Gráfico de Relação entre Cartões Amarelos e Vermelhos
  output$cartoes <- renderPlot({
    req(data2()) # Certificar que os dados filtrados existem
    
    data2 <- data2()
    data2$Total_Cartoes <- 2 * data2$CrdR + data2$CrdY #Coluna para gradiente de cor com base nos cartões
    
    
    ggplot(data2, aes(x = CrdY, y = CrdR, label = Squad, color = Total_Cartoes)) +
      geom_point(size = 3) +
      geom_text(hjust = 0.5, vjust = -1, size = 3) + 
      scale_color_gradient(low = "lightcoral", high = "darkred") +
      labs(
        title = "Relação entre Cartões Amarelos e Vermelhos",
        x = "Cartões Amarelos",
        y = "Cartões Vermelhos"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  
}

# Executar a aplicação
shinyApp(ui, server)
