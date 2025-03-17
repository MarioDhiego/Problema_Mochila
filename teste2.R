library(shiny)
library(lpSolve)
library(ggplot2)
library(DT)
library(dplyr)
library(tidyr)
library(ggalluvial)
library(viridis)

# Lista de nomes aleatórios para os itens
nomes_aleatorios <- c("Notebook", "Câmera", "Livro", "Tablet", "Celular", "Fone", "Monitor", 
                      "Teclado", "Mouse", "Impressora", "Drone", "HD Externo", "Roteador", 
                      "Smartwatch", "Console", "Projetor", "Caixa de Som", "Microfone", "Luminária")


ui <- fluidPage(
  titlePanel("Problema da Mochila Múltipla"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("num_itens", "Número de Itens:", 10, min = 5, max = 100),
      numericInput("num_mochilas", "Número de Mochilas:", 3, min = 2, max = 20),
      actionButton("resolver", "Resolver Problema")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Resumo", DTOutput("resumo")),
        tabPanel("Resumo das Mochilas", DTOutput("resumo_mochilas")),  # Nova aba para o resumo total
        tabPanel("Gráfico", plotOutput("grafico", height = "600px"))
      )
    )
  )
)

server <- function(input, output) {
  resultado <- eventReactive(input$resolver, {
    # Geração dos itens com pesos e valores aleatórios
    itens <- data.frame(
      Item = sample(nomes_aleatorios, input$num_itens, replace = TRUE),
      Valor = sample(10:1500, input$num_itens, replace = TRUE),  # Valor (R$)
      Peso = sample(50:200, input$num_itens, replace = TRUE)   # Peso (gramas)
    )
    
    # Geração das mochilas com capacidades aleatórias
    mochilas <- data.frame(
      Mochila = paste0("Mochila_", 1:input$num_mochilas),
      Capacidade = sample(1500:8000, input$num_mochilas, replace = TRUE)
    )
    
    # Criando a matriz de restrição
    num_variaveis <- input$num_itens * input$num_mochilas
    mat <- matrix(0, nrow = input$num_mochilas + input$num_itens, ncol = num_variaveis)
    
    # Restrições de cada item ir para apenas uma mochila
    for (j in 1:input$num_itens) {
      mat[input$num_mochilas + j, ((j-1) * input$num_mochilas + 1):(j * input$num_mochilas)] <- 1
    }
    
    # Restrições de capacidade das mochilas
    for (i in 1:input$num_mochilas) {
      for (j in 1:input$num_itens) {
        mat[i, (j-1) * input$num_mochilas + i] <- itens$Peso[j]
      }
    }
    
    rhs <- c(mochilas$Capacidade, rep(1, input$num_itens))
    dir <- c(rep("<=", input$num_mochilas), rep("=", input$num_itens))
    
    resultado <- lp(
      direction = "max",
      objective.in = rep(itens$Valor, each = input$num_mochilas),
      const.mat = mat,
      const.dir = dir,
      const.rhs = rhs,
      all.bin = TRUE
    )
    
    # Criando a tabela de solução
    solucao <- data.frame(Item = itens$Item, Mochila = NA, Valor_Reais = itens$Valor, Peso = itens$Peso)
    
    if (resultado$status == 0) {  # Se houver solução ótima
      alocacao <- matrix(resultado$solution, nrow = input$num_mochilas, byrow = TRUE)
      for (j in 1:input$num_itens) {
        mochila_atual <- which(alocacao[, j] == 1)
        if (length(mochila_atual) > 0) {
          solucao$Mochila[j] <- paste0("Mochila_", mochila_atual)
        }
      }
    } else {
      solucao <- data.frame(Item = "Nenhuma solução encontrada", Mochila = NA, Valor_Reais = NA, Peso = NA)
    }
    
    # Criando a tabela de resumo das mochilas
    resumo_mochilas <- solucao %>%
      filter(!is.na(Mochila)) %>%
      group_by(Mochila) %>%
      summarise(
        `Peso Total (g)` = sum(Peso),
        `Valor Total (R$)` = sum(Valor_Reais)
      ) %>%
      left_join(mochilas, by = c("Mochila")) %>%
      rename(`Capacidade Total (g)` = Capacidade) %>%
      mutate(`Espaço Livre (g)` = `Capacidade Total (g)` - `Peso Total (g)`) %>%
      relocate(`Capacidade Total (g)`, .after = Mochila) %>%
      relocate(`Espaço Livre (g)`, .after = `Peso Total (g)`)
    
    list(solucao = solucao, mochilas = mochilas, resumo_mochilas = resumo_mochilas)
  })
  
  output$resumo <- renderDT({
    datatable(resultado()$solucao, options = list(pageLength = 10)) %>%
      formatCurrency(columns = "Valor_Reais", currency = "R$ ", digits = 2)
  })
  
  output$resumo_mochilas <- renderDT({
    datatable(resultado()$resumo_mochilas, options = list(pageLength = 10)) %>%
      formatCurrency(columns = "Valor Total (R$)", currency = "R$ ", digits = 2)
  })
  
  output$grafico <- renderPlot({
    solucao <- resultado()$solucao
    
    validate(
      need(!all(is.na(solucao$Mochila)), "Nenhuma solução encontrada para a configuração atual.")
    )
    
    ggplot(solucao, aes(axis1 = Item, axis2 = Mochila, y = Valor_Reais)) +
      geom_alluvium(aes(fill = Mochila), alpha = 0.7) +
      geom_stratum(fill = "gray80") +
      geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
      scale_x_discrete(limits = c("Itens", "Mochilas")) +
      labs(title = "Distribuição dos Itens nas Mochilas", x = "", y = "Valor Total (R$)") +
      theme_minimal()
  })
}

shinyApp(ui, server)

