
library(shiny)
library(bs4Dash)
library(visNetwork)

# Função para resolver o problema da mochila fracionário e gerar dados para o grafo
mochila_fracionaria <- function(capacidade, pesos, valores) {
  n <- length(pesos)
  razao <- valores / pesos
  ordem <- order(razao, decreasing = TRUE)
  
  valor_total <- 0
  peso_total <- 0
  itens_selecionados <- data.frame(Peso = numeric(), Valor = numeric(), Fracao = numeric())
  
  # Dados para o grafo
  nodes <- data.frame(id = 1, label = "Início", peso = 0, valor = 0)
  edges <- data.frame(from = integer(), to = integer(), label = character())
  
  node_id <- 1
  for (i in ordem) {
    if (peso_total + pesos[i] <= capacidade) {
      valor_total <- valor_total + valores[i]
      peso_total <- peso_total + pesos[i]
      itens_selecionados <- rbind(itens_selecionados, data.frame(Peso = pesos[i], Valor = valores[i], Fracao = 1))
      
      # Adicionar nó e aresta ao grafo
      node_id <- node_id + 1
      nodes <- rbind(nodes, data.frame(id = node_id, label = paste("Item", i), peso = peso_total, valor = valor_total))
      edges <- rbind(edges, data.frame(from = node_id - 1, to = node_id, label = paste("+ Item", i)))
    } else {
      restante <- capacidade - peso_total
      valor_total <- valor_total + (restante / pesos[i]) * valores[i]
      itens_selecionados <- rbind(itens_selecionados, data.frame(Peso = pesos[i], Valor = valores[i], Fracao = restante / pesos[i]))
      
      # Adicionar nó e aresta ao grafo
      node_id <- node_id + 1
      nodes <- rbind(nodes, data.frame(id = node_id, label = paste("Item", i, "(Parcial)"), peso = capacidade, valor = valor_total))
      edges <- rbind(edges, data.frame(from = node_id - 1, to = node_id, label = paste("+ Item", i, "(Parcial)")))
      break
    }
  }
  
  return(list(valor_total = valor_total, itens_selecionados = itens_selecionados, nodes = nodes, edges = edges))
}

# Interface do usuário
ui <- bs4DashPage(
  header = bs4DashNavbar(title = "Problema da Mochila"),
  sidebar = bs4DashSidebar(
    bs4SidebarMenu(
      bs4SidebarMenuItem("Dashboard", tabName = "dashboard", icon = icon("home"))
    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      bs4TabItem(
        tabName = "dashboard",
        fluidRow(
          column(
            width = 4,
            bs4Card(
              title = "Entrada de Dados",
              width = 12,
              numericInput("peso", "Peso do Item:", value = 0),
              numericInput("valor", "Valor do Item:", value = 0),
              actionButton("adicionar", "Adicionar Item"),
              hr(),
              numericInput("capacidade", "Capacidade da Mochila:", value = 50),
              actionButton("calcular", "Calcular Solução")
            )
          ),
          column(
            width = 8,
            bs4Card(
              title = "Resultados",
              width = 12,
              tableOutput("tabela_itens"),
              h4("Valor Total:"),
              textOutput("valor_total"),
              hr(),
              h4("Grafo de Decisões:"),
              visNetworkOutput("grafo")
            )
          )
        )
      )
    )
  )
)

# Lógica do servidor
server <- function(input, output, session) {
  # Armazenar os itens inseridos
  itens <- reactiveVal(data.frame(Peso = numeric(), Valor = numeric()))
  
  # Adicionar item à lista
  observeEvent(input$adicionar, {
    novo_item <- data.frame(Peso = input$peso, Valor = input$valor)
    itens(rbind(itens(), novo_item))
  })
  
  # Calcular solução
  observeEvent(input$calcular, {
    capacidade <- input$capacidade
    pesos <- itens()$Peso
    valores <- itens()$Valor
    
    solucao <- mochila_fracionaria(capacidade, pesos, valores)
    
    # Exibir resultados
    output$tabela_itens <- renderTable({
      solucao$itens_selecionados
    })
    
    output$valor_total <- renderText({
      paste("Valor Total na Mochila:", solucao$valor_total)
    })
    
    # Exibir grafo
    output$grafo <- renderVisNetwork({
      visNetwork(solucao$nodes, solucao$edges) %>%
        visEdges(arrows = "to") %>%
        visNodes(shape = "box", color = "lightblue") %>%
        visLayout(randomSeed = 123)  # Para manter o layout consistente
    })
  })
}

# Rodar o aplicativo
shinyApp(ui, server)