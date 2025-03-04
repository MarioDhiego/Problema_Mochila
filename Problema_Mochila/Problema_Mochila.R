
# Carregar os pacotes necessários
library(shiny)
library(shinydashboard)

# Função para resolver o Problema da Mochila 0/1 usando Programação Dinâmica
knapsack <- function(pesos, valores, capacidade) {
  n <- length(valores)  # Número de itens
  # Matriz para armazenar o valor máximo que pode ser alcançado para cada subproblema
  dp <- matrix(0, nrow = n + 1, ncol = capacidade + 1)
  
  # Preenchendo a tabela dp
  for (i in 1:n) {
    for (j in 1:capacidade) {
      # Se o peso do item i é maior que a capacidade j, não podemos incluí-lo
      if (pesos[i] <= j) {
        dp[i + 1, j + 1] <- max(dp[i, j + 1], dp[i, j + 1 - pesos[i]] + valores[i])
      } else {
        dp[i + 1, j + 1] <- dp[i, j + 1]
      }
    }
  }
  
  # Agora vamos rastrear quais itens foram escolhidos para a solução ótima
  items_included <- vector("list", n)
  w <- capacidade
  for (i in n:1) {
    if (dp[i + 1, w + 1] != dp[i, w + 1]) {
      # Item i foi incluído
      items_included[[i]] <- list("Item" = i, "Peso" = pesos[i], "Valor" = valores[i])
      w <- w - pesos[i]  # Atualizar a capacidade restante
    }
  }
  
  # Retornar o valor máximo e os itens escolhidos
  list("valor_maximo" = dp[n + 1, capacidade + 1], "itens_incluidos" = items_included[!sapply(items_included, is.null)])
}

# Definir a interface do usuário (UI) usando shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "Problema da Mochila 0/1 - Algoritmo de Programação Dinâmica"),
  
  dashboardSidebar(
    # Entrada para os pesos dos itens
    textInput("pesos", "Pesos dos Itens (separados por vírgula)", "2, 3, 4, 5"),
    
    # Entrada para os valores dos itens
    textInput("valores", "Valores dos Itens (separados por vírgula)", "3, 4, 5, 7"),
    
    # Entrada para a capacidade da mochila
    numericInput("capacidade", "Capacidade da Mochila", 5, min = 1, max = 100),
    
    # Botão para calcular
    actionButton("calcular", "Calcular Valor Máximo")
  ),
  
  dashboardBody(
    # Resultado do valor máximo
    h4("Resultado:"),
    verbatimTextOutput("resultado"),
    
    # Exibir os itens escolhidos
    h4("Itens Incluídos na Mochila:"),
    verbatimTextOutput("itens_incluidos")
  )
)

# Definir a função server
server <- function(input, output) {
  
  # Reagir ao clique no botão "Calcular"
  observeEvent(input$calcular, {
    
    # Obter os valores de entrada
    pesos <- as.numeric(unlist(strsplit(input$pesos, ",")))
    valores <- as.numeric(unlist(strsplit(input$valores, ",")))
    capacidade <- input$capacidade
    
    # Verificar se os vetores de pesos e valores têm o mesmo comprimento
    if (length(pesos) != length(valores)) {
      output$resultado <- renderText({
        "Erro: o número de pesos e valores não corresponde."
      })
      return()
    }
    
    # Chamar a função de programação dinâmica para resolver o problema
    resultado <- knapsack(pesos, valores, capacidade)
    
    # Exibir o valor máximo
    output$resultado <- renderText({
      paste("O valor máximo que pode ser obtido na mochila é:", resultado$valor_maximo)
    })
    
    # Exibir os itens escolhidos
    output$itens_incluidos <- renderText({
      if (length(resultado$itens_incluidos) == 0) {
        "Nenhum item foi incluído na mochila."
      } else {
        itens_texto <- sapply(resultado$itens_incluidos, function(item) {
          paste("Item", item$Item, "(Peso:", item$Peso, ", Valor:", item$Valor, ")")
        })
        paste(itens_texto, collapse = "\n")
      }
    })
  })
}

# Rodar o aplicativo shiny com shinydashboard
shinyApp(ui = ui, server = server)
