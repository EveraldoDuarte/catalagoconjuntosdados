# Certifique-se de ter os pacotes instalados:
# install.packages("shiny")
# install.packages("jsonlite")
# install.packages("DT")
# install.packages("bslib") # Adicionado bslib para temas e componentes

library(shiny)
library(bslib) # Para temas modernos e componentes de layout como navset_pill_list e accordion
library(jsonlite) # Para manipular JSON
library(DT)       # Para tabelas interativas

# Define o nome do arquivo JSON para salvar os dados.
# Este arquivo será criado/atualizado no mesmo diretório onde o seu 'app.R' está.
DATA_FILE <- "dados_catalogo.json"

# --- Funções de Manipulação de Dados ---

# Função para carregar os dados do arquivo JSON
load_data <- function() {
  if (file.exists(DATA_FILE)) {
    # Lê o conteúdo do arquivo, tratando possíveis problemas de encoding/formato
    raw_json <- readLines(DATA_FILE, warn = FALSE, encoding = "UTF-8")
    if (length(raw_json) == 0 || all(nchar(raw_json) == 0)) {
      message("Arquivo JSON vazio ou sem conteúdo. Iniciando com lista vazia.")
      return(list())
    }
    # Tenta parsear o JSON
    tryCatch({
      fromJSON(txt = paste(raw_json, collapse = "\n"), simplifyVector = FALSE)
    }, error = function(e) {
      message("Erro ao carregar dados do arquivo JSON: ", e$message)
      message("Conteúdo do arquivo: ", paste(raw_json, collapse = "\n"))
      showNotification(paste0("Erro ao carregar dados existentes: ", e$message, ". Iniciando com dados vazios."), type = "error")
      return(list()) # Retorna uma lista vazia em caso de erro
    })
  } else {
    message("Arquivo JSON não encontrado. Iniciando com lista vazia.")
    return(list()) # Retorna uma lista vazia se o arquivo não existir
  }
}

# Função para salvar os dados no arquivo JSON
save_data <- function(data_list) {
  # Adiciona um tryCatch para lidar com erros de escrita
  tryCatch({
    write(toJSON(data_list, pretty = TRUE, auto_unbox = TRUE, null = "null"), DATA_FILE, encoding = "UTF-8")
    TRUE # Retorna TRUE em caso de sucesso
  }, error = function(e) {
    message("Erro ao salvar dados no arquivo JSON: ", e$message)
    showNotification(paste0("Erro ao salvar dados: ", e$message), type = "error")
    FALSE # Retorna FALSE em caso de erro
  })
}

# Função para limpar todos os dados (excluir o arquivo JSON)
clear_data_file <- function() {
  if (file.exists(DATA_FILE)) {
    file.remove(DATA_FILE)
    message("Arquivo JSON de dados removido.")
    TRUE
  } else {
    message("Nenhum arquivo JSON de dados para remover.")
    FALSE
  }
}

# --- Interface do Usuário (UI) ---
ui <- fluidPage(
  # Adiciona um tema bslib para uma aparência mais moderna
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  titlePanel("CATÁLOGO DE CONJUNTO DE DADOS - IFPE"),
  
  # Usando navset_pill_list como o principal container de layout
  navset_pill_list(
    # --- Painel de Visão Geral (para o Catálogo e Tabelas) ---
    nav_panel(
      title = "Visão Geral do Catálogo",
      value = "visao_geral", # Valor para referência programática
      icon = icon("table"), # Ícone para a aba
      
      mainPanel(
        width = 12, # Usa toda a largura disponível no mainPanel
        hr(),
        helpText(HTML(paste0("Este cadastro é baseado no modelo de Nível Bronze proposto pelo Catálogo Nacional de Dados, adaptado para o IFPE."))),
        hr(),
        h3("Conjuntos de Dados Cadastrados:"),
        uiOutput("total_registros_info"), # Novo output para a contagem de registros
        uiOutput("collapsible_data_view"), # Novo output para a visualização colapsável
        hr(),
        
        fluidRow(
         downloadButton("download_json", "Download do JSON", class = "btn-success", icon = icon("download"))), # Botão de download
        br(), # Quebra de linha para espaçamento
        helpText(HTML(paste0("Os dados são salvos no arquivo: <b>", DATA_FILE, "</b>. "))),
        br(),
        helpText(HTML(paste0("Em caso de alteração ou correção de cadastro favor contactar a CDES/PRODIN")))
      )
    ),
    
    # --- Painel de Cadastro de Dados ---
    nav_panel(
      title = "Cadastro de Novo Conjunto de Dados",
      value = "cadastro", # Valor para referência programática
      icon = icon("plus-circle"), # Ícone para a aba
      
      sidebarLayout( # Usa sidebarLayout dentro deste nav_panel
        sidebarPanel(
          width = 12, # Largura do sidebarPanel
          h4("Preencha os dados do conjunto:"),
          textInput("titulo", "1. Título do Conjunto de Dados:", placeholder = "Ex: Dados de População"),
          textAreaInput("descricao", "2. Descrição:", rows = 3, placeholder = "Resumo conciso do conteúdo do conjunto de dados."),
          textInput("publicador", "3. Publicador/Organização:", placeholder = "Ex: IFPE"),
          textInput("palavras_chave", "4. Palavras-chave (separadas por vírgula):", placeholder = "Ex: Educação, Alunos, Campus"),
          dateInput("data_publicacao", "5. Data de Publicação:", format = "yyyy-mm-dd", language = "pt-BR", value = Sys.Date()),
          selectInput("frequencia_atualizacao", "6. Frequência de Atualização:",
                      choices = c("Nunca", "Anual", "Semestral", "Trimestral", "Mensal", "Semanal", "Diária", "Contínua", "Desconhecido"),
                      selected = "Desconhecido"),
          selectInput("licenca", "7. Licença:",
                      choices = c("Creative Commons Atribuição 4.0 Internacional (CC BY 4.0)",
                                  "Creative Commons Zero (CC0)",
                                  "Open Data Commons Public Domain Dedication and License (PDDL)",
                                  "Domínio Público",
                                  "Open Data Commons Open Database License (ODbL)",
                                  "Outra",
                                  "Desconhecido"),
                      selected = "Desconhecido"),
          selectInput("observancia","8. Observância Legal:",
                      choices = c("Público","Restrito","Sigiloso"),
                      selected ="Público"),
          numericInput("versao","9. Versão:",value= 1,min=1,max=100),
          
          selectInput("tema", "10. Área temática:",
                      choices = c(
                        "Administração Pública. Governo. Estado.",
                        "Agricultura, Pecuária e Pesca.",
                        "Alimentação e Nutrição.",
                        "Ciência. Pesquisa. Metodologia. Análise Estatística.",
                        "Comércio Internacional.",
                        "Comércio Interno.",
                        "Cooperação Internacional. Relações Internacionais.",
                        "Demografia. População.",
                        "Desenvolvimento Regional.",
                        "Desenvolvimento Social.",
                        "Direito. Legislação.",
                        "Economia. Desenvolvimento Econômico.",
                        "Economia da Saúde.",
                        "Educação.",
                        "Emprego. Trabalho.",
                        "Energia.",
                        "Gênero e Raça.",
                        "Habitação.",
                        "Indústria.",
                        "Meio Ambiente. Recursos Naturais.",
                        "Pequenas, Médias e Grandes Empresas.",
                        "Previdência. Previdência Social.",
                        "Saneamento.",
                        "Saúde.",
                        "Sistema Monetário. Finanças. Bancos.",
                        "Sistema Tributário.",
                        "Sociedade. Participação Social. Controle Social.",
                        "Tecnologia. Inovação. Informação. Conhecimento.",
                        "Terceiro Setor. Serviços. Turismo.",
                        "Transportes."),
                      selected = "Educação"),
          
          selectInput("area_responsavel","11. Área técnica responsável:",
                      choices = c("", "GESTEXE", "DRIN", "PRODIN", "INTEGR", "PRODEN",
                                  "PROPESQ", "PROEXT", "DAE", "PROAD", "DOPE", "DCOM", "DGPE", "DTI"),
                      selected = ""), # Adicionado uma opção vazia
          radioButtons("dados_aberto",
                       "12. Dados abertos:",
                       choices= list ("Sim"= TRUE,"Não" = FALSE),
                       selected = FALSE), # Definir um valor padrão para radioButtons
          textInput("url_dados", "13. URL para o Conjunto de Dados:", placeholder = "Ex: https://dados.ifpe.edu.br/meus_dados/"),
          
          actionButton("salvar_e_visualizar", "Salvar Cadastro", icon = icon("save"), class = "btn-primary")
        ),
        
        mainPanel(
          
        )
      )
    )
  )
)

# --- Lógica do Servidor (Server) ---
server <- function(input, output, session) {
  
  # Reativo para armazenar a lista de dados (carregados do JSON ou novos).
  # `reactiveVal` é ideal para objetos reativos que são diretamente manipulados.
  dados_cadastro_list <- reactiveVal(load_data())
  
  # Observa o clique no botão "Salvar Cadastro"
  observeEvent(input$salvar_e_visualizar, {
    # Validação básica: Título e URL são essenciais
    if (nchar(trimws(input$titulo)) == 0 || nchar(trimws(input$url_dados)) == 0) {
      showNotification("Por favor, preencha o Título e a URL do Conjunto de Dados.", type = "warning", duration = 3)
      return()
    }
    
    # Coleta e formata as palavras-chave como um vetor de strings
    palavras_chave_formatadas <- unlist(strsplit(input$palavras_chave, ",\\s*|;\\s*"))
    palavras_chave_formatadas <- trimws(palavras_chave_formatadas[palavras_chave_formatadas != ""])
    palavras_chave_formatadas <- palavras_chave_formatadas[palavras_chave_formatadas != ""] # Remove entradas vazias residuais
    
    # Cria uma lista R para o novo registro
    novo_registro_list <- list(
      titulo = input$titulo,
      descricao = input$descricao,
      publicador = input$publicador,
      palavras_chave = palavras_chave_formatadas,
      data_publicacao = as.character(input$data_publicacao), # Garante formato AAAA-MM-DD
      frequencia_atualizacao = input$frequencia_atualizacao,
      licenca = input$licenca,
      observancia = input$observancia,
      versao = input$versao,
      tema = input$tema,
      area_responsavel = input$area_responsavel,
      dados_aberto = as.logical(input$dados_aberto), # radioButtons retorna string "TRUE"/"FALSE", converter para lógico
      url_dados = input$url_dados
    )
    
    # Adiciona o novo registro à lista reativa de dados
    current_data <- dados_cadastro_list()
    current_data[[length(current_data) + 1]] <- novo_registro_list
    dados_cadastro_list(current_data) # Atualiza o valor do reactiveVal
    
    # Salva a lista atualizada no arquivo JSON
    if (save_data(current_data)) {
      showNotification("Dados salvos com sucesso no arquivo JSON!", type = "message")
      
      # Atualiza o output JSON com o último registro para facilitar cópia para API
      output$json_output <- renderPrint({
        toJSON(novo_registro_list, pretty = TRUE, auto_unbox = TRUE, null = "null")
      })
      
      # Limpa os campos após salvar
      updateTextInput(session, "titulo", value = "")
      updateTextAreaInput(session, "descricao", value = "")
      updateTextInput(session, "publicador", value = "")
      updateTextInput(session, "palavras_chave", value = "")
      updateDateInput(session, "data_publicacao", value = Sys.Date())
      updateSelectInput(session, "frequencia_atualizacao", selected = "Desconhecido")
      updateSelectInput(session, "licenca", selected = "Desconhecido")
      updateSelectInput(session, "observancia", selected = "Público")
      updateNumericInput(session, "versao", value = 1) # Corrigido para updateNumericInput
      updateTextInput(session, "tema", value = "")
      updateSelectInput(session, "area_responsavel", selected = "")
      updateRadioButtons(session, "dados_aberto", selected = FALSE) # Corrigido para updateRadioButtons
      updateTextInput(session, "url_dados", value = "")
      
    } else {
      showNotification("Erro ao salvar os dados. Verifique o console para mais detalhes.", type = "error")
    }
  })
  
  # Observa o clique no botão "Limpar Todos os Dados"
  observeEvent(input$limpar_tudo, {
    # Confirmação antes de limpar (opcional, mas recomendado para evitar exclusão acidental)
    showModal(modalDialog(
      title = "Confirmar Limpeza",
      "Tem certeza que deseja apagar TODOS os dados cadastrados? Esta ação é irreversível.",
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirm_clear", "Sim, Apagar Tudo", class = "btn-danger")
      )
    ))
  })
  
  # Observa a confirmação de limpeza
  observeEvent(input$confirm_clear, {
    removeModal() # Fecha o modal de confirmação
    
    if (clear_data_file()) {
      dados_cadastro_list(list()) # Reseta o reactiveVal para uma lista vazia
      output$json_output <- renderPrint({ "Nenhum JSON gerado ainda." }) # Limpa o JSON de saída
      showNotification("Todos os dados foram apagados.", type = "message")
    } else {
      showNotification("Não foi possível apagar os dados. O arquivo pode não existir.", type = "info")
    }
  })
  
  # Renderiza a contagem total de registros
  output$total_registros_info <- renderUI({
    num_registros <- length(dados_cadastro_list())
    p(strong("Total de registros cadastrados: "), num_registros)
  })
  
  # Renderiza a visualização colapsável dos dados
  output$collapsible_data_view <- renderUI({
    dados <- dados_cadastro_list()
    
    if (length(dados) == 0) {
      return(p("Nenhum dado cadastrado ainda. Por favor, adicione novos conjuntos de dados na aba 'Cadastro de Novo Conjunto de Dados'."))
    }
    
    # Cria uma lista de accordion_panels
    accordion_panels <- lapply(seq_along(dados), function(i) {
      x <- dados[[i]]
      accordion_panel(
        title = HTML(paste0("<strong>", i, ". ", x$titulo, "</strong> (Área resp.: ", x$area_responsavel, ")")),
        value = paste0("item_", i), # Valor único para cada painel
        
        # Conteúdo detalhado dentro de cada painel colapsável
        tagList(
          p(strong("Descrição: "), x$descricao),
          p(strong("Palavras-chave: "), paste(x$palavras_chave, collapse = ", ")),
          p(strong("Data de Publicação: "), x$data_publicacao),
          p(strong("Frequência de Atualização: "), x$frequencia_atualizacao),
          p(strong("Licença: "), x$licenca),
          p(strong("Observância Legal: "), x$observancia),
          p(strong("Versão: "), x$versao),
          p(strong("Tema: "), x$tema),
          p(strong("Área Responsável: "), x$area_responsavel),
          p(strong("Dados Abertos: "), ifelse(x$dados_aberto, "Sim", "Não")),
          p(strong("URL: "), a(href = x$url_dados, x$url_dados, target = "_blank"))
        )
      )
    })
    
    # Retorna o componente accordion com todos os painéis
    accordion(
      !!!accordion_panels, # Usa !!! para desdobrar a lista de painéis
      id = "data_accordion" # Um ID para o accordion principal
    )
  })
  
 
  
  # Handler para o botão de download
  output$download_json <- downloadHandler(
    filename = function() {
      "dados_catalogo.json" # Nome do arquivo que será baixado
    },
    content = function(file) {
      # Garante que o arquivo JSON seja lido e escrito corretamente para o download
      # Usa o mesmo DATA_FILE que o app usa para salvar
      file.copy(DATA_FILE, file)
    },
    contentType = "application/json" # Tipo de conteúdo para o download
  )
  
}

# --- Executa o Aplicativo Shiny ---
shinyApp(ui = ui, server = server)
