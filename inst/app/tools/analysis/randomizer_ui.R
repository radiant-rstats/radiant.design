## list of function arguments
rndr_args <- as.list(formals(randomizer))

## list of function inputs selected by user
rndr_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  rndr_args$data_filter <- if (input$show_filter) input$data_filter else ""
  rndr_args$dataset <- input$dataset
  for (i in r_drop(names(rndr_args)))
    rndr_args[[i]] <- input[[paste0("rndr_", i)]]

  rndr_args$conditions <- unlist(strsplit(rndr_args$conditions, "(\\s*,\\s*|\\s*;\\s*)")) %>%
        fix_names() %T>%
        {updateTextInput(session, "rndr_conditions", value = paste0(., collapse = ", "))}

  rndr_args
})

output$ui_rndr_vars <- renderUI({
  vars <- varnames()
  selectInput(
    inputId = "rndr_vars", label = "Variables:",
    choices = vars, selected = state_multiple("rndr_vars", vars, vars),
    multiple = TRUE, selectize = FALSE,
    size = min(12, length(vars))
  )
})

output$ui_rndr_blocks <- renderUI({
  vars <- varnames()
  selectizeInput(
    inputId = "rndr_blocks", label = "Blocking variables:",
    choices = vars, selected = state_multiple("rndr_blocks", vars, c()),
    multiple = TRUE,
    options = list(
      placeholder = "Select blocking variables",
      plugins = list("remove_button")
    )
  )
})

output$ui_rndr_conditions <- renderUI({
  textAreaInput(
    "rndr_conditions", "Condition labels:", rows = 2,
    placeholder = "Type condition labels separated by comma's and press return",
    value = state_init("rndr_conditions", "A, B")
  )
})

output$ui_rndr_probs <- renderUI({
  req(input$rndr_conditions)
  textInput(
    "rndr_probs", "Probabilities:",
    value = state_init("rndr_probs", ""),
    placeholder = "Enter probabilities (e.g., 1/2 1/2)"
  )
})

output$ui_rndr_name <- renderUI({
  req(input$dataset)
  textInput("rndr_name", "Store as:", "", placeholder = "Provide a name")
})

## add a spinning refresh icon if the simulation needs to be (re)run
run_refresh(rndr_args, "rndr", init = "vars", label = "Assign conditions", relabel = "Re-assign conditions")

output$ui_randomizer <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      actionButton("rndr_run", "Assign conditions", width = "100%", icon = icon("play"), class = "btn-success")
    ),
    wellPanel(
      uiOutput("ui_rndr_vars"),
      uiOutput("ui_rndr_blocks"),
      uiOutput("ui_rndr_conditions"),
      uiOutput("ui_rndr_probs"),
      textInput(
        "rndr_label", "Condition variable name:",
        placeholder = "Provide a variable name",
        value = state_init("rndr_label", ".conditions")
      ),
      numericInput("rndr_seed", label = "Rnd. seed:", min = 0, value = state_init("rndr_seed", init = 1234))
    ),
    wellPanel(
      tags$table(
        tags$td(uiOutput("ui_rndr_name")),
        tags$td(actionButton("rndr_store", "Store", icon = icon("plus")), style = "padding-top:30px;")
      )
    ),
    help_and_report(
      modal_title = "Random assignment",
      fun_name = "randomizer",
      help_file = inclMD(file.path(getOption("radiant.path.design"), "app/tools/help/randomizer.md"))
    )
  )
})

output$randomizer <- renderUI({
  register_print_output("summary_randomizer", ".summary_randomizer")

  ## one output with components stacked
  rndr_output_panels <- tagList(
    tabPanel(
      "Summary",
      download_link("dl_randomizer"), br(),
      verbatimTextOutput("summary_randomizer"),
      conditionalPanel(
        "input.rndr_vars != undefined && input.rndr_vars != null && input.rndr_vars.length > 0",
        DT::dataTableOutput("table_randomizer")
      )
    )
  )

  stat_tab_panel(
    menu = "Design > Sample",
    tool = "Random assignment",
    tool_ui = "ui_randomizer",
    output_panels = rndr_output_panels
  )
})

.randomizer <- eventReactive(input$rndr_run, {
  validate(
    need(input$rndr_vars, "Select at least one variables")
  )

  withProgress(message = "Randomly assigning", value = 1, {
    rndi <- rndr_inputs()
    rndi$envir <- r_data
    asNum <- function(x) ifelse(length(x) > 1, as.numeric(x[1]) / as.numeric(x[2]), as.numeric(x))
    rndi$probs <- unlist(strsplit(rndi$probs, "(\\s*,\\s*|\\s*;\\s*|\\s+)")) %>%
      strsplit("/") %>%
      sapply(asNum)
    do.call(randomizer, rndi)
  })
})

.summary_randomizer <- reactive({
  if (not_pressed(input$rndr_run) || not_available(input$rndr_vars)) {
    "For random assignment each row in the data should be distinct\n(i.e., no duplicates). Please select an appropriate dataset.\n\n" %>%
      suggest_data("rndnames")
  } else {
    summary(.randomizer())
  }
})

output$table_randomizer <- DT::renderDataTable({
  req(input$rndr_run)
  withProgress(message = "Generating assignments", value = 1, {
    isolate(.randomizer()$dataset) %>% dtab(dom = "tip")
  })
})

observeEvent(input$randomizer_report, {

  xcmd <- "# dtab(result$dataset, dom = \"tip\", nr = 100)"

  if (!is_empty(input$rndr_name)) {
    dataset <- fix_names(input$rndr_name)
    if (input$rndr_name != dataset) {
      updateTextInput(session, inputId = "rndr_name", value = dataset)
    }
    xcmd <- paste0(xcmd, "\n", dataset, " <- result$dataset\nregister(\"", dataset, "\")")
  }

  rndi <- rndr_inputs()
  rndi$probs <- radiant.data::make_vec(rndi$probs)

  update_report(
    inp_main = clean_args(rndi, rndr_args),
    fun_name = "randomizer", outputs = "summary",
    xcmd = xcmd, figs = FALSE
  )
})

dl_randomizer <- function(path) {
  resp <- .randomizer()
  if ("dataset" %in% names(resp)) {
    resp$dataset %>% write.csv(file = path, row.names = FALSE)
  } else {
    cat("No valid dataset available", file = path)
  }
}

download_handler(
  id = "dl_randomizer",
  fun = dl_randomizer,
  fn = function() paste0(input$dataset, "_rnd"),
  type = "csv",
  caption = "Save random assignment"
)

observeEvent(input$rndr_store, {
  req(input$rndr_name)
  resp <- .randomizer()
  if (!"dataset" %in% names(resp)) {
    cat("No valid dataset available")
    return()
  }

  dataset <- fix_names(input$rndr_name)
  if (input$rndr_name != dataset) {
    updateTextInput(session, inputId = "rndr_name", value = dataset)
  }

  r_data[[dataset]] <- resp$dataset
  register(dataset)
  updateSelectInput(session, "dataset", selected = input$dataset)

  ## See https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
  showModal(
    modalDialog(
      title = "Data Stored",
      span(
        paste0("Dataset '", dataset, "' was successfully added to the
                datasets dropdown. Add code to Report > Rmd or
                Report > R to (re)create the results by clicking the
                report icon on the bottom left of your screen.")
      ),
      footer = modalButton("OK"),
      size = "s",
      easyClose = TRUE
    )
  )
})
