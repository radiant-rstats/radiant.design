###############################
# Sampling
###############################

## list of function arguments
smp_args <- as.list(formals(sampling))

## list of function inputs selected by user
smp_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  smp_args$data_filter <- if (input$show_filter) input$data_filter else ""
  smp_args$dataset <- input$dataset
  for (i in r_drop(names(smp_args)))
    smp_args[[i]] <- input[[paste0("smp_", i)]]
  smp_args
})

output$ui_smp_vars <- renderUI({
  vars <- varnames()
  selectInput(
    inputId = "smp_vars", label = "Variables:",
    choices = vars, selected = state_multiple("smp_vars", vars, vars),
    multiple = TRUE, selectize = FALSE,
    size = min(12, length(vars))
  )
})

output$ui_smp_name <- renderUI({
  req(input$dataset)
  textInput("smp_name", "Store as:", "", placeholder = "Provide a name")
})

output$ui_sampling <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      uiOutput("ui_smp_vars"),
      tags$table(
        tags$td(numericInput(
          "smp_sample_size", "Sample size:", min = 1,
          value = state_init("smp_sample_size", 1)
        )),
        tags$td(numericInput(
          "smp_seed", label = "Rnd. seed:", min = 0,
          value = state_init("smp_seed", init = 1234)
        ))
      ),
      checkboxInput("smp_sframe", "Show sampling frame ", value = state_init("smp_sframe", FALSE))
    ),
    wellPanel(
      tags$table(
        tags$td(uiOutput("ui_smp_name")),
        tags$td(actionButton("smp_store", "Store", icon = icon("plus")), style = "padding-top:30px;")
      )
    ),
    help_and_report(
      modal_title = "Sampling", fun_name = "sampling",
      help_file = inclMD(file.path(getOption("radiant.path.design"), "app/tools/help/sampling.md"))
    )
  )
})

output$sampling <- renderUI({
  register_print_output("summary_sampling", ".summary_sampling")

  ## one output with components stacked
  smp_output_panels <- tagList(
    tabPanel(
      "Summary",
      download_link("dl_sample"), br(),
      verbatimTextOutput("summary_sampling"),
      DT::dataTableOutput("table_sampling"),
      conditionalPanel(
        "input.smp_sframe == true",
        DT::dataTableOutput("table_sampling_frame")
      )
    )
  )

  stat_tab_panel(
    menu = "Design > Sample",
    tool = "Random sampling",
    tool_ui = "ui_sampling",
    output_panels = smp_output_panels
  )
})

.sampling <- reactive({
  validate(
    need(input$smp_vars, "Select at least one variable"),
    need(available(input$smp_vars), "Some selected variables are not available in this dataset")
  )
  smpi <- smp_inputs()
  smpi$envir <- r_data
  do.call(sampling, smpi)
})

.summary_sampling <- reactive({
  if (not_available(input$smp_vars)) {
    "For random sampling each row in the data should be distinct\n(i.e., no duplicates). Please select an appropriate dataset.\n\n" %>%
      suggest_data("rndnames")
  } else if (is_empty(input$smp_sample_size)) {
    "Please select a sample size of 1 or greater"
  } else {
    summary(.sampling())
  }
})

output$table_sampling <- DT::renderDataTable({
  req(input$smp_vars, input$smp_sample_size)
  withProgress(message = "Generating sample", value = 1, {
    smp <- .sampling()$seldat
    dom <- ifelse(nrow(smp) <= 10, "t", "tip")
    dtab(smp, dom = dom, caption = "Selected cases")
  })
})

output$table_sampling_frame <- DT::renderDataTable({
  req(input$smp_vars, input$smp_sample_size, input$smp_sframe)
  withProgress(message = "Show sampling frame", value = 1, {
    smp <- .sampling()
    dtab(smp$dataset, dom = "tip", caption = "Sampling frame")
  })
})

observeEvent(input$sampling_report, {
  req(input$smp_sample_size)
  nr <- min(100, max(input$smp_sample_size, 1))
  xcmd <- paste0("# dtab(result$seldat, dom = \"tip\", caption = \"Selected cases\", nr = ", nr, ")")
  if (isTRUE(input$smp_sframe)) {
    xcmd <- paste0(xcmd, "\n# dtab(result$dataset, dom = \"tip\", caption = \"Sampling frame\", nr = 100)")
  }
  if (!is_empty(input$smp_name)) {
    dataset <- fix_names(input$smp_name)
    if (input$smp_name != dataset) {
      updateTextInput(session, inputId = "smp_name", value = dataset)
    }
    xcmd <- paste0(xcmd, "\n", dataset, " <- select(result$seldat, -rnd_number)\nregister(\"", dataset, "\")")
  } 

  update_report(
    inp_main = clean_args(smp_inputs(), smp_args),
    fun_name = "sampling", outputs = "summary", 
    xcmd = xcmd, figs = FALSE
  )
})

dl_sample <- function(path) {
  resp <- .sampling()
  if ("seldat" %in% names(resp)) {
    seldat <- resp$seldat %>% select_at(setdiff(colnames(.), "rnd_number"))
    write.csv(seldat, file = path, row.names = FALSE)
  } else {
    cat("No valid sample available", file = path)
  }
}

download_handler(
  id = "dl_sample", 
  fun = dl_sample, 
  fn = function() paste0(input$dataset, "_sample"),
  type = "csv",
  caption = "Save random sample"
)

observeEvent(input$smp_store, {
  req(input$smp_name)
  resp <- .sampling()
  if (!"seldat" %in% names(resp)) {
    cat("No valid sample available")
    return()
  } 
 
  dataset <- fix_names(input$smp_name)
  if (input$smp_name != dataset) {
    updateTextInput(session, inputId = "smp_name", value = dataset)
  }

  r_data[[dataset]] <- resp$seldat %>% select_at(setdiff(colnames(.), "rnd_number"))
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
