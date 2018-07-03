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

output$ui_smp_var <- renderUI({
  vars <- varnames()
  selectInput(
    inputId = "smp_var", label = "ID variable:",
    choices = vars, selected = state_single("smp_var", vars),
    multiple = FALSE
  )
})

output$ui_sampling <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      uiOutput("ui_smp_var"),
      tags$table(
        tags$td(numericInput(
          "smp_sample_size", "Sample size:", min = 1,
          value = state_init("smp_sample_size", 1)
        )),
        tags$td(numericInput(
          "smp_seed", label = "Rnd. seed:", min = 0,
          value = state_init("doe_seed", init = NA)
        ))
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
      verbatimTextOutput("summary_sampling")
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
  do.call(sampling, smp_inputs())
})

.summary_sampling <- reactive({
  rt <-
    "Entries for the selected ID variable should be unique (i.e., no duplicates).\nIf a variable of this type is not available please select another dataset.\n\n" %>%
    suggest_data("rndnames")

  if (not_available(input$smp_var)) return(rt)
  if (is_not(input$smp_sample_size)) return("Please select a sample size of 1 or greater.")
  if (has_duplicates(.get_data()[[input$smp_var]])) return(rt)

  summary(.sampling(), prn = TRUE)
})

observeEvent(input$sampling_report, {
  update_report(
    inp_main = clean_args(smp_inputs(), smp_args),
    fun_name = "sampling", outputs = "summary", figs = FALSE
  )
})

dl_sample <- function(path) {
  resp <- .sampling()
  if ("seldat" %in% names(resp)) {
    write.csv(resp$seldat, file = path, row.names = FALSE)
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
