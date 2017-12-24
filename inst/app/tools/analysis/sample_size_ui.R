###############################
# Sample size
###############################
ss_type <- c("Mean" = "mean", "Proportion" = "proportion")
ss_pop_correction <- c("Yes" = "yes", "No" = "no")

## list of function arguments
ss_args <- as.list(formals(sample_size))

## list of function inputs selected by user
ss_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(ss_args))
    ss_args[[i]] <- input[[paste0("ss_", i)]]
  ss_args
})

output$ui_sample_size <- renderUI({
  tagList(
    wellPanel(
      radioButtons(
        inputId = "ss_type", label = NULL, choices = ss_type,
        selected = state_init("ss_type", "mean"), inline = TRUE
      ),
      conditionalPanel(
        condition = "input.ss_type == 'mean'",
        numericInput(
          "ss_err_mean", "Acceptable Error (e.g., $10):", min = 0,
          value = state_init("ss_err_mean", 2), step = .1
        ),
        numericInput(
          "ss_sd_mean", "Sample std. deviation:", min = 0,
          value = state_init("ss_sd_mean", 10), step = .1
        )
      ),
      conditionalPanel(
        condition = "input.ss_type != 'mean'",
        numericInput(
          "ss_err_prop", "Acceptable Error (e.g., .03):", min = 0,
          max = 1, value = state_init("ss_err_prop", .1), step = .01
        ),
        numericInput(
          "ss_p_prop", "Sample proportion:", min = 0, max = 1,
          value = state_init("ss_p_prop", .5), step = .05
        )
      ),
      numericInput(
        "ss_conf_lev", "Confidence level (z-value):", min = 0,
        value = state_init("ss_conf_lev", 1.96), step = .1
      ),
      numericInput(
        "ss_incidence", "Incidence rate:", min = 0, max = 1,
        value = state_init("ss_incidence", 1), step = .05
      ),
      numericInput(
        "ss_response", "Response rate:", min = 0, max = 1,
        value = state_init("ss_response", 1), step = .05
      ),
      radioButtons(
        inputId = "ss_pop_correction",
        choices = ss_pop_correction,
        label = "Correct for population size:",
        selected = state_init("ss_pop_correction", "no"),
        inline = TRUE
      ),
      conditionalPanel(
        condition = "input.ss_pop_correction == 'yes'",
        numericInput(
          "ss_pop_size", "Population size:", min = 1,
          value = state_init("ss_pop_size", 10 ^ 6), step = 1000
        )
      )
    ),
    help_and_report(
      modal_title = "Sample size (single)", fun_name = "sample_size",
      help_file = inclRmd(file.path(getOption("radiant.path.design"), "app/tools/help/sample_size.Rmd"))
    )
  )
})

output$sample_size <- renderUI({
  register_print_output("summary_sample_size", ".summary_sample_size")

  ## one output with components stacked
  ss_output_panels <- tagList(
    tabPanel("Summary", verbatimTextOutput("summary_sample_size"))
  )

  stat_tab_panel(
    menu = "Design > Sample",
    tool = "Sample size (single)",
    data = NULL,
    tool_ui = "ui_sample_size",
    output_panels = ss_output_panels
  )
})

.sample_size <- reactive({
  do.call(sample_size, ss_inputs())
})

.summary_sample_size <- reactive({
  if (is.null(input$ss_type)) return(invisible())
  summary(.sample_size())
})

observeEvent(input$sample_size_report, {
  ss <- ss_inputs()
  if (input$ss_type == "mean") {
    ss$err_prop <- ss$p_prop <- NULL
  } else {
    ss$err_mean <- ss$sd_mean <- NULL
  }

  inp_main <- clean_args(ss, ss_args)
  update_report(
    inp_main = inp_main,
    fun_name = "sample_size", outputs = "summary", figs = FALSE
  )
})
