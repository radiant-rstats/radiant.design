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
        tags$div(title = "The acceptable error is the level of precision you require (i.e., the range within which the true mean should lie). For example, ± $10. A lower acceptable error requires a larger sample size.",
          # HTML('<label>Acceptable Error:</label><i class="fa fa-info-circle" title="The acceptable error is the level of precision you require (i.e., the range within which the true mean should lie). For example, ± $10. A lower acceptable error requires a larger sample size."></i>'),
          numericInput(
            "ss_err_mean", "Acceptable Error:", min = 0,
            value = state_init("ss_err_mean", 2), step = .1
          )
        ),
        tags$div(title = "How much variation is there likely to be in the population? This number is often determined from a previous survey or a pilot study. The higher the standard deviation, the larger the required sample size.",
          numericInput(
            "ss_sd_mean", "Standard deviation:", min = 0,
            value = state_init("ss_sd_mean", 10), step = .1
          )
        )
      ),
      conditionalPanel(
        condition = "input.ss_type != 'mean'",
        tags$div(title = "The acceptable error is the level of precision you require (i.e., the range within which the true proportion should lie). For example, ± 0.02. A lower acceptable error requires a larger sample size.",
          numericInput(
            "ss_err_prop", "Acceptable Error:", min = 0,
            max = 1, value = state_init("ss_err_prop", .1), step = .01
          )
        ),
        tags$div(title = "What do you expect the sample proportion to be? This number is often determined from a previous survey or a pilot study. If no such information is availabvle use 0.5.",
          numericInput(
            "ss_p_prop", "Proportion:", min = 0, max = 1,
            value = state_init("ss_p_prop", .5), step = .05
          )
        )
      ),

      tags$div(title = "Common values for the confidence level are 0.9, 0.95, and 0.99",
        numericInput(
          "ss_conf_lev", "Confidence level:", min = 0, max = 1,
          value = state_init("ss_conf_lev", 0.95), step = .1
        )
      ),
      tags$div(title = "The probability that a respondent will be part of the target segment of interest",
        numericInput(
          "ss_incidence", "Incidence rate:", min = 0, max = 1,
          value = state_init("ss_incidence", 1), step = .05
        )
      ),
      tags$div(title = "The probability of a response",
        numericInput(
          "ss_response", "Response rate:", min = 0, max = 1,
          value = state_init("ss_response", 1), step = .05
        )
      ),
      tags$div(title = "If the sample size is relatively larger compared to the size of the target population you should consider adjusting for population size",
        radioButtons(
          inputId = "ss_pop_correction",
          choices = ss_pop_correction,
          label = "Correct for population size:",
          selected = state_init("ss_pop_correction", "no"),
          inline = TRUE
        )
      ),
      conditionalPanel(
        condition = "input.ss_pop_correction == 'yes'",
        tags$div(title = "Size of the target population of interest",
          numericInput(
            "ss_pop_size", "Population size:", min = 1,
            value = state_init("ss_pop_size", 10 ^ 6), step = 1000
          )
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
