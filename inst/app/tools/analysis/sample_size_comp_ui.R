###############################
# Sample size
###############################
ssc_type <- c("Mean" = "mean", "Proportion" = "proportion")
# ssc_alternative <- c("Two sided" = "two.sided", "One sided" = "one.sided")
ssc_alternative <- c("Two sided" = "two.sided", "Group 1 less than Group 2" = "less", "Group 1 greater than Group 2" = "greater")

## list of function arguments
ssc_args <- as.list(formals(sample_size_comp))

## list of function inputs selected by user
ssc_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(ssc_args))
    ssc_args[[i]] <- input[[paste0("ssc_", i)]]
  ssc_args
})

output$ui_sample_size_comp <- renderUI({
  tagList(
    wellPanel(
      radioButtons(
        inputId = "ssc_type", label = NULL, choices = ssc_type,
        selected = state_init("ssc_type", "mean"), inline = TRUE
      ),
      numericInput(
        "ssc_n1", "Sample size (n1):", min = 1,
        value = state_init("ssc_n1", NA), step = 1
      ),
      numericInput(
        "ssc_n2", "Sample size (n2):", min = 1,
        value = state_init("ssc_n2", NA), step = 1
      ),
      conditionalPanel(
        condition = "input.ssc_type == 'mean'",
        numericInput(
          "ssc_delta", "Delta:",
          value = state_init("ssc_delta", 2), step = 1
        ),
        numericInput(
          "ssc_sd", "Standard deviation:", min = 0,
          value = state_init("ssc_sd", 10), step = 1
        )
      ),
      conditionalPanel(
        condition = "input.ssc_type != 'mean'",
        numericInput(
          "ssc_p1", "Proportion 1 (p1):", min = 0,
          max = 1, value = state_init("ssc_p1", .1), step = .05
        ),
        numericInput(
          "ssc_p2", "Proportion 2 (p2):", min = 0, max = 1,
          value = state_init("ssc_p2", .15), step = .05
        )
      ),
      numericInput(
        "ssc_conf_lev", "Confidence level:", min = 0, max = 1,
        value = state_init("ssc_conf_lev", 0.95), step = .05
      ),
      numericInput(
        "ssc_power", "Power:", min = 0, max = 1,
        value = state_init("ssc_power", 0.8), step = .05
      ),
      selectInput(
        inputId = "ssc_alternative", label = "Alternative hypothesis:",
        choices = ssc_alternative,
        selected = state_single("ssc_alternative", ssc_alternative, "two.sided")

      ),
      checkboxInput("ssc_show_plot", "Show plot" , state_init("ssc_show_plot", FALSE))
    ),
    help_and_report(
      modal_title = "Sample size (compare)", fun_name = "sample_size_comp",
      help_file = inclRmd(file.path(getOption("radiant.path.design"), "app/tools/help/sample_size_comp.Rmd"))
    )
  )
})

ssc_plot_width <- function() 650
ssc_plot_height <- function() 650

output$sample_size_comp <- renderUI({
  register_print_output("summary_sample_size_comp", ".summary_sample_size_comp")
  register_plot_output(
    "plot_sample_size_comp", ".plot_sample_size_comp",
    width_fun = "ssc_plot_width",
    height_fun = "ssc_plot_height"
  )

  ## one output with components stacked
  ssc_output_panels <- tagList(
    tabPanel("Summary", verbatimTextOutput("summary_sample_size_comp")),
    tabPanel(
      "Plot",
      conditionalPanel(
        "input.ssc_show_plot == true",
        download_link("dlp_ssc"),
        plotOutput("plot_sample_size_comp", height = "100%")
      )
    )
  )

  stat_tab_panel(
    menu = "Design > Sample",
    tool = "Sample size (compare)",
    data = NULL,
    tool_ui = "ui_sample_size_comp",
    output_panels = ssc_output_panels
  )
})

.sample_size_comp <- reactive({
  do.call(sample_size_comp, ssc_inputs())
})

.summary_sample_size_comp <- reactive({
  if (is.null(input$ssc_type)) return(invisible())
  summary(.sample_size_comp())
})

.plot_sample_size_comp <- reactive({
  req(input$ssc_show_plot == TRUE)
  plot(.sample_size_comp())
})

observeEvent(input$sample_size_comp_report, {
  ssc <- ssc_inputs()
  if (input$ssc_type == "mean") {
    ssc$p1 <- ssc$p2 <- NULL
  } else {
    ssc$delta <- ssc$sd <- NULL
  }

  inp_out <- list("", "")
  outputs <- "summary"
  figs <- FALSE
  if (isTRUE(input$ssc_show_plot)) {
    inp_out[[2]] <- list(custom = FALSE)
    outputs <- c("summary", "plot")
    figs <- TRUE
  }

  update_report(
    inp_main = clean_args(ssc, ssc_args),
    fun_name = "sample_size_comp",
    inp_out = inp_out,
    outputs = outputs,
    figs = figs,
    fig.width = ssc_plot_width(),
    fig.height = ssc_plot_height()
  )
})

download_handler(
  id = "dlp_ssc",
  fun = download_handler_plot,
  fn = function() paste0("sample_size_comp_", input$ssc_type),
  type = "png",
  caption = "Save sample size comparison plot",
  plot = .plot_sample_size_comp,
  width = ssc_plot_width,
  height = ssc_plot_height
)
