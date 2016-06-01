## source shared files
source(file.path(getOption("radiant.path.data"),"app/init.R"), encoding = getOption("radiant.encoding"), local = TRUE)
source(file.path(getOption("radiant.path.data"),"app/radiant.R"), encoding = getOption("radiant.encoding"), local = TRUE)

help_design <- c("Design of Experiments" = "doe.md", "Random sampling" = "sampling.md", "Sample size (single)" = "sample_size.Rmd",
                 "Sample size (compare)" = "sample_size_comp.Rmd")
output$help_design <- reactive(append_help("help_design", "tools/help/", Rmd = TRUE))

observeEvent(input$help_design_all, {help_switch(input$help_design_all, "help_design")})
observeEvent(input$help_design_none,{help_switch(input$help_design_none, "help_design", help_on = FALSE)})

help_design_panel <- tagList(
  wellPanel(
    HTML("<label>Design menu: <i id='help_design_all' title='Check all' href='#' class='action-button glyphicon glyphicon-ok'></i>
    <i id='help_design_none' title='Uncheck all' href='#' class='action-button glyphicon glyphicon-remove'></i></label>"),
    checkboxGroupInput("help_design", NULL, help_design,
       selected = state_init("help_design"), inline = TRUE)
  )
)

output$help_design_ui <- renderUI({
  sidebarLayout(
    sidebarPanel(
      help_data_panel,
      help_design_panel,
      uiOutput("help_text")
    ),
    mainPanel(
      HTML(paste0("<h2>Select help files to show and search</h2><hr>")),
      htmlOutput("help_data"),
      htmlOutput("help_design")
    )
  )
})
