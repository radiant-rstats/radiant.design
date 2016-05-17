help_example <- c("App1" = "app1.md","App2" = "app2.md")

output$help_example <- reactive(append_help("help_example", file.path(r_path,"example/tools/help/")))

observeEvent(input$help_example_all, {help_switch(input$help_example_all, "help_example")})
observeEvent(input$help_example_none, {help_switch(input$help_example_none, "help_example", help_on = FALSE)})

help_example_ui <-
  wellPanel(
    HTML("<label>Example menu: <i id='help_example_all' title='Check all' href='#' class='action-button glyphicon glyphicon-ok'></i>
    <i id='help_example_none' title='Uncheck all' href='#' class='action-button glyphicon glyphicon-remove'></i></label>"),
    checkboxGroupInput("help_example", NULL, help_example,
       selected = state_init("help_example"), inline = TRUE)
  )

output$help_example_app <- renderUI({
  sidebarLayout(
    sidebarPanel(
      help_base_ui,
      help_example_ui,
      uiOutput("help_text")
    ),
    mainPanel(
      htmlOutput("help_data"),
      htmlOutput("help_example")
    )
  )
})
