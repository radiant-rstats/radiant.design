help_design <- c("Design of Experiments" = "doe.md", "Random sampling" = "sampling.md", "Sample size (single)" = "sample_size.Rmd",
                 "Sample size (compare)" = "sample_size_comp.Rmd")
output$help_design <- reactive(append_help("help_design", file.path(getOption("radiant.path.design"),"app/tools/help"), Rmd = TRUE))

observeEvent(input$help_design_all, {help_switch(input$help_design_all, "help_design")})
observeEvent(input$help_design_none,{help_switch(input$help_design_none, "help_design", help_on = FALSE)})

help_design_panel <- tagList(
  wellPanel(
    HTML("<label>Design menu: <i id='help_design_all' title='Check all' href='#' class='action-button glyphicon glyphicon-ok'></i>
    <i id='help_design_none' title='Uncheck all' href='#' class='action-button glyphicon glyphicon-remove'></i></label>"),
    checkboxGroupInput("help_design", NULL, help_design,
       selected = state_group("help_design"), inline = TRUE)
  )
)
