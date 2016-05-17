help_menu <-
  tagList(
    navbarMenu("", icon = icon("question-circle"),
      tabPanel("Help", uiOutput("help_example_app"), icon = icon("question")),
      tabPanel("Videos", uiOutput("help_videos"), icon = icon("film")),
      tabPanel("About", uiOutput("help_about"), icon = icon("info")),
      tabPanel(tags$a("", href = "http://vnijs.github.io/radiant/", target = "_blank",
               list(icon("globe"), "Radiant docs"))),
      tabPanel(tags$a("", href = "https://github.com/vnijs/radiant/issues", target = "_blank",
               list(icon("github"), "Report issue")))
    ),
    js_head
  )

example_ui <- tagList(
  navbarMenu("Example",
    tabPanel("App 1", uiOutput("app1")),
    tabPanel("App 2", uiOutput("app2"))
  )
)

shinyUI(
  do.call(navbarPage, c("Example", nav_ui, example_ui, shared_ui, help_menu))
)
