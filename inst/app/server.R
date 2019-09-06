if (isTRUE(getOption("radiant.from.package"))) {
  library(radiant.design)
}

shinyServer(function(input, output, session) {

  ## source shared functions
  source(file.path(getOption("radiant.path.data"), "app/init.R"), encoding = getOption("radiant.encoding"), local = TRUE)
  source(file.path(getOption("radiant.path.data"), "app/radiant.R"), encoding = getOption("radiant.encoding"), local = TRUE)

  ## source data & app tools from radiant.data
  for (file in list.files(
    c(
      file.path(getOption("radiant.path.data"), "app/tools/app"),
      file.path(getOption("radiant.path.data"), "app/tools/data")
    ),
    pattern = "\\.(r|R)$",
    full.names = TRUE)) {
    source(file, encoding = getOption("radiant.encoding"), local = TRUE)
  }

  ## setting up help
  source("help.R", encoding = getOption("radiant.encoding"), local = TRUE)

  ## help ui
  output$help_design_ui <- renderUI({
    sidebarLayout(
      sidebarPanel(
        help_data_panel,
        help_design_panel,
        uiOutput("help_text"),
        width = 3
      ),
      mainPanel(
        HTML(paste0("<h2>Select help files to show and search</h2><hr>")),
        htmlOutput("help_data"),
        htmlOutput("help_design")
      )
    )
  })

  ## packages to use for example data
  options(radiant.example.data = c("radiant.data", "radiant.design"))

  ## 'sourcing' package functions in the server.R environment for development
  if (!isTRUE(getOption("radiant.from.package"))) {
    for (file in list.files("../../R", pattern = "\\.(r|R)$", full.names = TRUE)) {
      source(file, encoding = getOption("radiant.encoding"), local = TRUE)
    }
    cat("\nGetting radiant.design from source ...\n")
  }

  ## source analysis tools for design app
  for (file in list.files(c("tools/analysis"), pattern = "\\.(r|R)$", full.names = TRUE))
    source(file, encoding = getOption("radiant.encoding"), local = TRUE)

  ## save state on refresh or browser close
  saveStateOnRefresh(session)
})
