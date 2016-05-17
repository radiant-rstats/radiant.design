design_ui <- tagList(
  navbarMenu("Design",
    tabPanel("Sampling", uiOutput("sampling")),
    tabPanel("Sample size (single)", uiOutput("sample_size")),
    tabPanel("Sample size (compare)", uiOutput("sample_size_comp"))
  )
)
