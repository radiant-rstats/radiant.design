import_fs("radiant.design", libs = "mvtnorm", incl = "pmvnorm")

## urls for menu
r_url_list <- getOption("radiant.url.list")
r_url_list[["Random sampling"]] <- "design/sampling/"
r_url_list[["Random assignment"]] <- "design/randomize/"
r_url_list[["Sample size (single)"]] <- "design/sample-size/"
r_url_list[["Sample size (compare)"]] <- "design/sample-size-comp/"
r_url_list[["Design of Experiments"]] <- "design/doe/"
options(radiant.url.list = r_url_list)
rm(r_url_list)

## design menu
options(
  radiant.design_ui =
    tagList(
      navbarMenu(
        "Design",
        tags$head(
          tags$script(src = "www_design/js/run_return.js")
        ),
        "DOE",
        tabPanel("Design of Experiments", uiOutput("doe")),
        "----", "Sample",
        tabPanel("Random sampling", uiOutput("sampling")),
        tabPanel("Random assignment", uiOutput("randomizer")),
        tabPanel("Sample size (single)", uiOutput("sample_size")),
        tabPanel("Sample size (compare)", uiOutput("sample_size_comp"))
      )
    )
)
