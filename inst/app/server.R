shinyServer(function(input, output, session) {

  ## source shared functions
  source("init.R", encoding = getOption("radiant.encoding"), local = TRUE)

  ## generate url patterns
  # r_url_patterns <- make_url_patterns()

  ## packages to use for example data
  # r_example_data = c("radiant.data","radiant.design")
  options(radiant.example.data = c("radiant.data","radiant.design"))

  ## source data & app tools from radiant.data
  for (file in list.files(c(file.path(getOption("radiant.path.data"),"app/tools/app"),
                            file.path(getOption("radiant.path.data"),"app/tools/data")),
                          pattern="\\.(r|R)$", full.names = TRUE))
    source(file, encoding = getOption("radiant.encoding"), local = TRUE)

  ## 'sourcing' radiant's package functions in the server.R environment
  if (!"package:radiant.design" %in% search() && getOption("radiant.path.design") == "..") {
    ## for shiny-server and development
    for (file in list.files("../../R", pattern="\\.(r|R)$", full.names = TRUE))
      source(file, encoding = getOption("radiant.encoding"), local = TRUE)
  } else {
    ## for use with launcher
    radiant.data::copy_all(radiant.design)
  }

 	## source analysis tools for design app
  for (file in list.files(c("tools/analysis"), pattern="\\.(r|R)$", full.names = TRUE))
    source(file, encoding = getOption("radiant.encoding"), local = TRUE)

  ## save state on refresh or browser close
  saveStateOnRefresh(session)

})
