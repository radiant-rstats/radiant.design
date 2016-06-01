design_ui <-
	tagList(
	  navbarMenu("Design",
	    "DOE",
	    tabPanel("Design of Experiments", uiOutput("doe")),
	    "----", "Sample",
	    tabPanel("Random sampling", uiOutput("sampling")),
	    tabPanel("Sample size (single)", uiOutput("sample_size")),
	    tabPanel("Sample size (compare)", uiOutput("sample_size_comp"))
	  )
	)

## ui for design menu in radiant
do.call(navbarPage,
  c("Radiant", getOption("radiant.nav_ui"), design_ui, getOption("radiant.shared_ui"),
    help_menu("help_design_ui"))
)
