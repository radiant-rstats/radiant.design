## ui for design menu in radiant
navbar_proj(
  do.call(
    navbarPage,
    c(
      "Radiant",
      getOption("radiant.nav_ui"),
      getOption("radiant.design_ui"),
      getOption("radiant.shared_ui"),
      help_menu("help_design_ui")
    )
  )
)
