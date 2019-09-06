## sourcing from radiant.data
options(radiant.path.data = system.file(package = "radiant.data"))
source(file.path(getOption("radiant.path.data"), "app/global.R"), encoding = getOption("radiant.encoding", default = "UTF-8"), local = TRUE)

ifelse(grepl("radiant.design", getwd()) && file.exists("../../inst"), "..", system.file(package = "radiant.design")) %>%
  options(radiant.path.design = .)

## setting path for figures in help files
addResourcePath("figures_design", "tools/help/figures/")

## setting path for www resources
addResourcePath("www_design", file.path(getOption("radiant.path.design"), "app/www/"))

## loading urls and ui
source("init.R", encoding = getOption("radiant.encoding", "UTF-8"), local = TRUE)
options(radiant.url.patterns = make_url_patterns())

if (!"package:radiant.design" %in% search() &&
    isTRUE(getOption("radiant.development")) &&
    getOption("radiant.path.design") == "..") {
  options(radiant.from.package = FALSE)
} else {
  options(radiant.from.package = TRUE)
}
