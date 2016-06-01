tmp <-c("radiant.data", "AlgDesign")
tmp <- sapply(tmp, library, character.only = TRUE)
rm(tmp)

options(radiant.path.data = system.file(package = "radiant.data"))

# sourcing from radiant base, note that path is set in base/global.R
source(file.path(getOption("radiant.path.data"), "app/global.R"), encoding = "UTF-8", local = TRUE)

ifelse (grepl("radiant.design", getwd()) && file.exists("../inst") , "..", system.file(package = "radiant.design")) %>%
  options(radiant.path.design = .)

addResourcePath("figures_design", "tools/help/figures/")

r_url_list <- getOption("radiant.url.list")
r_url_list[["Random sampling"]] <- "design/sampling/"
r_url_list[["Sample size (single)"]] <- "design/sample-size/"
r_url_list[["Sample size (compare)"]] <- "design/sample-size-comp/"
r_url_list[["Design of Experiments"]] <- "design/doe/"
options(radiant.url.list = r_url_list); rm(r_url_list)
options(radiant.url.patterns = make_url_patterns())
