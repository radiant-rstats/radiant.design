# sourcing from radiant base, note that path is set in base/global.R

if (!exists("r_path")) r_path <- system.file(package = "radiant.data")

source(file.path(r_path, "base/global.R"), encoding = "UTF-8", local = TRUE)

addResourcePath("figures_example", "tools/help/figures/")
