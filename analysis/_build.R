# Builds HTML pages for hosting reports
# URL: vandeynze.github.io/salmon_culverts/
# Files saved in "docs" subdirectory

rm(list = ls())

library(workflowr)
library(here)
library(knitr)

# Replace the example text with your information
# wflow_git_config(user.name = "Braeden Van Deynze", user.email = "vandeynz@uw.edu")

# Start wflow (sets up folder structure w/o overwriting)
# wflow_start(
#   here(),
#   existing = TRUE
# )
# wflow_use_github(create_on_github = FALSE)

# Copy reports generated from .R rather than in .Rmd
spin(here("R/C.culvertsspatial/06.spatialexplore.R"), knit = FALSE, format = "Rmd", precious = TRUE)
file.copy(here("R/C.culvertsspatial/06.spatialexplore.Rmd"), to = here("analysis/spatial_variables_summary.Rmd"), overwrite = TRUE)
file.remove(here("R/C.culvertsspatial/06.spatialexplore.Rmd"))

spin(here("R/C.culvertsspatial/07.spatialcostmodels.R"), knit = FALSE, format = "Rmd", precious = TRUE)
file.copy(here("R/C.culvertsspatial/07.spatialcostmodels.Rmd"), to = here("analysis/spatial_cost_models.Rmd"), overwrite = TRUE)
file.remove(here("R/C.culvertsspatial/07.spatialcostmodels.Rmd"))

spin(here("R/C.culvertsspatial/09.spatialdependence.R"), knit = FALSE, format = "Rmd", precious = TRUE)
file.copy(here("R/C.culvertsspatial/09.spatialdependence.Rmd"), to = here("analysis/spatial_dependence_models.Rmd"), overwrite = TRUE)
file.remove(here("R/C.culvertsspatial/09.spatialdependence.Rmd"))


# Build site
wflow_build(
  files = list.files(
    here("analysis"), 
    "*.Rmd",
    full.names = TRUE
  )
)
