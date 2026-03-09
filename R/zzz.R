.onLoad <- function(libname, pkgname) {
  cache_dir <- nhanes_default_cache_dir()
  rds_file  <- file.path(cache_dir, "nhanes_2017-2018.rds")
  if (!file.exists(rds_file)) {
    tryCatch(
      {
        packageStartupMessage(
          "irxforge: Downloading NHANES 2017-2018 (DEMO, LAB, EXAM) to ",
          cache_dir, " ...\n",
          "  This only happens once. ",
          "Call download_nhanes_cache() to add more years or groups."
        )
        download_nhanes_cache(
          groups = c("DEMO", "LAB", "EXAM"),
          years  = "2017-2018",
          path   = cache_dir
        )
        packageStartupMessage("irxforge: NHANES cache ready.")
      },
      error = function(e) {
        packageStartupMessage(
          "irxforge: Could not download NHANES cache: ", conditionMessage(e), "\n",
          "  Call download_nhanes_cache() when a network connection is available."
        )
      }
    )
  }
}
