.onLoad <- function(libname, pkgname) {
  cache_dir <- nhanes_default_cache_dir()
  if (!dir.exists(cache_dir) || length(list.files(cache_dir, pattern = "\\.rds$")) == 0) {
    tryCatch(
      {
        packageStartupMessage(
          "irxforge: Downloading default NHANES tables (DEMO, BMX; 2017-2018) ",
          "to ", cache_dir, " ...\n",
          "  This only happens once. Call download_nhanes_cache() to add more tables."
        )
        download_nhanes_cache(
          tables = c("DEMO", "BMX"),
          years  = "2017-2018",
          path   = cache_dir
        )
        packageStartupMessage("irxforge: NHANES cache ready.")
      },
      error = function(e) {
        packageStartupMessage(
          "irxforge: Could not download NHANES cache: ", conditionMessage(e), "\n",
          "  Call download_nhanes_cache() to set it up when a network is available."
        )
      }
    )
  }
}
