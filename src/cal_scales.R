# Color scales for plotting
cal_sc <- c("Reference" = "#984ea3", 
            "Planting only" = "#4daf4a", 
            "Planting & maturity" = "#377eb8", 
            "Optimal cycle" = "#e41a1c")

el <- c("1934-1943" = "#386cb0", 
        "1973-1984" = "#f0027f", 
        "2005-2014" = "#bf5b17")

# Labelling GCM facets
model_lbls <- function(y) {
  sapply(y, function(x) {
    if (x == "gfdl-esm4") {
      expression(bold(paste("gfdl-esm4 ("*symbol('\257')*")")))
    } else if (x == "ipsl-cm6a-lr") {
      expression(bold(paste("ipsl-cm6a-lr ("*symbol('\255')*")")))
    } else if (x == "mpi-esm1-2-hr") {
      expression(bold(paste("mpi-esm1-2-hr ("*symbol('\257')*")")))
    } else if (x == "mri-esm2-0") {
      expression(bold(paste("mri-esm2-0 ("*symbol('\257')*")")))
    } else {
      expression(bold(paste("ukesm1-0-ll ("*symbol('\255')*")")))
    }
  })
}
