# Bootstrap summaries
bc_sum <- function(x, alpha = 0.95) {
  x <- x[!is.na(x)]
  c("Mean" = mean(x), 
    "Lower" = coxed::bca(x, conf.level = alpha)[1], 
    "Upper" = coxed::bca(x, conf.level = alpha)[2])
}
