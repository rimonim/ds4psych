set.seed(2023)

library(tidyverse)

knitr::opts_chunk$set(
  comment = "#>",
  # collapse = TRUE,
  fig.retina = 2,
  fig.width = 8
  # fig.show = "hold"
)

# # For PDF output
# knitr::opts_chunk$set(
#   comment = "#>",
#   size = "scriptsize",
#   collapse = TRUE,
#   cache = TRUE,
#   cache.lazy = FALSE,
#   fig.width = 10,
#   width = 30,
#   out.width = 550
# )
# 
# default_chunk_hook  <- knitr::knit_hooks$get("chunk")
# 
# latex_font_size <- c("Huge", "huge", "LARGE", "Large", 
#                      "large", "normalsize", "small", 
#                      "footnotesize", "scriptsize", "tiny")
# 
# knitr::knit_hooks$set(chunk = function(x, options) {
#   x <- default_chunk_hook(x, options)
#   if(options$size %in% latex_font_size) {
#     paste0("\n \\", options$size, "\n\n", 
#            x, 
#            "\n\n \\normalsize"
#     )
#   } else {
#     x
#   }
# })
