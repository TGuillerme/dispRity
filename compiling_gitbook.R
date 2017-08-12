# WORKS IN TERMINAL, NOT RSTUDIO OR RGUI

r
# setwd to folder that contains the manual files, dispRity/gitbook

bookdown::render_book("index.Rmd", "bookdown::gitbook") # HTML version

# To get PDF - this currently doesn't work due to some special characters
# But could be fixed at some point
bookdown::render_book("index.Rmd", "bookdown::pdf_book") # PDF version