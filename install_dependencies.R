dependencies = c('tidyverse', 'patchwork', 'shinycustomloader', 'DT', 'shiny', 'shinyWidgets') 

###########################################################

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    cat(paste("Installing", p, "\n"))
    install.packages(p, dependencies = TRUE)
  }
  else {
    cat(paste("Already installed package:", p, "\n"))
  }
}

invisible(sapply(dependencies, install_if_missing))
