CheminLib <- "Packages/"
# CheminDir <- "Packages/ZipPkg/"

install.packages("shiny",lib = CheminLib,dependencies = T)
install.packages("shinythemes",lib = CheminLib,dependencies = T)

install.packages("dplyr",lib = CheminLib,dependencies = T)
install.packages("jsonlite",lib = CheminLib)
install.packages("curl",lib = CheminLib)
install.packages("httr",lib = CheminLib)
install.packages("devtools",lib = CheminLib)
install.packages("DT",lib = CheminLib)
 
devtools::install_github("rstudio/ggvis",lib = CheminLib)

