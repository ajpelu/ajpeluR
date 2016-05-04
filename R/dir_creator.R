# Function to create a directory tree within a R project 
# version 1 
# Author: Perez-Luque AJ (@ajpelu)
# Date: 2016 April 
# See Rodriguez-Sanchez et al. 2016 (Ecosistemas)


dir_creator <- function(mypath) { 
  # Store temporal wd 
  tempwd <- getwd() 
  
  # set wd to path provided in the funcion 
  setwd(mypath)
  
  # Create the directory tree
  dir.create("data") # depured data
  dir.create("data_raw") # raw data
  dir.create("analysis") # anaylisis scripts 
  dir.create("R") # customized functions 
  dir.create("man") # function documentation, texts, etc. 
  
  # Set old wd
  setwd(tempwd)
} 