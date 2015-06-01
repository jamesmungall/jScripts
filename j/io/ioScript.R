# Input & Output Operations
# ==================================
# Start date: 14th July 2012

# Run j.io.startup() to load
# dependent packages: 

j.io.startup=function(showPrints=FALSE){
  if(showPrints){
    print("loading j/io/ioScript.R");
    print("no dependencies");
  }
}
# 
# Aim: To perform input & output operations
# Functions: (all preceeded by j.io. as a namespace)
#   1. writeVectorToCsv   TODO testing
#   2. readCsvToVector    TODO

j.io.writeVectorToCsv = function(v, fileName){
  write(v, file = fileName, ncolumns = if(is.character(v)) 1 else length(v), append = TRUE, sep = ",")
}
j.io.readCsvToVector = function(fileName){
   return(read.table(fileName,header=FALSE));
}