library(tidyverse)
library(dplyr)
library(stringi)
library(goTools)
library(argparser)
library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)

args <- commandArgs(trailingOnly = TRUE)

infile <- args[1]
outfile <- args[2]

inputtable <- read.csv(infile)

#Group GO IDs
biologicalprocess <- CustomEndNodeList("GO:0008150", 1)
molecularfunction <- CustomEndNodeList("GO:0003674", 1)
cellularcomponent <- CustomEndNodeList("GO:0005575", 1)

#GOFunctions
bpgoclassifier <- function(table) {
  transmute(table, GOID=GOID, GOIDClassBP=NA, Frequency=Frequency)
  for (parent in biologicalprocess) {
    lista <- CustomEndNodeList(parent, 5)
    for (child in lista) {
      rown <- which(grepl(child, table$GOID))
      if (0 != length(rown)){
        table[rown, "GOIDClassBP"] <- parent    
      }
    }
  }
  return(table)
}

mfgoclassifier <- function(table) {
  transmute(table, GOID=GOID, GOIDClassBP=GOIDClassBP, GOIDClassMF=NA, Frequency=Frequency)
  for (parent in molecularfunction) {
    lista <- CustomEndNodeList(parent, 5)
    for (child in lista) {
      rown <- which(grepl(child, table$GOID))
      if (0 != length(rown)){
        table[rown, "GOIDClassMF"] <- parent    
      }
    }
  }
  return(table)
}

ccgoclassifier <- function(table) {
  transmute(table, GOID=GOID, GOIDClassBP=GOIDClassBP, GOIDClassMF=GOIDClassMF, GOIDClassCC=NA, Frequency=Frequency)
  for (parent in cellularcomponent) {
    lista <- CustomEndNodeList(parent, 5)
    for (child in lista) {
      rown <- which(grepl(child, table$GOID))
      if (0 != length(rown)){
        table[rown, "GOIDClassCC"] <- parent    
      }
    }
  }
  return(table)
}

MegaGOClassifier <- function(table) {
  bproc <- bpgoclassifier(table)
  print("bprocdone")
  mfunc <- mfgoclassifier(bproc)
  print("mfdone")
  end <- ccgoclassifier(mfunc)
  return(end)
}

endtable <- MegaGOClassifier(inputtable)

write.csv(endtable, outfile)
