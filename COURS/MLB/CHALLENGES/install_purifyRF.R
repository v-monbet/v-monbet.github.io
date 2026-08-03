if(!require(minfi)){
  if(!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager") BiocManager::install("minfi")
}
if(!require(devtools)) install.packages("devtools")
library(devtools)
BiocManager::install("IlluminaHumanMethylationEPICmanifest")
if(!require(minfiData)){
  if(!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  BiocManager::install("minfiData")
}
# prevent warnings from beeing converted to errors when calling install_github
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS=TRUE)
install_github('mwsill/RFpurify')
