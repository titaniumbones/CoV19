getmitigations <- function(){
  mitigations <- read.csv("inst/Mitigations.csv")
cols <- colnames(mitigations)[-1]
for(i in cols) mitigations[,i] <- as.Date(mitigations[,i])
#mitigations[is.na(mitigations)] <- as.Date("2100-01-01")
save(mitigations, file="data/mitigations.RData")
}