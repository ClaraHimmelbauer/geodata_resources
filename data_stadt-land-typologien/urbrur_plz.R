rm(list = ls()); gc()

packages <- c("dplyr", "readxl", "tidyr")
lapply(packages, library, character.only = T)

setwd("X:/2_Projekte-IST/2022/AP06_BMDW_KMU-Monitoring/Stadt_Land_Typologie")

#-------------------------------------------------------------------------------#

plz <- read_xlsx("PLZ_list.xlsx", col_types = c("numeric", "guess", "guess"))
plz <- plz[, 1]
gem <- read_xlsx("Gemeinden_Postleitzahlen.xlsx", col_types = c("numeric", "guess", rep("numeric", 18)))

out <- plz
out$GKZ2 <- NA

for(i in 1:17){
  colname <- paste0("PLZ", i)
  sub <- gem[, c(3, i+3)]
  sub <- sub[!is.na(sub[, 2]), ]
  merge <- full_join(plz, sub, by = c("Postleitzahl" = colname))
  out <- rbind(out, merge)
}

out <- out[!is.na(out$GKZ2), ]

urbrur <- read_xlsx("urbrur_import.xlsx")

out <- left_join(out, urbrur, by = c("GKZ2" = "GKZ"))
# Wiener Bezirke bei urbrur nicht alle erkannt, aber die sind eh alle städtisch
out$urbrur[is.na(out$urbrur)] <- 0

df <- aggregate(urbrur ~ Postleitzahl, data = out, FUN = min)
write.csv2(df, "urbrur_plz.csv", row.names = F)
