###############################################################################
## Load libraries and relevant datasets
###############################################################################

## Load libraries
library(dplyr)
library(plantecophys)

# Load clean and compiled Licor data sheet
licor.data <- read.csv("../data/TT23_licor_merged_pre_closure.csv") %>%
  select(id, machine, date, A, Ci, Ca, CO2_r, gsw, Tleaf, VPDleaf, Qin, 
         S, Flow, Tair, Tleaf, Fan_speed) %>%
  mutate_at(c("A", "Ci", "Ca", "CO2_r", "gsw", "Tleaf", "VPDleaf", "Qin", 
              "S", "Flow", "Tair", "Tleaf", "Fan_speed"), as.numeric)

# Load species metadata
spp.meta <- read.csv("../log/TT_tag_metadata.csv")

# Add keep row delimiter to remove points
licor.data$keep.row <- "yes"

###############################################################################
## Load custom fxns
###############################################################################
source("/Users/eaperkowski/git/r_functions/temp_standardize.R")

###############################################################################
## Plot 3: Trillium
###############################################################################
## Tag: 6578
tri.6578 <- fitaci(data = subset(licor.data, id == "6578_b" & keep.row == "yes"),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"),
                   fitTPU = TRUE)
plot(tri.6578)
aci.coefs <- data.frame(tag.id = "6578", spp = "Tri", timepoint = "pre_closure", 
                        t(coef(tri.6578)))

## Tag: 5488
tri.5488 <- fitaci(data = subset(licor.data, id == "5488" & keep.row == "yes"),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"),
                   fitTPU = TRUE)
plot(tri.5488)
aci.coefs[2,] <- c(tag.id = "5488", spp = "Tri", timepoint = "pre_closure", 
                        t(coef(tri.5488)))

## Tag: 4714
tri.4714 <- fitaci(data = subset(licor.data, id == "4714_a" & keep.row == "yes"),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"),
                   fitTPU = TRUE)
plot(tri.4714)
aci.coefs[3,] <- c(tag.id = "4714", spp = "Tri", timepoint = "pre_closure", 
                   t(coef(tri.4714)))

## Tag: 902
tri.902 <- fitaci(data = subset(licor.data, id == "902" & keep.row == "yes"),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"),
                   fitTPU = TRUE)
plot(tri.902)
aci.coefs[4,] <- c(tag.id = "902", spp = "Tri", timepoint = "pre_closure", 
                   t(coef(tri.902)))

## Tag: 3563
tri.3563 <- fitaci(data = subset(licor.data, id == "3563" & keep.row == "yes"),
                  varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                   Ci = "Ci", PPFD = "Qin"),
                  fitTPU = TRUE)
plot(tri.3563)
aci.coefs[5,] <- c(tag.id = "3563", spp = "Tri", timepoint = "pre_closure", 
                   t(coef(tri.3563)))

## Tag: 1686
tri.1686 <- fitaci(data = subset(licor.data, id == "1686" & keep.row == "yes"),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"),
                   fitTPU = TRUE)
plot(tri.1686)
aci.coefs[6,] <- c(tag.id = "1686", spp = "Tri", timepoint = "pre_closure", 
                   t(coef(tri.1686)))

## Tag: 425
tri.425 <- fitaci(data = subset(licor.data, id == "425" & keep.row == "yes"),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"),
                   fitTPU = TRUE)
plot(tri.425)
aci.coefs[7,] <- c(tag.id = "425", spp = "Tri", timepoint = "pre_closure", 
                   NA, NA, NA, NA)

## Tag: 2416
tri.2416 <- fitaci(data = subset(licor.data, id == "2416" & keep.row == "yes"),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"),
                   fitTPU = TRUE)
plot(tri.2416)
aci.coefs[8,] <- c(tag.id = "2416", spp = "Tri", timepoint = "pre_closure", 
                   t(coef(tri.2416)))

## Tag: 552
tri.552 <- fitaci(data = subset(licor.data, id == "552" & keep.row == "yes"),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"),
                   fitTPU = TRUE)
plot(tri.552)
aci.coefs[9,] <- c(tag.id = "552", spp = "Tri", timepoint = "pre_closure", 
                   t(coef(tri.552)))

## Tag: 5495
tri.5495 <- fitaci(data = subset(licor.data, id == "5495" & keep.row == "yes"),
                  varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                   Ci = "Ci", PPFD = "Qin"),
                  fitTPU = TRUE)
plot(tri.5495)
aci.coefs[10,] <- c(tag.id = "5495", spp = "Tri", timepoint = "pre_closure", 
                   t(coef(tri.5495)))

## Tag: 916
tri.916 <- fitaci(data = subset(licor.data, id == "916" & keep.row == "yes"),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"),
                   fitTPU = TRUE)
plot(tri.916)
aci.coefs[11,] <- c(tag.id = "916", spp = "Tri", timepoint = "pre_closure", 
                   t(coef(tri.916)))


## Tag: 583
tri.583 <- fitaci(data = subset(licor.data, id == "583a" & keep.row == "yes"),
                  varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                   Ci = "Ci", PPFD = "Qin"),
                  fitTPU = TRUE)
plot(tri.583)
aci.coefs[12,] <- c(tag.id = "583", spp = "Tri", timepoint = "pre_closure", 
                    t(coef(tri.583)))

## Tag: 5479
tri.5479 <- fitaci(data = subset(licor.data, id == "5479a" & keep.row == "yes"),
                  varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                   Ci = "Ci", PPFD = "Qin"),
                  fitTPU = TRUE)
plot(tri.5479)
aci.coefs[13,] <- c(tag.id = "5479", spp = "Tri", timepoint = "pre_closure", 
                    t(coef(tri.5479)))

## Tag: 4934
tri.4934 <- fitaci(data = subset(licor.data, id == "4934a_redo" & keep.row == "yes"),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"),
                   fitTPU = TRUE)
plot(tri.4934)
aci.coefs[14,] <- c(tag.id = "4934", spp = "Tri", timepoint = "pre_closure", 
                    t(coef(tri.4934)))

## Tag: 6558
tri.6558 <- fitaci(data = subset(licor.data, id == "6558" & keep.row == "yes"),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"),
                   fitTPU = TRUE)
plot(tri.6558)
aci.coefs[15,] <- c(tag.id = "6558", spp = "Tri", timepoint = "pre_closure", 
                    t(coef(tri.6558)))
## Tag: 2329
tri.2329 <- fitaci(data = subset(licor.data, id == "2329" & keep.row == "yes"),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"),
                   fitTPU = TRUE)
plot(tri.2329)
aci.coefs[16,] <- c(tag.id = "2329", spp = "Tri", timepoint = "pre_closure", 
                    t(coef(tri.2329)))

## Tag: 774
tri.774 <- fitaci(data = subset(licor.data, id == "774" & keep.row == "yes"),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"),
                   fitTPU = TRUE)
plot(tri.774)
aci.coefs[17,] <- c(tag.id = "774", spp = "Tri", timepoint = "pre_closure", 
                    t(coef(tri.774)))

## Tag: 4942
tri.4942 <- fitaci(data = subset(licor.data, id == "4942" & keep.row == "yes"),
                  varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                   Ci = "Ci", PPFD = "Qin"),
                  fitTPU = TRUE)
plot(tri.4942)
aci.coefs[18,] <- c(tag.id = "4942", spp = "Tri", timepoint = "pre_closure", 
                    t(coef(tri.4942)))


###############################################################################
## Plot 5: Trillium
###############################################################################








###############################################################################
## Plot 7: Trillium
###############################################################################


###############################################################################
## Plot 3: Maianthemum
###############################################################################


###############################################################################
## Plot 5: Maianthemum
###############################################################################


###############################################################################
## Plot 7: Maianthemum
###############################################################################
