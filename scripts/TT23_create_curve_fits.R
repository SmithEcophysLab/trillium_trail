###############################################################################
## Load libraries and relevant datasets
###############################################################################

## Load libraries
library(dplyr)
library(plantecophys)

# Load clean and compiled Licor data sheet
licor.data <- read.csv("../data/TT23_licor_merged_pre_closure.csv") %>%
  dplyr::select(id, machine, date, A, Ci, Ca, CO2_r, gsw, Tleaf, VPDleaf, Qin, 
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
tri.6578.pre <- fitaci(data = subset(licor.data, id == "6578_b" & keep.row == "yes"),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"))
plot(tri.6578.pre)
aci.coefs <- data.frame(tag.id = "6578", spp = "Tri", plot = "3", timepoint = "pre_closure", 
                        t(coef(tri.6578.pre)))

## Tag: 5488
tri.5488.pre <- fitaci(data = subset(licor.data, id == "5488"),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"))
plot(tri.5488.pre)
aci.coefs[2,] <- c(tag.id = "5488", spp = "Tri", plot = "3", timepoint = "pre_closure", 
                        t(coef(tri.5488.pre)))

## Tag: 4714
tri.4714.pre <- fitaci(data = subset(licor.data, id == "4714_a" & keep.row == "yes"),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"))
plot(tri.4714.pre)
aci.coefs[3,] <- c(tag.id = "4714", spp = "Tri", plot = "3", timepoint = "pre_closure", 
                   t(coef(tri.4714.pre)))

## Tag: 902
tri.902.pre <- fitaci(data = subset(licor.data, id == "902" & (A < 1 | A > 16)),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"))
plot(tri.902.pre)
aci.coefs[4,] <- c(tag.id = "902", spp = "Tri", plot = "3", timepoint = "pre_closure", 
                   t(coef(tri.902.pre)))

## Tag: 3563
tri.3563.pre <- fitaci(data = subset(licor.data, id == "3563" & (A < 1.5 | A > 13)),
                  varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                   Ci = "Ci", PPFD = "Qin"))
plot(tri.3563.pre)
aci.coefs[5,] <- c(tag.id = "3563", spp = "Tri", plot = "3", timepoint = "pre_closure", 
                   t(coef(tri.3563.pre)))

## Tag: 1686
tri.1686.pre <- fitaci(data = subset(licor.data, id == "1686" & (A < 1.25 | A > 15)),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"))
plot(tri.1686.pre)
aci.coefs[6,] <- c(tag.id = "1686", spp = "Tri", plot = "3", timepoint = "pre_closure", 
                   t(coef(tri.1686.pre)))

## Tag: 425
# tri.425.pre <- fitaci(data = subset(licor.data, id == "425" & keep.row == "yes"),
#                    varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
#                                     Ci = "Ci", PPFD = "Qin"),
#                    fitTPU = TRUE)
# plot(tri.425.pre)
aci.coefs[7,] <- c(tag.id = "425", spp = "Tri", plot = "3", timepoint = "pre_closure", 
                   Vcmax = NA, Jmax = NA, Rd = NA)

## Tag: 2416
tri.2416.pre <- fitaci(data = subset(licor.data, id == "2416" & (A < 1.25 | A > 14)),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"))
plot(tri.2416.pre)
aci.coefs[8,] <- c(tag.id = "2416", spp = "Tri", plot = "3", timepoint = "pre_closure", 
                   t(coef(tri.2416.pre)))

## Tag: 552
tri.552.pre <- fitaci(data = subset(licor.data, id == "552" & keep.row == "yes"),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"))
plot(tri.552.pre)
aci.coefs[9,] <- c(tag.id = "552", spp = "Tri", plot = "3", timepoint = "pre_closure", 
                   t(coef(tri.552.pre)))

## Tag: 5495
tri.5495.pre <- fitaci(data = subset(licor.data, id == "5495" & keep.row == "yes"),
                  varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                   Ci = "Ci", PPFD = "Qin"))
plot(tri.5495.pre)
aci.coefs[10,] <- c(tag.id = "5495", spp = "Tri", plot = "3", timepoint = "pre_closure", 
                   t(coef(tri.5495.pre)))

## Tag: 916
tri.916.pre <- fitaci(data = subset(licor.data, id == "916" & keep.row == "yes"),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"))
plot(tri.916.pre)
aci.coefs[11,] <- c(tag.id = "916", spp = "Tri", plot = "3", timepoint = "pre_closure", 
                   t(coef(tri.916.pre)))


## Tag: 583
tri.583.pre <- fitaci(data = subset(licor.data, id == "583a" & (A < 0.5 | A > 11.3)),
                  varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                   Ci = "Ci", PPFD = "Qin"))
plot(tri.583.pre)
aci.coefs[12,] <- c(tag.id = "583", spp = "Tri", plot = "3", timepoint = "pre_closure", 
                    t(coef(tri.583.pre)))

## Tag: 5479
tri.5479.pre <- fitaci(data = subset(licor.data, id == "5479a" & (A > 0 & A < 1 | A > 13.5)),
                  varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                   Ci = "Ci", PPFD = "Qin"))
plot(tri.5479.pre)
aci.coefs[13,] <- c(tag.id = "5479", spp = "Tri", plot = "3", timepoint = "pre_closure", 
                    t(coef(tri.5479.pre)))

## Tag: 4934
tri.4934.pre <- fitaci(data = subset(licor.data, id == "4934a_redo" & keep.row == "yes"),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"))
plot(tri.4934.pre)
aci.coefs[14,] <- c(tag.id = "4934", spp = "Tri", plot = "3", timepoint = "pre_closure", 
                    t(coef(tri.4934.pre)))

## Tag: 6558
tri.6558.pre <- fitaci(data = subset(licor.data, id == "6558" & (A < 1.5 | A > 16)),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"))
plot(tri.6558.pre)
aci.coefs[15,] <- c(tag.id = "6558", spp = "Tri", plot = "3", timepoint = "pre_closure", 
                    t(coef(tri.6558.pre)))
## Tag: 2329
tri.2329.pre <- fitaci(data = subset(licor.data, id == "2329" & (A < 1.5 | A > 12.1)),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"),
                   citransition = 250)
plot(tri.2329.pre)
aci.coefs[16,] <- c(tag.id = "2329", spp = "Tri", plot = "3", timepoint = "pre_closure", 
                    t(coef(tri.2329.pre)))

## Tag: 774
tri.774.pre <- fitaci(data = subset(licor.data, id == "774" & (A < 1 | A > 14)),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"),
                   citransition = 300)
plot(tri.774.pre)
aci.coefs[17,] <- c(tag.id = "774", spp = "Tri", plot = "3", timepoint = "pre_closure", 
                    t(coef(tri.774.pre)))

## Tag: 4942
tri.4942.pre <- fitaci(data = subset(licor.data, id == "4942" & (A < 1.5 | A > 15)),
                  varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                   Ci = "Ci", PPFD = "Qin"))
plot(tri.4942.pre)
aci.coefs[18,] <- c(tag.id = "4942", spp = "Tri", plot = "3", timepoint = "pre_closure", 
                    t(coef(tri.4942.pre)))


###############################################################################
## Plot 5: Trillium
###############################################################################

## Tag: 2547
tri.2547.pre <- fitaci(data = subset(licor.data, id == "2547b" & keep.row == "yes"),
                   varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                    Ci = "Ci", PPFD = "Qin"))
plot(tri.2547.pre)
aci.coefs[19,] <- c(tag.id = "2547", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.2547.pre)))

## Tag: 5115
tri.5115.pre <- fitaci(data = subset(licor.data, id == "5115" & (A < 1 | A > 15)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"),
                       citransition = 300)
plot(tri.5115.pre)
aci.coefs[20,] <- c(tag.id = "5115", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.5115.pre)))

## Tag: 5228
tri.5228.pre <- fitaci(data = subset(licor.data, id == "5228" & (A > 0 & A < 1 | A > 10)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"),
                       fitmethod = 'bilinear', citransition = 250)
plot(tri.5228.pre)
aci.coefs[21,] <- c(tag.id = "5228", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.5228.pre)))

## Tag: 4000
tri.4000.pre <- fitaci(data = subset(licor.data, id == "4000" & (A > 0 & A < 1.5 | A > 18)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.4000.pre)
aci.coefs[22,] <- c(tag.id = "4000", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.4000.pre)))


## Tag: 3004
tri.3004.pre <- fitaci(data = subset(licor.data, id == "3004" & (A < 1 | A > 15)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"),
                       citransition = 300)
plot(tri.3004.pre)
aci.coefs[23,] <- c(tag.id = "3004", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.3004.pre)))


## Tag: 3077
tri.3077.pre <- fitaci(data = subset(licor.data, id == "3077" & (A > 0 & A < 1 | A > 15)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"),
                       citransition = 250)
plot(tri.3077.pre)
aci.coefs[24,] <- c(tag.id = "3077", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.3077.pre)))


## Tag: 4576
tri.4576.pre <- fitaci(data = subset(licor.data, id == "4576" & (A > 0 & A < 1 | A > 12.5)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"),
                       citransition = 290)
plot(tri.4576.pre)
aci.coefs[25,] <- c(tag.id = "4576", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.4576.pre)))


## Tag: 482
tri.482.pre <- fitaci(data = subset(licor.data, id == "482" & (A > 0 & A < 1 | A > 12)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"),
                      citransition = 250)
plot(tri.482.pre)
aci.coefs[26,] <- c(tag.id = "482", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.482.pre)))


## Tag: 4414
tri.4414.pre <- fitaci(data = subset(licor.data, id == "4414" & (A > 0 & A < 1 | A > 15.7)),
                      varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                       Ci = "Ci", PPFD = "Qin"))
plot(tri.4414.pre)
aci.coefs[27,] <- c(tag.id = "4414", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.4414.pre)))


## Tag: 4959
tri.4959.pre <- fitaci(data = subset(licor.data, id == "4959" & (A > 0 & A < 1 | A > 14)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"),
                       citransition = 225)
plot(tri.4959.pre)
aci.coefs[28,] <- c(tag.id = "4959", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.4959.pre)))


## Tag: 4990
tri.4990.pre <- fitaci(data = subset(licor.data, id == "4990" & (A > 0 & A < 1.5 | A > 15)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"),
                       citransition = 252)
plot(tri.4990.pre)
aci.coefs[29,] <- c(tag.id = "4990", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.4990.pre)))


## Tag: 4431
tri.4431.pre <- fitaci(data = subset(licor.data, id == "4431" & (A > 0 & A < 2 | A > 11)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.4431.pre)
aci.coefs[30,] <- c(tag.id = "4431", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.4431.pre)))


## Tag: 4630 (note: can't find in plot)
aci.coefs[31,] <- c(tag.id = "4630", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    Vcmax = NA, Jmax = NA, Rd = NA)


## Tag: 5877
tri.5877.pre <- fitaci(data = subset(licor.data, id == "5877" & (A > 0 & A < 1 | A > 14)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"),
                       citransition = 240)
plot(tri.5877.pre)
aci.coefs[32,] <- c(tag.id = "5877", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.5877.pre)))


## Tag: 2988
tri.2988.pre <- fitaci(data = subset(licor.data, id == "2988" & (A > 0 & A < 2 | A > 14.5)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"),
                       citransition = 240)
plot(tri.2988.pre)
aci.coefs[33,] <- c(tag.id = "2988", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.2988.pre)))


## Tag: 4109
tri.4109.pre <- fitaci(data = subset(licor.data, id == "4109" & (A > 0 & A < 2 | A > 14.5)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.4109.pre)
aci.coefs[34,] <- c(tag.id = "4109", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.4109.pre)))


## Tag: 2508
tri.2508.pre <- fitaci(data = subset(licor.data, id == "2508" & (A > 0 & A < 2 | A > 14.5)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.2508.pre)
aci.coefs[35,] <- c(tag.id = "2508", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.2508.pre)))


## Tag: 43
tri.2508.pre <- fitaci(data = subset(licor.data, id == "2508" & (A > 0 & A < 2 | A > 14.5)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.2508.pre)
aci.coefs[35,] <- c(tag.id = "2508", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.2508.pre)))


## Tag: 86
tri.86.pre <- fitaci(data = subset(licor.data, id == "86"),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.86.pre)
aci.coefs[36,] <- c(tag.id = "86", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.86.pre)))


## Tag: 5904
tri.5904.pre <- fitaci(data = subset(licor.data, id == "5904"),
                     varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                      Ci = "Ci", PPFD = "Qin"))
plot(tri.5904.pre)
aci.coefs[37,] <- c(tag.id = "5904", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.5904.pre)))


## Tag: 7147
tri.7147.pre <- fitaci(data = subset(licor.data, id == "7147"),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.7147.pre)
aci.coefs[38,] <- c(tag.id = "7147", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.7147.pre)))


## Tag: 1374
tri.1374.pre <- fitaci(data = subset(licor.data, id == "1374" & (A < 4 | A > 14)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.1374.pre)
aci.coefs[39,] <- c(tag.id = "1374", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.1374.pre)))


## Tag: 4105
tri.4105.pre <- fitaci(data = subset(licor.data, id == "4105" & (A < 4 | A > 14)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"),
                       citransition = 250)
plot(tri.4105.pre)
aci.coefs[40,] <- c(tag.id = "4105", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.4105.pre)))


## Tag: 4547
tri.4547.pre <- fitaci(data = subset(licor.data, id == "4547" & (A < 4 | A > 16)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.4547.pre)
aci.coefs[41,] <- c(tag.id = "4547", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.4547.pre)))


## Tag: 5381
tri.5381.pre <- fitaci(data = subset(licor.data, id == "5381" & (A < 4 | A > 16)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.5381.pre)
aci.coefs[42,] <- c(tag.id = "5381", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.5381.pre)))


## Tag: 614
tri.614.pre <- fitaci(data = subset(licor.data, id == "614" & (A < 4 | A > 16)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.614.pre)
aci.coefs[43,] <- c(tag.id = "614", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.614.pre)))


## Tag: 2912
tri.2912.pre <- fitaci(data = subset(licor.data, id == "2912" & (A < 6 | A > 16)),
                      varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                       Ci = "Ci", PPFD = "Qin"))
plot(tri.2912.pre)
aci.coefs[44,] <- c(tag.id = "2912", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.2912.pre)))


## Tag: 1795
tri.1795.pre <- fitaci(data = subset(licor.data, id == "1795" & (A < 6 | A > 16)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.1795.pre)
aci.coefs[45,] <- c(tag.id = "1795", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.1795.pre)))


## Tag: 2563
tri.2563.pre <- fitaci(data = subset(licor.data, id == "2563" & (A < 4 | A > 16)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.2563.pre)
aci.coefs[46,] <- c(tag.id = "2563", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.2563.pre)))


## Tag: flag_8
tri.flag_8.pre <- fitaci(data = subset(licor.data, id == "flag_8" & (A < 4 | A > 16)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.flag_8.pre)
aci.coefs[47,] <- c(tag.id = "flag_8", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.flag_8.pre)))


## Tag: 2573
tri.2573.pre <- fitaci(data = subset(licor.data, id == "2573" & (A < 4 | A > 16)),
                         varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                          Ci = "Ci", PPFD = "Qin"))
plot(tri.2573.pre)
aci.coefs[48,] <- c(tag.id = "2573", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.2573.pre)))


## Tag: flag_3
tri.flag_3.pre <- fitaci(data = subset(licor.data, id == "flag3" & (A < 4 | A > 16)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.flag_3.pre)
aci.coefs[49,] <- c(tag.id = "flag_3", spp = "Tri", plot = "5", timepoint = "pre_closure", 
                    t(coef(tri.flag_3.pre)))


###############################################################################
## Plot 7: Trillium
###############################################################################

## Tag: 632
tri.632.pre <- fitaci(data = subset(licor.data, id == "632"),
                         varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                          Ci = "Ci", PPFD = "Qin"))
plot(tri.632.pre)
aci.coefs[50,] <- c(tag.id = "632", spp = "Tri", plot = "7", timepoint = "pre_closure", 
                    t(coef(tri.632.pre)))


## Tag: 1827
tri.1827.pre <- fitaci(data = subset(licor.data, id == "1827"),
                      varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                       Ci = "Ci", PPFD = "Qin"))
plot(tri.1827.pre)
aci.coefs[51,] <- c(tag.id = "1827", spp = "Tri", plot = "7", timepoint = "pre_closure", 
                    t(coef(tri.1827.pre)))


## Tag: 1851
tri.1851.pre <- fitaci(data = subset(licor.data, id == "1851" & (A < 4 | A > 15)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.1851.pre)
aci.coefs[52,] <- c(tag.id = "1851", spp = "Tri", plot = "7", timepoint = "pre_closure", 
                    t(coef(tri.1851.pre)))


## Tag: 3071
tri.3071.pre <- fitaci(data = subset(licor.data, id == "3071" & (A < 4 | A > 15.5)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.3071.pre)
aci.coefs[53,] <- c(tag.id = "3071", spp = "Tri", plot = "7", timepoint = "pre_closure", 
                    t(coef(tri.3071.pre)))


## Tag: 2927
tri.2927.pre <- fitaci(data = subset(licor.data, id == "2927" & (A < 4 | A > 15.5)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.2927.pre)
aci.coefs[54,] <- c(tag.id = "2927", spp = "Tri", plot = "7", timepoint = "pre_closure", 
                    t(coef(tri.2927.pre)))


## Tag: 2996
tri.2996.pre <- fitaci(data = subset(licor.data, id == "2996" & (A < 4 | A > 15.5)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.2996.pre)
aci.coefs[55,] <- c(tag.id = "2996", spp = "Tri", plot = "7", timepoint = "pre_closure", 
                    t(coef(tri.2996.pre)))


## Tag: 972
tri.972.pre <- fitaci(data = subset(licor.data, id == "972" & (A < 4 | A > 13.5)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.972.pre)
aci.coefs[56,] <- c(tag.id = "972", spp = "Tri", plot = "7", timepoint = "pre_closure", 
                    t(coef(tri.972.pre)))


## Tag: 1647
tri.1647.pre <- fitaci(data = subset(licor.data, id == "1647" & (A < 4 | A > 12)),
                      varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                       Ci = "Ci", PPFD = "Qin"))
plot(tri.1647.pre)
aci.coefs[57,] <- c(tag.id = "1647", spp = "Tri", plot = "7", timepoint = "pre_closure", 
                    t(coef(tri.1647.pre)))


## Tag: 1669
tri.1669.pre <- fitaci(data = subset(licor.data, id == "1669" & (A < 4 | A > 15)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.1669.pre)
aci.coefs[58,] <- c(tag.id = "1669", spp = "Tri", plot = "7", timepoint = "pre_closure", 
                    t(coef(tri.1669.pre)))


## Tag: 2131 (RESOLVE; two curves)
tri.2131.pre <- fitaci(data = subset(licor.data, id == "2131"),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.2131.pre)
aci.coefs[59,] <- c(tag.id = "2131", spp = "Tri", plot = "7", timepoint = "pre_closure", 
                    Vcmax = NA, Jmax = NA, Rd = NA)


## Tag: 2289
tri.2289.pre <- fitaci(data = subset(licor.data, id == "2289" & (A < 4 | A > 16.5)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.2289.pre)
aci.coefs[60,] <- c(tag.id = "2289", spp = "Tri", plot = "7", timepoint = "pre_closure", 
                    t(coef(tri.2289.pre)))


## Tag: 2379
tri.2379.pre <- fitaci(data = subset(licor.data, id == "2379" & (A < 4 | A > 16.5)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.2379.pre)
aci.coefs[61,] <- c(tag.id = "2379", spp = "Tri", plot = "7", timepoint = "pre_closure", 
                    t(coef(tri.2379.pre)))


## Tag: 2382
tri.2382.pre <- fitaci(data = subset(licor.data, id == "2382" & (A < 4 | A > 12)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.2382.pre)
aci.coefs[62,] <- c(tag.id = "2382", spp = "Tri", plot = "7", timepoint = "pre_closure", 
                    t(coef(tri.2382.pre)))


## Tag: 1997
tri.1997.pre <- fitaci(data = subset(licor.data, id == "1997" & (A < 4 | A > 14)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.1997.pre)
aci.coefs[63,] <- c(tag.id = "1997", spp = "Tri", plot = "7", timepoint = "pre_closure", 
                    t(coef(tri.1997.pre)))


## Tag: 2886
tri.2886.pre <- fitaci(data = subset(licor.data, id == "2886" & (A < 4 | A > 11.5)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.2886.pre)
aci.coefs[64,] <- c(tag.id = "2886", spp = "Tri", plot = "7", timepoint = "pre_closure", 
                    t(coef(tri.2886.pre)))


## Tag: 3043
tri.3043.pre <- fitaci(data = subset(licor.data, id == "3043" & (A < 4 | A > 11.5)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.3043.pre)
aci.coefs[65,] <- c(tag.id = "3043", spp = "Tri", plot = "7", timepoint = "pre_closure", 
                    t(coef(tri.3043.pre)))


## Tag: 3427
tri.3427.pre <- fitaci(data = subset(licor.data, id == "3427" & (A < 3 | A > 10.5)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.3427.pre)
aci.coefs[66,] <- c(tag.id = "3427", spp = "Tri", plot = "7", timepoint = "pre_closure", 
                    t(coef(tri.3427.pre)))


## Tag: 3526
tri.3526.pre <- fitaci(data = subset(licor.data, id == "3526" & (A < 3 | A > 14)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.3526.pre)
aci.coefs[67,] <- c(tag.id = "3526", spp = "Tri", plot = "7", timepoint = "pre_closure", 
                    t(coef(tri.3526.pre)))


## Tag: 1975
tri.1975.pre <- fitaci(data = subset(licor.data, id == "1975" & (A < 2 | A > 12)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.1975.pre)
aci.coefs[68,] <- c(tag.id = "1975", spp = "Tri", plot = "7", timepoint = "pre_closure", 
                    t(coef(tri.1975.pre)))


## Tag: 4081
tri.4081.pre <- fitaci(data = subset(licor.data, id == "4081" & (A < 3 | A > 14)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.4081.pre)
aci.coefs[69,] <- c(tag.id = "4081", spp = "Tri", plot = "7", timepoint = "pre_closure", 
                    t(coef(tri.4081.pre)))


## Tag: 6507
tri.6507.pre <- fitaci(data = subset(licor.data, id == "6507" & (A < 10 | A > 14)),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.6507.pre)
aci.coefs[70,] <- c(tag.id = "6507", spp = "Tri", plot = "7", timepoint = "pre_closure", 
                    t(coef(tri.6507.pre)))


## Tag: 7574
tri.7574.pre <- fitaci(data = subset(licor.data, id == "7574"),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(tri.7574.pre)
aci.coefs[71,] <- c(tag.id = "7574", spp = "Tri", plot = "7", timepoint = "pre_closure", 
                    t(coef(tri.7574.pre)))


###############################################################################
## Plot 3: Maianthemum
###############################################################################
mai.7574.pre <- fitaci(data = subset(licor.data, id == "7574"),
                       varnames = list (ALEAF = "A", Tleaf = "Tleaf", 
                                        Ci = "Ci", PPFD = "Qin"))
plot(mai.7574.pre)
aci.coefs[71,] <- c(tag.id = "7574", spp = "Mai", plot = "7", timepoint = "pre_closure", 
                    t(coef(mai.7574.pre)))





###############################################################################
## Plot 5: Maianthemum
###############################################################################


###############################################################################
## Plot 7: Maianthemum
###############################################################################
