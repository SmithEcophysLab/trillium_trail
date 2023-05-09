# Compiling script for CO2 response curves and dark respiration values
# Note that all paths in the first part of this script assume working directory 
# root is the path to this script in the "trillium_trail" git
# repository. Path assigned via Apple operating system, may differ  on Windows 
# operating systems.

###############################################################################
## Load readLicorData package
###############################################################################
library(readLicorData)
library(dplyr)

###############################################################################
## Load co2 response curve data files for initial field campaign
###############################################################################

# Albert (note: using a modified .txt file where leaf area was originally 
# modified from Zinny's experiment. .txt file corrected to 6 cm2, so no need 
# to change calculation here. Albert data also filtered for initial curves 
# where gasket issues were apparent)
alb_0419 <- licorData("../2023_field_data/licor_raw/apr2023/2023-04-19_co2resp_albert.txt") %>%
  filter(id != "5488" & id != "5488_light")
# write.csv(alb_0419, "../2023_field_data/licor_cleaned/2023-04-19_co2resp_albert.csv",
#           row.names = FALSE)

# Ozzie (no issues to report)
ozz_0419 <- licorData("../2023_field_data/licor_raw/apr2023/2023-04-19_co2resp_ozz")
# write.csv(ozz_0419, "../2023_field_data/licor_cleaned/2023-04-19_co2resp_ozz.csv",
#           row.names = FALSE)

# Stan
stan_0419 <- licorData("../2023_field_data/licor_raw/apr2023/2023-04-19_co2resp_stan")
# write.csv(stan_0419, "../2023_field_data/licor_cleaned/2023-04-19_co2resp_stan.csv",
#           row.names = FALSE)

stan_0420 <- licorData("../2023_field_data/licor_raw/apr2023/2023-04-20_co2resp_stan")
# write.csv(stan_0420, "../2023_field_data/licor_cleaned/2023-04-20_co2resp_stan.csv",
#           row.names = FALSE)

stan_0421 <- licorData("../2023_field_data/licor_raw/apr2023/2023-04-21_co2resp_stan")
# write.csv(stan_0421, "../2023_field_data/licor_cleaned/2023-04-21_co2resp_stan.csv",
#           row.names = FALSE)

# Yadi
yadi_0419 <- licorData("../2023_field_data/licor_raw/apr2023/2023-04-19_co2resp_yadi")
# write.csv(yadi_0419, "../2023_field_data/licor_cleaned/2023-04-19_co2resp_yadi.csv",
#           row.names = FALSE)

yadi_0420 <- licorData("../2023_field_data/licor_raw/apr2023/2023-04-20_co2resp_yadi")
# write.csv(yadi_0420, "../2023_field_data/licor_cleaned/2023-04-20_co2resp_yadi.csv",
#           row.names = FALSE)

yadi_0421 <- licorData("../2023_field_data/licor_raw/apr2023/2023-04-21_co2resp_yadi")
# write.csv(yadi_0421, "../2023_field_data/licor_cleaned/2023-04-21_co2resp_yadi.csv",
#           row.names = FALSE)

###############################################################################
## Curve-fitting prep
###############################################################################
file.list <- list.files(path = "../2023_field_data/licor_cleaned",
                             recursive = TRUE, pattern = "\\.csv$",
                             full.names = TRUE)

file.list <- setNames(file.list, stringr::str_extract(basename(file.list), 
                                                      '.*(?=\\.csv)'))

# Merge list of data frames, arrange by marchine, measurement type, id, and time elapsed
merged_curves <- lapply(file.list, read.csv) %>%
  reshape::merge_all() %>%
  arrange(machine, id, elapsed)









