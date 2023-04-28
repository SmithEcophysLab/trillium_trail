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
camp_1_alb <- licorData("../2023_field_data/licor_raw/apr2023/2023-04-19_co2resp_albert.txt") %>%
  filter(id != "5488" & id != "5488_light")
write.csv(camp_1_alb, "../2023_field_data/licor_cleaned/2023-04-19_co2resp_albert.csv",
          row.names = FALSE)

# Ozzie (no issues to report)
camp_1_ozz <- licorData("../2023_field_data/licor_raw/apr2023/2023-04-19_co2resp_ozz")
write.csv(camp_1_ozz, "../2023_field_data/licor_cleaned/2023-04-19_co2resp_ozz.csv",
          row.names = FALSE)

# Stan

# Yadi




