# .libPaths(<Mappen som indeholder de installerede pakker du selv har hentet>)
# .libPaths skal kun bruges, hvis installerede pakker bliver manuelt lagt i en bestemt mappe!
.libPaths(c("C:/Rdevelop/myLIBRARY")) # FTHs sti til installerede pakker

library(dplyr, quietly = T, warn.conflicts = F, verbose = F)
library(tidyr, quietly = T, warn.conflicts = F, verbose = F)
library(lubridate, quietly = T, warn.conflicts = F, verbose = F)
library(readxl, quietly = T, warn.conflicts = F, verbose = F)
library(ggplot2, quietly = T, warn.conflicts = F, verbose = F)
library(gridExtra)
library(grid)
# library(addfuns)
# library(skillskore)

source("libs/fncs.R")
