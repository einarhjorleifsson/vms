library(rio)
library(readxl)
library(lubridate)
library(tidyverse)

# ------------------------------------------------------------------------------
# Code objective
#  A MINIMUM code to match the "Icelandic" fleet mmsi in the AIS_EmodNet-database
#  with the local Icelandic (fisheries) vessel identification system that is
#  the basic entity in the Icelandic logbook system.
#
#  The code below is based on the tidyverse approach

# ------------------------------------------------------------------------------
# DATA

# 1. mmsi variable and the corresponding vid variable from the Post and
#    Telecom Administration of Iceland (PTA)
#    main page of PTA: https://www.pfs.is/fjarskipti/skipafjarskipti
#
#    The PTA source data
#url <- "https://www.pfs.is/library/Skrar/Tidnir-og-taekni/NUMER%20Query20022019.xlsx"
#download.file(url, destfile = "dataIS/pfs-vid-mmsi_2018-09-18.xlsx")
#    Import into R
pta <-
  read_excel("dataIS/pfs-vid-mmsi_2018-09-18.xlsx") %>%
  rename(name.pta = Skip,
         vid = Sknr,
         callsign.pta = Kallm,
         mmsi = `MMSI nr`,
         standardC.pta = `Standard-C`) %>%
  mutate(mmsi = as.integer(mmsi))

# 2. vid and number of tows/settings in the Icelandic Logbooks

# library(mar)
# con <- connect_mar()
# afli_stofn(con) %>%
#   filter(ar == 2017) %>%
#   collect(n = Inf) %>%
#   write_rds("dataIS/is-lb_base.rds")

lb.vid <-
  read_rds("dataIS/is-lb_base.rds") %>%
  rename(vid = skipnr,
         gid = veidarf,
         date = vedags,
         lon1 = lengd,
         lat1 = breidd,
         sq = reitur,
         ssq = smareitur,
         z1 = dypi,
         winddir = vindatt,
         beaufort = vindstig,
         ldate = ldags,
         lon2 = lengd_lok,
         lat2 = breidd_lok,
         z2 = dypi_lok,
         year = ar,
         towlength = toglengd) %>%
  group_by(vid) %>%
  summarise(n.lb = n()) %>%
  drop_na()

# 3. The vessel registry - used to match the few vessels that
#    could not be match by the PTA mmsi (see below)
siglo.imo <-
  read_csv("dataIS/is-vessel_siglo.csv") %>%
  select(vid, imo = imonr) %>%
  filter(imo != 0)

# 4. AIS_EmodNet-database AIS data for the Icelandic fleet
#    Input: "dataIS/is2017.csv"

maxFreq2 <- function(x) {
  tt <-
    data.frame(table(x)) %>%
    filter(x != "")
  if(nrow(tt) == 0) {
    return("")
  } else {
    return(as.character(tt[which.max(tt$Freq),1]))
  }
}
meandiff <- function(x) {
  mean(lead(x) - x, na.rm = TRUE)
}
mediandiff <- function(x) {
  median(lead(x) - x, na.rm = TRUE)
}

is17.tbl <-
  import("dataIS/is2017.csv", setclass = "tibble") %>%
  dplyr::select(-c(aisshiptype, flagisocode2)) %>%
  mutate(utime = ymd_hms(utime)) %>%
  arrange(mmsi, utime)
vessel.tbl <-
  is17.tbl %>%
  group_by(mmsi) %>%
  summarise(shipname = maxFreq2(shipname),
            shiplength = maxFreq2(shiplength),
            shipwidth = maxFreq2(shipwidth),
            N = n(),
            meandt = meandiff(utime),
            mediandt = mediandiff(utime),
            imo = maxFreq2(imo),
            callsign = maxFreq2(callsign)) %>%
  mutate(imo = as.integer(imo))
#write_csv(vessel.tbl, "dataIS/vesselRecap_EH_2019-03-26.csv")

# ------------------------------------------------------------------------------
# THE MATCH

# mmsi vid match via pta
is17.vessel <-
  vessel.tbl %>%
  left_join(pta %>%
              select(mmsi, vid))
Match <-
  is17.vessel %>%
  filter(!is.na(vid))
Rest <-
  is17.vessel %>%
  filter(is.na(vid)) %>%
  select(-vid) %>%
  mutate(imo = as.integer(imo)) %>%
  # use imo from siglo for the match
  left_join(siglo.imo)
mmsi.vid.lb.match <-
  bind_rows(Match, Rest) %>%
  arrange(mmsi) %>%
  # NOTE: A full join on the logbooks here
  full_join(lb.vid)

# ------------------------------------------------------------------------------
# ON MISSING MMSI IN THE AIS
mmsi.missing <-
  mmsi.vid.lb.match %>%
  filter(is.na(mmsi)) %>%
  select(vid, n.lb) %>%
  left_join(pta %>%
              filter(mmsi < 990000000)) %>%
  select(mmsi, name.pta, vid, n.lb) %>%
  drop_na()

#write_csv(mmsi.missing, "dataIS/mmsi_missing-in-ais.csv")
