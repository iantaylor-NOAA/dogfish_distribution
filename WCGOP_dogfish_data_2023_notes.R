# load WCGOP data (contains confidential info)
load("C:/data/WCGOP/2023_dogfish/WCGOP_2002-2022_Dogfish_2023-11-29.RDA")

# exploring total catch by gear
WCGOP_DOGFISH |>
  dplyr::group_by(gear) |>
  dplyr::summarize(sum = sum(dogfish_lbs_tot)) |>
  dplyr::arrange(desc(sum))

# # A tibble: 6 x 2
#   gear                sum
#   <chr>             <dbl>
# 1 Bottom Trawl   8817611.
# 2 Hook & Line    2951096.
# 3 Midwater Trawl 1317435.
# 4 Pot              16832.
# 5 Shrimp Trawl      8039.
# 6 Fixed Gears       2544.

# example stat of how few longline vessels in winter in the north
# (only 4 vessels north of 44 degrees in February, across all years)
test <- WCGOP_DOGFISH |> dplyr::filter(
  AVG_LAT > 44 &
    gear == "Hook & Line" &
    format(SET_DATE, "%m") == "02"
)
length(unique(test$DRVID))

# source functions to add bins and make plots
source("bin_depth_lat.R")
# plot all years/months for each gear
for (gear in c("Bottom Trawl", "Midwater Trawl", "Hook & Line")) {
  WCGOP_DOGFISH |>
    bin_dat(
      years = 2002:2022,
      my_gear = gear,
      months = 1:12,
      subdir = "annual"
    )
}

# plot all years/months for each gear within early/late groups of years
for (gear in c("Bottom Trawl", "Midwater Trawl", "Hook & Line")) {
  for (year_block in 1:2) {
    if (year_block == 1) {
      years = 2002:2012
    } else {
      years = 2013:2022
    }
    WCGOP_DOGFISH |>
      bin_dat(
        years = years,
        my_gear = gear,
        months = 1:12,
        subdir = "annual_early-late"
      )
  }
}

# plot seasons across all years for each gear
for (gear in c("Bottom Trawl", "Midwater Trawl", "Hook & Line")) {
  for (seas in 1:4) {
    WCGOP_DOGFISH |>
      bin_dat(
        years = 2002:2022,
        my_gear = gear,
        months = 1:3 + (seas - 1) * 3,
        subdir = "quarterly"
      )
  }
}

# plot seasons across all years for each gear within early/late groups of years
for (gear in c("Bottom Trawl", "Midwater Trawl", "Hook & Line")) {
  for (seas in 1:4) {
    for (year_block in 1:2) {
      if (year_block == 1) {
        years = 2002:2012
      } else {
        years = 2013:2022
      }
      WCGOP_DOGFISH |>
        bin_dat(
          years = years,
          my_gear = gear,
          months = 1:3 + (seas - 1) * 3,
          subdir = "quarterly"
        )
    }
  }
}

# plot months across all years for each gear
for (gear in c("Bottom Trawl", "Midwater Trawl", "Hook & Line")) {
  for (month in 1:12) {
    WCGOP_DOGFISH |>
      bin_dat(
        years = 2002:2022,
        my_gear = gear,
        months = month,
        subdir = "monthly"
      )
  }
}
