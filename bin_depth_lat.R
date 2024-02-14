require(ggplot2)

# funtion to bin WCGOP data
bin_dat <- function(
    dat,
    my_gear = "Hook & Line",
    years = 2002:2022,
    months = 1:12,
    depth_bin_width = 25,
    depth_bin_min = 0,
    depth_bin_max = 400,
    lat_bin_width = 0.5,
    lat_bin_min = 42,
    lat_bin_max = 48,
    meters = FALSE,
    text = TRUE,
    confidential = FALSE,
    save = TRUE,
    subdir = "") {
  # convert to meters if requested
  if (meters) {
    dat$depth <- dat$AVG_DEPTH * 1.8288
  } else {
    dat$depth <- dat$AVG_DEPTH
  }

  # set default max depth by meters or fathoms
  if (is.null(depth_bin_max)) {
    depth_bin_max <- ifelse(meters, 1300, 700)
  }
  # define bins
  depth_bins <- seq(from = depth_bin_min, to = depth_bin_max, by = depth_bin_width)
  lat_bins <- seq(from = lat_bin_min, to = lat_bin_max, by = lat_bin_width)

  # filter for gear, depth, lat, years
  dat <- dat |>
    dplyr::filter(
      gear == my_gear &
        depth < depth_bin_max &
        depth > depth_bin_min &
        AVG_LAT > lat_bin_min &
        AVG_LAT < lat_bin_max &
        YEAR %in% years &
        !is.na(SET_DATE) &
        format(SET_DATE, "%m") %in% sprintf("%02d", months) # changes 1 to "01"
    )

  # bin lat and depth using cut()
  dat <- dat |>
    dplyr::mutate(depth_bin = cut(depth, depth_bins, right = FALSE), ordered_result = TRUE) |>
    dplyr::mutate(lat_bin = cut(AVG_LAT, lat_bins, right = FALSE), ordered_result = TRUE)

  # bin lat and depth using floor()
  dat <- dat |>
    dplyr::mutate(depth_bin_low = depth_bin_width * floor(depth / depth_bin_width)) |>
    dplyr::mutate(lat_bin_low = lat_bin_width * floor(AVG_LAT / lat_bin_width))

  # # exploring bins created by cut() vs floor()
  # # cut is more precise but I don't know how to use for plotting
  # table(dat$depth_bin, dat$depth_bin_low)
  # #              0   25   50   75  100  125  150  175  200  225  250  275  300  325  350  375
  # # [0,25)     331    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
  # # [25,50)      0  209    0    0    0    0    0    0    0    0    0    0    0    0    0    0
  # # [50,75)      0    0 2238    0    0    0    0    0    0    0    0    0    0    0    0    0
  # # [75,100)     0    0    0 3820    0    0    0    0    0    0    0    0    0    0    0    0
  # # [100,125)    0    0    0    0 3701    0    0    0    0    0    0    0    0    0    0    0
  # # ...

  # calculate CPUE (initially just for H&L)
  if (my_gear == "Hook & Line") {
    dat <- dat |>
      dplyr::mutate(cpue = dogfish_lbs_tot / (TOTAL_HOOKS / 100)) |>
      dplyr::filter(!is.na(cpue)) # small fraction is missing effort units
    cpue_units <- "avg. dogfish CPUE\n(lbs / 100 hooks)"
  }

  if (my_gear %in% c("Bottom Trawl", "Midwater Trawl")) {
    dat <- dat |>
      dplyr::mutate(cpue = dogfish_lbs_tot / (HAUL_DURATION / 1)) |>
      dplyr::filter(!is.na(cpue)) # small fraction is missing effort units
    cpue_units <- "avg. dogfish CPUE\n(lbs / hour of trawl)"
  }
  # add column for logical with/without dogfish
  dat <- dat |>
    dplyr::mutate(has_dogfish = dogfish_lbs_tot > 0)

  # bin data
  dat_binned <- dat |>
    dplyr::filter(!is.na(depth_bin_low)) |>
    dplyr::group_by(depth_bin_low, lat_bin_low) |>
    dplyr::summarize(
      dogfish_cpue = mean(cpue, na.rm = TRUE),
      dogfish_lbs_tot = sum(dogfish_lbs_tot),
      nvessels = dplyr::n_distinct(DRVID),
      nsets = dplyr::n(),
      # nsets = dplyr::n_distinct(HAUL_ID), # same result as n() above, no duplicate HAUL_ID values
      nsets_with_dogfish = sum(has_dogfish)
    ) |>
    dplyr::mutate(
      hide = nvessels < 3,
      fraction_with_dogfish = nsets_with_dogfish / nsets
    )

  # calculate the fraction of the catch within bins
  # that have fewer than 3 vessels
  z <- dat_binned |>
    dplyr::filter(nvessels < 3) |>
    dplyr::pull(dogfish_lbs_tot) |>
    sum()
  fraction_excluded <- z / sum(dat_binned$dogfish_lbs_tot)

  # filter cells with < 3 vessels (if requested)
  if (!confidential) {
    dat_binned <- dat_binned |>
      dplyr::filter(!hide)
  }

  # plot mean CPUE
  my_breaks <- c(0.00001, 0.01, 0.1, 1, 10)
  my_breaks <- c(0, 0.1, 1, 10, 100, 1000)
  p <- dat_binned |>
    ggplot(aes(
      x = -depth_bin_low - depth_bin_width / 2,
      y = lat_bin_low - lat_bin_width / 2,
      fill = dogfish_cpue + 0.01
    )) +
    geom_tile() +
    scale_fill_viridis_c(
      name = cpue_units,
      trans = "log",
      breaks = my_breaks,
      labels = my_breaks
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    xlim(-depth_bin_max, -depth_bin_min) +
    ylim(lat_bin_min, lat_bin_max) +
    xlab(paste0("Depth ", ifelse(meters, "(m)", "(fathoms)"))) +
    ylab(paste0("Latitude (degrees N)"))

  # if (any(dat_binned$nvessels < 3)) {
  #   p <- p +
  #     geom_text(
  #       data = ~ subset(.x, nvessels < 3),
  #       aes(label = "X", color = "red")
  #     ) +
  #     labs(color = "fewer than\n3 vessels")
  # }

  if (text) {
    p <- p +
      geom_text(
        aes(label = paste0(nvessels, ",", nsets, "\n", round(100 * fraction_with_dogfish), "%")),
        size = 2,
        color = "white",
        # aes(label = paste0(nvessels, ",", nsets), color = !hide)
      ) #+
    # labs("vessels, sets")
  }

  # add title
  month_string <- ifelse(length(months) == 1,
    paste0("month ", months), paste0("months ", min(months), "-", max(months))
  )
  title <- paste0(
    "Dogfish catch rates in ", my_gear,
    " fishery ", min(years), "-", max(years), " (", month_string, ")"
  )
  p <- p +
    labs(title = title)

  # add fraction excluded
  if (!confidential) {
    p <- p +
      labs(
        subtitle = paste0(
          "Text in cells is N vessels, N sets, and % sets with dogfish\n",
          round(100 * fraction_excluded, 1),
          "% of total dogfish catch in cells excluded due to fewer than 3 vessels"
        )
      )
  }

  # make plot
  if (!save) {
    # create ggplot but don't save
    p
  } else {
browser()
    # clean up filename
    filename <- gsub(pattern = "Dogfish catch rates in ", replacement = "", x = title, fixed = TRUE)
    filename <- gsub(pattern = " & ", replacement = "_", x = filename, fixed = TRUE)
    filename <- gsub(pattern = " ", replacement = "_", x = filename, fixed = TRUE)
    filename <- paste0(filename, ".png")
    filename <- file.path(subdir, filename)
    # save plot
    p |> ggsave(
      filename = file.path("plots", filename),
      width = 7,
      height = ifelse(lat_bin_min < 42, 10, 7)
    )
  }
}
