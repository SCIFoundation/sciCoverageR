#' Custom Unlimit Health branded scale colour/fill function for ggplot2.
#'
#' Colours are assigned according to the order in the palette.
#' If variable has more levels than specified in palette, colourRamp to generate additional colours matching the palette.
#' The skip argument is optional but useful where some IUs are not treated
#'
#' @param palette A palette name from one of:
#' * `all` - all Unlimit Health branded colours
#' * `main` - the primary and secondary Unlimit Health colours (7)
#' * `primary` - the 3 primary Unlimit Health colours
#' * `secondary` - the 4 secondary Unlimit Health colours
#' * `complementary` - the 4 highlight colours that are to complement & contrast the secondary palette, though similar in colours.
#' * `yesno` - contrasting dark blue and neutral from the primary palette
#' * `gender` - contrasting red earth and deep aqua from the secondary palette
#' @param discrete Default is TRUE. FALSE if a continuous colour gradient is required.
#' @param reverse Reserve the direction of the palette? Default is FALSE.
#' @param skip A numeric vector. Index/Indices of colours in the palette to be skipped over.
#' @param ... Other arguments passed on to ggplot2 scale function to control limits, breaks and labels etc.
#'
#' @return A grob object to be plotted using grid.draw()
#' @examples  scale_color_uh(palette="main",skip = 3) skip is specified and the 3rd colour in the main palette will be skipped
#'
#' @noRD
#' list of Unlimit Health colours
uh_colours <- c(
  # primary palette
  `dark blue`     = "#003654",
  `clean blue`    = "#0db7e1",
  `neutral`       = "#f0ddc8",

  # secondary palette
  `red earth`     = "#c44829",
  `deep aqua`     = "#008e84",
  `soft purple`   = "#8e5d81",
  `leaf green`    = "#768838",

  # highlight palette
  `sunshine`      = "#ed8923",
  `teal`          = "#63c29d",
  `lilac`         = "#ae84bb",
  `fresh fields`  = "#b8a831")

#' @noRD
#' This returns colours in the colour list above as hex codes
uh_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (uh_colours)

  uh_colours[cols]
}

#' @noRD internal
#' Define palettes
uh_palettes <- list(
  `all`           = uh_cols("dark blue", "clean blue", "neutral", "red earth", "deep aqua","soft purple",
                            "leaf green", "sunshine", "teal", "lilac", "fresh fields"),
  `main`          = uh_cols("dark blue", "clean blue", "neutral", "red earth", "deep aqua","soft purple",
                            "leaf green"),
  `primary`       = uh_cols("dark blue", "clean blue", "neutral"),
  `secondary`     = uh_cols("red earth", "deep aqua","soft purple", "leaf green"),
  `complementary` = uh_cols("red earth", "deep aqua","soft purple",
                            "leaf green", "sunshine", "teal", "lilac", "fresh fields"),
  `yesno`         = uh_cols("dark blue","neutral"),
  `gender`        = uh_cols("red earth", "deep aqua")
)

#' @noRD
#' palette function
uh_pal <- function(palette = "all", reverse = FALSE, skip,...) {

  pal <- uh_palettes[[palette]]
  nmax <- length(pal)
  if (reverse) pal <- rev(pal)
  if (!missing(skip)) pal <- pal[-c(skip)]

  function(n) {

    if (n > nmax) {

      colorRampPalette(pal, ...)

    } else {

      colour_list <- pal[1:n]
      colour_list <- unname(unlist(colour_list))

    }
  }
}


#' @export
scale_color_uh <- function(palette = "all", discrete = TRUE, reverse = FALSE, skip,...) {
  pal <- uh_pal(palette = palette, reverse = reverse,skip)

  if (discrete) {
    discrete_scale("colour", paste0("uh_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


#' @rdname scale_color_uh
scale_fill_uh <- function(palette = "all", discrete = TRUE, reverse = FALSE, skip,...) {
  pal <- uh_pal(palette = palette, reverse = reverse, skip)

  if (discrete) {
    discrete_scale("fill", paste0("uh_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

