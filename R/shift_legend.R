#' Shift legend to empty space
#'
#' Function to shift legends into empty facets of a ggplot2 object
#' Ownership of this specific function belongs to Z.Lin
#' (Taken from https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2)
#' As far as the authors of this package are aware, this specific function has not been published in a package.
#' Therefore, to aid data visualisation for works by SCIF, this function is compiled into sciCoverageR.
#'
#' @param p A ggplot2 object or grob generated from ggplotGrob with empty facet
#'
#' @return A grob object to be plotted using grid.draw()
#' @export
#'
#' @examples
#' library(ggplot2)
#' myplot <- ggplot(diamonds, aes(x = carat, fill = cut)) +
#' geom_density(position = "stack") +
#' facet_wrap(~ color)
#' 3 libraries required for grid.draw
#' library(grid)
#' library(cowplot)
#' library(gtable)
#' grid.draw(shift_legend(myplot))

shift_legend <- function(p){

  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }

  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }

  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")

  # extract legend & copy over to location of unfilled facet panels
  all_guidebox <- c("guide-box-right","guide-box-left","guide-box-bottom","guide-box-top","guide-box-inside")
  guide.grob <- which(gp[["layout"]][["name"]] %in% all_guidebox)[1]
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp <- gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")

  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- gtable_remove_grobs(gp, all_guidebox)

  return(gp)
}
