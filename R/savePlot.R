#' MegaR savePlot
#'
#' This is a internal function. This function allows the program to
#' save the plot generated in the program.

#' @param file name of the file where data is to be stored
#' @param plotIn the plot which is stored
#' import ggsave from ggplot2**
savePlot <- function(file, plotIn) {
    ggplot2::ggsave(
        device = "pdf",
        plot = plotIn,
        width = 5,
        height = 5,
        units = "in",
        dpi = 200
    )
}
