#' Number line plot of dredge IC model results
#'
#'
#' This function provides a ggplot2 of dredge model information-criteria results,
#' from the dredge package from the MuMIn R package, by Mark Greenwood
#'
#'
#' @param data Dredge model selection table output
#' @param deltasubset default is NA to show all results, cuts results to numeric delta IC value
#' @param colors default cet_pal(6, "l3"), colors for four binned delta IC values
#' @param name label for the x-axis number line in the plot
#' @param max.overlaps Number of overlapping labels for models, defaults to 200
#'
#' @details Function for visual display of IC dredge model selection results.
#' @examples
#' library(dplyr)
#' library(ggrepel)
#' library(cetcolor)
#' data(powerplantoutput)
#' set.seed(406)
#' training_data <- slice_sample(powerplantoutput, prop = 0.7)
#' m1 <- lm(PE ~ (AT + V + AP + RH)^2, data = training_data)
#' library(MuMIn)
#' options(na.action = "na.fail")
#' dd <- dredge(m1, rank = "BIC")
#' g1 <- ggdredgeplot(data = dd) + labs(title = "BIC results for all models")
#' g2 <- ggdredgeplot(data = dd, deltasubset = 8)
#' library(patchwork)
#' g1 / g2 + plot_annotation((tag_levels = "a"))
#' @export
ggdredgeplot <- function(data,
                         deltasubset = NA,
                         colors = cet_pal(6, "l3"),
                         name = "Delta IC",
                         max.overlaps = 200){

  if (is.null(attributes(data)$call)) {
    eval("Please provide results directly from dredge for plotting dredge model selection results.")
  } else{

    toplot <- data
    if (!is.na(deltasubset)){
      toplot <- subset(toplot, delta < deltasubset)
    }
    toplot <- as.data.frame(toplot)
    breaksS <- c(0,2,4,6,Inf)

    gplot <- toplot %>% mutate(ModelNumber = rownames(toplot),
                               DeltaBin =
                                 factor(cut(delta,
                                             breaks = breaksS,
                                            include.lowest = T))
                               ) %>%
      ggplot(aes(x = delta, y = 0, label = ModelNumber)) +
      geom_point(aes(color = DeltaBin), shape = 3, alpha = 0.8) +
      geom_label_repel(aes(color = DeltaBin), alpha = 0.6,
                       direction = "y",
                       max.overlaps = max.overlaps,
                       show.legend = FALSE) +
      labs(y = "", x = name,
           title = "Plot of Delta IC values") +
      scale_color_manual(values = colors, name = name) +
      theme_bw() +
      theme(axis.text.y = element_blank())

    return(gplot)
  }

}

