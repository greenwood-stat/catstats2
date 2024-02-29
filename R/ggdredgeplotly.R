#' Interactive number line plot of dredge IC model results
#'
#'
#' This function provides a plotly display of dredge model information-criteria results,
#' from the dredge package from the MuMIn R package, by Mark Greenwood
#'
#'
#' @param data Dredge model selection table output
#' @param deltasubset default is max delta + 1 to show all results, cuts results to numeric delta IC value
#' @param colors default cet_pal(6, "l3"), colors for four binned delta IC values
#' @param name label for the x-axis number line in the plot
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
#' ggdredgeplotly(dd)
#'
#' @export
ggdredgeplotly = function(data,
                          deltasubset = max(data$delta) + 1,
                          colors = cet_pal(6, "l3"),
                          name = "Delta IC"){

  if (is.null(attributes(data)$call)) {
    eval("Please provide results directly from dredge for plotting dredge model selection results.")
  } else{

    toplot <- data
    toplot <- subset(toplot, delta < deltasubset)
    toplot <- as.data.frame(toplot)
    breaksS <- c(0,2,4,6,Inf)

    toplot <- toplot %>% mutate(ModelNumber = rownames(toplot),
                                DeltaBin =
                                  factor(cut(delta,
                                             breaks = breaksS,
                                             include.lowest = T))
    ) %>%
      mutate_if(is.numeric, round, digits = 3)


    ForLabelInfo <- toplot
    names_data <- names(ForLabelInfo)

    #Add name to rows
    for(i in 1:ncol(ForLabelInfo)){
      ForLabelInfo[,i] <- paste(names_data[i],": " ,
                                ForLabelInfo[,i], sep="")
    }

    #Formatting to use "label" information in hover in plotly
    LabelInfo <- ForLabelInfo[,1]
    for(i in 2:ncol(ForLabelInfo)){
      LabelInfo <- paste(LabelInfo, "\n", ForLabelInfo[,{i}])
    }
    LabelInfo <- paste(LabelInfo, "\n")
    LabelInfo <- paste("\n", LabelInfo)

    gplot2 <- ggplot(toplot ,
                    aes_string(x = "delta",
                               y = 0, color = "delta",
                               label = "LabelInfo")) +
      geom_point(aes(color = DeltaBin),
                 shape = 3, alpha = 0.8) +
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank()) +
      labs(y = "", x = name,
           title = "Interactive plot of delta IC values") +
      scale_color_manual(values = colors, name = name) +
      theme_bw()


    gplotly <- ggplotly(gplot2, tooltip = c("x", "LabelInfo"))
    return(gplotly)
    }

}
