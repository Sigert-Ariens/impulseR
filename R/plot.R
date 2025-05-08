
#' Plot impulse responses
#' 
#' Plots the impulse response functions based on the provided input.
#' 
#' @details
#' Two versions of this method exist. The first version takes in the output 
#' of the \code{\link[impulseR]{irf}} function and builds a plot based on this output.
#' The second version takes in the same arguments as \code{\link[impulseR]{irf}} 
#' and builds a plot based on its results. 
#' 
#' The first method is more useful for visualization of a completed analysis, 
#' while the second method is more useful when exploring the effect of different 
#' parameter sets/models.
#' 
#' @return Plot visualizing the impulse response for the predefined model.
#' 
#' @examples 
#' # Using predefined parameters
#' irf_plot(
#'   1, 
#'   c(0.5, 0.25), 
#'   c(2, 0.5),
#'   x = c(1, rep(0, 9))
#' )
#' 
#' # Using the output of irf
#' output <- irf(
#'   1, 
#'   c(0.5, 0.25), 
#'   c(2, 0.5),
#'   x = c(1, rep(0, 9))
#' )
#' 
#' irf_plot(output)
#' 
#' @rdname irf_plot
#' 
#' @export
setGeneric("irf_plot", function(object, ...) standardGeneric("irf_plot"))

#' @param intercept Numeric denoting the intercept to use for the impulse response 
#' function. Defaults to \code{0}
#' @param ar_params Numeric vector denoting the autoregressive parameters to be
#' used for the impulse response function. Parameters need to be given in order
#' of increased lag (i.e., first element for t - 1, second element for t - 2,...).
#' Defaults to \code{0}
#' @param x_params Numeric vector denoting the values of the slopes for the 
#' exogenous variables. Parameters again need to be given in order of increased
#' lag (i.e., first element for t, second element for t - 1,...). Defaults to 
#' \code{0}
#' @param x Numeric vector denoting the values of the exogenous variables 
#' X at each time t. These values serve as one type of impulses to the system to 
#' be simulated. Defaults to \code{NULL}, which means an empty vector of the same
#' length as \code{residuals} (if provided).
#' @param residuals Numeric vector denoting the values of the residuals at each 
#' time t. These values serve as one type of impulses to the system to be 
#' simulated. Defaults to \code{NULL}, which means an empty vector of the same
#' length as \code{x} (if provided).
#' @param burnin Logical denoting whether to use a burnin for the simulation. 
#' Recommended to be \code{TRUE} when you expect the initial conditions to lie 
#' far away from the mean of the process. Note that we recommend to set this 
#' argument to \code{FALSE} when you provide a vector of regression residuals
#' to the argument \code{residuals}. Defaults to \code{TRUE}.
#' @param ... Arguments passed on to the \code{\link[impulseR]{irf_plot}} method for 
#' \code{data.frame}
#' 
#' @rdname irf_plot 
#' 
#' @export
setMethod("irf_plot", signature(), function(intercept = 0, 
                                            ar_params = 0, 
                                            x_params = 0,
                                            x = NULL,
                                            residuals = NULL,
                                            burnin = TRUE,
                                            ...) {
    
    # Execute the `irf` function to retrieve the needed impulse responses
    data <- irf(
      intercept = intercept, 
      ar_params = ar_params, 
      x_params = x_params, 
      x = x, 
      residuals = residuals, 
      burnin = burnin
    )

    # Execute the other `irf_plot` method to create and return the plot
    return(
      irf_plot(
        data$irf, 
        ...
      )
    )
  }
)

#' @param object Output of the \code{\link[impulseR]{irf}} function
#' @param cols Character vector denoting the impulse responses you would like 
#' to plot. The values in this vector should be columns in \code{data} and are 
#' thus typically limited to \code{"irf"}, \code{"irf_x"}, \code{"irf_eps"},
#' or \code{"irf_intercept"}, or any combination of those. Defaults to all 
#' impulse responses
#' @param title Character denoting the title of the plot. Defaults to \code{NULL},
#' that is no title
#' @param xlabel Character denoting the label of the x-axis. Defaults to
#' \code{"s (time since input)"} for a single input and to \code{"t (time)"}
#' for multiple inputs
#' @param ylabel Character denoting the label of the y-axis. Defaults to 
#' \code{NULL}, that is no such label
#' @param title.size Integer denoting the size of the title. Defaults to a 
#' relative size of \code{1.5} as defined by the \code{\link[ggplot2]{rel}} function
#' @param title.hjust Numeric denoting the justification of the title. Defaults 
#' to \code{0.5}, or centered justification
#' @param label.size Integer denoting the size of the axis labels. Defaults to a 
#' relative size of \code{1} as defined by the \code{\link[ggplot2]{rel}} function
#' @param axis.text.size Integer denoting the size of the axis text. Defaults to 
#' a relative size of \code{0.75} as defined by the \code{\link[ggplot2]{rel}} 
#' function
#' @param legend.title.size Integer denoting the size of the legend title. 
#' Defaults to a relative size of \code{1.25} as defined by the 
#' \code{\link[ggplot2]{rel}} function
#' @param legend.text.size Integer denoting the size of the legend text. 
#' Defaults to a relative size of \code{1} as defined by the 
#' \code{\link[ggplot2]{rel}} function
#' @param irf.color,x.color,eps.color,intercept.color Character denoting the 
#' colors to use to visualize the total response (\code{irf.}), the response to 
#' impulses of the variable x (\code{x.}), the response to the residuals 
#' (\code{eps.}), and the intercept responses (\code{intercept.}) respectively. 
#' Defaults are \code{"black"}, \code{"red4"}, \code{"green4"}, and \code{"gray"} 
#' respectively
#' @param irf.linetype,x.linetype,eps.linetype,intercept.linetype Character 
#' denoting the linetype to use to visualize the total response (\code{irf.}), 
#' the response to impulses of the variable x (\code{x.}), the response to the 
#' residuals (\code{eps.}), and the intercept responses (\code{intercept.}) 
#' respectively. Defaults are \code{"dashed"}, \code{"solid"}, \code{"solid"}, 
#' and \code{"solid"} respectively
#' @param irf.linewidth,x.linewidth,eps.linewidth,intercept.linewidth Numeric 
#' denoting the linewidth to use to visualize the total response (\code{irf.}), 
#' the response to impulses of the variable x (\code{x.}), the response to the 
#' residuals (\code{eps.}), and the intercept responses (\code{intercept.}) 
#' respectively. Defaults to \code{1} for all variables
#' @param irf.label,x.label,eps.label,intercept.label Character denoting the 
#' legend label to give to the the total response (\code{irf.}), the response to 
#' impulses of the variable x (\code{x.}), the response to the residuals 
#' (\code{eps.}), and the intercept responses (\code{intercept.}) respectively.
#' Defaults to the mathematical labels for each specific part
#' @param impulse.size Numeric denoting the size of the point that indicates an 
#' impulse has been given. Defaults to \code{2}
#' @param impulse.shape,impulse.x.shape,impulse.eps.shape Integer or character 
#' denoting the shape of the point that indicates an impulse has been given. 
#' You can give impulses of variable x (\code{.x.}) and impulses of the 
#' residuals (\code{.eps.}) different shapes if desired. Defaults to \code{19}.
#' @param impulse.linetype Character denoting the linetype of the line connecting 
#' the impulses to a given time on the x-axis. Defaults to \code{"dotted"}
#' @param impulse.linewidth Numeric denoting the linewidth of the line connecting
#' the impulses to a given time on the x-axis. Defaults to \code{1}
#' @param impulse.alpha Numeric denoting the opacity of the impulses (point and 
#' line). Defaults to \code{0.95}
#' @param legend Logical denoting whether a legend should be printed. Defaults 
#' to \code{TRUE},
#' @param legend.title Character denoting the title of the legend. Ignored if 
#' \code{legend = FALSE}. Defaults to \code{"Impulse response"} for a single 
#' input and to \code{"Cumulative responses"} for multiple inputs
#' @param legend.position Character denoting the position of the legend. Ignored
#' if \code{legend = FALSE}. Defaults to \code{"right"}
#' 
#' @rdname irf_plot
#' 
#' @export
setMethod("irf_plot", signature(object = "data.frame"), function(object, 
                                                                 cols = NULL,
                                                                 title = NULL,
                                                                 xlabel = NULL,
                                                                 ylabel = NULL,
                                                                 title.size = ggplot2::rel(1.5),
                                                                 title.hjust = 0.5,
                                                                 label.size = ggplot2::rel(1),
                                                                 axis.text.size = ggplot2::rel(0.75),
                                                                 legend.title.size = ggplot2::rel(1.25),
                                                                 legend.text.size = ggplot2::rel(1),
                                                                 x.color = "red4",
                                                                 x.linetype = "solid",
                                                                 x.linewidth = 1,
                                                                 x.label = latex2exp::TeX("$h_x * x$"),
                                                                 eps.color = "green4",
                                                                 eps.linetype = "solid",
                                                                 eps.linewidth = 1,
                                                                 eps.label = latex2exp::TeX("$h_v * v$"),
                                                                 irf.color = "black",
                                                                 irf.linetype = "dashed",
                                                                 irf.linewidth = 1,
                                                                 irf.label = latex2exp::TeX("$y_t$"),
                                                                 intercept.color = "gray",
                                                                 intercept.linetype = "solid",
                                                                 intercept.linewidth = 1,
                                                                 intercept.label = latex2exp::TeX("$h_1 * 1$"),
                                                                 impulse.size = 2,
                                                                 impulse.shape = 19,
                                                                 impulse.x.shape = impulse.shape,
                                                                 impulse.eps.shape = impulse.shape,
                                                                 impulse.linetype = "dotted",
                                                                 impulse.linewidth = 1,
                                                                 impulse.alpha = 0.95,
                                                                 legend = TRUE,
                                                                 legend.title = NULL,
                                                                 legend.position = "right") {
    
    # Convert dataframe to long format, making the call to ggplot2 somewhat 
    # easier.
    data <- tidyr::pivot_longer(
      object,
      cols = c("irf", "irf_x", "irf_eps", "irf_intercept")
    )

    # Determine which IRFs to plot, based on the `cols` argument. If NULL, then 
    # it will use the default of all columns.
    if(is.null(cols)) {
      cols <- unique(data$name) 
    }

    # Filter out all of the IRFs that you don't want to plot.
    #
    # TO DO: Sigert has an additional filter here looking for whether X or Eps 
    # was filled with values. If not, he filtered them out as well. All these 
    # filters commented out for now.
    data <- data[data$name %in% cols, ]
    
    # if(any(data$IRFintercept != 0)){
    #   datalong <- datalong
    # } else{
    #   datalong <- datalong |> filter(name != "IRFintercept")
    # }
    
    # if(type == "Impulse response"){
    #   datalong <- datalong |> filter(name != "IRFintercept")
    # }
    
    
    # if(any(data$X != 0) & any(data$Eps != 0)){
    #   datalong <- datalong
    # } else{
    #   datalong <- datalong |> filter(name != "IRFtotal")
    # }
    
    # if(any(data$X != 0)){
    #   datalong <- datalong
    # } else{
    #   datalong <- datalong |> filter(name != "IRF_x")
    # }
    
    # if(any(data$Eps != 0)){
    #   datalong <- datalong
    # } else{
    #   datalong <- datalong |> filter(name != "IRFe")
    # }
    
    # Create some of the plotting variables that will influence what the plot 
    # will look like for each of the separate IRFs. These variables are made 
    # based on the values provided by the user.
    colors <- c(
      "irf" = irf.color,
      "irf_x" = x.color,
      "irf_eps" = eps.color,
      "irf_intercept" = intercept.color
    )

    linetypes <- c(
      "irf" = irf.linetype,
      "irf_x" = x.linetype,
      "irf_eps" = eps.linetype,
      "irf_intercept" = intercept.linetype
    )

    linewidths <- c(
      "irf" = irf.linewidth,
      "irf_x" = x.linewidth,
      "irf_eps" = eps.linewidth,
      "irf_intercept" = intercept.linewidth
    )

    labels <- c(
      "irf" = irf.label,
      "irf_x" = x.label,
      "irf_eps" = eps.label,
      "irf_intercept" = intercept.label
    )

    # Some other plotting characteristics to take care of
    if(is.null(legend.title)) {
      # Differentiate between cumulative responses and single impulses
      legend.title <- ifelse(
        (sum(object$x != 0) == 1) | (sum(object$residuals != 0) == 1),
        "Impulse response",
        "Cumulative response"
      )
    }

    if(is.null(xlabel)) {
      # Differentiate between cumulative responses and single impulses
      xlabel <- ifelse(
        (sum(object$x != 0) == 1) | (sum(object$residuals != 0) == 1),
        "s (time since input)",
        "t (time)"
      )
    }

    impulses <- object[, c("time", "x", "residuals")]
    


    # Create the actual plot.
    data$name <- factor(data$name)
    x_impulse <- impulses[impulses$x != 0, ]
    eps_impulse <- impulses[impulses$residuals != 0, ]
    axis.breaks <- 1:max(data$time)

    plt <- ggplot2::ggplot(
      data = data, 
      ggplot2::aes(
        x = time, 
        y = value, 
        color = name, 
        linetype = name,
        linewidth = name
      )
    ) +
      # Content
      ggplot2::geom_line() +  

      ggplot2::annotate(
        "segment",
        x = x_impulse$time, 
        xend = x_impulse$time, 
        y = 0,
        yend = x_impulse$x,
        color = x.color,
        linewidth = impulse.linewidth,
        linetype = impulse.linetype,
        alpha = impulse.alpha
      ) +
      ggplot2::annotate(
        "point",
        x = x_impulse$time, 
        y = x_impulse$x, 
        fill = x.color,
        color = x.color,
        size = impulse.size,
        shape = impulse.x.shape,
        alpha = impulse.alpha
      ) +

      ggplot2::annotate(
        "segment",
        x = eps_impulse$time, 
        xend = eps_impulse$time, 
        y = 0,
        yend = eps_impulse$residuals,
        color = eps.color,
        linewidth = impulse.linewidth,
        linetype = impulse.linetype,
        alpha = impulse.alpha
      ) +
      ggplot2::annotate(
        "point",
        x = eps_impulse$time, 
        y = eps_impulse$residuals, 
        fill = eps.color,
        color = eps.color,
        size = impulse.size,
        shape = impulse.eps.shape,
        alpha = impulse.alpha
      ) + 
      
      # Aesthetics of the content otherwise not defined in the geoms
      ggplot2::scale_color_manual(
        values = colors, 
        labels = labels
      ) + 
      ggplot2::scale_linetype_manual(
        values = linetypes
      ) +
      ggplot2::scale_discrete_manual(
        aesthetic = "linewidth",
        values = linewidths
      ) +
      
      # Theme and other stuff
      # TO DO: Ask Sigert about the scale_x_continuous: Why done this way?
      # ggplot2::scale_x_continuous(
      #   breaks = axis.breaks, 
      #   labels = c('0','1','2','3', rep("", max(datalong$time) -3))
      # ) + 
      ggplot2::labs(
        title = ifelse(is.null(title), "", title),
        x = xlabel, 
        y = ifelse(is.null(ylabel), "", ylabel)
      ) +
      ggplot2::theme_minimal() + 
      ggplot2::theme(
        panel.grid.minor.x = ggplot2::element_blank(), 
        plot.title = ggplot2::element_text(
          size = title.size,
          hjust = title.hjust
        ),
        axis.text = ggplot2::element_text(size = axis.text.size), 
        axis.title = ggplot2::element_text(size = label.size),
        legend.position = ifelse(legend, legend.position, "none"),
        legend.title = ggplot2::element_text(size = legend.title.size),
        legend.text = ggplot2::element_text(size = legend.text.size)
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(title = legend.title), 
        linetype = "none",
        linewidth = "none"
      )
    
    return(plt)
  }
)
  
