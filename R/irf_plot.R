#' Plot system responses
#'
#' Plots the system responses based on the provided input. This input
#' should be the output of the [irf_generator()] or the [irf_empirical()] functions.
#'
#' @param data Output of the [irf_generator()] or [irf_empirical()] function.
#' @param cols Character vector denoting the impulse responses you would like
#' to plot. The values in this vector should be columns in `data` and are
#' thus typically limited to `"irf"`, `"irf_x"`, `"irf_v"`,
#' or `"irf_intercept"`, or any combination of those. Defaults to all
#' impulse responses.
#' @param title Character denoting the title of the plot. Defaults to `NULL`,
#' that is no title.
#' @param xlabel Character denoting the label of the x-axis. Defaults to
#' `"s (time since input)"` for a single input and to `"t (time)"`
#' for multiple inputs.
#' @param ylabel Character denoting the label of the y-axis. Defaults to
#' `NULL`, that is no such label.
#' @param title.size Integer denoting the size of the title. Defaults to a
#' relative size of `1` as defined by the [ggplot2::rel()] function.
#' @param title.hjust Numeric denoting the justification of the title. Defaults
#' to \code{0.5}, or centered justification
#' @param label.size Integer denoting the size of the axis labels. Defaults to a
#' relative size of `1` as defined by the [ggplot2::rel()] function.
#' @param axis.text.size Integer denoting the size of the axis text. Defaults to
#' a relative size of `0.75` as defined by the [ggplot2::rel()] function.
#' @param legend.title.size Integer denoting the size of the legend title.
#' Defaults to a relative size of `1.25` as defined by the
#' \code{\link[ggplot2]{rel}} function.
#' @param legend.text.size Integer denoting the size of the legend text.
#' Defaults to a relative size of `1` as defined by the
#' \code{\link[ggplot2]{rel}} function
#' @param irf.color,x.color,v.color,intercept.color Character denoting the
#' colors to use to visualize the total response (`irf.`), the response to
#' impulses of the variable x (`x.`), the response to the innovations
#' (`v.`), and the intercept responses (`intercept.`) respectively.
#' Defaults are `"black"`, `"red4"`, `"green4"`, and  `"gray"` respectively.
#' @param irf.linetype,x.linetype,v.linetype,intercept.linetype Character
#' denoting the linetype to use to visualize the total response (`irf.`),
#' the response to impulses of the variable x (`x.`), the response to the
#' innovations (`v.`), and the intercept responses (`intercept.`)
#' respectively. Defaults are `"dashed"`, `"solid"`, `"solid"`,
#' and `"solid"` respectively
#' @param irf.linewidth,x.linewidth,v.linewidth,intercept.linewidth Numeric
#' denoting the linewidth to use to visualize the total response (`irf.`),
#' the response to impulses of the variable x (`x.`), the response to the
#' innovations (`v.`), and the intercept responses (`intercept.`)
#' respectively. Defaults to `1` for all variables
#' @param irf.label,x.label,v.label,intercept.label Character denoting the
#' legend label to give to the the total response (`irf.`), the response to
#' impulses of the variable x (`x.`), the response to the innovations
#' (`v.`), and the intercept responses (`intercept.`) respectively.
#' Defaults to the mathematical labels for each specific part
#' @param impulse.size Numeric denoting the size of the point that indicates an
#' impulse has been given. Defaults to `2`.
#' @param impulse.shape,impulse.x.shape,impulse.v.shape Integer or character
#' denoting the shape of the point that indicates an impulse has been given.
#' You can give impulses of variable x (`.x.`) and impulses of the
#' innovations (`.v.`) different shapes if desired. Defaults to `19`.
#' @param impulse.linetype,impulse.x.linetype,impulse.v.linetype Character
#' denoting the linetype of the line connecting the impulses to a given time on
#' the x-axis. You can give impulses of variable x (`.x.`) and impulses of the
#' innovations (`.v.`) different linetypes if desired. Defaults to
#' `"dotted"`.
#' @param impulse.linewidth Numeric denoting the linewidth of the line connecting
#' the impulses to a given time on the x-axis. Defaults to `1`.
#' @param impulse.alpha Numeric denoting the opacity of the impulses (point and
#' line). Defaults to `0.95`.
#' @param confidence_interval Logical denoting whether to plot the confidence 
#' interval when available in the \code{data}. Defaults to \code{TRUE}.
#' @param interval.alpha Numeric between `0` and `1` denoting the opacity of the
#' confidence interval. Defaults to `0.20`.
#' @param legend Logical denoting whether a legend should be printed. Defaults
#' to `TRUE`,
#' @param legend.title Character denoting the title of the legend. Ignored if
#' `legend = FALSE`. Defaults to `"Impulse response"` for a single
#' input and to `"Cumulative responses"` for multiple inputs.
#' @param legend.position Character denoting the position of the legend. Ignored
#' if `legend = FALSE`. Defaults to `"right"`.
#' @param background.fill Character denoting the color of the background of the
#' plot. Defaults to `"white"`.
#' @param breaks Integer denoting the number of breaks to allow in the time-axis.
#' Defaults to `10`.
#' @param estimated Logical denoting whether the provided \code{data} are system
#' responses estimated through data. When \code{TRUE}, it will change the labels
#' of the decomposed responses to denote their estimated nature. Defaults to 
#' \code{TRUE} when \code{data} contains information on the innovation type, 
#' an internal column only created when estimating system responses based on 
#' data. Otherwise defaults to \code{FALSE}.
#'
#' @return Plot visualizing the impulse response for the predefined model.
#'
#' @examples
#' # Create an impulse response function
#' output <- irf_generator(
#'   intercept = 1,
#'   ar_params = c(0.5, 0.25),
#'   x_params = c(2, 0.5),
#'   x = c(1, rep(0, 9))
#' )
#'
#' # Plot its results
#' irf_plot(output$irf)
#'
#' @rdname irf_plot
#'
#' @export
irf_plot <- function(
  data,
  cols = NULL,
  title = NULL,
  xlabel = NULL,
  ylabel = NULL,
  title.size = ggplot2::rel(1),
  title.hjust = 0.5,
  label.size = ggplot2::rel(1),
  axis.text.size = ggplot2::rel(0.75),
  legend.title.size = ggplot2::rel(1),
  legend.text.size = ggplot2::rel(1),
  x.color = "red4",
  x.linetype = "solid",
  x.linewidth = 1,
  x.label = NULL,
  v.color = "green4",
  v.linetype = "solid",
  v.linewidth = 1,
  v.label = NULL,
  irf.color = "black",
  irf.linetype = "dashed",
  irf.linewidth = 1,
  irf.label = "y[t]",
  intercept.color = "gray",
  intercept.linetype = "solid",
  intercept.linewidth = 1,
  intercept.label = NULL,
  impulse.size = 2,
  impulse.shape = 19,
  impulse.x.shape = impulse.shape,
  impulse.v.shape = impulse.shape,
  impulse.linetype = "dotted",
  impulse.x.linetype = impulse.linetype,
  impulse.v.linetype = impulse.linetype,
  impulse.linewidth = 1,
  impulse.alpha = 0.95,
  legend = TRUE,
  legend.title = NULL,
  legend.position = "right",
  background.fill = "white",
  confidence_interval = TRUE,
  interval.alpha = 0.20,
  breaks = 10,
  estimated = FALSE
) {
  # Determine which IRFs to plot, based on the `cols` argument. If NULL, then
  # it will use the default of all columns.
  if (is.null(cols)) {
    cols <- c("irf_intercept", "irf_x", "irf_v", "irf")
  }

  # Check whether innovation_type is part of the data.frame. If not, then we 
  # need to add it. In the meantime, this defines whether `estimated` remains
  # FALSE
  estimated <- "innovation_type" %in% colnames(data)
  if(!("innovation_type" %in% colnames(data))) {
    data$innovation_type <- "residual"
  } 

  # Convert dataframe to long format, making the call to ggplot2 somewhat
  # easier. Keep a copy of the original to ensure correct labeling of the
  # system responses
  data_long <- tidyr::pivot_longer(
    data,
    cols = tidyr::all_of(cols)
  ) |>
    dplyr::select(time, x, innovations, innovation_type, name, value)

  # If confidence intervals should be plotted as well (and are available), use 
  # a similar trick on the confidence intervals and merge them with the pivotted
  # dataset
  if(confidence_interval & any(grepl("lower", colnames(data), fixed = TRUE))) {
    lower <- tidyr::pivot_longer(
      data, 
      cols = tidyr::all_of(paste(cols, "_lower", sep = ""))
    ) |>
      dplyr::mutate(name = gsub("_lower", "", name, fixed = TRUE)) |>
      dplyr::rename(lower = value) |>
      dplyr::select(time, x, innovations, name, lower)

    upper <- tidyr::pivot_longer(
      data, 
      cols = tidyr::all_of(paste(cols, "_upper", sep = ""))
    ) |>
      dplyr::mutate(name = gsub("_upper", "", name, fixed = TRUE)) |>
      dplyr::rename(upper = value) |>
      dplyr::select(time, x, innovations, name, upper)

    data_long <- data_long |>
      dplyr::full_join(
        lower, 
        by = c("time", "x", "innovations", "innovation_type", "name")
      ) |>
      dplyr::full_join(
        upper, 
        by = c("time", "x", "innovations", "innovation_type", "name")
      )
  } else {
    # If no confidence interval is specified or recoverable, add lower and upper
    # columns to the pivotted data anyway. Removes the need for an additional 
    # if-statement along the way
    data_long$lower <- data_long$value
    data_long$upper <- data_long$value
  }

  # Filter out all of the IRFs that you don't want to plot.
  #
  # TO DO: Sigert has an additional filter here looking for whether X or Eps
  # was filled with values. If not, he filtered them out as well. All these
  # filters commented out for now.
  data_long <- data_long[data_long$name %in% cols, ]

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
    "irf_intercept" = intercept.color,
    "irf_x" = x.color,
    "irf_v" = v.color,
    "irf" = irf.color
  )

  linetypes <- c(
    "irf_intercept" = intercept.linetype,
    "irf_x" = x.linetype,
    "irf_v" = v.linetype,
    "irf" = irf.linetype
  )

  linewidths <- c(
    "irf_intercept" = intercept.linewidth,
    "irf_x" = x.linewidth,
    "irf_v" = v.linewidth,
    "irf" = irf.linewidth
  )

  # Some other plotting characteristics to take care of
  if (is.null(legend.title)) {
    legend.title <- "System response"
  }

  if (is.null(xlabel)) {
    xlabel <- ifelse(
      all(data$x[-1] == 0) & all(data$innovations[-1] == 0),
      "s (time step since input)",
      "t (time)"
    )
  }

  # Legend labels
  if (is.null(intercept.label)) {
    intercept.label <- ifelse(
      estimated,
      "(hat(h)[1] ~ symbol('*') ~ 1)[t]",
      "(h[1] ~ symbol('*') ~ 1)[t]"
    )
  }
  
  if (is.null(x.label)) {
    x.label <- ifelse(
      all(data$x[-1] == 0) & all(data$innovations[-1] == 0),
      ifelse(
        data$x[1] == 1,
        "h[x](s)",
        "h[x](s)*x[0]"
      ),
      ifelse(
        estimated, 
        "(hat(h)[x] ~ symbol('*') ~ x)[t]",
        "(h[x] ~ symbol('*') ~ x)[t]"
      )
    )
  }

  if (is.null(v.label)) {
    v.label <- ifelse(
      all(data$x[-1] == 0) & all(data$innovations[-1] == 0),
      ifelse(
        data$innovations[1] == 1,
        "h[v](s)",
        "h[v](s)*v[0]"
      ),
      ifelse(
        estimated, 
        "(hat(h)[v] ~ symbol('*') ~ hat(v))[t]",
        "(h[v] ~ symbol('*') ~ v)[t]"
      )
    )
  }

  labels <- c(
    "irf_intercept" = parse(text = intercept.label),
    "irf_x" = parse(text = x.label),
    "irf_v" = parse(text = v.label),
    "irf" = parse(text = irf.label)
  )

  impulses <- data_long[, c("time", "x", "innovations", "innovation_type")]

  # Create the actual plot.
  data_long$name <- factor(
    data_long$name,
    levels = c(
      "irf_intercept",
      "irf_x",
      "irf_v",
      "irf"
    )
  )
  x_impulse <- impulses[impulses$x != 0, ]
  v_impulse <- impulses[impulses$innovations != 0, ]

  plt <- ggplot2::ggplot(
    data = data_long,
    ggplot2::aes(
      x = .data$time,
      y = .data$value,
      ymin = .data$lower,
      ymax = .data$upper,
      color = .data$name,
      fill = .data$name,
      linetype = .data$name,
      linewidth = .data$name
    )
  ) +
    # Content
    ggplot2::geom_ribbon(
      alpha = interval.alpha,
      color = NA
    ) +
    ggplot2::geom_line() +
    ggplot2::annotate(
      "segment",
      x = x_impulse$time,
      xend = x_impulse$time,
      y = 0,
      yend = x_impulse$x,
      color = x.color,
      linewidth = impulse.linewidth,
      linetype = impulse.x.linetype,
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
      x = v_impulse$time,
      xend = v_impulse$time,
      y = 0,
      yend = v_impulse$innovations,
      color = v.color,
      linewidth = impulse.linewidth,
      linetype = impulse.v.linetype,
      alpha = impulse.alpha
    ) +

    ggplot2::annotate(
      "point",
      x = v_impulse$time,
      y = v_impulse$innovations,
      fill = v.color,
      color = v.color,
      size = impulse.size,
      shape = ifelse(
        v_impulse$innovation_type == "residual",
        impulse.v.shape,
        ifelse(
          impulse.v.shape == 13,
          4,
          13
        )
      ),
      alpha = impulse.alpha
    ) +

    # Aesthetics of the content otherwise not defined in the geoms
    ggplot2::scale_color_manual(
      values = colors,
      labels = labels
    ) +
    ggplot2::scale_fill_manual(
      values = colors,
      labels = labels
    ) +
    ggplot2::scale_linetype_manual(
      values = linetypes
    ) +
    ggplot2::scale_discrete_manual(
      aesthetics = "linewidth",
      values = linewidths
    ) +

    # Theme and other stuff
    ggplot2::scale_x_continuous(
      breaks = round(
        seq(
          floor(min(data$time)),
          ceiling(max(data$time)),
          length.out = breaks
        )
      )
    ) +
    ggplot2::labs(
      title = ifelse(is.null(title), "", title),
      x = xlabel,
      y = ifelse(is.null(ylabel), "", ylabel)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = background.fill),
      panel.grid.minor.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        size = title.size,
        hjust = title.hjust
      ),
      axis.text = ggplot2::element_text(size = axis.text.size),
      axis.title = ggplot2::element_text(size = label.size),
      legend.background = ggplot2::element_rect(
        fill = background.fill,
        color = NA
      ),
      legend.key = ggplot2::element_rect(fill = background.fill, color = NA),
      legend.position = ifelse(legend, legend.position, "none"),
      legend.title = ggplot2::element_text(size = legend.title.size),
      legend.text = ggplot2::element_text(size = legend.text.size)
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(title = legend.title),
      linetype = "none",
      linewidth = "none",
      fill = "none"
    )
  return(plt)
}
