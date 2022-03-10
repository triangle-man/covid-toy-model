## Modified from geom-path.r in the ggplot2 distribution

## TODO
## - [ ] Fix code
## Likely bugs:
##   - won't cope with line segments that don't go up and to the right?
##   - won't cope with non-rectangular coordinate systems
## - [ ] Fix documentation
## - [ ] Fix tests

#' Connect observations
#'
#' `geom_gappath()` connects the observations in the order in which they appear
#' in the data. `geom_gapline()` connects them in order of the variable on the x
#' axis. The `group` aesthetic determines which cases are connected together.
#'
#' An alternative parameterisation is [geom_gapsegment()], where each line
#' corresponds to a single case which provides the start and end coordinates.
#'
#' @eval rd_orientation()
#'
#' @eval rd_aesthetics("geom", "path")
#' @inheritParams layer
#' @inheritParams geom_bar
#' @param lineend Line end style (round, butt, square).
#' @param linejoin Line join style (round, mitre, bevel).
#' @param linemitre Line mitre limit (number greater than 1).
#' @param arrow Arrow specification, as created by [grid::arrow()].
#' @seealso
#'  [geom_polygon()]: Filled paths (polygons);
#'  [geom_segment()]: Line segments
#' @section Missing value handling:
#' `geom_path()`, `geom_line()`, and `geom_step` handle `NA` as follows:
#'
#' * If an `NA` occurs in the middle of a line, it breaks the line. No warning
#'   is shown, regardless of whether `na.rm` is `TRUE` or `FALSE`.
#' * If an `NA` occurs at the start or the end of the line and `na.rm` is `FALSE`
#'   (default), the `NA` is removed with a warning.
#' * If an `NA` occurs at the start or the end of the line and `na.rm` is `TRUE`,
#'   the `NA` is removed silently, without warning.
#' @export
#' @examples
#' # geom_line() is suitable for time series
#' ggplot(economics, aes(date, unemploy)) + geom_line()
#' ggplot(economics_long, aes(date, value01, colour = variable)) +
#'   geom_line()
#'
#' # You can get a timeseries that run vertically by setting the orientation
#' ggplot(economics, aes(unemploy, date)) + geom_line(orientation = "y")
#'
#' # geom_step() is useful when you want to highlight exactly when
#' # the y value changes
#' recent <- economics[economics$date > as.Date("2013-01-01"), ]
#' ggplot(recent, aes(date, unemploy)) + geom_line()
#' ggplot(recent, aes(date, unemploy)) + geom_step()
#'
#' # geom_path lets you explore how two variables are related over time,
#' # e.g. unemployment and personal savings rate
#' m <- ggplot(economics, aes(unemploy/pop, psavert))
#' m + geom_path()
#' m + geom_path(aes(colour = as.numeric(date)))
#'
#' # Changing parameters ----------------------------------------------
#' ggplot(economics, aes(date, unemploy)) +
#'   geom_line(colour = "red")
#'
#' # Use the arrow parameter to add an arrow to the line
#' # See ?arrow for more details
#' c <- ggplot(economics, aes(x = date, y = pop))
#' c + geom_line(arrow = arrow())
#' c + geom_line(
#'   arrow = arrow(angle = 15, ends = "both", type = "closed")
#' )
#'
#' # Control line join parameters
#' df <- data.frame(x = 1:3, y = c(4, 1, 9))
#' base <- ggplot(df, aes(x, y))
#' base + geom_path(size = 10)
#' base + geom_path(size = 10, lineend = "round")
#' base + geom_path(size = 10, linejoin = "mitre", lineend = "butt")
#'
#' # You can use NAs to break the line.
#' df <- data.frame(x = 1:5, y = c(1, 2, NA, 4, 5))
#' ggplot(df, aes(x, y)) + geom_point() + geom_line()
#'
#' \donttest{
#' # Setting line type vs colour/size
#' # Line type needs to be applied to a line as a whole, so it can
#' # not be used with colour or size that vary across a line
#' x <- seq(0.01, .99, length.out = 100)
#' df <- data.frame(
#'   x = rep(x, 2),
#'   y = c(qlogis(x), 2 * qlogis(x)),
#'   group = rep(c("a","b"),
#'   each = 100)
#' )
#' p <- ggplot(df, aes(x=x, y=y, group=group))
#' # These work
#' p + geom_line(linetype = 2)
#' p + geom_line(aes(colour = group), linetype = 2)
#' p + geom_line(aes(colour = x))
#' # But this doesn't
#' should_stop(p + geom_line(aes(colour = x), linetype=2))
#' }
geom_gappath <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,
                         lineend = "butt",
                         gap = 0.015,
                         arrow = NULL,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomGappath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      lineend = lineend,
      gap = gap,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomGappath <- ggproto("GeomGappath", Geom,
  required_aes = c("x", "y"),

  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),

  handle_na = function(data, params) {
    # Drop missing values at the start or end of a line - can't drop in the
    # middle since you expect those to be shown by a break in the line
    complete <- stats::complete.cases(data[c("x", "y", "size", "colour", "linetype")])
    kept <- stats::ave(complete, data$group, FUN = keep_mid_true)
    data <- data[kept, ]

    if (!all(kept) && !params$na.rm) {
      warn(glue("Removed {sum(!kept)} row(s) containing missing values (geom_gappath)."))
    }

    data
  },

  draw_panel = function(data, panel_params, coord, arrow = NULL,
                        lineend = "butt",
                        gap = gap,
                        na.rm = FALSE) {
    if (!anyDuplicated(data$group)) {
      message_wrap("geom_gappath: Each group consists of only one observation. ",
        "Do you need to adjust the group aesthetic?")
    }

    # must be sorted on group
    data <- data[order(data$group), , drop = FALSE]
    munched <- coord_munch(coord, data, panel_params)

    # Silently drop lines with less than two points, preserving order
    rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
    munched <- munched[rows >= 2, ]
    if (nrow(munched) < 2) return(zeroGrob())

    ## Work out whether aesthetics are changing over the path. If the lines are
    ## dotted or dashed, then colour, size, and linetype cannot change over the
    ## path. (The original comment here said "Work out whether we should use
    ## lines or segments," but gappath always uses segments and in fact allows
    ## varying aesthetics with dotted or dashed lines.)

    ## attr <- dapply(munched, "group", function(df) {
    ##   linetype <- unique(df$linetype)
    ##   new_data_frame(list(
    ##     solid = identical(linetype, 1) || identical(linetype, "solid"),
    ##     constant = nrow(unique(df[, c("alpha", "colour","size", "linetype")])) == 1
    ##   ), n = 1)
    ## })
    ## solid_lines <- all(attr$solid)
    ## constant <- all(attr$constant)
    ## if (!solid_lines && !constant) {
    ##   abort("geom_path: If you are using dotted or dashed lines, colour, size and linetype must be constant over the line")
    ## }

    # Work out grouping variables for grobs
    n <- nrow(munched)
    group_diff <- munched$group[-1] != munched$group[-n]
    start <- c(TRUE, group_diff)
    end <-   c(group_diff, TRUE)

    ## remove `gap` from the ends of each line segment
    shrunk <- shrink(munched$x[!end], munched$y[!end], munched$x[!start], munched$y[!start], gap)
    
    ## if (!constant) { 
      segmentsGrob(
        shrunk$x1, shrunk$y1, shrunk$x2, shrunk$y2,
        default.units = "native", arrow = arrow,
        gp = gpar(
          col = alpha(munched$colour, munched$alpha)[!end],
          fill = alpha(munched$colour, munched$alpha)[!end],
          lwd = munched$size[!end] * .pt,
          lty = munched$linetype[!end],
          lineend = lineend
        )
      )
    ## } else {
    ##   id <- match(munched$group, unique(munched$group))
    ##   polylineGrob(
    ##     munched$x, munched$y, id = id,
    ##     default.units = "native", arrow = arrow,
    ##     gp = gpar(
    ##       col = alpha(munched$colour, munched$alpha)[start],
    ##       fill = alpha(munched$colour, munched$alpha)[start],
    ##       lwd = munched$size[start] * .pt,
    ##       lty = munched$linetype[start],
    ##       lineend = lineend,
    ##       linejoin = linejoin,
    ##       linemitre = linemitre
    ##     )
    ##   )
    ## }
  },

  draw_key = draw_key_path
)

## Remove `gap` from the end of each line segment
shrink <- function(x1, y1, x2, y2, gap) {
    dx <- x2 - x1
    dy <- y2 - y1
    dd <- sqrt(dx^2 + dy^2)
    gdd <- pmin(gap / dd, 0.49) # Leave a small dot if the gap is > half the line
    gdx <- gdd * dx
    gdy <- gdd * dy
    
    data.frame(x1 = x1 + gdx,
               y1 = y1 + gdy,
               x2 = x2 - gdx,
               y2 = y2 - gdy)
}


# Trim false values from left and right: keep all values from
# first TRUE to last TRUE
keep_mid_true <- function(x) {
  first <- match(TRUE, x) - 1
  if (is.na(first)) {
    return(rep(FALSE, length(x)))
  }

  last <- length(x) - match(TRUE, rev(x)) + 1
  c(
    rep(FALSE, first),
    rep(TRUE, last - first),
    rep(FALSE, length(x) - last)
  )
}


#' @export
#' @rdname geom_path
geom_gapline <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", na.rm = FALSE, orientation = NA,
                      show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-path.r
GeomGapline <- ggproto("GeomLine", GeomPath,
  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
    params
  },

  extra_params = c("na.rm", "orientation"),

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data <- data[order(data$PANEL, data$group, data$x), ]
    flip_data(data, params$flipped_aes)
  }
)

