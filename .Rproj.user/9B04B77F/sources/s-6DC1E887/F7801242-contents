#' Trend
#'
#' Create a trend line.
#' 
#' @param data A vector of numerical values.
#' @param draw Whether to use a drawing animation.
#' @param draw_duration Duration of draw animation in milliseconds.
#' @param draw_easing Easing to use for draw animation.
#' @param stroke Color of trend line.
#' @param stroke_width Width of stroke.
#' @param stroke_opacity Opacity of stroke.
#' @param smooth Whether to smooth trend line.
#' @param line_cap Shape of line edges.
#' @param radius Controls curve when \code{smooth} is set top \code{TRUE}.
#' @param dash A vector of dash length.
#' @param dash_offset Controls where dash starts.
#' @param gradient Vector of colors to use as gradient.
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param elementId A valid CSS element id.
#' 
#' @examples 
#' reactrend(runif(100))
#' 
#' reactrend(
#'   runif(20), 
#'   gradient = c('#00c6ff', '#F0F', '#FF0'),
#'   smooth = TRUE,
#'   draw = TRUE,
#'   dash = c(1,7,3)
#' )
#'
#' @import htmlwidgets
#'
#' @export
reactrend <- function(data, draw = FALSE, draw_duration = 3000, draw_easing = "ease-in", 
                      stroke = "#000", stroke_width = 1, stroke_opacity = 1, smooth = FALSE,
                      line_cap = c("butt", "round", "square"), radius = 10,
                      dash = NULL, dash_offset = 10, gradient = NULL,
                      width = NULL, height = NULL, elementId = NULL) {
  
  if(missing(data))
    stop("missing data", call. = FALSE)

  # describe a React component to send to the browser for rendering.
  component <- reactR::reactMarkup(
    htmltools::tag(
      "Trend", 
      list(
        data = data,
        stroke = stroke,
        strokeWidth = stroke_width,
        strokeOpacity = stroke_opacity,
        smooth = smooth,
        strokeLinecap = match.arg(line_cap),
        strokeDasharray = dash,
        strokeDashoffset = dash_offset,
        gradient = gradient,
        autoDraw = draw,
        autoDrawDuration = draw_duration,
        autoDrawEasing = draw_easing
      )
    )
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'reactrend',
    component,
    width = width,
    height = height,
    package = 'reactrend',
    elementId = elementId
  )
}

#' Shiny bindings for reactrend
#'
#' Output and render functions for using reactrend within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from.
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a reactrend
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @param style CSS.
#' @param id,class Valid CSS id and class.
#' @param ... Any other arguments to pass to the HTML element.
#'
#' @name reactrend-shiny
#'
#' @export
reactrendOutput <- function(outputId, width = '100%%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'reactrend', width, height, package = 'reactrend')
}

#' @rdname reactrend-shiny
#' @export
renderReactrend <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, reactrendOutput, env, quoted = TRUE)
}

#' Called by HTMLWidgets to produce the widget's root element.
#' @rdname reactrend-shiny
reactrend_html <- function(id, style, class, ...) {
  htmltools::tagList(
    # Necessary for RStudio viewer version < 1.2
    reactR::html_dependency_corejs(),
    reactR::html_dependency_react(),
    reactR::html_dependency_reacttools(),
    htmltools::tags$div(id = id, class = class)
  )
}