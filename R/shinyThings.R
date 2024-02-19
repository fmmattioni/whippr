# functions copied from gadenbuie/shinyThings
# this is needed to prevent the import from github for CRAN submission

#' Undo/Redo History Buttons
#'
#' This is a simple Shiny module for undo/redo history. The Shiny module accepts
#' an arbitrary reactive data value. Changes in the state of this reactive value
#' are tracked and added to the user's history. The user can then repeatedly
#' undo and redo to walk through this stack. The module returns the current
#' selected value of the reactive from this historical stack, or `NULL` when
#' the app state was changed by the user. Because this reactive can hold
#' arbitrary data about the state of the Shiny app, it is up to the app
#' developer to use the returned current value to update the Shiny apps' inputs
#' and UI elements.
#'
#' @param id The module id
#' @param value The reactive expression with the values should be saved for the
#'   user's history. This expression can contain arbitrary data and be of any
#'   structure as long as it returns a single value (or list). Each change in
#'   this value is stored, so the module may not work well for storing large
#'   data sets.
#' @param value_debounce_rate Debounce rate in milliseconds for the `value`
#'   reactive expression. To avoid saving spurious changes in `value`, the
#'   expression is debounced. See [shiny::debounce()] for more information.
#'
#' @return The `undoHistory()` module returns the currently selected history
#'   item as the user moves through the stack, or `NULL` if the last update
#'   was the result of user input. The returned value has the same structure as
#'   the reactive `value` passed to `undoHistory()`.
#' @keywords internal
undoHistory <- function(id, value, value_debounce_rate = 500) {
  shiny::callModule(
    undoHistoryModule,
    id = id,
    value = value,
    value_debounce_rate = value_debounce_rate
  )
}

#' @keywords internal
undoHistoryUI <- function(
  id,
  class = NULL,
  btn_class = "btn btn-default",
  back_text = NULL,
  back_title = "Undo",
  back_icon = "undo",
  fwd_text = NULL,
  fwd_title = "Redo",
  fwd_icon = "redo"
) {
  ns <- shiny::NS(id)
  stopifnot(is.null(class) || is.character(class))
  stopifnot(is.character(btn_class))
  if (length(btn_class) == 1) {
    btn_class <- rep(btn_class, 2)
  } else if (length(btn_class) != 2) {
    stop(paste(
      "`btn_class` must be length 1 (applied to both buttons) or 2 (applied to",
      "the undo/redo buttons respectively)."
    ))
  }
  spaces <- function(...) {
    x <- lapply(list(...), function(x) paste(x, collapse = " "))
    paste(x, collapse = " ")
  }
  htmltools::tagList(
    htmltools::htmlDependency(
      name    = "shinythings",
      version = utils::packageVersion("whippr"),
      package = "whippr",
      src     = "shinyThings",
      script  = "undoHistory.js"
    ),
    shiny::tags$div(
      class = spaces("btn-group", class),
      role = "group",
      `aria-label` = "Undo/Redo History",
      shiny::tags$button(
        id = ns("history_back"),
        class = spaces(btn_class[1], "action-button disabled"),
        `data-val` = 0L,
        disabled = TRUE,
        title = back_title,
        if (!is.null(back_icon)) shiny::icon(back_icon),
        back_text
      ),
      shiny::tags$button(
        id = ns("history_forward"),
        type = "button",
        class = spaces(btn_class[2], "action-button disabled"),
        `data-val` = 0L,
        disabled = TRUE,
        title = fwd_title,
        if (!is.null(fwd_icon)) shiny::icon(fwd_icon),
        fwd_text
      )
    )
  )
}

undoHistoryModule <- function(
  input,
  output,
  session,
  value = shiny::reactive(NULL),
  value_debounce_rate = 500
) {
  ns <- session$ns


  # changes in record get pushed to top of `stack$history`
  # if the user backs into historical values,
  # then they are moved to top of stack_future
  stack <- shiny::reactiveValues(history = list(), future = list(), current = NULL)

  output$v_stack <- shiny::renderPrint({
    utils::str(shiny::reactiveValuesToList(stack)[c("history", "current", "future")])
  })

  value_debounced <- shiny::debounce(value, value_debounce_rate)

  # Add updates to value_debounced() into the stack$history
  ref_id <- 0L
  shiny::observe({
    shiny::req(!is.null(value_debounced()))
    current_value <- shiny::isolate(stack$current)
    if (!is.null(current_value) && identical(current_value, value_debounced())) {
      # Don't store latest change in history because it came from the module
      # or is the same as the most recent state
      return()
    }
    this <- list()
    ref_id <<- ref_id + 1L
    this[[paste(ref_id)]] <- value_debounced()
    stack$history <- c(this, shiny::isolate(stack$history))
    stack$future <- list()
    stack$current <- NULL
  })

  # Enable forward/backward buttons if there are values in stack
  has_history <- shiny::reactive({
    !(is.null(stack$history) || length(stack$history) <= 1)
  })

  has_future <- shiny::reactive({
    !(is.null(stack$future) || length(stack$future) == 0)
  })

  btn_state_lag <- c(FALSE, FALSE)

  shiny::observe({
    btn_state <- c(has_history(), has_future())
    if (identical(btn_state, btn_state_lag)) return()

    btn_ids <- ns(c("history_back", "history_forward"))

    btn_state_send <- list()
    if (any(btn_state)) btn_state_send$enable  <- as.list(btn_ids[btn_state])
    if (any(!btn_state)) btn_state_send$disable <- as.list(btn_ids[!btn_state])
    btn_state_lag <<- btn_state
    session$sendCustomMessage("undoHistoryButtons", btn_state_send)
  })

  restore_stack_item <- function(item) {
    ref_id <- names(item)[1]
    stack$current <- item[[1]]
  }

  # Move back in time
  shiny::observeEvent(input$history_back, {
    shiny::req(length(stack$history) > 1)

    # copy stack to save all changes at once at the end
    .stack <- shiny::reactiveValuesToList(stack)
    .stack$current <- NULL

    # current value goes to the future stack
    .stack$future <- c(.stack$history[1], .stack$future)

    # pop the current value off of the history stack
    .stack$history <- .stack$history[-1]

    # restore the previous value
    stack$future <- .stack$future
    stack$history <- .stack$history
    restore_stack_item(.stack$history[1])
  }, priority = 1000)

  # Move forward in time
  shiny::observeEvent(input$history_forward, {
    shiny::req(length(stack$history) > 0, length(stack$future) > 0)

    .stack <- shiny::reactiveValuesToList(stack)
    .stack$current <- NULL

    # top of future stack goes to top of history stack
    .stack$history <- c(.stack$future[1], .stack$history)

    # pop the top of future stack
    .stack$future <- .stack$future[-1]

    # restore the (pseudo-)future value
    stack$future <- .stack$future
    stack$history <- .stack$history
    restore_stack_item(.stack$history[1])
  }, priority = 1000)

  return(shiny::reactive(stack$current))
}
