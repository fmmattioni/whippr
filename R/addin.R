#' Manual data cleaner
#'
#' Usually manual data cleaning should be avoided. However, sometimes in gas exchange data
#' there is the need to delete a few clear "bad breaths" (noise). In these situations you may use this function.
#' Although it is encouraged that you use the `detect_outliers()` function, you may use this function at your own risk.
#' This function can also be used to clean other kind of data, like heart rate data.
#'
#' @param .data The data to be manually cleaned. The first column will be always treated as the x-axis.
#' @param width The width, in pixels, of the window.
#' @param height the height, in pixels, of the window.
#'
#' @return The code to reproduce the manual data cleaning.
#'
#' @export
run_manual_cleaner <- function(.data, width = 1200, height = 900) {

  # check additional needed packages
  rlang::check_installed(c("miniUI", "shiny", "rstudioapi", "datapasta", "htmltools"))

  ## check data
  if(missing(.data))
    stop("It looks like you forgot to pass a data frame to the function.", call. = FALSE)

  if(!is.data.frame(.data))
    stop("I am sorry, this function only supports data frames.", call. = FALSE)

  data_input <- deparse(substitute(.data))

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Manual data cleaning"),
    miniUI::miniContentPanel(

      shiny::fluidRow(
        shiny::column(width = 4,
                      shiny::selectInput(
                 inputId = "select_y_axis",
                 label = "Select y axis",
                 choices = NULL)
        ),
        shiny::column(width = 4,
                      shiny::textInput(
                 inputId = "output_df",
                 label = "Enter the name of the new data frame:"
               )
        )
      ),

      shiny::plotOutput(
        outputId = "plot",
        height = 400,
        brush = shiny::brushOpts(
          id = "plot_brush"
        )
      ),

      shiny::actionButton(inputId = "exclude_toggle", label = "Exclude points"),
      shiny::actionButton(inputId = "exclude_reset", label = "Reset"),
      undoHistoryUI(id = "history", back_text = "Step backward", fwd_text = "Step forward"),
      # Show which data points are being excluded
      shiny::tags$h4("Data points being excluded (x-axis value):"),
      shiny::verbatimTextOutput("v")
    )
  )

  server <- function(input, output, session) {

    r <- shiny::reactiveValues(
      data = .data,
      data_keep = NULL,
      exclude_rows = NULL
    )

    ## keep history of points to exclude
    undo_app_state <- undoHistory(
      id = "history",
      value = shiny::reactive({
        r$exclude_rows
      })
    )

    ## receive updates from undoHistory() and update the app
    shiny::observe({
      shiny::req(!is.null(undo_app_state()))

      r$exclude_rows <- undo_app_state()
    })

    # Just for debugging
    output$v <- shiny::renderPrint(r$exclude_rows)

    shiny::observe({
      shiny::updateSelectInput(
        session,
        inputId = "select_y_axis",
        choices = colnames(r$data),
        selected = 0
      )
    })

    shiny::observe({
      shiny::req(is.null(r$exclude_rows))

      r$data_keep <- r$data
    })

    shiny::observe({
      shiny::req(r$exclude_rows)

      r$data_keep <- r$data %>%
        dplyr::filter_at(1, function(x) !x %in% r$exclude_rows)
    })

    # Toggle points that are brushed, when button is clicked
    shiny::observeEvent(input$exclude_toggle, {
      time_column <- colnames(r$data_keep)[1]

      res <- shiny::brushedPoints(r$data_keep, input$plot_brush, xvar = time_column, yvar = input$select_y_axis)

      r$exclude_rows <- c(r$exclude_rows, res[[1]])
    })

    # Reset all points
    shiny::observeEvent(input$exclude_reset, {
      r$exclude_rows <- NULL
    })

    output$plot <- shiny::renderPlot({
      shiny::req(input$select_y_axis)

      time_column <- colnames(r$data_keep)[1]

      ggplot2::ggplot(r$data_keep, ggplot2::aes_string(time_column, input$select_y_axis)) +
        ggplot2::geom_point() +
        theme_whippr()

    }, res = 96)

    ## cancel button
    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })

    ## done button
    shiny::observeEvent(input$done, {
      ## prepare return code
      points_to_filter <- sort(unique(r$exclude_rows)) %>%
        datapasta::vector_construct() %>%
        stringr::str_remove("\n")
      time_column <- colnames(r$data_keep)[1]

      ## get current row in the R code
      context <- rstudioapi::getActiveDocumentContext()
      context_row <- context$selection[[1]]$range$end["row"]

      return_code <- glue::glue("\n\n\n## code from manual cleaning\n{input$output_df} <- {data_input} %>% \n\tdplyr::filter(!{time_column} %in% {points_to_filter})\n\n")

      rstudioapi::insertText(text = return_code, location = c(context_row + 1, 1))
      shiny::stopApp()
    })
  }

  shiny::runGadget(app = ui, server = server, viewer = shiny::dialogViewer(dialogName = "Manual cleaner", width = width, height = height), stopOnCancel = FALSE)
}
