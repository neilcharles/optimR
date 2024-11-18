maintain_schedule_interactive <- function(schedule){

  global_schedule_obj <- deparse(substitute(schedule))

  ui <- bslib::page_fluid(

    shiny::tags$head(
      # Stops borders being red in timevis
      shiny::tags$style(shiny::HTML("
      .vis-group-level-unknown-but-gte1 {
          border: none;
      }"
      ))),

      waiter::use_waiter(),

      title = "Maintain Schedule",

      theme = sequenceR::seq_shiny_bslib_theme(),

      shiny::h3(
        shiny::textOutput("loaded_schedule")
        ),

      shiny::actionButton("uiCommit", "Save Changes"),
      shiny::hr(),

      schedule_timevisUI("gantt")
      # gt::gt_output("gantt_table")
  )

  server <- function(input, output, session) {

    schedule_unnest <- schedule |>
      unnest_schedule(level = "media")

    output$loaded_schedule <- shiny::renderText(
      glue::glue("Editing: {global_schedule_obj}")
    )

    schedule_timevisServer("gantt", schedule)

    output$gantt_table <- gt::render_gt({

      shiny::req(input$`gantt-gantt_data`)

      input$`gantt-gantt_data` |>
          dplyr::filter(id %in% input$gantt_selected) |>
          dplyr::select(dplyr::contains("media_"), -media_id) |>
          # NEED TO SELECT START END ETC.
          dplyr::rename_all(~stringr::str_replace(.,"media_","")) |>
          gt::gt() |>
          sequenceR::seq_gt_theme()
    })

    shiny::observeEvent(input$uiCommit, {

      amended_schedule <- input$gantt_data |>
        dplyr::mutate(
          media_name = content,
          media_start_date = lubridate::as_date(start),
          media_end_date = lubridate::as_date(end),
        ) |>
        dplyr::select(-id, -content, -start, -end, -editable) |>
        schedule_nest()

      # https://stackoverflow.com/questions/32944961/modify-global-data-from-within-a-function-in-r
      assign(global_schedule_obj, amended_schedule, envir =  globalenv())
    })

    current_media <- reactive({
      req(input$`gantt-gantt_selected`)
      media_get(schedule, input$`gantt-gantt_selected`)
    })

    edit_recordServer("selected_media", current_media)  #media_get(schedule, "47f651d7-24e1-497d-8b0b-46105b006a4a"))

    observeEvent(current_media(), {

      req(current_media())

      shiny::showModal(
        shiny::modalDialog(
          edit_recordUI("selected_media"),
          easyClose = TRUE,
          footer = shiny::actionButton("uiSaveMedia", "Save", icon = shiny::icon("save")))
      )

        })

  }

  shiny::runGadget(ui, server, viewer = shiny::paneViewer())
}
