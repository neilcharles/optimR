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

      # shiny::uiOutput("date_range_select"),
      # shiny::uiOutput("campaign_select"),
      # shiny::uiOutput("media_select"),
      # rhandsontable::rHandsontableOutput("editable_table")
      schedule_timevisUI("gantt"),
      gt::gt_output("gantt_table")
  )

  server <- function(input, output, session) {

    schedule_unnest <- schedule |>
      unnest_schedule(level = "media")

    output$loaded_schedule <- shiny::renderText(
      glue::glue("Editing: {global_schedule_obj}")
    )

    output$date_range_select <- shiny::renderUI({
      shiny::dateRangeInput("uiDateRange", "Date Range", schedule$schedule_start_date, schedule$schedule_end_date)
    })

    output$campaign_select <- shiny::renderUI({
      shiny::selectInput("uiCampaign", "Campaign", schedule_unnest |>
                                                     dplyr::pull(campaign_name) |>
                                                     unique())
    })

    output$media_select <- shiny::renderUI({
      shiny::selectInput("uiMedia", "Media", schedule_unnest |>
                                                dplyr::filter(campaign_name==input$uiCampaign) |>
                                                dplyr::pull(media_name) |>
                                                unique())
    })

    output$editable_table <- rhandsontable::renderRHandsontable({
      schedule_unnest |>
        dplyr::filter(campaign_name %in% input$uiCampaign,
                      media_name %in% input$uiMedia) |>
      rhandsontable::rhandsontable()
    })

    schedule_timevisServer("gantt", schedule)

    output$gantt_table <- gt::render_gt({
        shiny::req(input$gantt_data)

        input$gantt_data |>
          dplyr::filter(id %in% input$gantt_selected) |>
          dplyr::select(dplyr::contains("media_"), -media_id) |>
          #NEED TO SELECT START END ETC.
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
        nest_schedule()

      # https://stackoverflow.com/questions/32944961/modify-global-data-from-within-a-function-in-r
      assign(global_schedule_obj, amended_schedule, envir =  globalenv())
    })

  }

  shiny::runGadget(ui, server, viewer = shiny::paneViewer())
}
