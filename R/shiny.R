maintain_schedule_interactive <- function(schedule){

  global_schedule_obj <- deparse(substitute(schedule))

  ui <- bslib::page_fluid(

      waiter::use_waiter(),

      title = "Maintain Schedule",

      theme = sequenceR::seq_shiny_bslib_theme(),

      shiny::textOutput("loaded_schedule"),

      shiny::uiOutput("date_range_select"),
      shiny::uiOutput("campaign_select"),
      shiny::uiOutput("media_select"),
      rhandsontable::rHandsontableOutput("editable_table")
  )

  server <- function(input, output, session) {

    schedule_unnest <- schedule |>
      unnest_schedule()

    output$loaded_schedule <- shiny::renderText(
      glue::glue("Editing: {global_schedule_obj}")
    )

    output$date_range_select <- shiny::renderUI({
      shiny::dateRangeInput("uiDateRange", "Date Range", schedule$start_date, schedule$end_date)
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


  }

  # https://stackoverflow.com/questions/32944961/modify-global-data-from-within-a-function-in-r
  # to assign obj at the end: assign(deparse(df_obj), df, parent.frame())

  shiny::runGadget(ui, server, viewer = shiny::paneViewer())
}
