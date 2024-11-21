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
      campaignEditUI("edit_campaign")
      # gt::gt_output("gantt_table")
  )

  server <- function(input, output, session) {

    output$loaded_schedule <- shiny::renderText(
      glue::glue("Editing: {global_schedule_obj}")
    )

    schedule_amended <- campaignEditServer("edit_campaign", schedule)

    shiny::observeEvent(input$uiCommit, {
      # https://stackoverflow.com/questions/32944961/modify-global-data-from-within-a-function-in-r
      assign(global_schedule_obj, schedule_amended(), envir =  globalenv())
    })

  }

  shiny::runGadget(ui, server, viewer = shiny::paneViewer())

}
