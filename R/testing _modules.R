testUI <- function(id){
  shiny::uiOutput(shiny::NS(id, "ui_elements"))
}

testServer <- function(id, instigator){

  shiny::moduleServer(id, function(input, output, session) {

    # Note that the shiny app must pass the reactive function itself not the values, then we convert it here.
    instigator_values <- shiny::reactive({
      seq(1, instigator())
    })

    observe({
      instigator_values() |>
        purrr::walk(
          .f = ~{
            output_name <- glue::glue("ui_{.}")
            output[[output_name]] <- shiny::renderText(.)
          }
        )
      })

    output$ui_elements <- shiny::renderUI({
        instigator_values() |>
          purrr::map(
            .f = ~shiny::textOutput(shiny::NS(id, glue::glue("ui_{.}")))
          )
    })
  })
}



test_purrr <- function(schedule){

  instigator_origin = list(1,2)

  ui <- shiny::fluidPage(

    shiny::numericInput("uiHowMany", "How Many?", 2),

    testUI("neil")
  )

  server <- function(input, output, session) {

    testServer("neil", reactive(input$uiHowMany))

  }

  shiny::runGadget(ui, server, viewer = shiny::paneViewer())
}
