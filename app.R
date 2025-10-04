library(shiny)
library(bslib)
library(leaflet)

# Define UI for application that draws a histogram
ui <- page_fluid(

  tags$head(
    tags$style(
      HTML("
      #shiny-notification-panel {
        top: 0;
        bottom: unset;
        left: 0;
        right: 0;
        margin-left: auto;
        margin-right: auto;
        width: 100%;
        max-width: 450px;
      }

      #submit {
        margin-bottom: 1rem;
      }"
      )
    )
  ),

  tags$head(
    tags$script(
      HTML("
$(function() {
  $('#postcode').on('keydown', function(e){
    if(e.which == 13){
      $('#submit').click();
    }
  });
});
           "
      )
    )
  ),

  # Application title
  titlePanel("VIVID study inclusion checker"),

  verticalLayout(
    p("The University of Sheffield is running the VIVID study to",
      "investigate how different respiratory viruses affect different",
      "groups of people with and without different clinical",
      "vulnerabilities. To do this, we will use data collected in the",
      "course of routine patient care. You can find out more about the",
      "study at:",
      a("https://www.vivid-study.co.uk",
        href="https://www.vivid-study.co.uk",
        target="_blank")),
    p("The research team will only have access to de-identified data,",
      "so we won’t know whose records we’re looking at. However, our data",
      "providers will process confidential patient information on our",
      "behalf."),
    p(strong("Your records may be included in the study if you were",
             "registered with the NHS as living in some areas of",
             "England, indicated on the map below, at any time between 1st April 2021",
             "and 31st March 2026.")),
    p("If you have registered a NHS National Data Opt Out, your records",
      "will not be included in the data made available for the VIVID study."),
    p("Otherwise, you can check to see if your records might be included by",
      "entering the postcodes at which you lived during this time in the",
      "search box below."),
    p("If you think your records might be included and you would like to opt",
      "out from your records being used in the VIVID study, you can find",
      "details of how to opt out on the study website:",
      a("https://www.vivid-study.co.uk",
        href="https://www.vivid-study.co.uk",
        target="_blank")),

    tagAppendAttributes(
      textInput("postcode",
                strong("Check a postcode:"),
                value = ""),
      .cssSelector = "#postcode",
      autocomplete = "off"
    )
    ,
    textOutput("outcome"),
    actionButton(
      inputId = "submit",
      label = "Submit"),
    leafletOutput("map",
                  height = 600)
  )
)

server <- function(input, output, session) {

  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(-1.4649, 52.5619, zoom = 6)
  })

  observeEvent(
    eventExpr = input[["submit"]],
    handlerExpr = {
      output$outcome <- NULL
      clean_postcode <- gsub("\\s+", "", toupper(input$postcode))
      clean_postcode_len <- nchar(clean_postcode)
      if(clean_postcode_len < 5 | clean_postcode_len > 7) {
        showNotification(
          ui = "The text submitted is not a valid postcode.",
          id = "note_postcode_invalid",
          type = "warning",
          duration = NULL)
      } else {
        postcode_output <- paste(substr(clean_postcode,
                                        1,
                                        clean_postcode_len - 3),
                                 substr(clean_postcode,
                                        clean_postcode_len - 2,
                                        clean_postcode_len))

        updateTextInput(session, "postcode", value = postcode_output)
        removeNotification("note_postcode_invalid")
      }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
