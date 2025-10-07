library(data.table)
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

      .map-legend {
        margin-bottom: 1rem;
        background-color: rgba(82, 12, 137, 0.6);
        padding: 0.3rem 1rem;
        font-weight: bold;
      }

      .font-small {
        font-size: x-small;
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
      "investigate how certain respiratory viruses affect different",
      "groups of people with and without a variety of clinical",
      "vulnerabilities. To do this, we will use data collected in the",
      "course of routine patient care. You can find out more about the",
      "study at:",
      a("www.vivid-study.co.uk",
        href="https://www.vivid-study.co.uk",
        target="_blank")),
    p("The research team will only have access to de-identified data,",
      "so we won’t know whose records we’re looking at. However, before we",
      "receive the de-identified, our data providers (NHS England and the",
      "UK Health Security Agency) will process confidential patient",
      "information on our behalf."),
    p(strong("Your records may be included in the study if you were",
             "registered with the NHS as living in some areas of",
             "England, indicated on the map below, at any time between",
             "1st April 2021 and 31st March 2026.")),
    p("If you have registered a NHS National Data Opt Out, your records",
      "will not be included in the data made available for the VIVID study."),
    p("Otherwise, you can check to see if your records might be included by",
      "entering the postcodes at which you lived during this time in the",
      "search box below."),
    p("If you think your records might be included and you would like to opt",
      "out from your records being used in the VIVID study, you can find",
      "details of how to opt out on the study website:",
      a("www.vivid-study.co.uk",
        href="https://www.vivid-study.co.uk",
        target="_blank")),

    tagAppendAttributes(
      textInput("postcode",
                strong("Check a postcode:"),
                value = ""),
      .cssSelector = "#postcode",
      autocomplete = "off"
    ),
    actionButton(
      inputId = "submit",
      label = "Submit"),
    htmlOutput("outcome"),
    tagAppendAttributes(
      div("In the map, the purple overlay indicates areas in the study."),
      class = "map-legend"),
    leafletOutput("map",
                  height = 600),
    p("This app does not store any user data, nor does it use any",
      "non-essential cookies."),
    tagAppendAttributes(
      p("This app uses the Gridlink NHS Postcode Directory and digital boundary",
        "data from the Office for National Statistics licensed under the Open",
        "Government Licence:"
      ),
      class = "font-small"),
    tagAppendAttributes(
      tags$ul(
        tags$li("Contains Ordnance Survey data © Crown copyright and database right 2025"),
        tags$li("Contains Royal Mail data © Royal Mail copyright and database right 2025"),
        tags$li("Source: Office for National Statistics licensed under the Open Government Licence v.3.0")
      ),
      class = "font-small")
  )
)

server <- function(input, output, session) {

  result_coords <- NA
  postcode_catchment_area_lookup <- readRDS("data/postcode_catchment_area_lookup.rds")
  postcode_district_catchment_area_lookup <- readRDS("data/postcode_district_catchment_area_lookup.rds")
  site_catchment_areas_4326 <- readRDS("data/site_catchment_areas_4326.rds")

  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      addPolygons(data = site_catchment_areas_4326,
                  stroke = FALSE,
                  fillOpacity = 0.6,
                  fillColor = "#520c89") |>
      setView(lng = -1.4649, lat = 52.5619, zoom = 6)
  })

  # See pattern @ https://shiny.posit.co/r/reference/shiny/1.2.0/observeevent.html
  #   clean_postcode <- eventReactive(input$submit,{
  #     input$submit
  #   },
  #   ignoreInit = TRUE)

  observeEvent(input$submit, {
    output$outcome <- NULL
    clean_postcode <- gsub("\\s+", "", toupper(input$postcode))
    clean_postcode_len <- nchar(clean_postcode)
    if(!grepl("^[A-Z]{1,2}[0-9][A-Z0-9]?[0-9][A-Z]{2}$", clean_postcode)) {
      showNotification(
        ui = paste0("The entered text is not a valid postcode.\nPlease check and try again."),
        id = "note_postcode_invalid",
        type = "warning")
    } else {
      pc_district <- substr(clean_postcode,
                            1,
                            clean_postcode_len - 3)
      pc_output <- paste(pc_district,
                         substr(clean_postcode,
                                clean_postcode_len - 2,
                                clean_postcode_len))

      pc_details <- postcode_catchment_area_lookup[postcode == pc_output]
      pc_district_details <- postcode_district_catchment_area_lookup[postcode_district == pc_district]

      input_error <- FALSE

      if(nrow(pc_details) == 0 & nrow(pc_district_details) == 0) {
        showNotification(
          ui = "We can't find the submitted postcode.\nPlease check and try again or use the interactive map.",
          id = "note_postcode_invalid",
          type = "warning",
          duration = NULL)

        input_error <- TRUE
      } else if(nrow(pc_details) == 0 & nrow(pc_district_details) == 1) {
        if(!pc_district_details$in_catchment_area) {
          result_catchment = FALSE
          result_text = paste("The postcode",
                              strong(pc_output),
                              "is",
                              strong("not"),
                              "in the study area.")
        } else {
          result_catchment = as.logical(NA)
          result_text = paste0("We can't find the precise postcode submitted: ", pc_output, "<br />",
                               "The submitted postcode district (",
                               strong(pc_district),
                               ") contains some postcodes included in the study.<br />",
                               "Please check the map to be sure.")
        }

        if(is.na(pc_district_details$longitude)) {
          result_coords <- NA
        } else {
          result_coords <- c(pc_district_details$longitude,
                             pc_district_details$latitude,
                             12)
        }
      } else {
        if(!pc_details$in_catchment_area) {
          result_catchment = FALSE
          result_text = paste("The postcode ",
                              strong(pc_output),
                              "is",
                              strong("not"),
                              "in the study area.")
        } else {
          result_catchment = TRUE
          result_text = paste("The postcode",
                              strong(pc_output),
                              "is in the study area.")
        }

        if(!is.na(pc_details$longitude)) {
          result_coords <- c(pc_details$longitude,
                             pc_details$latitude,
                             16)
        } else {
          if(is.na(pc_district_details$longitude)) {
            result_coords <- NA
          } else {
            result_coords <- c(pc_district_details$longitude,
                               pc_district_details$latitude,
                               12)
          }
        }
      }

      updateTextInput(session, "postcode", value = pc_output)
      if(!input_error) {
        removeNotification("note_postcode_invalid")

        output$outcome <- renderText(paste0("<div class='alert alert-info' role='alert'>",
                                            result_text,
                                            "</div>"))

        if(!any(is.na(result_coords))) {
          leafletProxy("map", session) |>
            flyTo(lng = result_coords[1], lat = result_coords[2], zoom = as.integer(result_coords[3]))
        } else {
          leafletProxy("map", session) |>
            setView(lng = -1.4649, lat = 52.5619, zoom = 6)
        }
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
