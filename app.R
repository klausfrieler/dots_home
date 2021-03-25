library(shiny)

b <- function(...){
  shiny::tags$b(...)
}
unicode_to_html_entity <- function(x){
  xs <- strsplit(as.character(x), "")[[1]]
  paste0(sprintf("&#%d;", sapply(xs, utf8ToInt)), collapse="")
}


# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  column(6, offset = 3, 
         titlePanel(
            h1(
              img(src = "https://s3-eu-west-1.amazonaws.com/media.dots.org/img/dots_logo_v4.png", height = "76", style = "margin-right:20px"),
               "DGM Online Testing Server", style = "color:#5c9dd1",             
               img(src = "https://s3-eu-west-1.amazonaws.com/media.dots.org/img/dgm_logo_v2.png", height = "49", style = "margin-left:20px")
            )
           )),

  # Sidebar layout with input and output definitions ----
  fluidRow(
    
    # Main panel for displaying outputs ----
    column(6, offset = 3,
      tabsetPanel(type = "tabs",
                  tabPanel("Start", htmlOutput("home")),
                  tabPanel("Tests", htmlOutput("tests")),
                  tabPanel("Anleitung", htmlOutput("manual")),
                  tabPanel("Veröffentlichungen", htmlOutput("publications"))
        
      )
    )

  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  output$home <- renderUI({
    includeHTML("home.html")
  })
  output$tests <- renderUI({
    includeHTML("tests.html")
  })
  output$manual <- renderUI({
    includeHTML("manual.html")
  })
  
  output$publications <- renderUI({
    includeHTML("publications.html")
  })
  
}
# Run the application
shinyApp(ui = ui, server = server)

