library(shiny)
source("show_tests.R", encoding = "UTF-8")

b <- function(...){
  shiny::tags$b(...)
}

unicode_to_html_entity <- function(x){
  xs <- strsplit(as.character(x), "")[[1]]
  paste0(sprintf("&#%d;", sapply(xs, utf8ToInt)), collapse="")
}

read_test_info(fname = "data/dots_test_def.xlsx")
test_names <- update_test_names(fname = "data/dots_test_def.xlsx", test_names) 

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  column(6, offset = 3, 
         titlePanel(
            h1(
              img(src = "https://s3-eu-west-1.amazonaws.com/media.dots.org/img/dots_logo_v4.png", height = "76", style = "margin-right:20px"),
               shiny::tags$span("DGM Online Testing Server", style = "white-space:nowrap"), 
              style = "color:#5c9dd1;",             
               img(src = "https://s3-eu-west-1.amazonaws.com/media.dots.org/img/dgm_logo_v2.png", height = "49", style = "margin-left:20px")
            ), windowTitle = "DOTS Home"
           )),

  # Sidebar layout with input and output definitions ----
  fluidRow(
    
    # Main panel for displaying outputs ----
    column(6, offset = 3, 
      tabsetPanel(type = "tabs", id = "dots",
                  tabPanel("Start", htmlOutput("home")),
                  tabPanel("Über", htmlOutput("about")),
                  tabPanel("Tests", htmlOutput("tests")),
                  tabPanel("Services", htmlOutput("services")),
                  tabPanel("Umfragen", htmlOutput("studies")),
                  tabPanel("Shiny Apps", htmlOutput("shiny_apps")),
                  tabPanel("FAQ", htmlOutput("faq")),
                  tabPanel("Team", htmlOutput("team"))
#                  , tabPanel("Veröffentlichungen", htmlOutput("publications"))
        
      )
    )

  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  output$home <- renderUI({
    includeHTML("home.html")
  })
  output$about <- renderUI({
    includeHTML("about.html")
  })
  output$tests <- renderUI({
    shiny::div(static_selection_page(test_names, test_info),
      shiny::p("", style = "margin-bottom:50px"))
    #includeHTML("tests.html")
  })
  output$services <- renderUI({
    includeHTML("services.html")
  })
  output$studies <- renderUI({
    includeHTML("studies.html")
  })
  output$shiny_apps <- renderUI({
    includeHTML("shiny_apps.html")
  })
  
  output$faq <- renderUI({
    includeHTML("faq.html")
  })
  
  output$publications <- renderUI({
    includeHTML("publications.html")
  })
  output$team <- renderUI({
    includeHTML("team.html")
  })
  
}
# Run the application
shinyApp(ui = ui, server = server)

