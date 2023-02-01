library(shiny)
A_batters <- read.csv("A_batters.csv")
A_pitchers <- read.csv("A_pitchers.csv")

colnames(A_pitchers)[colnames(A_pitchers) == "W.L."] = "W-L%"
colnames(A_pitchers)[colnames(A_pitchers) == "SO.W"] = "SO/W"

col_select_A_batters <- colnames(A_batters)[-c(1,26)]
col_select_A_pitchers <- colnames(A_pitchers)[-c(1,31)]


position <- c("Pitchers","Batters")
league <- c("A","AA","AAA")

#build UI:
shiny::fluidPage(
    theme = shinythemes::shinytheme("cerulean"),

    #title
    shiny::titlePanel("Effect on Minor League Promotion"),
    shiny::navbarPage("MILB Call Up",
                      shiny::tabPanel(icon("home"),
                                      fluidRow(
                                          column(
                                              br(),
                                              p("This project was a project that I created during my data science in sports class at Brigham Young University during the winter of 2022.
                                                This project takes the data of minor league players over the last 10 years 2012-2021 (Not including the 2020 year since there was no minor league season).
                                                I have focused on A, AA and AAA (Combining both low A and high A together, since starting last season they were combined).
                                                After taking into account all of this data, I created a model, a gradiant boosting model (gbm) allowing the user to see how much front office members should take into account the specific stat when
                                                calling up the athlete.", style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                              br(),
                                          width=8),

                                          column(
                                            br(),
                                            p("There are a few different statistics. Here is a dictionary of all of them: ", style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                            br(),
                                            width=8),

                                          column(
                                            br(),
                                            p("Pitching Stats:", br(),
                                              "Age", br(), "W", br(), "L", br(), "W-L%", br(), "ERA", br(), "G", br(),
                                              "GS", br(), "GF", br(), "CG", br(), "SHO", br(), "SV", br(), "IP", br(), "H", br(), "R", br(), "ER", br(),
                                              "HR", br(), "BB", br(), "IBB", br(), "SO", br(), "HBP", br(), "BK", br(), "WP", br(), "BF", br(), "WHIP", br(), "H9", br(), "HR9", br(), "BB9", br(),
                                              "SO9", br(), "SO/W", br(), style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                            br(),
                                            width=8),

                                          column(
                                            br(),
                                            p("Hitting Stats:", br(),
                                              "Age", br(), "G", br(), "PA", br(), "AB", br(), "R", br(), "H", br(), "X2B", br(), "X3B", br(), "HR", br(), "RBI", br(),
                                              "SB", br(), "CS", br(), "BB", br(), "SO", br(), "BA", br(), "OBP", br(), "SLG", br(), "OPS", br(), "TB", br(), "GDP", br(),
                                              "HBP", br(), "SH", br(), "SF", br(), "IBB", style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                            br(),
                                            width=8),

                                      )
                      ),
                      shiny::tabPanel(("Graph"),
                                      # Create a sidebar for our dropdowns
                                      shiny::sidebarPanel(
                                          # a selector dropdown for the columns we want to plot
                                          shiny::selectInput(
                                              inputId <- "position",
                                              label <- "Pitcher/Batter",
                                              choices <- position,
                                              selected = "Pitchers"
                                          ),
                                          shiny::selectInput(
                                              inputId <- "league",
                                              label <- "League",
                                              choices <- league,
                                              selected <- "A"
                                          ),
                                          shiny::conditionalPanel(
                                              condition = "input.position == 'Pitchers'",
                                              shiny::selectInput(
                                                  inputId <- "PitchersCol",
                                                  label <- "Pitchers Data Choices",
                                                  choices <- col_select_A_pitchers,
                                                  selected <- "ERA"
                                              )
                                          ),
                                          shiny::conditionalPanel(
                                              condition = "input.position == 'Batters'",
                                              shiny::selectInput(
                                                  inputId <- "BattersCol",
                                                  label <- "Batters Data Choices",
                                                  choices <- col_select_A_batters,
                                                  selected <- "BA"
                                              )
                                          ),

                                      ),
                                      # create main panel to display the plot
                                      shiny::mainPanel(
                                          # call plot from the server
                                          shiny::plotOutput('plot1')
                                      )

                      )),

)
