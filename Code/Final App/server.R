library(gbm)
A_batters <- read.csv("A_batters.csv")
AA_batters <- read.csv("AA_batters.csv")
AAA_batters <- read.csv("AAA_batters.csv")
A_pitchers <- read.csv("A_pitchers.csv")
AA_pitchers <- read.csv("AA_pitchers.csv")
AAA_pitchers <- read.csv("AAA_pitchers.csv")

#colnames(A_pitchers)[colnames(A_pitchers) == "W.L."] = "W-L%"
#colnames(AA_pitchers)[colnames(AA_pitchers) == "W.L."] = "W-L%"
#colnames(AAA_pitchers)[colnames(AAA_pitchers) == "W.L."] = "W-L%"

#colnames(A_pitchers)[colnames(A_pitchers) == "SO.W"] = "SO/W"
#colnames(AA_pitchers)[colnames(AA_pitchers) == "SO.W"] = "SO/W"
#colnames(AAA_pitchers)[colnames(AAA_pitchers) == "SO.W"] = "SO/W"

A_battersX <- model.matrix(Promoted ~ ., data=A_batters)[,-c(1)]
gbm_mod_A_batters = gbm(A_batters$Promoted ~ ., data=data.frame(A_battersX))
AA_battersX <- model.matrix(Promoted ~ ., data=AA_batters)[,-c(1)]
gbm_mod_AA_batters = gbm(AA_batters$Promoted ~ ., data=data.frame(AA_battersX))
AAA_battersX <- model.matrix(Promoted ~ ., data=AAA_batters)[,-c(1)]
gbm_mod_AAA_batters = gbm(AAA_batters$Promoted ~ ., data=data.frame(AAA_battersX))
A_pitchersX <- model.matrix(Promoted ~ ., data=A_pitchers)[,-c(1)]
gbm_mod_A_pitchers = gbm(A_pitchers$Promoted ~ ., data=data.frame(A_pitchersX))
AA_pitchersX <- model.matrix(Promoted ~ ., data=AA_pitchers)[,-c(1)]
gbm_mod_AA_pitchers = gbm(AA_pitchers$Promoted ~ ., data=data.frame(AA_pitchersX))
AAA_pitchersX <- model.matrix(Promoted ~ ., data=AAA_pitchers)[,-c(1)]
gbm_mod_AAA_pitchers = gbm(AAA_pitchers$Promoted ~ ., data=data.frame(AAA_pitchersX))

gbm_mod_A_pitchers$var.names <- gsub('W.L.', 'W-L%', gbm_mod_A_pitchers$var.names)
gbm_mod_AA_pitchers$var.names <- gsub('W.L.', 'W-L%', gbm_mod_AA_pitchers$var.names)
gbm_mod_AAA_pitchers$var.names <- gsub('W.L.', 'W-L%', gbm_mod_AAA_pitchers$var.names)

gbm_mod_A_pitchers$var.names <- gsub('SO.W', 'SO/W', gbm_mod_A_pitchers$var.names)
gbm_mod_AA_pitchers$var.names <- gsub('SO.W', 'SO/W', gbm_mod_AA_pitchers$var.names)
gbm_mod_AAA_pitchers$var.names <- gsub('SO.W', 'SO/W', gbm_mod_AAA_pitchers$var.names)

#Define server logic required to draw plot
function(input, output){
    # build plot using inputs from the UI
    #using reactive function to react to inputs and get data

    #build reactive plot
    output$plot1 <- shiny::renderPlot({
        if(input$position  == "Pitchers") {
            col <- input$PitchersCol
            if(input$league == "A") {
                league <- gbm_mod_A_pitchers
            }
            if(input$league == "AA") {
                league <- gbm_mod_AA_pitchers
            }
            if(input$league == "AAA") {
                league <- gbm_mod_AAA_pitchers
            }
        }
        else {
            col <- input$BattersCol
            if(input$league == "A") {
                league <- gbm_mod_A_batters
            }
            if(input$league == "AA") {
                league <- gbm_mod_AA_batters
            }
            if(input$league == "AAA") {
                league <- gbm_mod_AAA_batters
            }
        }
        plot(league,i.var= col, ylab = "Effect on Player Promotion")
    })

}
