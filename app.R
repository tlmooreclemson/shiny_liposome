library(shiny)

# Load data ----
diam_url <- "https://raw.githubusercontent.com/tlmooreclemson/shiny_liposome/main/data/ann_diameter_predictions.csv"
diam_data <- read.csv(url(diam_url))
svm_url <- "https://raw.githubusercontent.com/tlmooreclemson/shiny_liposome/main/data/svm_predictions.csv"
svm_data <- read.csv(url(svm_url))
df <- merge(diam_data, svm_data, by=c("flow_rate", "conc", "ratio_aq_org", "curc_ug"))
rm(diam_data, svm_data, diam_url, svm_url)
df <- subset(df, select=-c(curc_ug))
df <- df[which(df$pred_dispersity == "monodisperse" &
                 df$pred_stability == "stable"),]
df <- subset(df, select=-c(pred_dispersity, pred_stability))
names(df)[names(df) == "flow_rate"] <- "Total Flow Rate (ml/min)"
names(df)[names(df) == "conc"] <- "Organic Phase Concentration (mg/ml)"
names(df)[names(df) == "ratio_aq_org"] <- "Aqueous:Organic Volume Ratio"
names(df)[names(df) == "ANN_diameter_nm"] <- "Predicted Diameter (nm)"

# Shiny app -----
ui <- fluidPage(
  ## App title -----
  titlePanel("Shiny Liposome"),
  
  ## Sidebar layout with input/output definitions ----
  sidebarLayout(
    ### Sidebar panel for inputs ----
    sidebarPanel(
      helpText("Use the inputs below to select desired",
               "liposome properties. Click 'Submit'",
               "when you are finished to get appropriate",
               "formulations."),
      helpText("Press 'Reset' to start a new prediction."),
      # Select whether curcumin loading is desired
      #radioButtons("curc_load", h4("Curcumin loading?"),
      #             choices = list("Yes" = 50, "No" = 0),
      #             selected = 50),
      # Input the desired diameter
      sliderInput("desired_diam", h4("Desired diameter (nm)"),
                  min = 23, max = 184, value = 50),
      # Add a submit button to re-run the app
      actionButton("submit", "Submit"),
      actionButton("reset", "Reset")
      #actionButton("reset", "Reset")
    ),
    ### Main panel for displaying outputs ----
    mainPanel(
      # Output table as a result
      tableOutput('table')
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  # Store the desired_diam variable when "submit" is pressed -----
  result_df = reactiveValues(d1 = as.data.frame(matrix(nrow=366, ncol=4)))
  
  # Reset button for reseting initial conditions
  observeEvent(input$reset, {result_df$d1 = as.data.frame(matrix(nrow=366, ncol=4))})
  
  # Submit to report conditions
  observeEvent(input$submit, {
    temp = result_df$d1
    names(temp) = c("Total Flow Rate (ml/min)", "Organic Phase Concentration (mg/ml)", "Aqueous:Organic Volume Ratio", 
                    "Predicted Diameter (nm)")
    temp$"Total Flow Rate (ml/min)" = df$"Total Flow Rate (ml/min)"
    temp$"Organic Phase Concentration (mg/ml)" = df$`Organic Phase Concentration (mg/ml)`
    temp$"Aqueous:Organic Volume Ratio" = df$"Aqueous:Organic Volume Ratio"
    temp$"Predicted Diameter (nm)" = df$"Predicted Diameter (nm)"
    temp = temp[which(temp$"Predicted Diameter (nm)" < input$desired_diam+10 &
                        temp$"Predicted Diameter (nm)" > input$desired_diam-10),]
    temp$"Absolute Error (nm)" = abs(input$desired_diam-temp$"Predicted Diameter (nm)")
    temp = temp[order(temp$"Absolute Error (nm)"),] # Order results by absolute error
    
    result_df$d1 = temp
  })
  
  output$table <- renderTable({
    head(result_df$d1, n=10)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
