#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- navbarPage("Statsapp",
                 
                 tabPanel("Descriptive",
    
    # Application title
    titlePanel("Validation and descriptive statistics"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput('uploadfile', 'Choose xlsx file',
                      accept = c(".xlsx")
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("heatPlot"),
            plotOutput("rawvis"),
            textOutput("outliers"),
            tableOutput("descriptive")
        )
    )
),

            tabPanel("Predictive",
                     
        # Application title
        titlePanel("Correlations and ED50"),
                     
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
        sidebarPanel(
        radioButtons("plotType", "Plot type",
                    c("Scatter"="p", "Line"="l")
              )
        ),
                         
        # Show a plot of the generated distribution
        mainPanel(
        plotOutput("corPlot")
       )
     )    
                     
                     
),

        tabPanel("Inference",
         
         # Application title
         titlePanel("Hypothesis testing"),
         
         # Sidebar with a slider input for number of bins 
         sidebarLayout(
             sidebarPanel(
                 radioButtons("var", "Variable",
                              c("Dosage"="Dosage", "Plate"="Plate",
                                "Molecule" = "Molecule")
                 )
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
             plotOutput("inference")    
             )
         )    
         
         
),

        tabPanel("DoE",
         
         # Application title
         titlePanel("Experimental design"),
         
         # Sidebar with a slider input for number of bins 
         sidebarLayout(
             sidebarPanel(
                 radioButtons("plotType", "Plot type",
                              c("Scatter"="p", "Line"="l")
                 )
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
                 
             )
         )    
         
         
),

        navbarMenu("DIY",
                   
                   tabPanel("Visualisation",
                            
                            # Application title
                            titlePanel("Visualisation"),
                            
                            # Sidebar with a slider input for number of bins 
                            sidebarLayout(
                                sidebarPanel(
                                    radioButtons("plotType", "Plot type",
                                                 c("Scatter plot"="sp", 
                                                   "Histogram/density plot"="hdp",
                                                   "Boxplot" = "bxp")
                                    )
                
                                    
                                ),
                                
                                # Show a plot of the generated distribution
                                mainPanel(
                                    
                                )
                            )    
                            
                            
                   ),
         
                   tabPanel("Hypothesis testing",
                            
                            # Application title
                            titlePanel("Test of Hypothesis"),
                            
                            # Sidebar with a slider input for number of bins 
                            sidebarLayout(
                                sidebarPanel(
                                    radioButtons("plotType", "Plot type",
                                                 c("Scatter"="p", "Line"="l")
                                    )
                                ),
                                
                                # Show a plot of the generated distribution
                                mainPanel(
                                    
                                )
                            )    
                            
                            
                   ),
                   
                   tabPanel("Power analysis",
                            
                            # Application title
                            titlePanel("Power analysis"),
                            
                            # Sidebar with a slider input for number of bins 
                            sidebarLayout(
                                sidebarPanel(
                                    radioButtons("datatype", "Type of data",
                                                 c("Percentage"="p", "Absolute"="l")
                                    ),
                                    radioButtons("Hypot", "Hypothesis:",
                                                 c("One-tailed (directional hypothesis)"="one.sided",
                                                   "Two-tailed (non-directional hypothesis)"="two.sided")
                                    ),
                                    selectInput("value.res", "Looking for...",
                                                  c("Mean difference", "Standard deviation", "Sample size", 
                                                    "Significance level", "Power"
                                                  )
                                    ),
                                    uiOutput("ui2")
                                
                                ),
                                
                                # Show a plot of the generated distribution
                                mainPanel(
                                tableOutput("power")    
                                )
                            )    
                            
                            
                   )
)

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    alldat <- reactive({
        
        #req(input$upload)
        
        inFile <- input$uploadfile 
        dat<-read_xlsx(inFile$datapath, sheet =  1)
        
        dat
        
    })
    
    output$outliers <- renderText({
        
        all.values <- alldat()
        
        reps <- all.values %>%
            group_by(Molecule, Plate, Dosage) %>%
            summarise(med = median(Value), mad = mad(Value), number = n()) %>%
            filter(number > 3)

        rep.values <- left_join(reps, all.values, by = c("Molecule", "Plate", "Dosage")) %>%
            mutate(Outlier = ifelse(abs(Value-med)>5.2*mad, 'yes', 'no'))
        
        paste("There are ", nrow(reps), "groups for which we have more than 3 reps:")
        #print(reps[,c(1:3, 6)])
        
        #cat(paste("Within these groups, there are", sum(rep.values$Outlier=="yes"), "outliers or unexpected observations."), "\n")
        
        #if(sum(rep.values$Outlier=="yes")>1) {
            
        #    cat(paste("These unexpected observations are:"), "\n")
            #print(rep.values[which(rep.values$Outlier=="yes"),])
        #}
        
    })
    
    output$esize <- renderTable({
        
        values <- alldat()
        
        reps <- values %>%
            group_by(Molecule, Plate, Dosage) %>%
            summarise(mean = mean(Value, na.rm = T), sd = sd(Value, na.rm = T), number = n())

        reps.dmso <- reps %>%
            filter(Molecule=="DMSO")
        
        reps.mols <- reps %>%
            filter(Molecule!="DMSO")
                
        rep.values <- left_join(reps.mols, reps.dmso, by = c("Plate")) 
        
        #paste("There are ", nrow(reps), "groups for which we have more than 3 reps:")
        #print(reps[,c(1:3, 6)])
        
        #cat(paste("Within these groups, there are", sum(rep.values$Outlier=="yes"), "outliers or unexpected observations."), "\n")
        
        #if(sum(rep.values$Outlier=="yes")>1) {
        
        #    cat(paste("These unexpected observations are:"), "\n")
        #print(rep.values[which(rep.values$Outlier=="yes"),])
        #}
        
    })
    
    output$heatPlot <- renderPlot({
        
        x    <- alldat()
        
        # draw the histogram with the specified number of bins
        ggplot(x, aes(y= reorder(Row, desc(Row)), x=as.factor(Column), fill = Value))+
            geom_raster() +
            theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
            facet_wrap(~Plate, ncol = 1, dir = "v")
    })
    
    output$rawvis <- renderPlot({
        
        x    <- alldat()
        
        # draw the histogram with the specified number of bins
        ggplot(x[x$Molecule=="DMSO",], aes(x= Value, col = as.factor(Plate)))+
            geom_histogram(aes(y=..density.., fill = as.factor(Plate)), alpha = 0.5, position = "identity")+
            geom_density() + 
            theme_bw()
        
    })
    
    output$descriptive <- renderTable({
        
        values <- alldat()
        
        sum.stats <- values %>%
            group_by(Molecule, Plate, Dosage) %>%
            summarise(mean = mean(Value, na.rm = T), sd = sd(Value , na.rm = T), 
                      median = median(Value, na.rm = T), mad = mad(Value, na.rm = T), number = n())
    
        sum.stats
        
        })
    
    output$corPlot <- renderPlot({
        
        x    <- alldat()
        
        # draw the histogram with the specified number of bins
        ggplot(x, aes(x= log(Dosage), y = Value, col = as.factor(Plate)))+
            geom_point()+
            geom_smooth() +
            theme_bw()
        
    })
    
    
    
    output$inference <- renderPlot({
        
        x    <- alldat()
        
        # draw the histogram with the specified number of bins
        ggplot(x, aes_string(x= sprintf("factor(%s)", input$var), y = "Value"))+
            geom_boxplot() +
            geom_point(position = position_jitter(width = 0.0001)) +
            theme_bw()
        
    })
    
    output$ui2 <- renderUI({
        
        if (is.null(input$value.res))
            return()
            
        buttons <- list(textInput(inputId = "m.dif",
                      label = "Mean difference:",
                      value = 10.0),
            textInput(inputId = "sd",
                      label = "Stand dev:",
                      value = 2.0),
            textInput(inputId = "N",
                      label = "Sample size (per group):",
                      value = 4),
            sliderInput(inputId = "confidence",
                      label = paste("Alpha ", "\u03B1"),
                      min = 0.01, max = 0.1, value = 0.05, width = '300px', post = "%"),
            sliderInput(inputId = "power",
                      label = paste("Power: ", "\u03B2"),
                      min = 0, max = 1, value = 0.8, width = '300px', post = "%"))
        
        switch(input$value.res,
        
            "Mean difference" = buttons[-1],
            "Standard deviation" = buttons[-2],
            "Sample size" = buttons[-3],
            "Significance level" = buttons[-4],
            "Power" = buttons[-5]
        
               )
            
    })

    
    output$power <- renderTable({
        
        params <- list(N = as.numeric(input$N),
                       mdif = as.numeric(input$m.dif), 
                       std = as.numeric(input$sd), 
                       conf = as.numeric(input$confidence), 
                       pwr = as.numeric(input$power),
                       hypot = input$Hypot)
        
        params[which(c("Sample size", "Mean difference", "Standard deviation", "Significance level", "Power")==input$value.res)] <- NULL
        
        analysis <- power.t.test(n = params$N, delta = params$mdif, sd = params$std,
                     sig.level = params$conf, power = params$pwr,
                     alternative = params$hypot)
        
        analysis.tab <- data.frame("Stat" = c("n", "delta", "sd","sig.level", "power"),
                                   "Estimation" = c(analysis$n, analysis$delta, analysis$sd, analysis$sig.level, analysis$power))
        
        analysis.tab
           
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
