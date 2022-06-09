#
# Statsapp is a Shiny web application developed by Ceferino Varon Gonzalez
# for a preliminary statistical analysis of the lab data.
# 
#
# Please contact Ceferino for any comment or suggestion you might have:
#
#    ceferino.varongonzalez@bayer.com
#

# This Shiny app uses the following libraries:

library(shiny)
library(readxl)
library(tidyverse)

# The UI decomposes the analyses in different tabs,
# each one related to one type of analysis (descriptive, predictive,
# inference & design of experiments) and all using the dataset imported
# in the Descriptive tab,
# plus a number of Do It Yourself analyses for any other dataset imported

# App name

ui <- navbarPage("Statsapp",
     
                 # Descriptive stats tab title
                             
                 tabPanel("Descriptive",
    
    # Descriptive stats title in the tab
    
    titlePanel("Validation and descriptive statistics"),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            
            # Data upload
            
            fileInput('uploadfile', 'Choose .xlsx file',
                      accept = c(".xlsx")
            ),
            
            # Variables selection
            
            helpText(em("Note: Import the dataset to exectute the app")),
            uiOutput("selector"),
            actionButton("update", "Update Data set", class = "btn-primary")
        ),
        
        # Main panel
        mainPanel(
            
            # Heat map on the plates
            
            hr(),
            plotOutput("heatPlot"),
            
            # Histogram-density plot on the plates
            
            hr(),
            plotOutput("rawvis"),
            
            # Outlier detection
            
            hr(),
            textOutput("outliers"),
            
            # Table describing the data
            
            hr(),
            tableOutput("descriptive"),
            
            # Summary table with selected variables
            
            hr(),
            tableOutput("grouping")
        )
    )
),
            
            # Predictive stats tab title

            tabPanel("Predictive",
                     
        # Predictive stats title in the tab
        
        titlePanel("Correlations and ED50"),
                     
        # Sidebar selecting the type of plot to run
        # NOT DOING ANYTHING TODAY: WORK IN PROGRESS
        # In the future, selecting the type of prediction to run
        
        sidebarLayout(
        sidebarPanel(
            
        # Molecule choice
            
        uiOutput("ui_pred")
        ),
                         
        # Show a plot of the generated prediction
        mainPanel(
        plotOutput("corPlot")
       )
     )    
                     
                     
),

        # Inference stats tab title

        tabPanel("Inference",
         
         # Tab title
         titlePanel("Hypothesis testing"),
         
         # Sidebar with a radio button for variable selection 
         sidebarLayout(
             sidebarPanel(
                 radioButtons("var", "Variable",
                              c("Dosage"="Dosage", "Plate"="Plate",
                                "Molecule" = "Molecule")
                 )
             ),
             
             # Show a boxplot for the selected variable
             mainPanel(
             plotOutput("inference")    
             )
         )    
         
         
),

        # DoE stats tab title

        tabPanel("DoE",
         
         # Experimental design tab
         titlePanel("Experimental design"),
         
         # Sidebar selecting the type of plot to run
         # NOT DOING ANYTHING TODAY: WORK IN PROGRESS
         
         sidebarLayout(
             sidebarPanel(
                 radioButtons("plotType", "Plot type",
                              c("Scatter"="p", "Line"="l")
                 )
             ),
             
             # Empty panel today
             mainPanel(
                 
             )
         )    
         
         
),

        # New set of tabs where a particular dataset can be imported
        # for 'do it yourself' analyses
        
        navbarMenu("DIY",
                   
                   # Visualisation tab name
                   
                   tabPanel("Visualisation",
                            
                            # Within-tab title
                            titlePanel("Visualisation"),
                            
                            # Sidebar with a radio-type of button to select vis 
                            sidebarLayout(
                                sidebarPanel(
                                    radioButtons("plotType", "Plot type",
                                                 c("Scatter plot"="sp", 
                                                   "Histogram/density plot"="hdp",
                                                   "Boxplot" = "bxp")
                                    )
                
                                    
                                ),
                                
                                # Empty panel today
                                mainPanel(
                                    
                                )
                            )    
                            
                            
                   ),
         
                   # Inference tab name
                   
                   tabPanel("Hypothesis testing",
                            
                            # Within-tab title
                            titlePanel("Test of Hypothesis"),
                            
                            # Sidebar with a radio-type of button to select vis
                            # doing nothing today
                            
                            sidebarLayout(
                                sidebarPanel(
                                    
                                    textInput('g1name', label = h3("First group:"), value = "Name"),
                                    textInput('g2name', label = h3("Second group:"), value = "Name"),
                                    
                                    textInput('g1', label = h3("First group .xlxs coordinates"), value = "Coord1:Coord2"),
                                    textInput('g2', label = h3("Second group .xlxs coordinates"), value = "Coord1:Coord2"),
                                    
                                    # Data upload
                                    
                                    fileInput('uploadfile2', 'Choose .xlsx file',
                                              accept = c(".xlsx")
                                    ),
                                    
                                    radioButtons("param", "Data distribution",
                                                 c("Parametric"="param", 
                                                   "Non-parametric"="non-param")
                                    )
                                    
                                ),
                                
                                # Empty panel today
                                mainPanel(
                                    plotOutput("diy_infplot"),
                                    hr(),
                                    verbatimTextOutput("stats_diy")
                                )
                            )    
                            
                            
                   ),
                   
                   # Power analysis tab name
                   
                   tabPanel("Power analysis",
                            
                            # Within-tab title
                            
                            titlePanel("Power analysis"),
                            
                            # Sidebar with the options to be chosen
                            # for the power analysis
                            
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
                                
                                # Returns all values
                                mainPanel(
                                tableOutput("power")    
                                )
                            )    
                            
                            
                   )
)

)

# Define server logic required to run all estimates
server <- function(input, output) {
    
    # Function reading and returning the imported file
    
    alldat <- eventReactive(input$update, {
      validate(need(input$uploadfile != "", "Please select a data set (.xlsx format only)"))
      read_xlsx(input$uploadfile$datapath, sheet =  1)
    }, ignoreNULL = FALSE)
    
    # Select a maximum of 2 variables
    
    output$selector <- renderUI({
      selectizeInput("select", "Select columns to display", 
                     choices = colnames(alldat()), 
                     multiple = TRUE, options = list(maxItems=2))
    })
    
    # Select only the 'selector' variables of the dataset
    
    filtereddata <- eventReactive({
      input$update
      alldat()
    },  {
      req(alldat())
      if(is.null(input$select) || input$select == "")
        alldat() else 
          alldat()[, colnames(alldat()) %in% input$select]
    })
    
    # Function reading and returning the imported file for DIY
    
    diydat <- reactive({
        
        #req(input$upload)
        
        inFile <- input$uploadfile2 
        g1 <- read_xlsx(inFile$datapath, sheet =  1, range = input$g1, col_names = F)
        
        g2 <- read_xlsx(inFile$datapath, sheet =  1, range = input$g2, col_names = F)
        
        list(g1, g2, c(input$g1name, input$g2name))
        
    })
    
    # Function estimating the potential outliers
    # STILL IN PROGRES
    
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
    
    # Function estimating the effect sizes
    # WORK IN PROGRESS
    
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
    
    # Heatmap reflecting the plate variability
    
    output$heatPlot <- renderPlot({
        
        x    <- alldat()
        
        ggplot(x, aes(y= reorder(Row, desc(Row)), x=as.factor(Column), fill = Value))+
            geom_raster() +
            theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
            facet_wrap(~Plate, ncol = 1, dir = "v")
    })
    
    # Summary table according to the selected variables
    # In descriptive stats
    
    output$grouping <- renderTable({ 
        
        values <- alldat()
        
        descript <- values %>%
            group_by_at(vars(input$variables)) %>%
            summarise(med = median(Value), mean = mean(Value),
                      sd = sd(Value), mad = mad(Value), 
                      number = n())
        
        descript})
    
    # Histogram/density plot for controls among plates
    
    output$rawvis <- renderPlot({
        
        x    <- alldat()
        
        ggplot(x[x$Molecule=="DMSO",], aes(x= Value, col = as.factor(Plate)))+
            geom_histogram(aes(y=..density.., fill = as.factor(Plate)), alpha = 0.5, position = "identity")+
            geom_density() + 
            theme_bw()
        
    })
    
    # Summary table for all data
    
    output$descriptive <- renderTable({
        
        values <- alldat()
        
        sum.stats <- values %>%
            group_by(Molecule, Plate, Dosage) %>%
            summarise(mean = mean(Value, na.rm = T), sd = sd(Value , na.rm = T), 
                      median = median(Value, na.rm = T), mad = mad(Value, na.rm = T), number = n())
    
        sum.stats
        
        })
    
    # Scatter plot with inference representation
    
    output$corPlot <- renderPlot({
        
        x    <- alldat()
        
        ggplot(x[x$Molecule ==input$Molecule,], aes(x= log(Dosage), y = Value, col = as.factor(Plate)))+
            geom_point()+
            geom_smooth() +
            theme_bw()
        
    })
    
    # Boxplot for selected variable
    
    output$inference <- renderPlot({
        
        x    <- alldat()
        
        ggplot(x, aes_string(x= sprintf("factor(%s)", input$var), y = "Value"))+
            geom_boxplot() +
            geom_point(position = position_jitter(width = 0.0001)) +
            theme_bw()
        
    })
    
    # Interactive panel to get inference for the dosage of each molecule
    
    output$ui_pred <- renderUI({
        
        dataset <- alldat()
        
        selectInput("Molecule", "Molecule",
                    choices = unique(dataset$Molecule)
        )
        
    })
    
    
    # Interactive panel for the power analysis
    
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

    
    # Density plot for DIY inference
    
    output$diy_infplot <- renderPlot({
        
        groups <- diydat()
        
        groups.plot <- data.frame(vals = c(unlist(groups[[1]]), unlist(groups[[2]])), team = c(rep(groups[[3]][1], length(groups[[3]][1])),
                                                                                       rep(groups[[3]][2], length(groups[[3]][2]))))
        
        ggplot(groups.plot, aes(x= vals, col = as.factor(team)))+
            geom_histogram(aes(y=..density.., fill = as.factor(team)), alpha = 0.5, position = "identity")+
            geom_density() + 
            theme_bw()
        
        
    })
    
    # Stat tests for DIY inference
    
    output$stats_diy <- renderPrint({
        
        groups <- diydat()
        
        groups.plot <- data.frame(vals = c(unlist(groups[[1]]), unlist(groups[[2]])), team = c(rep(groups[[3]][1], length(groups[[3]][1])),
                                                                                               rep(groups[[3]][2], length(groups[[3]][2]))))
        
        if (input$param == "param") {
            
            summary(lm(vals~team, groups.plot))
            
        } else {
            
            kruskal.test(vals~team, groups.plot)
            
        }
        
        
    })
    
    
    # Results for the power analysis
    
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
