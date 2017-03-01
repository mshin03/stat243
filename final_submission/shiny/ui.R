
shinyUI(fluidPage(
  
  titlePanel("mfaMKTLT : Plot results"),
  # Margin
  sidebarLayout(
    sidebarPanel(width=3, 
      tags$style(type = 'text/css','form.well {height: 560px; overflow-y: auto; }'),
      
      #ConditionalPanel - 2.Compromise Score
        conditionalPanel("input.tabs === 'Compromise Score'", 
          div(style="height: 80px;",
              numericInput("FS_C1",
              label = "X-axis: Comp",
              min = 1,
              max = 100,
              value = 1))),
                 
         conditionalPanel("input.tabs === 'Compromise Score'", 
          div(style="height: 80px;",
              numericInput("FS_C2",
              label = "Y-axis: Comp",
              min = 1,
              max = 100,
              value = 2))),

          conditionalPanel("input.tabs === 'Compromise Score'", 
            div(style="height: 80px;",
                numericInput("FS_SZ",
                label = "Plot size",
                min = 1,
                max = 20,
                value = 4))),     
      
      #ConditionalPanel - 3.Partial Score
        conditionalPanel("input.tabs === 'Partial Score'", 
          div(style="height: 80px;",
              numericInput("PF_C1",
              label = "X-axis: Comp",
              min = 1,
              max = 100,
              value = 1))),
                 
        conditionalPanel("input.tabs === 'Partial Score'", 
          div(style="height: 80px;",
              numericInput("PF_C2",
              label = "Y-axis: Comp",
              min = 1,
              max = 100,
              value = 2))),
  
        conditionalPanel("input.tabs === 'Partial Score'", 
          div(style="height: 80px;",
              numericInput("PF_SZ",
              label = "Plot size",
              min = 1,
              max = 20,
              value = 4))),
                 
        conditionalPanel("input.tabs === 'Partial Score'", 
          checkboxGroupInput("PF_CB", 
           label = "Select sub-table", 
           choices = list("sub-table01" = 1,"sub-table02" = 2, "sub-table03" = 3,"sub-table04" = 4,
                          "sub-table05" = 5,"sub-table06" = 6,"sub-table07" = 7, "sub-table08" = 8,
                          "sub-table09" = 9,"sub-table10" = 10),
           selected=c(1,2,3,4,5,6,7,8,9,10))),
                 
      #ConditionalPanel - 4. Compromise + Partial Score
        conditionalPanel("input.tabs === 'Compromise + Partial'", 
          div(style="height: 80px;",
              numericInput("PF_F_C1",
              label = "X-axis: Comp",
              min = 1,
              max = 100,
              value = 1))),
                 
        conditionalPanel("input.tabs === 'Compromise + Partial'", 
          div(style="height: 80px;",
              numericInput("PF_F_C2",
              label = "Y-axis: Comp",
              min = 1,
              max = 100,
              value = 2))),

        conditionalPanel("input.tabs === 'Compromise + Partial'", 
          div(style="height: 80px;",
              numericInput("PF_F_SZ",
              label = "Plot size",
              min = 1,
              max = 20,
              value = 4))),
      
      
      #ConditionalPanel - 5.Loadings
        conditionalPanel("input.tabs === 'Loadings'", 
          div(style="height: 80px;",
              numericInput("ML_C1",
              label = "X-axis: Comp",
              min = 1,
              max = 100,
              value = 1))),
                 
         conditionalPanel("input.tabs === 'Loadings'", 
           div(style="height: 80px;",
               numericInput("ML_C2",
               label = "Y-axis: Comp",
               min = 1,
               max = 100,
               value = 2))),
      
         conditionalPanel("input.tabs === 'Loadings'", 
           div(style="height: 80px;",
               numericInput("ML_SZ",
               label = "Plot size",
               min = 1,
               max = 20,
               value = 4))),      
      
         conditionalPanel("input.tabs === 'Loadings'", 
           checkboxGroupInput("ML_CB", label = "Select sub-table", 
            choices = list("sub-table01" = 1,"sub-table02" = 2,"sub-table03" = 3,"sub-table04" = 4,
                           "sub-table05" = 5,"sub-table06" = 6,"sub-table07" = 7,"sub-table08" = 8,
                           "sub-table09" = 9,"sub-table10" = 10),
            selected=c(1,2,3,4,5,6,7,8,9,10))),
      
      #ConditionalPanel - 6. Bootstrap
         conditionalPanel("input.tabs === 'Bootstrap'", 
           div(style="height: 80px;",
               sliderInput("BT_SZ", 
               label = "Sample size",
               min = 0, 
               max = 10000, 
               value = 1000))),
         br(),
         conditionalPanel("input.tabs === 'Bootstrap'", 
           div(style="height: 80px;",
               numericInput("BT_FR",
               label = "Facetrows",
               min = 1,
               max = 100,
               value = 2))),
      
         conditionalPanel("input.tabs === 'Bootstrap'", 
           checkboxGroupInput("BT_CB", label = "Select Components",
                              choices = list("comp1" = 1,"comp2" = 2,"comp3" = 3,"comp4" = 4,
                                             "comp5" = 5,"comp6" = 6,"comp7" = 7,"comp8" = 8,
                                             "comp9" = 9,"comp10" = 10),
                              selected=c(1,2,3,4,5,6,7,8,9,10)))
      
      
    ),
    
    
    mainPanel( width=9,
               tabsetPanel(id = "tabs", 
                           tabPanel("Eigenvalues",br(), plotOutput("EigenPlot")), 
                           tabPanel("Compromise Score",br(), plotOutput("FactorPlot")),
                           tabPanel("Partial Score",br(), plotOutput("PFPlot")),
                           tabPanel("Compromise + Partial",br(), plotOutput("F_PFPlot")),
                           tabPanel("Loadings",br(), plotOutput("MLPlot")),
                           tabPanel("Bootstrap",br(), plotOutput("BTPlot"))
               ))
  )))