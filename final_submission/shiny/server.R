
# This is the server logic for a Shiny web application.
# packages mfaMKTLT should be installed to perform this shiny app.
#

library(shiny)
# NB: set working dir to the location of this file
install.packages("../package/mfaMKTLT_0.3.tar.gz", repos = NULL, type = "source")
library(mfaMKTLT)

shinyServer(function(input, output){

  # do MFA analysis with winedata
  sets.num <- list(c(1:6), c(7:12), c(13:18), c(19:23), c(24:29), c(30:34),
  c(35:38), c(39:44), c(45:49), c(50:53))
  mfa1 <- mfa(winedata, sets.num, ncomps = NULL, center = TRUE,
              scale = 'vector.norm')

  #1. Eigenvalues
  output$EigenPlot <- renderPlot({
    plot(mfa1, type = "eigenvalues")
  })

  #2. Compromise Score
  output$FactorPlot <- renderPlot({
    plot(mfa1, type = "compromise", size = input$FS_SZ,
         xdim = input$FS_C1, ydim = input$FS_C2,
         legend=substr(rownames(mfa1$Fcommon),1,2),
         label=substr(rownames(mfa1$Fcommon),3,3))
  })

  #3. Partial Score
  output$PFPlot <- renderPlot({
    plot(mfa1, type = "partial.factor", size = input$PF_SZ,
         xdim = input$PF_C1, ydim = input$PF_C2,
         subtabs = as.numeric(input$PF_CB),
         legend=substr(rownames(mfa1$Fpartial[[1]]),1,2),
         label=substr(rownames(mfa1$Fcommon),3,3))
  })

  #4. Compromise + Partial
  output$F_PFPlot <- renderPlot({
    plot(mfa1, type = "compromise.partial", size = input$PF_F_SZ,
         xdim = input$PF_F_C1, ydim = input$PF_F_C2,
         legend=substr(rownames(mfa1$Fcommon),1,2),
         label=substr(rownames(mfa1$Fcommon),3,3))
  })

  #5. Loadings
  output$MLPlot <- renderPlot({
    plot(mfa1, type = "loadings", size = input$ML_SZ,
         xdim = input$ML_C1, ydim = input$ML_C2,
         subtabs = as.numeric(input$ML_CB))
  })

  #6. Bootstrap
  output$BTPlot <- renderPlot({
    plot(mfa1, type = "loadings", size = input$ML_SZ,
         xdim = input$ML_C1, ydim = input$ML_C2,
         subtabs = as.numeric(input$ML_CB))
    plot(mfa1, type= "bootstrap",
         bootstrap_size = input$BT_SZ, facetrows=input$BT_FR,
         bootstrap_comps=as.numeric(input$BT_CB))
  })
})
