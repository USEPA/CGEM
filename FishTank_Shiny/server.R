source('R/funcs.R')

library(ggplot2)
library(dplyr)
library(tidyr)
library(dygraphs)
library(xts)
library(htmltools)
library(shinyjs)

# Define server logic required to generate and plot data
shinyServer(function(input, output, session) {
  
  ## dynamic UI
  
  # create separate reactive value for fl input
  # needed to reset both the ui and reactive value if resetbutton used
  flrv <- reactiveValues(data = NULL)
  observe({
    req(input$myfl)
    flrv$data <- input$myfl
  })
    
  # run model
  runmod <- eventReactive(input$runmod, {
    
    # progress
    progress <- shiny::Progress$new(session, min=1, max=1)
    progress$set(message = 'FishTank is running...')
    on.exit(progress$close())
  
    # format parameter inputs 
    parsin <- form_parinps(input, flrv)

    # format initial condition inputs
    iniin <- form_iniinps(input)

    # p1z1 switch
    p1z1 <- input$p1z1

    # run model
    run_mod(pars = parsin, inps = iniin, out_var = NULL,  p1z1 = p1z1)
      
  })

  # reset values
  observeEvent(input$resetAll, {
    session$sendCustomMessage(type = "resetFileInputHandler", "myfl")
    flrv$data <- NULL
    reset('myfl')
    reset('specifics')
    reset('optics')
    reset('temperature')
    reset('phytoplankton')
    reset('zooplankton')
    reset('organic')
    reset('other')
  })

  # first variable plot
  output$var1plot <- renderDygraph({
     
    alldat <- runmod()
    
    # data to plot
    varsel <- input$var1
    dylog1 <- input$dylog1
    plo_fun(varsel, alldat, logscale = dylog1)
    
    })
  
  # second variable plot
  output$var2plot <- renderDygraph({
     
    alldat <- runmod()

    # data to plot
    varsel <- input$var2
    dylog2 <- input$dylog2
    plo_fun(varsel, alldat, logscale = dylog2)
    
    })
  
  # these let you run the model when the plot tab is not shown
  outputOptions(output, "var1plot", suspendWhenHidden = FALSE)
  outputOptions(output, "var2plot", suspendWhenHidden = FALSE)
  
  ## downloads
  
  # whole plot  
  plotInput <- function(){
      
    # input from ui
    vars_in <- input$vars_in
    logspace <- input$logout
    alldat <- runmod()

    # output
    saveplo_fun(vars_in, alldat, logspace = logspace)
    
  }
  
  # download
  output$downloadplot <- downloadHandler(
    filename = function() { 'FishTank_plots.pdf' },
    content = function(file) {
    
      pdf(file, width = input$width, height =input$height, family = 'serif')
      plotInput()
      dev.off()
      
   }
  )
  
})
