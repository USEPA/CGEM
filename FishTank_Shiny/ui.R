library(shiny)
library(dygraphs)
library(shinyjs)

source('R/funcs.R')

# Define UI for application
shinyUI(fluidPage(
  
  theme = 'styles.css',
  useShinyjs(),
  
  # main panel for variable selection
  mainPanel(width = 12,
      
    # spacing
    fluidRow(p()),
    
    # top controls  
    fluidRow(
      
      column(width = 2,
        actionButton('runmod', label = img(src = "CGEM-0dLogo-ST.jpg", width = '100%'), width = '100%'),
        actionButton("resetAll", "Reset all", width = '100%')
      ),
      
      column(width = 10, 
        h5('This application runs the FishTank model as part of the Communitity General Ecosystem Model (CGEM).  CGEM calculates a set of bio-geo-chemical equations based on the Eldridge and Roelke model (Eldridge and Roelke 2010). The equations have been enhanced by J. Lehrter (Lehrter et al. 2014). A comprehensive light model has been added by B. Penta of the Naval Research Lab (NRL) (Penta et al., 2008). Hydrodynamics and transport were added by D.S. Ko of NRL (Ko et al. 2008). The model aims to explain the seasonal appearance of a large hypoxic zone off of the Louisiana continental shelf (LCS).  Input parameters are selected on the left tab and the model is run by clicking the button to the left.  Results are viewed as time series plots for 33 different state variables on the right tab.')
      )
     
    ),
    
    fluidRow(p(),
      
      tabsetPanel(
        
        tabPanel("Select parameters", 
          
          ######
          # column(12 , id = 'myfl', 
          #   
          #  h3('Upload external input file'),
          # 
          #   fileInput('myfl', label = NULL),
          #   tags$script('
          #    Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {
          #         var id = "#" + x + "_progress";
          #        var idBar = id + " .bar";
          #         $(id).css("visibility", "hidden");
          #         $(idBar).css("width", "0%");
          #     });
          #   ') ,
          #  h5('Inputs below are inoperable if user file is uploaded, hit "Reset all" above to remove the file.')
          #             
          # ),
            
          ######
          column(width = 6, 
            
            h3('Simulation specifics and switches'),
            wellPanel(id = "specifics", style = "overflow-y:scroll; height:400px; max-height: 400px; background-color:#daf1da", 
              dateRangeInput('times', 'Series length', start = '2006-01-01', end = '2006-12-31'),
              numericInput('dt1', HTML('dT (timestep, seconds)'), 300, step = 60),
              numericInput('dt2', HTML('dT_out (output interval, seconds)'), 86400, step = 60),
              p(strong('Which Fluxes, toggle on(1) off(0): O2 surface flux, CO2 surface flux')),
              fluidRow(
                column(width = 6, selectInput('Which_fluxes_1', label = NULL, choices = list('1' = 1, '0' = 0), width = '1200px', selected = '0')),
                column(width = 6, selectInput('Which_fluxes_2', label = NULL, choices = list('1' = 1, '0' = 0), width = '1200px', selected = '0'))
              ),
              selectInput('p1z1', label = 'One Phytoplankton, One zooplankton: toggle on(1) off(0)', 
                choices = list('1' = T, '0' = F), selected = F),
              selectInput('Which_temperature_1', label = 'Which_temperature: 1==Sigmoidal, 2==Optimum Temp. Thresh., 3==Arrenhius', 
                choices = list('1' = 1, '2' = 2, '3' = 3), selected = '1'),
              selectInput('Which_uptake_1', label = 'Which_uptake: 1==Michaelis-Menten, 2==Geider (needs nfQs), 3==Roelke', 
                choices = list('1' = 1, '2' = 2, '3' = 3), selected = '1'),
              selectInput('Which_quota_1', label = 'Which_quota:  1==Droop, 2==Nyholm, 3==Flynn, Nutrient dependant growth', 
                choices = list('1' = 1, '2' = 2, '3' = 3), selected = '1'),
              selectInput('Which_chlaC_1', label = 'Which_chlaC: 1==Regression, 2==Cloern chl:C', 
                choices = list('1' = 1, '2' = 2), selected = '1'),
              selectInput('Which_photosynthesis_1', label = 'Which_photosynthesis: 1==photoinhibition, 2==without photoinhibition, 3==nutrient dependent', 
                choices = list('1' = 1, '2' = 2, '3' = 3), selected = '2'),
              selectInput('Which_growth_1', label = 'Which_growth: 1==minimum, 2==product formulation, 3==umax is nutrient dependent', 
                choices = list('1' = 1, '2' = 2, '3' = 3), selected = '1')
#L3              selectInput('InitializeHow_1', label = 'InitializeHow: 0==Regression Equations, 1==Initialization File', 
#L3                choices = list('0' = 0, '1' = 1), selected = '1')
            
            )
            
          ),
          
          ######
          column(width = 6, 
            
            h3('Optics'),
            wellPanel(id = "optics", style = "overflow-y:scroll; max-height: 400px; background-color:#daf1da", 
              
              numericInput('astar490_1', HTML('astar490: Chla specific absorption at 490 nm, m<sup>-1</sup> (mg Chla m<sup>-3</sup>)<sup>-1</sup>'), 0.0375, step = 0.01),
              numericInput('aw490_1', HTML('aw490: seawater absorption at 490 nm, m<sup>-1</sup>'), 0.015, step = 0.01),
              numericInput('astarOMA_1', HTML('astarOMA: OM_A specific absorption at 490 nm, m<sup>-1</sup> (mg OM1_A m<sup>-3</sup>)<sup>-1</sup>'), 0.01, step = 0.01),
              numericInput('astarOMZ_1', HTML('astarOMZ: OM_Z specific absorption at 490 nm, m<sup>-1</sup> (mg OM1_Z m<sup>-3</sup>)<sup>-1</sup>'), 0.01, step = 0.01),
#L3              numericInput('astarOMR_1', HTML('astarOMR: OM_R specific absorption at 490 nm, m<sup>-1</sup> (mg OM1_R m<sup>-3</sup>)<sup>-1</sup>'), 0.01, step = 0.01),
              numericInput('astarOMBC_1', HTML('astarOMBC: OM_BC specific absorption at 490 nm, m<sup>-1</sup> (mg OM1_BC m<sup>-3</sup>)<sup>-1</sup>'),0.01, step = 0.01),
              numericInput('PARfac_1', HTML('PARfac: Multiplies surface PAR, dimensionless'), 1, step = 0.1)
#L3              numericInput('sink CDOM_1', HTML('sink CDOM: sinking rate'), 0, step = 0.1)
              )
            
          ),
          
          ######
          column(width = 6, 
            
            h3('Temperature'),
            wellPanel(id = "temperature", style = "overflow-y:scroll; max-height: 400px; background-color:#daf1da", 
              
              p(strong(HTML('Tref(nospA+nospZ): Optimum temperature for growth(C), switch 1, &deg;C'))),
              fluidRow(
                column(width = 3, numericInput('Tref(nospA+nospZ)_1', NULL, 17, step = 1)),
                column(width = 3, numericInput('Tref(nospA+nospZ)_2', NULL, 17, step = 1)),
                column(width = 3, numericInput('Tref(nospA+nospZ)_3', NULL, 17, step = 1)),
                column(width = 3, numericInput('Tref(nospA+nospZ)_4', NULL, 17, step = 1)),
                column(width = 3, numericInput('Tref(nospA+nospZ)_5', NULL, 17, step = 1)),
                column(width = 3, numericInput('Tref(nospA+nospZ)_6', NULL, 17, step = 1)),
                column(width = 3, numericInput('Tref(nospA+nospZ)_7', NULL, 22, step = 1)),
                column(width = 3, numericInput('Tref(nospA+nospZ)_8', NULL, 26, step = 1))
              ), 
              p(strong('KTg1(nospA+nospZ): Effect of T below Topt(C^2), switch 2, dimensionless')),
              fluidRow(
                column(width = 3, numericInput('KTg1(nospA+nospZ)_1', NULL, 0.0035, step = 0.001)),
                column(width = 3, numericInput('KTg1(nospA+nospZ)_2', NULL, 0.0035, step = 0.001)),
                column(width = 3, numericInput('KTg1(nospA+nospZ)_3', NULL, 0.0035, step = 0.001)),
                column(width = 3, numericInput('KTg1(nospA+nospZ)_4', NULL, 0.0035, step = 0.001)),
                column(width = 3, numericInput('KTg1(nospA+nospZ)_5', NULL, 0.0035, step = 0.001)),
                column(width = 3, numericInput('KTg1(nospA+nospZ)_6', NULL, 0.0035, step = 0.001)),
                column(width = 3, numericInput('KTg1(nospA+nospZ)_7', NULL, 0.0035, step = 0.001)),
                column(width = 3, numericInput('KTg1(nospA+nospZ)_8', NULL, 0.0035, step = 0.001))
              ), 
              p(strong('KTg2(nospA+nospZ): Effect of T above Topt(C^2), switch 2, dimensionless')),
              fluidRow(
                column(width = 3, numericInput('KTg2(nospA+nospZ)_1', NULL, 0.001, step = 0.001)),
                column(width = 3, numericInput('KTg2(nospA+nospZ)_2', NULL, 0.001, step = 0.001)),
                column(width = 3, numericInput('KTg2(nospA+nospZ)_3', NULL, 0.001, step = 0.001)),
                column(width = 3, numericInput('KTg2(nospA+nospZ)_4', NULL, 0.001, step = 0.001)),
                column(width = 3, numericInput('KTg2(nospA+nospZ)_5', NULL, 0.001, step = 0.001)),
                column(width = 3, numericInput('KTg2(nospA+nospZ)_6', NULL, 0.001, step = 0.001)),
                column(width = 3, numericInput('KTg2(nospA+nospZ)_7', NULL, 0.001, step = 0.001)),
                column(width = 3, numericInput('KTg2(nospA+nospZ)_8', NULL, 0.001, step = 0.001))
              ),
              p(strong('Ea(nospA+nospZ): Slope of Arrhenius plot, switch 3, eV')),
              fluidRow(
                column(width = 3, numericInput('Ea(nospA+nospZ)_1', NULL, 0.61, step = .1)),
                column(width = 3, numericInput('Ea(nospA+nospZ)_2', NULL, 0.61, step = .1)),
                column(width = 3, numericInput('Ea(nospA+nospZ)_3', NULL, 0.61, step = .1)),
                column(width = 3, numericInput('Ea(nospA+nospZ)_4', NULL, 0.61, step = .1)),
                column(width = 3, numericInput('Ea(nospA+nospZ)_5', NULL, 0.61, step = .1)),
                column(width = 3, numericInput('Ea(nospA+nospZ)_6', NULL, 0.61, step = .1)),
                column(width = 3, numericInput('Ea(nospA+nospZ)_7', NULL, 0.61, step = .1)),
                column(width = 3, numericInput('Ea(nospA+nospZ)_8', NULL, 0.61, step = .1))
              )
              
            )
            
          ),
          
          ######
          column(width = 6, 
            
            h3('Phytoplankton'),
            wellPanel(id = "phytoplankton", style = "overflow-y:scroll; max-height: 400px; background-color:#daf1da", 
              
              p(strong('ediblevector(Z1): edibility vector for Z1, dimensionless')),
              fluidRow(
                column(width = 4, numericInput('ediblevector(Z1)_1', label = NULL, 0.5, width = '200px', step = .1)),
                column(width = 4, numericInput('ediblevector(Z1)_2', label = NULL, 0.5, width = '200px', step = .1)),
                column(width = 4, numericInput('ediblevector(Z1)_3', label = NULL, 0.5, width = '200px', step = .1)),
                column(width = 4, numericInput('ediblevector(Z1)_4', label = NULL, 0.5, width = '200px', step = .1)),
                column(width = 4, numericInput('ediblevector(Z1)_5', label = NULL, 0.5, width = '200px', step = .1)),
                column(width = 4, numericInput('ediblevector(Z1)_6', label = NULL, 0.5, width = '200px', step = .1))
              ),
              p(strong('ediblevector(Z2): edibility vector for Z2, dimensionless')),
              fluidRow(
                column(width = 4, numericInput('ediblevector(Z2)_1', label = NULL, 0.5, width = '200px', step = .1)),
                column(width = 4, numericInput('ediblevector(Z2)_2', label = NULL, 0.5, width = '200px', step = .1)),
                column(width = 4, numericInput('ediblevector(Z2)_3', label = NULL, 0.5, width = '200px', step = .1)),
                column(width = 4, numericInput('ediblevector(Z2)_4', label = NULL, 0.5, width = '200px', step = .1)),
                column(width = 4, numericInput('ediblevector(Z2)_5', label = NULL, 0.5, width = '200px', step = .1)),
                column(width = 4, numericInput('ediblevector(Z2)_6', label = NULL, 0.5, width = '200px', step = .1))
              ),
              p(strong(HTML('umax: maximum growth rate, d<sup>-1</sup>'))),
              fluidRow(
                column(width = 4, numericInput('umax_1', label = NULL, 1.12, width = '200px', step = 0.1)),
                column(width = 4, numericInput('umax_2', label = NULL, 1.12, width = '200px', step = 0.1)),
                column(width = 4, numericInput('umax_3', label = NULL, 1.12, width = '200px', step = 0.1)),
                column(width = 4, numericInput('umax_4', label = NULL, 1.12, width = '200px', step = 0.1)),
                column(width = 4, numericInput('umax_5', label = NULL, 1.12, width = '200px', step = 0.1)),
                column(width = 4, numericInput('umax_6', label = NULL, 1.12, width = '200px', step = 0.1))
              ),
              p(strong(HTML('alpha: initial slope of the photosynthesis-irradiance relationship, cm<sup>2</sup> s quanta<sup>-1</sup> d<sup>-1</sup>'))),
              fluidRow(
                column(width = 4, numericInput('alpha_1', label = NULL, 3.96e-16, width = '200px', step = 1e-17)),
                column(width = 4, numericInput('alpha_2', label = NULL, 3.96e-16, width = '200px', step = 1e-17)),
                column(width = 4, numericInput('alpha_3', label = NULL, 3.96e-16, width = '200px', step = 1e-17)),
                column(width = 4, numericInput('alpha_4', label = NULL, 3.96e-16, width = '200px', step = 1e-17)),
                column(width = 4, numericInput('alpha_5', label = NULL, 3.96e-16, width = '200px', step = 1e-17)),
                column(width = 4, numericInput('alpha_6', label = NULL, 3.96e-16, width = '200px', step = 1e-17))
              ),
              p(strong(HTML('beta: photoinhibition constant, cm<sup>2</sup> s quanta<sup>-1</sup> d<sup>-1</sup>'))),
              fluidRow(
                column(width = 4, numericInput('beta_1', label = NULL, 1.1e-18, width = '200px', step = 1e-19)),
                column(width = 4, numericInput('beta_2', label = NULL, 1.1e-18, width = '200px', step = 1e-19)),
                column(width = 4, numericInput('beta_3', label = NULL, 1.1e-18, width = '200px', step = 1e-19)),
                column(width = 4, numericInput('beta_4', label = NULL, 1.1e-18, width = '200px', step = 1e-19)),
                column(width = 4, numericInput('beta_5', label = NULL, 1.1e-18, width = '200px', step = 1e-19)),
                column(width = 4, numericInput('beta_6', label = NULL, 1.1e-18, width = '200px', step = 1e-19))
              ),
              p(strong('respg: phytoplankton growth respiration coefficient, dimensionless')),
              fluidRow(
                column(width = 4, numericInput('respg_1', label = NULL, 0.1, width = '200px', step = 0.1)),
                column(width = 4, numericInput('respg_2', label = NULL, 0.1, width = '200px', step = 0.1)),
                column(width = 4, numericInput('respg_3', label = NULL, 0.1, width = '200px', step = 0.1)),
                column(width = 4, numericInput('respg_4', label = NULL, 0.1, width = '200px', step = 0.1)),
                column(width = 4, numericInput('respg_5', label = NULL, 0.1, width = '200px', step = 0.1)),
                column(width = 4, numericInput('respg_6', label = NULL, 0.1, width = '200px', step = 0.1))
              ),
              p(strong(HTML('respb: phytoplankton basal respiration coefficient, d<sup>-1</sup>'))),
              fluidRow(
                column(width = 4, numericInput('respb_1', label = NULL, 0.02, width = '200px', step = 0.1)),
                column(width = 4, numericInput('respb_2', label = NULL, 0.02, width = '200px', step = 0.1)),
                column(width = 4, numericInput('respb_3', label = NULL, 0.02, width = '200px', step = 0.1)),
                column(width = 4, numericInput('respb_4', label = NULL, 0.02, width = '200px', step = 0.1)),
                column(width = 4, numericInput('respb_5', label = NULL, 0.02, width = '200px', step = 0.1)),
                column(width = 4, numericInput('respb_6', label = NULL, 0.02, width = '200px', step = 0.1))
              ),
              p(strong(HTML('QminN: minimum N cell-quota, mmol N cell<sup>-1</sup>'))),
              fluidRow(
                column(width = 4, numericInput('QminN_1', label = NULL, 1.53e-10, width = '200px', step = 1e-11)),
                column(width = 4, numericInput('QminN_2', label = NULL, 1.53e-10, width = '200px', step = 1e-11)),
                column(width = 4, numericInput('QminN_3', label = NULL, 1.53e-10, width = '200px', step = 1e-11)),
                column(width = 4, numericInput('QminN_4', label = NULL, 1.53e-10, width = '200px', step = 1e-11)),
                column(width = 4, numericInput('QminN_5', label = NULL, 1.53e-10, width = '200px', step = 1e-11)),
                column(width = 4, numericInput('QminN_6', label = NULL, 1.53e-10, width = '200px', step = 1e-11))
              ),
              p(strong(HTML('QminP: minimum P cell-quota, mmol P cell<sup>-1</sup>'))),
              fluidRow(
                column(width = 4, numericInput('QminP_1', label = NULL, 1.07e-11, width = '200px', step = 1e-12)),
                column(width = 4, numericInput('QminP_2', label = NULL, 1.07e-11, width = '200px', step = 1e-12)),
                column(width = 4, numericInput('QminP_3', label = NULL, 1.07e-11, width = '200px', step = 1e-12)),
                column(width = 4, numericInput('QminP_4', label = NULL, 1.07e-11, width = '200px', step = 1e-12)),
                column(width = 4, numericInput('QminP_5', label = NULL, 1.07e-11, width = '200px', step = 1e-12)),
                column(width = 4, numericInput('QminP_6', label = NULL, 1.07e-11, width = '200px', step = 1e-12))
              ),
              p(strong(HTML('QmaxN: maximum N cell-quota, mmol N cell<sup>-1</sup>'))),
              fluidRow(
                column(width = 4, numericInput('QmaxN_1', label = NULL, 6.85e-09, width = '200px', step = 1e-10)),
                column(width = 4, numericInput('QmaxN_2', label = NULL, 6.85e-09, width = '200px', step = 1e-10)),
                column(width = 4, numericInput('QmaxN_3', label = NULL, 6.85e-09, width = '200px', step = 1e-10)),
                column(width = 4, numericInput('QmaxN_4', label = NULL, 6.85e-09, width = '200px', step = 1e-10)),
                column(width = 4, numericInput('QmaxN_5', label = NULL, 6.85e-09, width = '200px', step = 1e-10)),
                column(width = 4, numericInput('QmaxN_6', label = NULL, 6.85e-09, width = '200px', step = 1e-10))
              ),
              p(strong(HTML('QmaxP: maximum P cell-quota, mmol P cell<sup>-1</sup>'))),
              fluidRow(
                column(width = 4, numericInput('QmaxP_1', label = NULL, 4.28e-10, width = '200px', step = 1e-11)),
                column(width = 4, numericInput('QmaxP_2', label = NULL, 4.28e-10, width = '200px', step = 1e-11)),
                column(width = 4, numericInput('QmaxP_3', label = NULL, 4.28e-10, width = '200px', step = 1e-11)),
                column(width = 4, numericInput('QmaxP_4', label = NULL, 4.28e-10, width = '200px', step = 1e-11)),
                column(width = 4, numericInput('QmaxP_5', label = NULL, 4.28e-10, width = '200px', step = 1e-11)),
                column(width = 4, numericInput('QmaxP_6', label = NULL, 4.28e-10, width = '200px', step = 1e-11))
              ),
              p(strong(HTML('Kn: half-saturation constant for N, mmol N m<sup>-3</sup>'))),
              fluidRow(
                column(width = 4, numericInput('Kn_1', label = NULL, 1.13, width = '200px', step = 1e-2)),
                column(width = 4, numericInput('Kn_2', label = NULL, 1.13, width = '200px', step = 1e-2)),
                column(width = 4, numericInput('Kn_3', label = NULL, 1.13, width = '200px', step = 1e-2)),
                column(width = 4, numericInput('Kn_4', label = NULL, 1.13, width = '200px', step = 1e-2)),
                column(width = 4, numericInput('Kn_5', label = NULL, 1.13, width = '200px', step = 1e-2)),
                column(width = 4, numericInput('Kn_6', label = NULL, 1.13, width = '200px', step = 1e-2))
              ),
              p(strong(HTML('Kp: half-saturation constant for P, mmol P m<sup>-3</sup>'))),
              fluidRow(
                column(width = 4, numericInput('Kp_1', label = NULL, 0.51, width = '200px', step = 1e-2)),
                column(width = 4, numericInput('Kp_2', label = NULL, 0.51, width = '200px', step = 1e-2)),
                column(width = 4, numericInput('Kp_3', label = NULL, 0.51, width = '200px', step = 1e-2)),
                column(width = 4, numericInput('Kp_4', label = NULL, 0.51, width = '200px', step = 1e-2)),
                column(width = 4, numericInput('Kp_5', label = NULL, 0.51, width = '200px', step = 1e-2)),
                column(width = 4, numericInput('Kp_6', label = NULL, 0.51, width = '200px', step = 1e-2))
              ),
              p(strong(HTML('Ksi: half-saturation constant for Si uptake, mmol Si m<sup>-3</sup>'))),
              fluidRow(
                column(width = 4, numericInput('Ksi_1', label = NULL, 1.13, width = '200px', step = 1e-2)),
                column(width = 4, numericInput('Ksi_2', label = NULL, 1.13, width = '200px', step = 1e-2)),
                column(width = 4, numericInput('Ksi_3', label = NULL, 1.13, width = '200px', step = 1e-2)),
                column(width = 4, numericInput('Ksi_4', label = NULL, 1.13, width = '200px', step = 1e-2)),
                column(width = 4, numericInput('Ksi_5', label = NULL, 1.13, width = '200px', step = 1e-2)),
                column(width = 4, numericInput('Ksi_6', label = NULL, 1.13, width = '200px', step = 1e-2))
              ),
              p(strong(HTML('KQn: Qn constant for Flynn nutrient dependent growth model, mmol N m<sup>-3</sup>'))),
              fluidRow(
                column(width = 4, numericInput('KQn_1', label = NULL, 5, width = '200px', step = 0.1)),
                column(width = 4, numericInput('KQn_2', label = NULL, 5, width = '200px', step = 0.1)),
                column(width = 4, numericInput('KQn_3', label = NULL, 5, width = '200px', step = 0.1)),
                column(width = 4, numericInput('KQn_4', label = NULL, 5, width = '200px', step = 0.1)),
                column(width = 4, numericInput('KQn_5', label = NULL, 5, width = '200px', step = 0.1)),
                column(width = 4, numericInput('KQn_6', label = NULL, 5, width = '200px', step = 0.1))
              ),
              p(strong(HTML('KQp: Qp constant for Flynn nutrient dependent growth model, mmol P m<sup>-3</sup>'))),
              fluidRow(
                column(width = 4, numericInput('KQp_1', label = NULL, 0.2, width = '200px', step = 0.1)),
                column(width = 4, numericInput('KQp_2', label = NULL, 0.2, width = '200px', step = 0.1)),
                column(width = 4, numericInput('KQp_3', label = NULL, 0.2, width = '200px', step = 0.1)),
                column(width = 4, numericInput('KQp_4', label = NULL, 0.2, width = '200px', step = 0.1)),
                column(width = 4, numericInput('KQp_5', label = NULL, 0.2, width = '200px', step = 0.1)),
                column(width = 4, numericInput('KQp_6', label = NULL, 0.2, width = '200px', step = 0.1))
              ),
              p(strong('nfQs: exponent for Geider nutrient uptake model, dimensionless')),
              fluidRow(
                column(width = 4, numericInput('nfQs_1', label = NULL, 1, width = '200px', step = 0.1)),
                column(width = 4, numericInput('nfQs_2', label = NULL, 1, width = '200px', step = 0.1)),
                column(width = 4, numericInput('nfQs_3', label = NULL, 1, width = '200px', step = 0.1)),
                column(width = 4, numericInput('nfQs_4', label = NULL, 1, width = '200px', step = 0.1)),
                column(width = 4, numericInput('nfQs_5', label = NULL, 1, width = '200px', step = 0.1)),
                column(width = 4, numericInput('nfQs_6', label = NULL, 1, width = '200px', step = 0.1))
              ),
              p(strong(HTML('vmaxN: N-uptake rate measured at umax, mmol N cell<sup>-1</sup> d<sup>-1</sup>'))),
              fluidRow(
                column(width = 4, numericInput('vmaxN_1', label = NULL, 1.33e-09, width = '200px', step = 1e-10)),
                column(width = 4, numericInput('vmaxN_2', label = NULL, 1.33e-09, width = '200px', step = 1e-10)),
                column(width = 4, numericInput('vmaxN_3', label = NULL, 1.33e-09, width = '200px', step = 1e-10)),
                column(width = 4, numericInput('vmaxN_4', label = NULL, 1.33e-09, width = '200px', step = 1e-10)),
                column(width = 4, numericInput('vmaxN_5', label = NULL, 1.33e-09, width = '200px', step = 1e-10)),
                column(width = 4, numericInput('vmaxN_6', label = NULL, 1.33e-09, width = '200px', step = 1e-10))
              ),
              p(strong(HTML('vmaxP: P-uptake rate measured at umax, mmol P cell<sup>-1</sup> d<sup>-1</sup>'))),
              fluidRow(
                column(width = 4, numericInput('vmaxP_1', label = NULL, 4.07e-10, width = '200px', step = 1e-11)),
                column(width = 4, numericInput('vmaxP_2', label = NULL, 4.07e-10, width = '200px', step = 1e-11)),
                column(width = 4, numericInput('vmaxP_3', label = NULL, 4.07e-10, width = '200px', step = 1e-11)),
                column(width = 4, numericInput('vmaxP_4', label = NULL, 4.07e-10, width = '200px', step = 1e-11)),
                column(width = 4, numericInput('vmaxP_5', label = NULL, 4.07e-10, width = '200px', step = 1e-11)),
                column(width = 4, numericInput('vmaxP_6', label = NULL, 4.07e-10, width = '200px', step = 1e-11))
              ),
              p(strong(HTML('vmaxSi: Si-uptake rate measured at umax, mmol Si cell<sup>-1</sup> d<sup>-1</sup>'))),
              fluidRow(
                column(width = 4, numericInput('vmaxSi_1', label = NULL, 1.33e-09, width = '200px', step = 1e-10)),
                column(width = 4, numericInput('vmaxSi_2', label = NULL, 1.33e-09, width = '200px', step = 1e-10)),
                column(width = 4, numericInput('vmaxSi_3', label = NULL, 1.33e-09, width = '200px', step = 1e-10)),
                column(width = 4, numericInput('vmaxSi_4', label = NULL, 1.33e-09, width = '200px', step = 1e-10)),
                column(width = 4, numericInput('vmaxSi_5', label = NULL, 1.33e-09, width = '200px', step = 1e-10)),
                column(width = 4, numericInput('vmaxSi_6', label = NULL, 1.33e-09, width = '200px', step = 1e-10))
              ),
              p(strong('aN: coefficient for non-limiting nutrient, dimensionless')),
              fluidRow(
                column(width = 4, numericInput('aN_1', label = NULL, 1, width = '200px', step = 0.1)),
                column(width = 4, numericInput('aN_2', label = NULL, 1, width = '200px', step = 0.1)),
                column(width = 4, numericInput('aN_3', label = NULL, 1, width = '200px', step = 0.1)),
                column(width = 4, numericInput('aN_4', label = NULL, 1, width = '200px', step = 0.1)),
                column(width = 4, numericInput('aN_5', label = NULL, 1, width = '200px', step = 0.1)),
                column(width = 4, numericInput('aN_6', label = NULL, 1, width = '200px', step = 0.1))
              ),
              p(strong(HTML('volcell: phytoplankton volume/cell, &#181;m<sup>3</sup>'))),
              fluidRow(
                column(width = 4, numericInput('volcell_1', label = NULL, 513, width = '200px', step = 10)),
                column(width = 4, numericInput('volcell_2', label = NULL, 513, width = '200px', step = 10)),
                column(width = 4, numericInput('volcell_3', label = NULL, 513, width = '200px', step = 10)),
                column(width = 4, numericInput('volcell_4', label = NULL, 513, width = '200px', step = 10)),
                column(width = 4, numericInput('volcell_5', label = NULL, 513, width = '200px', step = 10)),
                column(width = 4, numericInput('volcell_6', label = NULL, 513, width = '200px', step = 10))
              ),
              p(strong(HTML('Qc: phytoplankton carbon/cell, mmol C cell<sup>-1</cell>'))),
              fluidRow(
                column(width = 4, numericInput('Qc_1', label = NULL, 4.54e-08, width = '200px', step = 1e-9)),
                column(width = 4, numericInput('Qc_2', label = NULL, 4.54e-08, width = '200px', step = 1e-9)),
                column(width = 4, numericInput('Qc_3', label = NULL, 4.54e-08, width = '200px', step = 1e-9)),
                column(width = 4, numericInput('Qc_4', label = NULL, 4.54e-08, width = '200px', step = 1e-9)),
                column(width = 4, numericInput('Qc_5', label = NULL, 4.54e-08, width = '200px', step = 1e-9)),
                column(width = 4, numericInput('Qc_6', label = NULL, 4.54e-08, width = '200px', step = 1e-9))
              ),
              p(strong(HTML('Athresh: Phytoplankton threshold for grazing, is multiplied by VOLcell, cells m<sup>-3</sup>'))),
              fluidRow(
                column(width = 4, numericInput('Athresh_1', label = NULL, 7e7, width = '200px', step = 1e6)),
                column(width = 4, numericInput('Athresh_2', label = NULL, 7e7, width = '200px', step = 1e6)),
                column(width = 4, numericInput('Athresh_3', label = NULL, 7e7, width = '200px', step = 1e6)),
                column(width = 4, numericInput('Athresh_4', label = NULL, 7e7, width = '200px', step = 1e6)),
                column(width = 4, numericInput('Athresh_5', label = NULL, 7e7, width = '200px', step = 1e6)),
                column(width = 4, numericInput('Athresh_6', label = NULL, 7e7, width = '200px', step = 1e6))
              ),
#L3              p(strong(HTML('sink A: sinking rate of phytoplankton cells'))),
#L3              fluidRow(
#L3                column(width = 4, numericInput('sink A_1', label = NULL, 0.29, width = '200px', step = 0.1)),
#L3                column(width = 4, numericInput('sink A_2', label = NULL, 0.29, width = '200px', step = 0.1)),
#L3                column(width = 4, numericInput('sink A_3', label = NULL, 0.29, width = '200px', step = 0.1)),
#L3                column(width = 4, numericInput('sink A_4', label = NULL, 0.29, width = '200px', step = 0.1)),
#L3                column(width = 4, numericInput('sink A_5', label = NULL, 0.29, width = '200px', step = 0.1)),
#L3                column(width = 4, numericInput('sink A_6', label = NULL, 0.29, width = '200px', step = 0.1))
#L3              ),
              p(strong(HTML('mA: mortality coefficient, d<sup>-1</sup>'))),
              fluidRow(
                column(width = 4, numericInput('mA_1', label = NULL, 0.11, width = '200px', step = 0.1)),
                column(width = 4, numericInput('mA_2', label = NULL, 0.11, width = '200px', step = 0.1)),
                column(width = 4, numericInput('mA_3', label = NULL, 0.11, width = '200px', step = 0.1)),
                column(width = 4, numericInput('mA_4', label = NULL, 0.11, width = '200px', step = 0.1)),
                column(width = 4, numericInput('mA_5', label = NULL, 0.11, width = '200px', step = 0.1)),
                column(width = 4, numericInput('mA_6', label = NULL, 0.11, width = '200px', step = 0.1))
              ),
              p(strong('A_wt: relative proportion of total Chla for initializing phytoplankton, dimensionless')),
              fluidRow(
                column(width = 4, numericInput('A_wt_1', label = NULL, 1, width = '200px', step = 0.1)),
                column(width = 4, numericInput('A_wt_2', label = NULL, 1, width = '200px', step = 0.1)),
                column(width = 4, numericInput('A_wt_3', label = NULL, 1, width = '200px', step = 0.1)),
                column(width = 4, numericInput('A_wt_4', label = NULL, 1, width = '200px', step = 0.1)),
                column(width = 4, numericInput('A_wt_5', label = NULL, 1, width = '200px', step = 0.1)),
                column(width = 4, numericInput('A_wt_6', label = NULL, 1, width = '200px', step = 0.1))
              )
              
            )
            
          ),
          
          ######
          column(width = 6, 
          
            h3('Zooplankton'),
            wellPanel(id = "zooplankton", style = "overflow-y:scroll; max-height: 400px; background-color:#daf1da", 
              
              p(strong('Zeffic: assimilation efficiency as a fraction of ingestion, dimensionless')),
              fluidRow(
                column(width = 6, numericInput('Zeffic_1', label = NULL, 0.4, width = '1200px', step = 0.01)),
                column(width = 6, numericInput('Zeffic_2', label = NULL, 0.4, width = '1200px', step = 0.01))
              ),
              p(strong('Zslop: proportion of grazed phytoplankton lost to sloppy feeding, dimensionless')),
              fluidRow(
                column(width = 6, numericInput('Zslop_1', label = NULL, 0.25, width = '1200px', step = 0.01)),
                column(width = 6, numericInput('Zslop_2', label = NULL, 0., width = '1200px', step = 0.01))
              ),
              p(strong(HTML('Zvolcell: zooplankton volume/individual, &#181;m<sup>3</sup> individual<sup>-1</sup>'))),
              fluidRow(
                column(width = 6, numericInput('Zvolcell_1', label = NULL, 2.98e7, width = '1200px', step = 1e6)),
                column(width = 6, numericInput('Zvolcell_2', label = NULL, 6.74e5, width = '1200px', step = 1e4))
              ),
              p(strong(HTML('ZQc: zooplankton carbon/individual, mmol C individual<sup>-1</sup>'))),
              fluidRow(
                column(width = 6, numericInput('ZQc_1', label = NULL, 3.13e-4, width = '1200px', step = 1e-5)),
                column(width = 6, numericInput('ZQc_2', label = NULL, 7.08e-7, width = '1200px', step = 1e-8))
              ),
              p(strong(HTML('ZQn: zooplankton nitrogen/individual, mmol N individual<sup>-1</sup>'))),
              fluidRow(
                column(width = 6, numericInput('ZQn_1', label = NULL, 6.95e-05, width = '1200px', step = 1e-6)),
                column(width = 6, numericInput('ZQn_2', label = NULL, 1.57e-07, width = '1200px', step = 1e-8))
              ),
              p(strong(HTML('ZQp: zooplankton phosphorus/individual, mmol P individual<sup>-1</sup>'))),
              fluidRow(
                column(width = 6, numericInput('ZQp_1', label = NULL, 3.77e-06, width = '1200px', step = 1e-7)),
                column(width = 6, numericInput('ZQp_2', label = NULL, 8.53e-09, width = '1200px', step = 1e-10))
              ),
              p(strong(HTML('ZKa: half saturation coefficient for grazing, &#181;m<sup>3</sup> m<sup>-3</sup>'))),
              fluidRow(
                column(width = 6, numericInput('ZKa_1', label = NULL, 1.12e12, width = '1200px', step = 1e11)),
                column(width = 6, numericInput('ZKa_2', label = NULL, 1.12e12, width = '1200px', step = 1e11))
              ),
              p(strong('Zrespg: Zooplankton growth-dependent respiration factor, dimensionless')),
              fluidRow(
                column(width = 6, numericInput('Zrespg_1', label = NULL, 0.2, width = '1200px', step = 0.01)),
                column(width = 6, numericInput('Zrespg_2', label = NULL, 0.3, width = '1200px', step = 0.01))
              ),
              p(strong(HTML('Zrespb: Zooplankton biomass-dependent respiration factor, d<sup>-1</sup>'))),
              fluidRow(
                column(width = 6, numericInput('Zrespb_1', label = NULL, 0.1, width = '1200px', step = 0.01)),
                column(width = 6, numericInput('Zrespb_2', label = NULL, 0.416, width = '1200px', step = 0.01))
              ),
              p(strong(HTML('Zumax: maximum growth rate of zooplankton, &#181;m<sup>3</sup> individual<sup>-1</sup> d<sup>-1</sup>'))),
              fluidRow(
                column(width = 6, numericInput('Zumax_1', label = NULL, 9.45e8, width = '1200px', step = 1e6)),
                column(width = 6, numericInput('Zumax_2', label = NULL, 2.98e7, width = '1200px', step = 1e5))
              ),
              p(strong(HTML('Zm: Zooplankton mortality constant for quadratic mortality, m<sup>6</sup> ind<sup>-2</sup> d<sup>-1</sup>'))),
              fluidRow(
                column(width = 6, numericInput('Zm_1', label = NULL, 0.00072, width = '1200px', step = 0.0001)),
                column(width = 6, numericInput('Zm_2', label = NULL, 0.00072, width = '1200px', step = 0.0001))
              )
              
            )
            
          ),
          
          ######
          column(width = 6, 
            
            h3('Organic Matter'),
            wellPanel(id = "organic", style = "overflow-y:scroll; max-height: 400px; background-color:#daf1da", 
              
              numericInput('KG1_1', HTML('KG1: turnover rate for OM1_A and OM1_G, y<sup>-1</sup>'), 30, step = 1),	
              numericInput('KG2_1', HTML('KG2: turnover rate for OM2_A and OM2_G, y<sup>-1</sup>'), 30, step = 1),
              numericInput('KG1_BC_1', HTML('KG1_BC: turnover rate for boundary condition OM1, y<sup>-1</sup>'), 1, step = 1),	
              numericInput('KG2_BC_1', HTML('KG2_BC: turnover rate for boundary condition OM2, y<sup>-1</sup>'), 1, step = 1),
              numericInput('KNH4_1', HTML('KNH4: NH4 rate constant for nitrification, mmol m<sup>-3</sup>'), 1, step = .1),
              numericInput('nitmax_1', HTML('nitmax: maximum rate of nitrification per day, mmol m<sup>-3</sup> d<sup>-1</day>'), 0.52, step = 0.1),
              numericInput('KO2_1', HTML('KO2: half-saturation concentration for O2 utilization, mmol m<sup>-3</sup>'), 10, step = 1),
              numericInput('KstarO2_1', HTML('KstarO2: O2 concentration that inhibits denitrification, mmol m<sup>-3</sup>'), 10, step = 1),
              numericInput('KNO3_1', HTML('KNO3: half-saturation concentration for NO3 used in denitrification, mmol m<sup>-3</sup>'), 5, step = 1),
              numericInput('pCO2_1', HTML('pCO2: atmospheric CO2, ppm'), 380, step = 1),
              numericInput('KGcdom_1', HTML('KGcdom: decay rate of CDOM, d<sup>-1</sup>'), 0.01, step = 0.01)
              
            )
            
          )

        ), 
        
        tabPanel("Select initial conditions", 
          
          ######
          column(width = 12, 
            
            h3('Initial conditions'),
            wellPanel(id = "other", style = "overflow-y:scroll; max-height: 800px; background-color:#daf1da", 
           
              p(strong('Phytoplankton abundance (cells m-3), A1:A6')),
              fluidRow(
                column(width = 2, numericInput('A1', label = NULL, 6E7, width = '200px', step = 1e7)),
                column(width = 2, numericInput('A2', label = NULL, 6E7, width = '200px', step = 1e7)),
                column(width = 2, numericInput('A3', label = NULL, 6E7, width = '200px', step = 1e7)),
                column(width = 2, numericInput('A4', label = NULL, 6E7, width = '200px', step = 1e7)),
                column(width = 2, numericInput('A5', label = NULL, 6E7, width = '200px', step = 1e7)),
                column(width = 2, numericInput('A6', label = NULL, 6E7, width = '200px', step = 1e7))
              ),
              
              p(strong('Phytoplankton cell quota for nitrogen, Qn1:Qn6')),
              fluidRow(
                column(width = 2, numericInput('Qn1', label = NULL, 1.54e-10, width = '200px', step = 1e-10)),
                column(width = 2, numericInput('Qn2', label = NULL, 1.54e-10, width = '200px', step = 1e-10)),
                column(width = 2, numericInput('Qn3', label = NULL, 1.54e-10, width = '200px', step = 1e-10)),
                column(width = 2, numericInput('Qn4', label = NULL, 1.54e-10, width = '200px', step = 1e-10)),
                column(width = 2, numericInput('Qn5', label = NULL, 1.54e-10, width = '200px', step = 1e-10)),
                column(width = 2, numericInput('Qn6', label = NULL, 1.54e-10, width = '200px', step = 1e-10))
              ),
              
              p(strong('Phytoplankton cell quota for phosphorus, Qp1:Qp6')),
              fluidRow(
                column(width = 2, numericInput('Qp1', label = NULL, 1.08e-11, width = '200px', step = 1e-11)),
                column(width = 2, numericInput('Qp2', label = NULL, 1.08e-11, width = '200px', step = 1e-11)),
                column(width = 2, numericInput('Qp3', label = NULL, 1.08e-11, width = '200px', step = 1e-11)),
                column(width = 2, numericInput('Qp4', label = NULL, 1.08e-11, width = '200px', step = 1e-11)),
                column(width = 2, numericInput('Qp5', label = NULL, 1.08e-11, width = '200px', step = 1e-11)),
                column(width = 2, numericInput('Qp6', label = NULL, 1.08e-11, width = '200px', step = 1e-11))
              ),
              
              fluidRow(
                column(width = 4, numericInput('G1', label = 'Zooplankton group 1 (individuals/m3)', 1500, width = '400px', step = 100)),
                column(width = 4, numericInput('G2', label = 'Zooplankton group 2 (individuals/m3)', 1500, width = '400px', step = 100)),
                column(width = 4, numericInput('Si', label = 'Si, silica (mmol/m3)', 71, width = '400px', step = 1))
              ),
              
              fluidRow(
                column(width = 4, numericInput('NO3', label = 'NO3: nitrate (mmol/m3)', 71, width = '400px', step = 1)),
                column(width = 4, numericInput('NH4', label = 'NH4: ammonium (mmol/m3)', 1.1, width = '400px', step = 0.1)),
                column(width = 4, numericInput('PO4', label = 'PO4: phosphate (mmol/m3)', 1.8, width = '400px', step = 0.1))
              ),

              fluidRow(
                column(width = 4, numericInput('DIC', label = 'DIC: dissolved inorganic carbon (mmol/m3)', 2130, width = '400px', step = 10)),
		column(width = 4, numericInput('O2', label = 'O2: dissolved oxygen (mmol/m3)', 172, width = '400px', step = 1)),
                column(width = 4, numericInput('ALK', label = 'ALK: alkalinity', 2523, width = '400px', step = 10))
  		),

	      fluidRow(
                column(width = 4, numericInput('OM1_A', label = 'OM1_A: particulate organic matter phytoplankton (mmolC/m3)', 0, width = '400px', step = 1)),
                column(width = 4, numericInput('OM1_fp', label = 'OM1_Z: particulate organic matter fecal pellets (mmolC/m3)', 0, width = '400px', step = 1)),
		column(width = 4, numericInput('OM1_bc', label = 'OM1_bc: organic matter boundary conditions (mmolC/m3)', 0, width = '400px', step = 1))
  		),
              
              fluidRow(
                column(width = 4, numericInput('OM2_A', label = 'OM2_A: dissolved organic matter phytoplankton (mmolC/m3)', 0, width = '400px', step = 1)),
                column(width = 4, numericInput('OM2_fp', label = 'OM2_Z: dissolved organic matter fecal pellets (mmolC/m3)', 0, width = '400px', step = 1)),
                column(width = 4, numericInput('OM2_bc', label = 'OM2_bc: dissolved organic matter boundary conditions (mmolC/m3)', 0, width = '400px', step = 1))
  		),
              
              fluidRow(
                column(width = 4, numericInput('CDOM', label = 'CDOM, chromophoric dissolved organic matter (ppb)', 20, width = '400px', step = 1)),
                column(width = 4, numericInput('OM1_rp', label = 'OM1_rp: particulate organic matter riverine (mmolC/m3)', 0, width = '400px', step = 1)),
                column(width = 4, numericInput('OM2_rp', label = 'OM2_rp, dissolved organic matter riverine (mmolC/m3)', 0, width = '400px', step = 1))
              )

            )
            
          )
          
        ), 
        
        tabPanel("Model output", 
          
          # first row of plot options
          fluidRow(
            
            p(),
            
            column(width = 6, 
              selectInput(inputId = 'var1',
                label = NULL,
                choices = '',
                width = '600px'
                )
              ),
            
            column(width = 1,
              h5("Log-space")
              ),
            
            column(width = 1,
              selectInput('dylog1', label = NULL, 
                choices = list('yes' = T, 'no' = F), selected = F)
              )
            
            ),
          
          # first plot
          fluidRow(
            
            column(width = 12,
                                     
              dygraphOutput("var1plot", height = "300px", width = "1200px")
            
              )
            
            ),
            
          # second row of plot options
          fluidRow(
            
              p(),
              
              column(width = 6,
                selectInput(inputId = 'var2',
                  label = NULL,
                  choices = '', 
                  width = '600px'
                )
              ),
      
              column(width = 1, 
                h5("Log-space")
                ),
              
              column(width = 1,
                selectInput('dylog2', label = NULL, 
                  choices = list('yes' = T, 'no' = F), selected = F)
              )
            
            ),
             
          # second plot  
          fluidRow(
            
            column(width = 12, 
              
              dygraphOutput("var2plot", height = "300px", width = "1200px")
          
            )
          
          )
            
        ),
        
        tabPanel("Save plots", 
          
          #
          fluidRow(
            
            # variables to plot
            column(width = 6,
              
              p(), 
              h3('Select variables to plot'),
              wellPanel(id = "variables", style = "overflow-y:scroll; height:400px; max-height: 400px; background-color:#daf1da", 
              
              checkboxGroupInput(inputId = 'vars_in', label = NULL, choices = NULL)
                                     
              )
            
            ), 
            
            # plot dimensions
            column(width = 6,
              
              p(), 
              h3('Set plot dimensions and save'),
              wellPanel(id = "dimensions", style = "overflow-y:scroll; height:400px; max-height: 400px; background-color:#daf1da", 
              
              numericInput('height', 'Plot height (in)', value = 4, min = 0, step = 1),
              numericInput('width', 'Plot width (in)', value = 8, min = 0, step = 1),
              selectInput('logout', label = 'Log-space', 
                  choices = list('yes' = T, 'no' = F), selected = F),
              HTML('<p><b>Save to home directory</b></p>'),
              downloadButton('downloadplot','click')
                                     
              )
            
            )
            
          )
            
        )
        
      )
        
    )
              
  )
  
))
