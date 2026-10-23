###############################################################################
# FLEXSEM DEB TOOLBOX
# run_deb_toolbox_interactive.R
#
# PURPOSE
# -------
# Interactive graphical launcher for the FLEXSEM DEB Toolbox.
#
# This interface allows the user to select workflow and analysis options
# before launching the toolbox.
#
# The launcher:
#
#   • Opens immediately
#   • Selects workflow and analysis options
#   • Loads the 3D farm structure only when Custom mode is selected
#   • Builds Custom spatial-selection controls dynamically
#   • Returns selected settings and, when available, the farm definition
#
# The processing engine is launched separately by run_DEB_toolbox.R.
#
# Author : Paulo F. Lagos
# Date   : 07 July 2026
###############################################################################

run_deb_toolbox_interactive <- function() {
  
  #---------------------------------------------------------------------------
  # Check required packages
  #---------------------------------------------------------------------------
  
  if (!requireNamespace("shiny", quietly = TRUE)) {
    
    stop(
      "Package 'shiny' is required for the interactive launcher."
    )
    
  }
  
  
  if (!requireNamespace("miniUI", quietly = TRUE)) {
    
    stop(
      "Package 'miniUI' is required for the interactive launcher."
    )
    
  }
  
  
  
  #---------------------------------------------------------------------------
  # Interface assets
  #---------------------------------------------------------------------------
  
  assets_folder <- normalizePath(
    "R_scripts/functions/interface/assets",
    winslash = "/",
    mustWork = TRUE
  )
  
  
  shiny::addResourcePath(
    "deb_toolbox_assets",
    assets_folder
  )

###############################################################################
# Load configuration
###############################################################################
  
  source(
    "R_scripts/00_get_project.R",
    local = FALSE
  )
  
  source(
    "R_scripts/01_get_config_DEBV2.R",
    local = FALSE
  )
  
  source(
    "R_scripts/01B_get_simulation_catalog.R",
    local = FALSE
  )
  
  cat("\n--- SIMULATION CATALOG AT UI STARTUP ---\n")
  print(simulation_catalog)
  cat("----------------------------------------\n\n")
  #---------------------------------------------------------------------------
  # User interface
  #---------------------------------------------------------------------------
  
  ui <- miniUI::miniPage(
    
    
    #-------------------------------------------------------------------------
    # Gadget title bar
    #-------------------------------------------------------------------------
    
    miniUI::gadgetTitleBar(
      "FLEXSEM DEB Toolbox"
    ),
    
    
    #-------------------------------------------------------------------------
    # Custom interface styling
    #-------------------------------------------------------------------------
    
    shiny::tags$head(
      
      shiny::tags$style(
        
        shiny::HTML(
          "
          
          body {
            background-color: #f5f7fa;
          }
          
          .toolbox-header {
            background-color: white;
            border: 1px solid #e1e5ea;
            border-radius: 8px;
            padding: 18px 22px;
            margin-bottom: 18px;
          }
          
          .logo-row {
            display: flex;
            align-items: center;
            justify-content: space-between;
            gap: 30px;
          }
          
          .au-logo {
            max-width: 260px;
            max-height: 85px;
            object-fit: contain;
          }
          
          .flexsem-logo {
            max-width: 260px;
            max-height: 85px;
            object-fit: contain;
          }
          
          .toolbox-title {
            text-align: center;
            margin-top: 18px;
          }
          
          .toolbox-title h2 {
            margin-bottom: 5px;
            font-weight: 600;
            letter-spacing: 0.5px;
          }
          
          .toolbox-subtitle {
            color: #667085;
            font-size: 15px;
            margin-bottom: 0;
          }
          
          .toolbox-card {
            background-color: white;
            border: 1px solid #e1e5ea;
            border-radius: 8px;
            padding: 18px 22px;
            margin-bottom: 16px;
          }
          
          .toolbox-card h4 {
            margin-top: 0;
            margin-bottom: 15px;
          }
          
          .custom-analysis-card {
            background-color: white;
            border: 1px solid #e1e5ea;
            border-radius: 8px;
            padding: 18px 22px;
            margin-top: 5px;
            margin-bottom: 16px;
          }
          
          .toolbox-footer {
            text-align: center;
            color: #667085;
            font-size: 12px;
            padding: 10px 0 5px 0;
          }
          
          .toolbox-footer strong {
            color: #344054;
          }
          
          "
        )
        
      )
      
    ),
    
    
    #-------------------------------------------------------------------------
    # Main content
    #-------------------------------------------------------------------------
    
    miniUI::miniContentPanel(
      
      
      #-----------------------------------------------------------------------
      # Branded header
      #-----------------------------------------------------------------------
      
      shiny::div(
        
        class = "toolbox-header",
        
        
        shiny::div(
          
          class = "logo-row",
          
          
          shiny::tags$img(
            
            src =
              "deb_toolbox_assets/aarhus-university-logo-png_seeklogo-480775.png",
            
            class = "au-logo"
            
          ),
          
          
          shiny::tags$img(
            
            src =
              "deb_toolbox_assets/logo3d100px.png",
            
            class = "flexsem-logo"
            
          )
          
        ),
        
        
        shiny::div(
          
          class = "toolbox-title",
          
          shiny::h2(
            "FLEXSEM DEB TOOLBOX"
          ),
          
          shiny::p(
            class = "toolbox-subtitle",
            "Farm-scale DEB model post-processing and analysis"
          )
          
        )
        
      ),
      
      
      #-----------------------------------------------------------------------
      # Main controls
      #-----------------------------------------------------------------------
      
      shiny::div(
        
        class = "toolbox-card",
        
        
        shiny::fluidRow(
          
          
          #-------------------------------------------------------------------
          # Workflow options
          #-------------------------------------------------------------------
          
          shiny::column(
            
            width = 5,
            
            shiny::h4(
              "Workflow"
            ),
            
            
            shiny::checkboxInput(
              "run_read",
              "Read model outputs",
              value = TRUE
            ),
            
            
            shiny::checkboxInput(
              "run_plots",
              "Generate figures",
              value = TRUE
            ),
            
            
            shiny::checkboxInput(
              "run_tables",
              "Generate tables",
              value = FALSE
            ),
            
            
            shiny::checkboxInput(
              "run_animations",
              "Generate animations",
              value = FALSE
            )
            
          ),
          
          
          #------------------------------------------------------------------
          # Simulation
          #------------------------------------------------------------------
          shiny::column(
            
            width = 7,
            
            shiny::h4("Simulations"),
            
            shiny::checkboxGroupInput(
              
              "selected_simulations",
              
              label = NULL,
              
              choices = simulation_catalog$SimulationID,
              
              selected = simulation_catalog$SimulationID
              
            )
            
          ),
          
          #-------------------------------------------------------------------
          # Analysis mode
          #-------------------------------------------------------------------
          
          shiny::column(
            
            width = 7,
            
            shiny::h4(
              "Analysis mode"
            ),
            
            
            shiny::radioButtons(
              
              "analysis_mode",
              
              label = NULL,
              
              choices = c(
                
                "Whole farm" =
                  "whole_farm",
                
                "By depth" =
                  "by_depth",
                
                "By mask" =
                  "by_mask",
                
                "By mask and depth" =
                  "by_mask_depth",
                
                "Custom" =
                  "custom"
                
              ),
              
              selected = "by_mask_depth"
              
            )
            
          )
          
        )
        
      ),
      
      
      #-----------------------------------------------------------------------
      # Custom analysis controls
      #-----------------------------------------------------------------------
      
      shiny::conditionalPanel(
        
        condition =
          "input.analysis_mode == 'custom'",
        
        shiny::div(
          
          class = "custom-analysis-card",
          
          shiny::h4(
            "Custom analysis"
          ),
          
          shiny::uiOutput(
            "custom_analysis_ui"
          )
    
        )
        
      ),
      
      #-----------------------------------------------------------------------
      # Author footer
      #-----------------------------------------------------------------------
      
      shiny::div(
        
        class = "toolbox-footer",
        
        shiny::HTML(
          paste0(
            "<strong>Developed by Paulo F. Lagos</strong>",
            "<br>",
            "Aarhus University",
            "<br>",
            "FLEXSEM DEB Toolbox · 2026"
          )
        )
        
      )
      
    )
    
  )
  
  
  #---------------------------------------------------------------------------
  # Server
  #---------------------------------------------------------------------------
  
  server <- function(input,
                     output,
                     session) {
    
    #-------------------------------------------------------------------------
    # 1. Farm structure storage
    #-------------------------------------------------------------------------
    
    farm_cells_data <- shiny::reactiveVal(
      NULL
    )
    
    #-------------------------------------------------------------------------
    # 2. Lazy farm-structure loading
    #-------------------------------------------------------------------------
    
    shiny::observeEvent(
      
      input$analysis_mode,
      
      {
        
        if (
          input$analysis_mode != "whole_farm" &&
          is.null(farm_cells_data())
        ) {
          
          shiny::withProgress(
            
            message = "Loading farm structure...",
            
            value = 0,
            
            {
              
              shiny::incProgress(
                0.2,
                detail = "Loading project configuration"
              )
              
              source(
                "R_scripts/00_get_project.R",
                local = FALSE
              )
              
              
              shiny::incProgress(
                0.4,
                detail = "Loading model-output configuration"
              )
              
              source(
                "R_scripts/01_get_config_DEBV2.R",
                local = FALSE
              )
              
              
              shiny::incProgress(
                0.6,
                detail = "Loading spatial functions"
              )
              
              spatial_files <- list.files(
                "R_scripts/functions/spatial",
                pattern = "\\.R$",
                full.names = TRUE
              )
              
              invisible(
                lapply(
                  spatial_files,
                  source
                )
              )
              
              
              shiny::incProgress(
                0.8,
                detail = "Reading farm masks"
              )
              
              farm_base_folder <- dirname(
                master_folder
              )
              
              cells <- build_farm_cells(
                project = project,
                base_folder = farm_base_folder
              )
              
              
              farm_cells_data(
                cells
              )
              
              shiny::incProgress(
                1,
                detail = "Farm structure ready"
              )
              
            }
            
          )
          
        }
        
      },
      
      ignoreInit = TRUE
      
    )
    #-------------------------------------------------------------------------
    # 4. Cancel
    #-------------------------------------------------------------------------
    
    shiny::observeEvent(
      
      input$cancel,
      
      {
        
        shiny::stopApp(
          NULL
        )
        
      }
      
    )
    
    
    #-------------------------------------------------------------------------
    # 5. Done
    #-------------------------------------------------------------------------
    
    shiny::observeEvent(
      
      input$done,
      
      {
        
        settings <- list(
          
          RUN_READ =
            input$run_read,
          
          RUN_PLOTS =
            input$run_plots,
          
          RUN_TABLES =
            input$run_tables,
          
          RUN_ANIMATIONS =
            input$run_animations,
          
          ANALYSIS_MODE =
            input$analysis_mode,
          
          CUSTOM_MASKS =
            input$custom_masks,
          
          CUSTOM_ELEMENTS =
            input$custom_elements,
          
          CUSTOM_LAYERS =
            input$custom_layers,
          
          CUSTOM_DEPTHS =
            input$custom_depths,
          
          CUSTOM_AGGREGATION =
            input$custom_aggregation,
          
          CUSTOM_PRESERVE =
            input$custom_preserve,
          
          SELECTED_SIMULATIONS =
            input$selected_simulations,
          
          FARM_CELLS = 
            farm_cells_data()
          
        )
        
        
        shiny::stopApp(
          settings
        )
        
      }
      
    )
    
    #-------------------------------------------------------------------------
    # Render custom analysis controls
    #-------------------------------------------------------------------------
    
    output$custom_analysis_ui <- shiny::renderUI({
      
      cells <- farm_cells_data()
      
      if (is.null(cells)) {
        
        return(
          shiny::p(
            "Loading farm structure..."
          )
        )
        
      }
      
      
      available_masks <- sort(
        unique(cells$Mask)
      )
      
      available_elements <- sort(
        unique(cells$Element)
      )
      
      available_layers <- sort(
        unique(cells$Layer)
      )
      
      available_depths <- sort(
        unique(cells$Depth)
      )
      
      
      shiny::fluidRow(
        
        
        #-----------------------------------------------------------------------
        # Selection controls
        #-----------------------------------------------------------------------
        
        shiny::column(
          
          width = 6,
          
          shiny::h5(
            "Select farm cells"
          ),
          
          shiny::selectizeInput(
            
            "custom_masks",
            
            "Farm masks",
            
            choices = available_masks,
            
            selected = available_masks,
            
            multiple = TRUE
            
          ),
          
          shiny::selectizeInput(
            
            "custom_elements",
            
            "Elements",
            
            choices = available_elements,
            
            selected = NULL,
            
            multiple = TRUE,
            
            options = list(
              placeholder = "All elements"
            )
            
          ),
          
          shiny::selectizeInput(
            
            "custom_layers",
            
            "Layers",
            
            choices = available_layers,
            
            selected = NULL,
            
            multiple = TRUE,
            
            options = list(
              placeholder = "All layers"
            )
            
          ),
          
          shiny::selectizeInput(
            
            "custom_depths",
            
            "Depths (m)",
            
            choices = available_depths,
            
            selected = NULL,
            
            multiple = TRUE,
            
            options = list(
              placeholder = "All depths"
            )
            
          )
          
        ),
        
        
        #-----------------------------------------------------------------------
        # Aggregation controls
        #-----------------------------------------------------------------------
        
        shiny::column(
          
          width = 6,
          
          shiny::h5(
            "Aggregate output"
          ),
          
          shiny::selectInput(
            
            "custom_aggregation",
            
            "Aggregation",
            
            choices = c(
              "Mean" = "mean",
              "Sum" = "sum",
              "Minimum" = "min",
              "Maximum" = "max",
              "None" = "none"
            ),
            
            selected = "mean"
            
          ),
          
          shiny::checkboxGroupInput(
            
            "custom_preserve",
            
            "Preserve dimensions",
            
            choices = c(
              "Mask" = "Mask",
              "Element" = "Element",
              "Layer" = "Layer",
              "Depth" = "Depth"
            ),
            
            selected = NULL
            
          )
          
        )
        
      )
      
    })
    
  }
  
  
  #---------------------------------------------------------------------------
  # Launch gadget
  #---------------------------------------------------------------------------
  
  settings <- shiny::runGadget(
    
    app = shiny::shinyApp(
      ui,
      server
    ),
    
    viewer = shiny::dialogViewer(
      "FLEXSEM DEB Toolbox",
      width = 800,
      height = 850
    )
    
  )
  
  
  #---------------------------------------------------------------------------
  # Return selected settings
  #---------------------------------------------------------------------------
  
  return(
    settings
  )
  
}