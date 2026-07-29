# Auto-generated from rFVS_BatchProcessor_rShiny_v2.R
# Split for package structure on 2026-07-29 11:06:54

ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  padding = 0,
  
  tags$head(
    tags$style(HTML("
      .shiny-progress-notification {
        position: fixed !important;
        top: 50% !important;
        left: 50% !important;
        transform: translate(-50%, -50%) !important;
        right: auto !important;
        bottom: auto !important;
        width: 480px !important;
        box-shadow: 0px 4px 25px rgba(0,0,0,0.25) !important;
        border-radius: 10px !important;
        padding: 6px !important;
        background-color: #ffffff !important;
      }
      .shiny-progress-notification .progress {
        margin: 14px 12px 6px 12px !important;
        height: 14px !important;
        border-radius: 6px !important;
      }
      .shiny-progress-notification .progress-text {
        padding: 4px 12px 10px 12px !important;
      }
      
      /* Custom Title Bar spanning entire top */
      .custom-top-bar {
        background-color: #2C3E50; /* Flatly dark blue/slate */
        color: #ffffff;
        padding: 12px 20px;
        font-size: 22px;
        width: 100%;
      }
      
      /* Custom styling for the right-side tabs to match exactly like a navbar */
      .main-panel-tabs > .nav {
        background-color: #2C3E50;
        padding: 0 10px;
        margin-bottom: 15px;
        border-radius: 4px;
      }
      .main-panel-tabs > .nav .nav-link {
        color: rgba(255,255,255,0.7);
        border: none !important;
        border-radius: 0;
        padding: 12px 20px !important;
        font-size: 16px;
      }
      .main-panel-tabs > .nav .nav-link:hover {
        color: rgba(255,255,255,0.9);
      }
      .main-panel-tabs > .nav .nav-link.active {
        color: #ffffff !important;
        border-bottom: 3px solid #18BC9C !important; /* Flatly teal accent */
        background: transparent !important;
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('update_progress', function(message) {
        let pbar = document.getElementById('pipeline_progress_bar');
        if (pbar) {
          pbar.style.width = message.pct + '%';
          pbar.innerHTML = message.pct + '%';
        }
        let ptext = document.getElementById('pipeline_progress_text');
        if (ptext) {
          ptext.innerHTML = message.detail;
        }
      });
    "))
  ),
  
  div(class = "custom-top-bar", "FVS Batch Processor"),
  
  shinyjs::useShinyjs(),
  
  layout_sidebar(
    class = "p-3", 
    border = FALSE,
    sidebar = sidebar(
      width = 380,
      conditionalPanel(
        condition = "input.main_tabs == 'about_tab'",
        h5("Welcome"),
        p("Navigate through the tabs on the right to configure and execute your FVS batch processing pipeline.")
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'tab1'",
        h5("Execution Settings"),
        
        tags$label("Root Folder Path", class = "form-label", `for` = "root_dir"),
        div(class = "input-group mb-3",
            tags$button(
              id = "browse_root",
              type = "button",
              class = "btn btn-default action-button",
              "Browse..."
            ),
            tags$input(
              id = "root_dir",
              type = "text",
              class = "form-control",
              value = RootDir,
              placeholder = "Select root folder path"
            )
        ),
        
        tags$label("Master Database File", class = "form-label", `for` = "master_db_upload"),
        fileInput("master_db_upload", NULL, accept = c(".db", ".sqlite"), buttonLabel = "Browse...", placeholder = DefaultDB, width = "100%"),
        div(style = "display:none;", textInput("master_db", label = NULL, value = DefaultDB)),
        
        tags$label("KCP Directory Name", class = "form-label", `for` = "kcp_dir"),
        div(class = "input-group mb-3",
            tags$button(
              id = "browse_kcp",
              type = "button",
              class = "btn btn-default action-button",
              "Browse..."
            ),
            tags$input(
              id = "kcp_dir",
              type = "text",
              class = "form-control",
              value = DefaultKCP,
              placeholder = "Select KCP directory"
            )
        ),
        hr(),
        textInput("stand_tbl", "Stand Initialization Table", value = "FVS_STANDINIT"),
        textInput("group_col", "Database Grouping Column", value = "VARIANT"),
        textInput("exclude_grps", "Excluded Groups (Comma-separated)", value = ""),
        hr(),
        actionButton("load_metadata", "Scan Directories & Connect DB", class = "btn-primary w-100")
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'tab2'",
        h5("KCP Lookup Table"),
        p("Create specific KCP combinations that will define your FVS runs. You can do this by editing the grid directly or export the excel file for further editing and reupload to define your Group/Prescription combinations."),
        selectInput("scenario_add_cols", "Scenario Columns (In Addition To GROUP_CODE)", choices = NULL, multiple = TRUE),
        downloadButton("download_excel", "Export Matrix to Excel", class = "btn-outline-primary w-100 mb-2"),
        fileInput("upload_excel", "Import Excel Matrix File", accept = c(".xlsx", ".xls", ".csv"), buttonLabel = "Browse..."),
        hr(),
        actionButton("save_matrix", "Save KCP Scenarios & Build Manifest", class = "btn-primary w-100")
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'tab3'",
        h5("Keyfile Parameters"),
        numericInput("num_cores", paste0("Compute Cores (Parallel Generation - ", sys_cores, " Available)"), value = def_cores, min = 1, step = 1),
        numericInput("num_cycles", "Simulation Cycles Count", value = 10, min = 1, step = 1),
        numericInput("time_int", "Cycle Time Interval (Years)", value = 10, min = 1, step = 1),
        numericInput("inv_year", "Inventory Baseline Start Year", value = 2024, min = 1900, step = 1),
        hr(),
        actionButton("gen_keyfiles", "Generate Stand Keyfiles", icon = icon("cogs"), class = "btn-primary w-100 mb-2"),
        shinyjs::hidden(actionButton("kill_gen_btn", "Cancel", icon = icon("xmark"), class = "btn-danger w-100"))
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'tab4'",
        h5("Simulation Parameters"),
        numericInput("num_cores_rfvs", paste0("Compute Cores (Parallel Execution - ", sys_cores, " Available)"), value = def_cores, min = 1, step = 1),
        hr(),
        h5("FVS Install Location"),
        textInput("bin_loc", "FVS Bin Path (Executable Location)", value = "C:/FVS/FVSSoftware/FVSbin"),
        hr(),
        h5("Execution Workflow"),
        selectInput("overwrite_scens", "Force Overwrite Specific Scenarios:", choices = NULL, multiple = TRUE),
        actionButton("run_rfvs", "Execute Parallel rFVS Engine", icon = icon("play"), class = "btn-primary w-100 mb-2"),
        shinyjs::hidden(actionButton("kill_run_btn", "Cancel", icon = icon("xmark"), class = "btn-danger w-100"))
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'tab5'",
        h5("Consolidate Outputs"),
        numericInput("num_cores_merge", paste0("Compute Cores (Parallel Consolidation - ", sys_cores, " Available)"), value = def_cores, min = 1, step = 1),
        hr(),
        p("Merge individual stand databases into Group/Scenario databases, and then combine them all into a single master database."),
        actionButton("merge_outputs", "Consolidate Master Outputs", icon = icon("database"), class = "btn-primary w-100 mb-2"),
        shinyjs::hidden(actionButton("kill_merge_btn", "Cancel", icon = icon("xmark"), class = "btn-danger w-100"))
      )
    ),
    
    div(class = "main-panel-tabs h-100",
        navset_tab(
          id = "main_tabs",
          nav_panel(
            title = "About",
            value = "about_tab",
            icon = icon("circle-info"),
            card(
              card_header("Pipeline Overview"),
              p("This application streamlines the process of linking a FVS-ready SQLite database with keyword components (KCPs) and executing them in parallel through the Forest Vegetation Simulator (rFVS)."),
              h5("Workflow Summary:"),
              tags$ul(
                tags$li(strong("Link Input Database:"), " Connect to your FVS-ready SQLite database."),
                tags$li(strong("Define Scenarios:"), " Specify a grouping column to organize stands for each FVS run."),
                tags$li(strong("Configure Runs:"), " Create a KCP lookup table to generate Group/Prescription specific scenarios using the interactive interface or by downloading, editing, and uploading the mapped Excel spreadsheet given the user's KCPs."),
                tags$li(strong("Create Keyfiles:"), " Generate standalone FVS .key configuration files with all specified scenarios on a per-stand basis, natively staged for parallel processing."),
                tags$li(strong("Execute & Consolidate:"), " Run rFVS instances concurrently across user-specified compute cores, merging all final database results on a Group/Prescription basis. ", strong("Note:"), " The system tracks previously completed runs. If you add new scenarios to an existing project, only the new un-simulated scenarios will be run, saving processing time. To rerun an already executed scenario, you must explicitly specify it in the 'Force Overwrite Specific Scenarios' dropdown on Tab 4.")
              ),
              hr(),
              h5("KCP Folder Organization:"),
              p("Users must organize their ", code(".kcp"), " files into categorical subfolders within their designated KCP directory (e.g., ", strong("KCP_Catalog"), "). These subfolders dictate the configuration combinations available for building scenarios."),
              p("Importantly, the alphabetical/numerical order of these subfolders dictates the sequence in which the KCP files are appended and read by FVS. We strongly recommend using numbered prefixes (e.g., ", code("01_Global"), ", ", code("02_Calibration"), ", ", code("03_Prescriptions"), ", ", code("04_Outputs"), ") to explicitly control this load order. Ensure your output-generating KCP folder is specified last (numbered highest) so its instructions are executed after all other parameters."),
              hr(),
              h5("Folder Structure & Expected Locations:"),
              p("Below is the recommended folder structure for organizing your project and running the FVS Batch Processor. The system generally expects your master database and KCP files to be located under your specified ", strong("Root Folder Path"), " as shown below e.g.,", code("FVS_BatchProcessing"), ". During execution, it builds standalone ", code("run.key"), " files per stand, executes them to dump temporary ", code(".out"), " and ", code(".db"), " files locally, and finally aggregates them into centralized merged output databases. While this structure is the default recommendation, users can specify custom distinct pathways using the overrides in Step 1."),
              pre("FVS_BatchProcessing
\u251C\u2500\u2500 Inputs
\u2502   \u2514\u2500\u2500 AllBKNF_Combined.db
\u251C\u2500\u2500 KCPs
\u2502   \u251C\u2500\u2500 01_Global
\u2502   \u2502   \u2514\u2500\u2500 BLK_HILLS_Global.kcp
\u2502   \u251C\u2500\u2500 02_Calibration
\u2502   \u2502   \u251C\u2500\u2500 Group_A_GrowthCalib.kcp
\u2502   \u2502   \u2514\u2500\u2500 Group_B_GrowthCalib.kcp
\u2502   \u251C\u2500\u2500 03_Prescriptions
\u2502   \u2502   \u251C\u2500\u2500 Rx01_TC01.kcp
\u2502   \u2502   \u251C\u2500\u2500 Rx01_TC02.kcp
\u2502   \u2502   \u251C\u2500\u2500 Rx02_TC01.kcp
\u2502   \u2502   \u2514\u2500\u2500 Rx02_TC02.kcp
\u2502   \u2514\u2500\u2500 04_Outputs
\u2502       \u2514\u2500\u2500 outputDB.kcp
\u251C\u2500\u2500 rFVS_Runs
\u2502   \u251C\u2500\u2500 Group_A
\u2502   \u2502   \u2514\u2500\u2500 Group_A_Rx01_TC01
\u2502   \u2502       \u2514\u2500\u2500 fvs_<Stand_ID>
\u2502   \u2502           \u251C\u2500\u2500 out.db
\u2502   \u2502           \u251C\u2500\u2500 run.key
\u2502   \u2502           \u2514\u2500\u2500 run.out
\u2502   \u2514\u2500\u2500 Group_B
\u2502       \u2514\u2500\u2500 Group_B_Rx01_TC01
\u2502           \u2514\u2500\u2500 fvs_<Stand_ID>
\u2502               \u251C\u2500\u2500 out.db
\u2502               \u251C\u2500\u2500 run.key
\u2502               \u2514\u2500\u2500 run.out
\u2514\u2500\u2500 Outputs
    \u251C\u2500\u2500 Group_A_Rx01_TC01
    \u2502   \u2514\u2500\u2500 FVSOut_Group_A_Rx01_TC01.db
    \u2514\u2500\u2500 Group_B_Rx01_TC01
        \u2514\u2500\u2500 FVSOut_Group_B_Rx01_TC01.db"),
              hr(),
              # h5("Workflow Diagram"),
              tags$figure(
                tags$img(
                  src = paste0(WorkflowResourcePrefix, "/", WorkflowImageFile),
                  alt = "FVS batch processing workflow diagram",
                  style = "display: block; margin: 0 auto; width: 100%; max-width: 1800px; height: auto; border: 1px solid #d9d9d9; border-radius: 6px;"
                ),
                # tags$figcaption(
                #   style = "margin-top: 8px; color: #666;",
                #   "Figure 1. End-to-end FVS batch workflow."
                # )
              ),
              hr(),
              h5("Author"),
              p(strong("Cody Dangerfield")),
              p("Email: ", tags$a(href = "mailto:cody.dangerfield@usda.gov", "cody.dangerfield@usda.gov")),
              p("If you have questions, please contact me.")
              
            )
          ),
          nav_panel(
            title = "1. Global Parameters",
            value = "tab1",
            icon = icon("sliders"),
            card(
              card_header("System Metadata Connection Output Summary"),
              verbatimTextOutput("meta_status")
              
            )
          ),
          nav_panel(
            title = "2. KCP Lookup Table",
            value = "tab2",
            icon = icon("table"),
            card(
              card_header("Editable Scenario Definitions Matrix"),
              p(em("Note: Changes made inside the grid synchronize automatically. You can right-click rows to expand/delete elements.")),
              p(strong("Reminder: "), "The columns below are dynamically built based on your KCP subfolders. The numerical/alphabetical order of those root folders dictates how those KCPs are stacked together for the simulation. If a Prescription is not specified, ", code("GROUP_CODE + _NG"), " will be used to specify the Scenario."),
              rHandsontableOutput("prescription_table", height = "600px")
              
            )
          ),
          nav_panel(
            title = "3. Create Keyfiles",
            value = "tab3",
            icon = icon("file-code"),
            card(
              card_header("Pipeline Generation Status & Queue Diagnostics"),
              p("Verifies setup context prior to generating standalone keyfile sequences:"),
              verbatimTextOutput("pipeline_diagnostics")
              
            )
          ),
          nav_panel(
            title = "4. Run rFVS Engine",
            value = "tab4",
            icon = icon("play"),
            card(
              card_header("Simulation Pipeline Execution Status"),
              p("Verifies parameters relative to active database context before starting runs:"),
              p(strong("Reminder:"), " Only scenarios that have not yet been simulated will be executed in order to save computation time. If you wish to rebuild and re-execute a previously completed scenario, use the 'Force Overwrite Specific Scenarios' dropdown on the left to mark it for deletion/rerun."),
              verbatimTextOutput("pipeline_diagnostics_rfvs")
            )
          ),
          nav_panel(
            title = "5. Consolidate Outputs",
            value = "tab5",
            icon = icon("database"),
            card(
              card_header("Master Database Consolidation"),
              p("This step consolidates all temporary stand-level SQLite databases generated by FVS into Scenario-specific databases, and then ultimately merges them into a single master FVS_Out_{Date}.db file in the Outputs folder."),
              verbatimTextOutput("pipeline_diagnostics_merge")
            )
          )
        )
    )
  )
)

