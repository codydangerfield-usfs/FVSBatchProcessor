# Auto-generated from rFVS_BatchProcessor_rShiny_v2.R
# Split for package structure on 2026-07-29 11:06:54

ui <- function(request) {
  page_fillable(
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
      
      // Update HTML title attribute on hover so truncated text shows up in a standard OS tooltip
      $(document).on('mouseover', '#root_dir, #master_db, #kcp_dir', function() {
        if ($(this).val()) {
          $(this).attr('title', $(this).val());
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
        h5("Global Parameters"),
        
        tags$label("Root Folder Path", class = "form-label", `for` = "root_dir"),
        div(class = "input-group mb-3",
            actionButton("browse_root", "Browse..."),
            tags$input(
              id = "root_dir",
              type = "text",
              class = "form-control",
              value = "",
              placeholder = "Select root folder path"
            )
        ),
        
        tags$label("Master Database File", class = "form-label", `for` = "master_db"),
        div(class = "input-group mb-3",
            actionButton("browse_db", "Browse..."),
            tags$input(
              id = "master_db",
              type = "text",
              class = "form-control",
              value = "",
              placeholder = "<FVS_Input.db>"
            )
        ),
        
        tags$label("KCP Directory Name", class = "form-label", `for` = "kcp_dir"),
        div(class = "input-group mb-3",
            actionButton("browse_kcp", "Browse..."),
            tags$input(
              id = "kcp_dir",
              type = "text",
              class = "form-control",
              value = "",
              placeholder = "Select KCP directory"
            )
        ),
        hr(),
        selectInput(
          "stand_tbl",
          "Stand Initialization Table",
          choices = "FVS_STANDINIT",
          selected = "FVS_STANDINIT"
        ),

        tags$details(
          style = "margin-top: 10px; margin-bottom: 15px; font-size: 0.9em; color: #555;",
          tags$summary(strong("ℹ️ How Grouping Works"), style = "cursor: pointer;"),
          tags$div(
            style = "margin-top: 10px; padding-left: 12px; border-left: 3px solid #18BC9C;",
            p("Organize your FVS runs in two ways:"),
            tags$ol(
              style = "padding-left: 15px; margin-bottom: 8px;",
              tags$li(strong("Direct Column (Default): "), "Select a standard column from the Stand Initialization table. All unique values become your groups."),
              tags$li(strong("Use GROUPS Column: "), "Parse the standard FVS ", code("GROUPS"), " into independent groups:")
            ),
            tags$ul(
              style = "padding-left: 20px; font-size: 0.95em;",
              tags$li("Parsed values, e.g., ", code("All_Stands"), ", without an equal sign are unique stand-alone groups."),
              tags$li("Parsed values with an equal sign (e.g., ", code("ForestType=Pine"), ") use the left side as the grouping column and the right side as the unique group value."),
              tags$li("Stands with empty or explicit null values (e.g., ", code("NA"), ") for a category are ignored.")
            ),
            p(strong("Creating New Combined Groups:")),
            p("If you would like to stratify your stand table across multiple columns, you can create a new Grouping Column via the collapsed ", strong("Create New Grouping Column"), " panel to dynamically merge two or more Direct Columns or parsed GROUPS entries. This will securely create a new derived column in the database specifically structured for use in this dropdown.")
          )
        ),

        checkboxInput("use_groups_col", "Use GROUPS", value = FALSE),
        selectInput("group_col", "Database Grouping Column", choices = "VARIANT", selected = "VARIANT"),
        selectizeInput("exclude_grps", "Excluded Groups", choices = NULL, selected = NULL, multiple = TRUE,
                       options = list(placeholder = "Select group values to exclude")),

        hr(),
        actionButton("load_metadata", "Scan Directories & Connect DB", class = "btn-primary w-100"),
        div(style = "height: 8rem;")
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'tab2'",
        h5("KCP Lookup Table"),
        p("Create specific KCP combinations that will define your FVS runs. You can do this by editing the grid directly or export the excel file for further editing and reupload to define your Group/Prescription combinations."),
        tags$details(
          style = "margin-top: 10px; margin-bottom: 15px; font-size: 0.9em; color: #555;",
          tags$summary(strong("ℹ️ How Scenario Naming Works"), style = "cursor: pointer;"),
          tags$div(
            style = "margin-top: 10px; padding-left: 12px; border-left: 3px solid #18BC9C;",
            p("The default programmatic naming scheme assumes you have a KCP folder like 'Prescriptions', where the Scenario becomes ", code("GROUP_CODE + Prescription"), ". However, you can specify any KCP folder to construct this name using the 'Scenario Columns' dropdown below, or it will default to ", code("GROUP_CODE + _NG"), " if no additional columns are specified."),
            p("These Scenario names can also be manually edited in the grid, but it is highly recommended to use a programmatic naming scheme so your runs remain correctly outlined and organized.")
          )
        ),
        selectInput("scenario_add_cols", "Scenario Columns (In Addition To GROUP_CODE)", choices = NULL, multiple = TRUE),
        hr(),
        actionButton("btn_expand_modal", " Auto-Populate Combinations", icon = icon("wand-magic-sparkles"), class = "btn-info w-100 mb-2"),
        hr(),
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
        div(
          id = "kill_gen_btn_wrap",
          style = "display:none;",
          actionButton("kill_gen_btn", "Cancel", icon = icon("xmark"), class = "btn-danger w-100")
        )
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
        div(
          checkboxInput(
            "test_run_mode",
            "Test mode: Run first stand in each group/scenario",
            value = FALSE
          ),
          div(
            style = "margin-top: -10px;",
            helpText("Uses one stand/keyfile per GROUP_CODE and Scenario. Overwrite selections still apply.")
          )
        ),
        hr(),
        actionButton("run_rfvs", "Execute Parallel rFVS Engine", icon = icon("play"), class = "btn-primary w-100 mb-2"),
        div(
          id = "kill_run_btn_wrap",
          style = "display:none;",
          actionButton("kill_run_btn", "Cancel", icon = icon("xmark"), class = "btn-danger w-100")
        )
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'tab5'",
        h5("Consolidate Outputs"),
        numericInput("num_cores_merge", paste0("Compute Cores (Parallel Consolidation - ", sys_cores, " Available)"), value = def_cores, min = 1, step = 1),
        hr(),
        p("Merge individual stand databases into Group/Scenario databases, and then combine them all into a single master database."),
        actionButton("merge_outputs", "Consolidate Master Outputs", icon = icon("database"), class = "btn-primary w-100 mb-2"),
        div(
          id = "kill_merge_btn_wrap",
          style = "display:none;",
          actionButton("kill_merge_btn", "Cancel", icon = icon("xmark"), class = "btn-danger w-100")
        )
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
              p("This application streamlines the process of linking a FVS-ready SQLite database with keyword component files (KCPs) and executing them in parallel through the Forest Vegetation Simulator (rFVS)."),
              h5("Workflow Summary:"),
              tags$ol(
                tags$li(
                  strong("Global Parameters"),
                  tags$ul(
                    tags$li("Link Input Database: Connect to your FVS-ready SQLite database."),
                    tags$li("Define Groups: Specify a grouping column to organize stands for each FVS run.")
                  )
                ),
                tags$li(
                  strong("KCP Lookup Table"),
                  tags$ul(
                    tags$li("Configure Runs: Create a KCP lookup table to generate Group/Prescription-specific scenarios using the interactive interface or by downloading, editing, and uploading the mapped Excel spreadsheet based on your KCPs.")
                  )
                ),
                tags$li(
                  strong("Create Keyfiles"),
                  tags$ul(
                    tags$li("Create Keyfiles: Generate standalone FVS .key configuration files with all specified scenarios on a per-stand basis, natively staged for parallel processing.")
                  )
                ),
                tags$li(
                  strong("Run rFVS Engine"),
                  tags$ul(
                    tags$li("Execute Scenarios: Run rFVS instances concurrently across user-specified compute cores."),
                    tags$li(strong("Note:"), " The system tracks previously completed runs. If you add new scenarios to an existing project, only new un-simulated scenarios will be run, saving processing time. To rerun an already executed scenario, explicitly specify it in the 'Force Overwrite Specific Scenarios' dropdown on Tab 4.")
                  )
                ),
                tags$li(
                  strong("Consolidate Outputs"),
                  tags$ul(
                    tags$li("Consolidate Results: Once all scenarios are configured and executed, consolidate stand-level outputs into scenario-specific databases (one per unique Group/Prescription run), then merge those into a single master database.")
                  )
                )
              ),
              hr(),
              h5("KCP Folder Organization:"),
              p("Users must organize their ", code(".kcp"), " files into categorical subfolders within their designated KCP directory (e.g., ", strong("KCP_Catalog"), "). These subfolders dictate the configuration combinations available for building scenarios."),
              p("Importantly, the alphabetical/numerical order of these subfolders dictates the sequence in which the KCP files are appended and read by FVS. We strongly recommend using numbered prefixes (e.g., ", code("01_Global"), ", ", code("02_Calibration"), ", ", code("03_Prescriptions"), ", ", code("04_Outputs"), ") to explicitly control this load order. Ensure your output-generating KCP folder is specified last (numbered highest) so its instructions are executed after all other parameters."),
              hr(),
              h5("Folder Structure & Expected Locations:"),
              p("Below is the recommended folder structure for organizing your project and running the FVS Batch Processor."),
              p("In this workflow, your R working directory becomes the project root and is used as the ", strong("Root Folder Path"), " (e.g., ", code("FVS_BatchProcessing"), ")."),
              p("Inside that folder, the app expects an ", code("Inputs"), " folder with a single input database and a folder with ", code("KCP"), " in its name (for example, ", code("KCP_Catalog"), "). If the FVS project directory is setup this way, the app auto-detects both the input database and KCP catalog. During processing, ", code("rFVS_Runs"), " and ", code("Outputs"), " are created automatically. ", code("rFVS_Runs"), " stores individual stand-level run outputs per scenario, and ", code("Outputs"), " stores consolidated scenario databases plus the final master database."),
              pre("FVS_BatchProcessing
\u251C\u2500\u2500 Inputs
\u2502   \u2514\u2500\u2500 AllBKNF_Combined.db
\u251C\u2500\u2500 KCP_Catalog
\u2502   \u251C\u2500\u2500 01_Global
\u2502   \u2502   \u2514\u2500\u2500 Global_rFVS.kcp
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
            tags$div(
              class = "d-flex flex-column", style = "min-height: calc(100vh - 160px);", # Use exact viewport height math
              card(
                fill = FALSE,
                card_header("System Metadata Connection Output Summary"),
                verbatimTextOutput("meta_status")
              ),
              accordion(
                open = FALSE,
                class = "flex-grow-1 overflow-visible", # Claim all remaining space and allow dropdowns to overlap
                accordion_panel(
                  "Create New Grouping Column (Optional)",
                  class = "overflow-visible d-flex flex-column", # Ensure the panel body allows flex layout to pin the button
                  p("Select multiple database columns or parsed GROUP entries to concatenate into a single, combined column (e.g., ", code("ColumnA_ColumnB"), "). The new derived column will be appended to the Stand Initialization table for selection in the main Grouping Column dropdown on the left."),
                  fluidRow(
                    column(12,
                           radioButtons("merge_col_mode", "Merge Mode:",
                                        choices = c("Merge Direct Database Columns" = "cols", "Merge Parsed GROUP Entries" = "groups"),
                                        inline = TRUE)
                    )
                  ),
                  conditionalPanel(
                    condition = "input.merge_col_mode == 'cols'",
                    selectizeInput("merge_cols_select", "Select Columns to Merge:", choices = NULL, multiple = TRUE, options = list(placeholder = "Select 2 or more columns"))
                  ),
                  conditionalPanel(
                    condition = "input.merge_col_mode == 'groups'",
                    selectizeInput("merge_groups_select", "Select GROUP Entries to Merge:", choices = NULL, multiple = TRUE, options = list(placeholder = "Select 2 or more GROUPS"))
                  ),
                  div(class = "mt-auto pt-3 border-top", # Emulate a card footer pinned to the bottom
                    actionButton("btn_create_merged_col", "Create Merged Column", icon = icon("layer-group"), class = "btn-secondary")
                  )
                )
              )
            )
          ),
          nav_panel(
            title = "2. KCP Lookup Table",
            value = "tab2",
            icon = icon("table"),
            card(
              card_header("Editable Scenario Definitions Matrix"),
              p(em("Note: Changes made inside the grid synchronize automatically. You can right-click rows to expand/delete elements.")),
              p(strong("Reminder: "), "The columns below are dynamically built based on your KCP subfolders. The numerical/alphabetical order of those root folders dictates how those KCPs are stacked together for the simulation. For each KCP subfolder, you can select ", code("ALL"), " in the drop down; this option stacks all KCPs in that folder together. To generate combinations, use Auto-Populate Combinations and select the target ", code("GROUP_CODE"), " values plus one or more KCP columns. Selecting multiple columns creates their full cross-product, such as every Prescription and Timing pairing. ", strong("Importantly, each distinct FVS run is predicated on its unique Scenario name. Duplicate scenario names are not allowed.")),
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

}
