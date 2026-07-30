# ---- FVS Batch Processor Shiny Application ----
# Description:
# This script provides an interactive R Shiny application to streamline 
# the Forest Vegetation Simulator (FVS) batch processing workflow. It allows users to:
#   1. Link to a master SQLite database for STAND and TREE initialization data.
#   2. Dynamically build Group/Prescription scenarios and map KCP keyword files.
#   3. Auto-generate standalone run.key FVS configuration files across isolated stand folders.
#   4. Execute parallel rFVS simulations utilizing worker core clusters.
#   5. Consolidate stand-level SQLite output databases back into merged master scenario databases.
#
# Outputs include generated .key files, temporary individual stand databases, 
# and final merged SQLite output databases per Group/Prescription.
#
# Author: Cody Dangerfield
# Last Updated: July 29, 2026
# ------------------------------------------------------------------------------

# --- AUTOLOAD PACKAGES ---
cran_packages <- c(
  "shiny", "bslib", "RSQLite", "rhandsontable", 
  "openxlsx", "foreach", "doSNOW", "uuid", "zip", "shinyjs", "dplyr", "callr", "shinyFiles"
)

new_packages <- cran_packages[!(cran_packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages, dependencies = TRUE)

if (!requireNamespace("rFVS", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("USDAForestService/ForestVegetationSimulator-Interface", subdir = "rFVS")
}

# Load all required packages (including base R and custom packages like rFVS)
all_packages <- c(cran_packages, "tools", "parallel", "rFVS")
invisible(lapply(all_packages, library, character.only = TRUE))

# ------------------------------------------------------------------------------
# 1. GLOBAL SETTINGS & UTILITIES
# ------------------------------------------------------------------------------
# Increase maximum upload size to 10GB for very large database/file transfers
options(shiny.maxRequestSize = 10000 * 1024^2)

# Determine root directory dynamically via shinyOptions, falling back to current dir
caller_wd <- getShinyOption("FVS_USER_WD", default = getwd())
RootDir <- normalizePath(caller_wd, winslash = "/", mustWork = FALSE)
# Define where simulation runs will be hosted
RunBaseDir <- file.path(RootDir, "rFVS_Runs")
# Set path for the KCP combinations manifest file
ManifestFile <- file.path(RunBaseDir, "KCP_AddFile_Manifest.csv")
# Set a visual icon/workflow diagram image name
WorkflowImageFile <- "FVS_BatchProcessing_WorkflowDiagram_v3.png"

# Define possible directories to locate the workflow diagram
workflow_img_dirs <- c(
  file.path(getwd(), "www"),
  file.path(getwd(), "Scripts", "www"),
  file.path(RootDir, "Scripts", "www")
)
# Match the first location that contains the image file
workflow_img_dir <- workflow_img_dirs[file.exists(file.path(workflow_img_dirs, WorkflowImageFile))][1]
# If found, add it as a resource path available to the Shiny client UI
if (!is.na(workflow_img_dir) && nzchar(workflow_img_dir)) {
  shiny::addResourcePath("workflow_assets", normalizePath(workflow_img_dir, winslash = "/", mustWork = TRUE))
}

# Determine default master database file based on contents of Inputs folder
InputsDir <- file.path(RootDir, "Inputs")
available_dbs <- list.files(InputsDir, pattern = "\\.(db|sqlite)$", ignore.case = TRUE, full.names = FALSE)
DefaultDB <- if (length(available_dbs) > 0) available_dbs[1] else "AllBKNF_Combined.db"

# Determine default KCP directory based on contents of RootDir
available_dirs <- list.dirs(RootDir, full.names = FALSE, recursive = FALSE)
kcp_matches <- available_dirs[grepl("KCP", available_dirs, ignore.case = TRUE)]
DefaultKCP <- if (length(kcp_matches) > 0) kcp_matches[1] else "KCP_Catalog"

# Ensure the base directory for runs exists
if (!dir.exists(RunBaseDir)) dir.create(RunBaseDir, recursive = TRUE, showWarnings = FALSE)

# Helper function to remove leading numbers and special characters from a folder name
clean_kcp_type <- function(folder_name) {
  cleaned <- sub("^\\d+[_ -]*", "", folder_name)
  make.names(cleaned, unique = TRUE)
}

# Helper function to wrap identifiers in quotes safely for SQL syntax
quote_sql_identifier <- function(x) {
  paste0("\"", gsub("\"", "\"\"", x), "\"")
}

# Recursively scans the master KCP directory structure to build a catalog dataframe of all .kcp files
discover_kcp_catalog <- function(master_dir) {
  if (!dir.exists(master_dir)) return(NULL)
  type_dirs <- list.dirs(master_dir, full.names = TRUE, recursive = FALSE)
  if (length(type_dirs) == 0) return(NULL)
  
  catalog <- do.call(rbind, lapply(seq_along(type_dirs), function(i) {
    type_dir <- normalizePath(type_dirs[i], winslash = "/", mustWork = TRUE)
    kcp_files <- list.files(type_dir, pattern = "\\.kcp$", full.names = TRUE, ignore.case = TRUE)
    
    if (length(kcp_files) == 0) {
      return(data.frame(
        TypeOrder = i,
        KCP_Type = clean_kcp_type(basename(type_dir)),
        Folder = basename(type_dir),
        KCP_Name = NA_character_,
        KCP_Path = NA_character_,
        stringsAsFactors = FALSE
      ))
    }
    
    data.frame(
      TypeOrder = i,
      KCP_Type = clean_kcp_type(basename(type_dir)),
      Folder = basename(type_dir),
      KCP_Name = tools::file_path_sans_ext(basename(kcp_files)),
      KCP_Path = normalizePath(kcp_files, winslash = "/"),
      stringsAsFactors = FALSE
    )
  }))
  if (!is.null(catalog)) catalog[order(catalog$TypeOrder, catalog$KCP_Name), ] else NULL
}

# Connects to SQLite DB and retrieves unique group strings, ignoring 'excluded_groups'
get_groups_from_db <- function(db_path, table_name, group_col, excluded_groups) {
  if (!file.exists(db_path)) return(character(0))
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE) # Ensure we close the DB connection automatically 
  
  # Ensure the target table actually exists
  if (!dbExistsTable(con, table_name)) {
    stop(sprintf("Table '%s' could not be found in the database. Please check the 'Stand Initialization Table' name.", table_name))
  }
  
  # Ensure the grouping column exists in the table
  table_cols <- dbListFields(con, table_name)
  if (!(group_col %in% table_cols)) {
    stop(sprintf("Column '%s' could not be found in the table '%s'. Please check the 'Database Grouping Column' name.", group_col, table_name))
  }
  
  # Execute select distinct query on the grouping column
  sql <- sprintf("SELECT DISTINCT %s AS GROUP_CODE FROM %s", quote_sql_identifier(group_col), quote_sql_identifier(table_name))
  groups <- dbGetQuery(con, sql)$GROUP_CODE
  groups <- as.character(groups)
  
  # Filter out empty strings, NA, or user excluded groups (e.g. 'Riparian')
  groups <- groups[!is.na(groups) & nzchar(trimws(groups))]
  groups <- groups[!(tolower(groups) %in% tolower(excluded_groups))]
  sort(unique(groups))
}

# Splits concatenated multiple string KCP entries inside a cell
split_kcp_cell <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(x))) return(character(0))
  values <- unlist(strsplit(as.character(x), "\\s*[;,|]\\s*", perl = TRUE))
  values <- values[nzchar(trimws(values))]
  tools::file_path_sans_ext(values)
}

# Utility function that defines Scenario from GROUP_CODE plus either:
# 1) user-selected columns, or 2) default Prescription-like column fallback.
recalculate_scenarios <- function(df, old_df = NULL, scenario_cols = NULL, force_auto = FALSE) {
  if (nrow(df) == 0) return(df)

  # Resolve effective columns: user selection wins; otherwise fall back to Prescription-like column.
  valid_extra_cols <- intersect(as.character(scenario_cols), names(df))
  valid_extra_cols <- setdiff(valid_extra_cols, c("GROUP_CODE", "Scenario"))
  if (length(valid_extra_cols) == 0) {
    rx_match <- names(df)[grep("prescription", names(df), ignore.case = TRUE)]
    if (length(rx_match) > 0) valid_extra_cols <- rx_match[1]
  }
  
  is_empty_grp <- is.na(df$GROUP_CODE) | trimws(as.character(df$GROUP_CODE)) == ""

  # Build suffix from selected/fallback columns. If all are blank, use _NG.
  if (length(valid_extra_cols) > 0) {
    suffix_vals <- vapply(seq_len(nrow(df)), function(i) {
      vals <- trimws(as.character(unlist(df[i, valid_extra_cols, drop = FALSE], use.names = FALSE)))
      vals <- vals[!is.na(vals) & nzchar(vals)]
      if (length(vals) == 0) "_NG" else paste0("_", paste(vals, collapse = "_"))
    }, character(1))
    auto_scen <- ifelse(is_empty_grp, "", paste0(trimws(df$GROUP_CODE), suffix_vals))
  } else {
    auto_scen <- ifelse(is_empty_grp, "", paste0(trimws(df$GROUP_CODE), "_NG"))
  }
  
  if (!("Scenario" %in% names(df))) {
    df$Scenario <- ""
  }

  # Force full replacement when naming rules are intentionally changed by the user.
  if (isTRUE(force_auto)) {
    df$Scenario <- auto_scen
    return(df)
  }
  
  if (is.null(old_df)) {
    for (i in seq_len(nrow(df))) {
      if (is.na(df$Scenario[i]) || trimws(df$Scenario[i]) == "") {
        df$Scenario[i] <- auto_scen[i]
      }
    }
  } else {
    for (i in seq_len(nrow(df))) {
      if (i > nrow(old_df)) {
        if (is.na(df$Scenario[i]) || trimws(df$Scenario[i]) == "") {
          df$Scenario[i] <- auto_scen[i]
        }
      } else {
        group_changed <- !identical(as.character(df$GROUP_CODE[i]), as.character(old_df$GROUP_CODE[i]))
        cols_changed <- FALSE
        for (col_name in valid_extra_cols) {
          if (!identical(as.character(df[[col_name]][i]), as.character(old_df[[col_name]][i]))) {
            cols_changed <- TRUE
            break
          }
        }
        scen_edited <- !identical(as.character(df$Scenario[i]), as.character(old_df$Scenario[i]))
        
        if (group_changed || cols_changed) {
          df$Scenario[i] <- auto_scen[i]
        } else if (scen_edited) {
          df$Scenario[i] <- df$Scenario[i]
        } else if (is.na(df$Scenario[i]) || trimws(df$Scenario[i]) == "") {
          df$Scenario[i] <- auto_scen[i]
        }
      }
    }
  }
  return(df)
}

# Deep cleans exported xlsx files to remove legacy drawings that corrupt rhandsontable formatting/export functionality
strip_missing_drawing_relationships <- function(xlsx_path) {
  tmp_dir <- tempfile("xlsx_clean_"); dir.create(tmp_dir); on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  unzip(xlsx_path, exdir = tmp_dir)
  rel_dir <- file.path(tmp_dir, "xl", "worksheets", "_rels")
  if (!dir.exists(rel_dir)) return(invisible(TRUE))
  rel_files <- list.files(rel_dir, pattern = "\\.rels$", full.names = TRUE)
  for (rel_file in rel_files) {
    rel_xml <- paste(readLines(rel_file, warn = FALSE), collapse = "")
    rel_xml <- gsub('<Relationship[^>]+Type="[^"]+/drawing"[^>]+Target="\\.\\./drawings/drawing[0-9]+\\.xml"[^>]*/>', "", rel_xml)
    rel_xml <- gsub('<Relationship[^>]+Type="[^"]+/vmlDrawing"[^>]+Target="\\.\\./drawings/vmlDrawing[0-9]+\\.vml"[^>]*/>', "", rel_xml)
    writeLines(rel_xml, rel_file, useBytes = TRUE)
  }
  sheet_files <- list.files(file.path(tmp_dir, "xl", "worksheets"), pattern = "^sheet[0-9]+\\.xml$", full.names = TRUE)
  for (sheet_file in sheet_files) {
    sheet_xml <- paste(readLines(sheet_file, warn = FALSE), collapse = "")
    sheet_xml <- gsub('<drawing[^>]*/>', "", sheet_xml)
    sheet_xml <- gsub('<legacyDrawing[^>]*/>', "", sheet_xml)
    writeLines(sheet_xml, sheet_file, useBytes = TRUE)
  }
  zip::zipr(zipfile = xlsx_path, files = list.files(tmp_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE), root = tmp_dir, mode = "mirror")
}

# Function to construct a formatted Excel workbook with predefined drop-down options for mapped KCP scenarios
create_lookup_wb <- function(df_export, meta_info, scenario_cols = NULL) {
  df_export$Scenario <- ""
  wb <- createWorkbook()
  addWorksheet(wb, "Lookup", gridLines = TRUE)
  addWorksheet(wb, "Options", gridLines = FALSE)
  
  # Prevent group strings that contain numbers from incrementing automatically inside Excel by converting to numeric explicitly if valid.
  numeric_group <- suppressWarnings(as.numeric(df_export$GROUP_CODE))
  if (!any(is.na(numeric_group))) {
    df_export$GROUP_CODE <- numeric_group
  }
  
  # Output the template structure layout into the primary mapped worksheet 
  writeDataTable(wb, sheet = "Lookup", x = df_export, tableName = "KCP_Lookup", withFilter = TRUE, tableStyle = "TableStyleMedium2")
  
  lookup_columns <- names(df_export)
  group_col_idx <- match("GROUP_CODE", lookup_columns)

  # Resolve effective Scenario columns for workbook formulas.
  effective_scenario_cols <- intersect(as.character(scenario_cols), lookup_columns)
  effective_scenario_cols <- setdiff(effective_scenario_cols, c("GROUP_CODE", "Scenario"))
  if (length(effective_scenario_cols) == 0) {
    rx_match <- lookup_columns[grep("prescription", lookup_columns, ignore.case = TRUE)]
    if (length(rx_match) > 0) effective_scenario_cols <- rx_match[1]
  }

  # Populate dynamic excel formulas so Scenario updates with user selections.
  if (!is.na(group_col_idx)) {
    group_col_letter <- openxlsx::int2col(group_col_idx)
    row_count <- nrow(df_export)
    if (row_count > 0) {
      row_indices <- 2:(row_count + 1)

      formulas <- vapply(row_indices, function(row_i) {
        if (length(effective_scenario_cols) == 0) {
          return(sprintf("=IF(TRIM(%s%d)=\"\", \"\", %s%d&\"_NG\")",
                         group_col_letter, row_i, group_col_letter, row_i))
        }

        pieces <- vapply(effective_scenario_cols, function(col_name) {
          col_letter <- openxlsx::int2col(match(col_name, lookup_columns))
          sprintf("IF(TRIM(%s%d)=\"\",\"\",\"_\"&%s%d)", col_letter, row_i, col_letter, row_i)
        }, character(1))

        joined_parts <- paste(pieces, collapse = "&")
        sprintf("=IF(TRIM(%s%d)=\"\", \"\", IF((%s)=\"\", %s%d&\"_NG\", %s%d&(%s)))",
                group_col_letter, row_i, joined_parts, group_col_letter, row_i, group_col_letter, row_i, joined_parts)
      }, character(1))

      writeFormula(wb, sheet = "Lookup", x = formulas, startCol = 2, startRow = 2)
    }
  }
  
  options_row <- 1
  validation_ranges <- list()
  all_token <- "ALL"
  
  # Construct list of dropdown values using the discovered KCP catalog items and apply to specific columns within the excel object
  for (kcp_type in meta_info$types) {
    type_rows <- meta_info$catalog[meta_info$catalog$KCP_Type == kcp_type & !is.na(meta_info$catalog$KCP_Name), ]
    values <- c(all_token, type_rows$KCP_Name)
    
    writeData(wb, "Options", x = kcp_type, startCol = 1, startRow = options_row)
    writeData(wb, "Options", x = data.frame(KCP_Name = values), startCol = 1, startRow = options_row + 1)
    
    validation_ranges[[kcp_type]] <- sprintf("'Options'!$A$%d:$A$%d", options_row + 2, options_row + 1 + length(values))
    options_row <- options_row + length(values) + 3
  }
  
  header_style <- createStyle(fgFill = "#1F4E78", fontColour = "#FFFFFF", textDecoration = "bold", halign = "center")
  addStyle(wb, "Lookup", header_style, rows = 1, cols = seq_along(names(df_export)), gridExpand = TRUE)
  
  freezePane(wb, "Lookup", firstActiveRow = 2, firstActiveCol = 3)
  setColWidths(wb, "Lookup", cols = 1:ncol(df_export), widths = "auto")
  
  for (kcp_type in meta_info$types) {
    col_idx <- match(kcp_type, lookup_columns)
    if (!is.na(col_idx)) {
      dataValidation(wb, sheet = "Lookup", cols = col_idx, rows = 2:500, 
                     type = "list", value = validation_ranges[[kcp_type]], allowBlank = TRUE)
    }
  }
  
  if (!is.na(group_col_idx)) {
    group_start <- options_row + 1
    writeData(wb, "Options", x = "GROUP_CODE", startCol = 1, startRow = group_start)
    writeData(wb, "Options", x = data.frame(GROUP_CODE = meta_info$groups), startCol = 1, startRow = group_start + 1)
    
    dataValidation(wb, sheet = "Lookup", cols = group_col_idx, rows = 2:500, 
                   type = "list", value = sprintf("'Options'!$A$%d:$A$%d", group_start + 2, group_start + 1 + length(meta_info$groups)), allowBlank = FALSE)
  }
  
  if (!is.null(meta_info$catalog)) {
    writeDataTable(wb, sheet = "Options", x = meta_info$catalog[, c("KCP_Type", "Folder", "KCP_Name", "KCP_Path")], startCol = 4, startRow = 1, tableName = "KCP_Options_Detail", tableStyle = "TableStyleMedium9")
  }
  
  sheetVisibility(wb)[which(names(wb) == "Options")] <- "hidden"
  return(wb)
}

# ------------------------------------------------------------------------------
# 2. USER INTERFACE ARCHITECTURE
# ------------------------------------------------------------------------------
sys_cores <- parallel::detectCores()
if (is.na(sys_cores)) sys_cores <- 1
def_cores <- max(1, floor(sys_cores / 4))

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
            shinyFiles::shinyDirButton(
              id = "browse_root",
              label = "Browse...",
              title = "Select Root Folder Path",
              buttonType = "default",
              class = "action-button"
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
            shinyFiles::shinyDirButton(
              id = "browse_kcp",
              label = "Browse...",
              title = "Select KCP Directory",
              buttonType = "default",
              class = "action-button"
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
                  src = paste0("workflow_assets/", WorkflowImageFile),
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

# ------------------------------------------------------------------------------
# 3. COMPONENT EXECUTION SERVER SIDE LOGIC
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  bg_gen <- reactiveVal(NULL)
  bg_run <- reactiveVal(NULL)
  bg_merge <- reactiveVal(NULL)

  step_start_gen <- reactiveVal(NULL)
  step_start_run <- reactiveVal(NULL)
  step_start_merge <- reactiveVal(NULL)
  
  gen_prog <- NULL
  run_prog <- NULL
  merge_prog <- NULL
  
  prog_file_gen <- tempfile(pattern = "gen_", fileext = ".txt")
  prog_file_run <- tempfile(pattern = "run_", fileext = ".txt")
  prog_file_merge <- tempfile(pattern = "merge_", fileext = ".txt")

  format_elapsed <- function(start_time) {
    if (is.null(start_time) || is.na(start_time)) return("N/A")
    secs <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    secs <- max(0, round(secs))
    hh <- secs %/% 3600
    mm <- (secs %% 3600) %/% 60
    ss <- secs %% 60
    sprintf("%02d:%02d:%02d", hh, mm, ss)
  }
  
  # --- UI BUTTON BROWSER EVENT OBSERVERS ---
  
  sys_volumes <- c("Current Workspace" = getwd(), shinyFiles::getVolumes()())
  shinyFiles::shinyDirChoose(input, "browse_root", roots = sys_volumes)
  
  observeEvent(input$browse_root, {
    if (!is.integer(input$browse_root)) {
      selected_dir <- shinyFiles::parseDirPath(sys_volumes, input$browse_root)
      if (length(selected_dir) > 0 && nzchar(selected_dir[1])) {
        updateTextInput(session, "root_dir", value = normalizePath(selected_dir[1], winslash = "/", mustWork = FALSE))
      }
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$master_db_upload, {
    file_info <- input$master_db_upload
    req(file_info)

    runtime_root <- normalizePath(input$root_dir, winslash = "/", mustWork = FALSE)
    inputs_dir <- normalizePath(file.path(runtime_root, "Inputs"), winslash = "/", mustWork = FALSE)
    if (!dir.exists(inputs_dir)) dir.create(inputs_dir, recursive = TRUE, showWarnings = FALSE)

    db_name <- basename(file_info$name)
    db_dest <- file.path(inputs_dir, db_name)
    copied <- tryCatch(file.copy(file_info$datapath, db_dest, overwrite = TRUE), error = function(e) FALSE)

    if (!isTRUE(copied)) {
      showNotification("Failed to stage uploaded database into Inputs.", type = "error")
      return()
    }

    updateTextInput(session, "master_db", value = db_name)
    showNotification(sprintf("Master database staged to %s", db_dest), type = "message")
  }, ignoreInit = TRUE)
  
  observeEvent(input$root_dir, {
    runtime_root <- normalizePath(input$root_dir, winslash = "/", mustWork = FALSE)
    inputs_dir <- normalizePath(file.path(runtime_root, "Inputs"), winslash = "/", mustWork = FALSE)
    if (dir.exists(inputs_dir)) {
      available_dbs <- list.files(inputs_dir, pattern = "\\.(db|sqlite)$", ignore.case = TRUE, full.names = FALSE)
      if (length(available_dbs) > 0) {
        new_db <- available_dbs[1]
        updateTextInput(session, "master_db", value = new_db)
        shinyjs::runjs(sprintf("$('#master_db_upload').closest('.input-group').find('input[type=\"text\"]').val('%s');", new_db))
      }
    }
  }, ignoreInit = TRUE)
  
  shinyFiles::shinyDirChoose(input, "browse_kcp", roots = sys_volumes)
  
  observeEvent(input$browse_kcp, {
    if (!is.integer(input$browse_kcp)) {
      selected_dir <- shinyFiles::parseDirPath(sys_volumes, input$browse_kcp)
      if (length(selected_dir) > 0 && nzchar(selected_dir[1])) {
        updateTextInput(session, "kcp_dir", value = normalizePath(selected_dir[1], winslash = "/", mustWork = FALSE))
      }
    }
  })
  
  resolve_db_path <- function(root_dir, db_name, slash = "/") {
    db_name <- trimws(db_name)
    if (grepl("^([A-Za-z]:|\\\\|/)", db_name)) {
      normalizePath(db_name, winslash = slash, mustWork = FALSE)
    } else {
      normalizePath(file.path(root_dir, "Inputs", db_name), winslash = slash, mustWork = FALSE)
    }
  }
  
  resolve_kcp_path <- function(root_dir, dir_name, slash = "/") {
    dir_name <- trimws(dir_name)
    if (grepl("^([A-Za-z]:|\\\\|/)", dir_name)) {
      normalizePath(dir_name, winslash = slash, mustWork = FALSE)
    } else {
      normalizePath(file.path(root_dir, dir_name), winslash = slash, mustWork = FALSE)
    }
  }
  
  meta <- reactiveValues(groups = NULL, catalog = NULL, types = NULL)
  grid_data <- reactiveVal(data.frame())
  
  observeEvent(c(input$master_db, input$root_dir), {
    req(input$master_db, input$root_dir)
    full_db_path <- resolve_db_path(input$root_dir, input$master_db)
    if (file.exists(full_db_path)) {
      tryCatch({
        local({
          con <- dbConnect(SQLite(), full_db_path)
          on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)
          tables <- dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table' AND name LIKE 'FVS_StandInit%'")$name
          if (length(tables) > 0) {
            tables_lower <- tolower(tables)
            if ("fvs_standinit_cond" %in% tables_lower) {
              target_tbl <- tables[tables_lower == "fvs_standinit_cond"][1]
            } else if ("fvs_standinit" %in% tables_lower) {
              target_tbl <- tables[tables_lower == "fvs_standinit"][1]
            } else if ("fvs_standinit_plot" %in% tables_lower) {
              target_tbl <- tables[tables_lower == "fvs_standinit_plot"][1]
            } else {
              target_tbl <- tables[1]
            }
            updateTextInput(session, "stand_tbl", value = target_tbl)
          }
        })
      }, error = function(e) {})
    }
  }, ignoreInit = FALSE)
  
  observeEvent(input$stand_tbl, {
    req(input$master_db, input$root_dir, input$stand_tbl)
    full_db_path <- resolve_db_path(input$root_dir, input$master_db)
    if (file.exists(full_db_path)) {
      tryCatch({
        local({
          con <- dbConnect(SQLite(), full_db_path)
          on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)
          if (dbExistsTable(con, input$stand_tbl)) {
            cols <- dbListFields(con, input$stand_tbl)
            if ("VARIANT" %in% toupper(cols)) {
              var_query <- sprintf("SELECT DISTINCT VARIANT FROM %s WHERE VARIANT IS NOT NULL AND TRIM(VARIANT) != ''", quote_sql_identifier(input$stand_tbl))
              var_df <- dbGetQuery(con, var_query)
              if (nrow(var_df) > 0) {
                unique_vars <- unique(tolower(trimws(var_df$VARIANT)))
                if (length(unique_vars) > 1) {
                  showNotification(sprintf("Multiple FVS Variants detected (%s).", paste(toupper(unique_vars), collapse = ", ")), type = "message")
                }
              }
            }
          }
        })
      }, error = function(e) {})
    }
  })
  
  manifest_path <- reactive({
    req(input$root_dir)
    normalizePath(file.path(input$root_dir, "rFVS_Runs", "KCP_AddFile_Manifest.csv"), winslash = "/", mustWork = FALSE)
  })
  
  # Reactive value to explicitly trigger job queue recalculation when the manifest is successfully written to disk
  manifest_trigger <- reactiveVal(0)
  
  observe({
    jq <- get_job_queue()
    if (!is.null(jq) && nrow(jq) > 0) {
      valid_choices <- c("ALL", unique(jq$Scenario))
      current_sel <- isolate(input$overwrite_scens)
      retained_sel <- current_sel[current_sel %in% valid_choices]
      updateSelectInput(session, "overwrite_scens", choices = valid_choices, selected = retained_sel)
    }
  })
  
  # Retrieves job queue payload connecting active inputs, user manifests and physical directories
  get_job_queue <- reactive({
    # Establish a reactive dependency on the save button so it re-evaluates when the user updates the manifest
    input$save_matrix
    
    # To prevent reactive race conditions between file writing and UI updates
    # we explicitly wait for this trigger (fired only AFTER the file finishes saving)
    manifest_trigger()
    
    req(input$root_dir, input$master_db, input$group_col, input$stand_tbl)
    
    full_db_path <- resolve_db_path(input$root_dir, input$master_db)
    m_file <- manifest_path()
    
    if (!file.exists(full_db_path) || !file.exists(m_file)) return(NULL)
    
    tryCatch({
      # Scan target database tables inside standard FVS SQLite structures
      stInitDF <- local({
        con <- dbConnect(SQLite(), full_db_path)
        on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)
        if (!dbExistsTable(con, input$stand_tbl)) {
          return(NULL)
        }
        
        cols <- dbListFields(con, input$stand_tbl)
        has_var <- "VARIANT" %in% toupper(cols)
        var_str <- if (has_var) ", VARIANT" else ""
        
        query <- sprintf("SELECT STAND_ID, STAND_CN, %s AS GROUP_CODE %s FROM %s", 
                         quote_sql_identifier(input$group_col), var_str, quote_sql_identifier(input$stand_tbl))
        res <- dbGetQuery(con, query)
        attr(res, "has_var") <- has_var
        res
      })
      
      if (is.null(stInitDF)) return(NULL)
      has_var <- attr(stInitDF, "has_var")
      
      stInitDF$GROUP_CODE <- trimws(as.character(stInitDF$GROUP_CODE))
      if (has_var) {
        stInitDF$VARIANT <- paste0("FVS", tolower(trimws(as.character(stInitDF$VARIANT))))
      }
      
      
      ex_groups <- unlist(strsplit(input$exclude_grps, "\\s*,\\s*"))
      stInitDF <- subset(stInitDF, !is.na(GROUP_CODE) & nzchar(GROUP_CODE) & !(tolower(GROUP_CODE) %in% tolower(ex_groups)))
      
      manifest_df <- read.csv(m_file, stringsAsFactors = FALSE, colClasses = "character")
      if (is.null(manifest_df) || nrow(manifest_df) == 0) return(NULL)
      
      manifest_df$GROUP_CODE <- trimws(as.character(manifest_df$GROUP_CODE))
      manifest_df <- unique(manifest_df)
      
      job_queue <- merge(stInitDF, manifest_df, by = "GROUP_CODE", all = FALSE)
      
      job_queue <- job_queue[!duplicated(job_queue[, c("STAND_ID", "GROUP_CODE", "Scenario")]), ]
      
      run_base_dir <- normalizePath(file.path(input$root_dir, "rFVS_Runs"), winslash = "/", mustWork = FALSE)
      job_queue$stand_dir <- file.path(run_base_dir, job_queue$GROUP_CODE, job_queue$Scenario, paste0("fvs_", job_queue$STAND_ID))
      
      return(job_queue)
    }, error = function(e) {
      showNotification(paste("Database Query Error:", e$message), type = "error", duration = 8)
      return(NULL)
    })
  })
  
  # --- SYSTEM SCANNING OBSERVER ---
  # Loads the available database context and dynamic KCP lookup combinations into a central reactive UI table
  observeEvent(input$load_metadata, {
    req(input$root_dir)
    full_db_path <- resolve_db_path(input$root_dir, input$master_db)
    full_kcp_dir <- resolve_kcp_path(input$root_dir, input$kcp_dir)
    
    if (!file.exists(full_db_path)) {
      showNotification("Database target not found at specified Root directory path.", type = "error")
      return()
    }
    if (!dir.exists(full_kcp_dir)) {
      showNotification("KCP directory mapping path could not be located.", type = "warning")
      return()
    }
    
    err_msg <- NULL
    withProgress(message = "Extracting file indexing mappings & building DB indexes...", value = 0.5, {
      ex_groups <- unlist(strsplit(input$exclude_grps, "\\s*,\\s*"))
      tryCatch({
        local({
          con_m <- dbConnect(SQLite(), full_db_path)
          on.exit(try(dbDisconnect(con_m), silent = TRUE), add = TRUE)
          
          # Build indexes on the Stand Init table
          try(dbExecute(con_m, sprintf("CREATE INDEX IF NOT EXISTS idx_%s_cn ON %s (STAND_CN)", input$stand_tbl, quote_sql_identifier(input$stand_tbl))), silent = TRUE)
          try(dbExecute(con_m, sprintf("CREATE INDEX IF NOT EXISTS idx_%s_id ON %s (STAND_ID)", input$stand_tbl, quote_sql_identifier(input$stand_tbl))), silent = TRUE)
          
          # Build indexes on any matching Tree Init tables
          tree_tbl_candidates <- dbGetQuery(con_m, "SELECT name FROM sqlite_master WHERE type='table' AND name LIKE '%treeinit%'")
          if (nrow(tree_tbl_candidates) > 0) {
            for (t_tbl in tree_tbl_candidates$name) {
              try(dbExecute(con_m, sprintf("CREATE INDEX IF NOT EXISTS idx_%s_cn ON %s (STAND_CN)", t_tbl, quote_sql_identifier(t_tbl))), silent = TRUE)
              try(dbExecute(con_m, sprintf("CREATE INDEX IF NOT EXISTS idx_%s_id ON %s (STAND_ID)", t_tbl, quote_sql_identifier(t_tbl))), silent = TRUE)
            }
          }
        })
        
        meta$groups <- get_groups_from_db(full_db_path, input$stand_tbl, input$group_col, ex_groups)
        meta$catalog <- discover_kcp_catalog(full_kcp_dir)
      }, error = function(e) {
        err_msg <<- e$message
      })
    })
    
    if (!is.null(err_msg)) {
      showNotification(err_msg, type = "error")
      return()
    }
    
    if (is.null(meta$catalog) || nrow(meta$catalog) == 0) {
      showNotification("No .kcp files located inside designated catalog directories.", type = "warning")
      return()
    }
    
    meta$types <- unique(meta$catalog$KCP_Type[order(meta$catalog$TypeOrder)])
    
    df <- data.frame(GROUP_CODE = meta$groups, Scenario = "", stringsAsFactors = FALSE)
    for (t in meta$types) {
      type_rows <- meta$catalog[meta$catalog$KCP_Type == t & !is.na(meta$catalog$KCP_Name), ]
      df[[t]] <- if (t %in% c("Global", "Output") && nrow(type_rows) == 1) type_rows$KCP_Name[1] else ""
    }

    # Allow scenario naming from any editable columns except reserved fields.
    scenario_col_choices <- setdiff(names(df), c("GROUP_CODE", "Scenario"))
    selected_cols <- isolate(input$scenario_add_cols)
    selected_cols <- selected_cols[selected_cols %in% scenario_col_choices]
    updateSelectInput(session, "scenario_add_cols", choices = scenario_col_choices, selected = selected_cols)

    df <- recalculate_scenarios(df, scenario_cols = selected_cols)
    grid_data(df)
    
    # Auto-export the default setup files
    tryCatch({
      out_dir <- file.path(input$root_dir, "rFVS_Runs")
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      
      auto_wb_path <- file.path(out_dir, "KCP_Lookup_Table.xlsx")
      if (!file.exists(auto_wb_path)) {
        wb <- create_lookup_wb(df, meta, selected_cols)
        saveWorkbook(wb, auto_wb_path, overwrite = TRUE)
        strip_missing_drawing_relationships(auto_wb_path)
      }
      
      opt_path <- file.path(out_dir, "KCP_Lookup_Options.csv")
      write.csv(meta$catalog[, c("KCP_Type", "Folder", "KCP_Name", "KCP_Path")], opt_path, row.names = FALSE, na = "")
      
    }, error = function(e) {
      warning("Could not auto-generate default setup files: ", e$message)
    })
    
    showNotification("Directory scan complete. Default matrix and options exported to rFVS_Runs.", type = "message")
  })
  
  output$meta_status <- renderText({
    if (is.null(meta$groups)) {
      "System State: Awaiting initialization scan triggers."
    } else {
      full_db_path <- resolve_db_path(input$root_dir, input$master_db)
      full_kcp_dir <- resolve_kcp_path(input$root_dir, input$kcp_dir)
      
      sprintf(
        "Active Catalog Status:\n - Database Path: %s\n - KCP Directory Path: %s\n - Discovered Groups: %d\n - Tracked .KCP Source Files: %d\n - Dynamic KCP Lookup Types: %s",
        full_db_path, full_kcp_dir, length(meta$groups), nrow(meta$catalog), paste(meta$types, collapse = ", ")
      )
    }
  })
  
  output$prescription_table <- renderRHandsontable({
    df <- grid_data()
    req(nrow(df) > 0)
    
    hot <- rhandsontable(df, rowHeaders = TRUE, stretchH = "all")
    
    if (!is.null(meta$catalog)) {
      hot <- hot %>% hot_col(col = "GROUP_CODE", type = "dropdown", source = c("", meta$groups), strict = FALSE)
      for (t in meta$types) {
        kcp_list <- meta$catalog$KCP_Name[meta$catalog$KCP_Type == t]
        kcp_list <- kcp_list[!is.na(kcp_list)]
        hot <- hot %>% hot_col(col = t, type = "dropdown", source = c("", "ALL", kcp_list), strict = FALSE)
      }
    }
    hot
  })
  
  observeEvent(input$prescription_table, {
    old_df <- grid_data()
    df <- hot_to_r(input$prescription_table)
    df <- recalculate_scenarios(df, old_df, input$scenario_add_cols)
    grid_data(df)
  })

  observeEvent(input$scenario_add_cols, {
    df <- grid_data()
    if (!is.null(df) && nrow(df) > 0) {
      df <- recalculate_scenarios(df, scenario_cols = input$scenario_add_cols, force_auto = TRUE)
      grid_data(df)
    }
  }, ignoreNULL = FALSE)
  
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("FVS_KCP_Lookup_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- grid_data()
      req(nrow(df) > 0)
      
      wb <- create_lookup_wb(df, meta, input$scenario_add_cols)
      saveWorkbook(wb, file, overwrite = TRUE)
      strip_missing_drawing_relationships(file)
    }
  )
  
  observeEvent(input$upload_excel, {
    file_info <- input$upload_excel
    req(file_info)
    
    tryCatch({
      ext <- tools::file_ext(file_info$name)
      uploaded_df <- if (tolower(ext) == "csv") {
        read.csv(file_info$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        read.xlsx(file_info$datapath, sheet = 1)
      }
      
      if (!("GROUP_CODE" %in% names(uploaded_df))) {
        stop("The uploaded spreadsheet is missing the required 'GROUP_CODE' header column.")
      }
      
      if (!is.null(meta$types)) {
        for (t in meta$types) {
          if (!(t %in% names(uploaded_df))) uploaded_df[[t]] <- ""
        }
      }

      # Keep scenario selector aligned with imported structure.
      scenario_col_choices <- setdiff(names(uploaded_df), c("GROUP_CODE", "Scenario"))
      selected_cols <- isolate(input$scenario_add_cols)
      selected_cols <- selected_cols[selected_cols %in% scenario_col_choices]
      updateSelectInput(session, "scenario_add_cols", choices = scenario_col_choices, selected = selected_cols)
      
      uploaded_df$GROUP_CODE <- trimws(as.character(uploaded_df$GROUP_CODE))
      uploaded_df <- uploaded_df[uploaded_df$GROUP_CODE != "" & !is.na(uploaded_df$GROUP_CODE), ]
      uploaded_df <- recalculate_scenarios(uploaded_df, scenario_cols = selected_cols, force_auto = TRUE)
      
      grid_data(uploaded_df)
      showNotification("Excel workbook layout imported successfully. Grid mirrors applied data.", type = "message")
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Spreadsheet Import Failure",
        p(e$message), easyClose = TRUE, footer = modalButton("Dismiss")
      ))
    })
  })
  
  observeEvent(input$save_matrix, {
    df <- grid_data()
    if (nrow(df) == 0) {
      showNotification("No data array available to build scenarios.", type = "error")
      return()
    }
    
    df$GROUP_CODE <- trimws(as.character(df$GROUP_CODE))
    df <- df[df$GROUP_CODE != "" & !is.na(df$GROUP_CODE), ]
    
    if (nrow(df) == 0) {
      showNotification("Table does not contain valid populated GROUP_CODE rows.", type = "error")
      return()
    }
    
    # Prevent ambiguous runs by disallowing duplicate Scenario names.
    scenario_vals <- trimws(as.character(df$Scenario))
    scenario_vals <- scenario_vals[!is.na(scenario_vals) & nzchar(scenario_vals)]
    dup_scenarios <- sort(unique(scenario_vals[duplicated(scenario_vals)]))
    if (length(dup_scenarios) > 0) {
      showModal(modalDialog(
        title = "Duplicate Scenario Names Found",
        p("Each Scenario must be unique. Please edit or remove duplicates before saving."),
        pre(style = "white-space: pre-wrap; word-break: break-all; max-height: 200px;", paste(dup_scenarios, collapse = "\n")),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
      return()
    }
    
    manifest_list <- vector("list", nrow(df))
    
    tryCatch({
      withProgress(message = "Compiling paths manifest...", value = 0, {
        for (i in seq_len(nrow(df))) {
          row_item <- df[i, , drop = FALSE]
          all_paths_extracted <- character(0)
          
          for (t in meta$types) {
            cell_input <- row_item[[t]]
            requested_names <- split_kcp_cell(cell_input)
            if (length(requested_names) == 0) next
            
            type_catalog_rows <- meta$catalog[meta$catalog$KCP_Type == t, ]
            
            if (any(toupper(requested_names) == "ALL")) {
              all_paths_extracted <- c(all_paths_extracted, type_catalog_rows$KCP_Path)
            } else {
              missing_elements <- setdiff(requested_names, type_catalog_rows$KCP_Name)
              if (length(missing_elements) > 0) {
                stop(sprintf("Row %d ('%s') references unknown file entries inside '%s': %s", 
                             i, row_item$GROUP_CODE, t, paste(missing_elements, collapse = ", ")))
              }
              matched_paths <- type_catalog_rows$KCP_Path[match(requested_names, type_catalog_rows$KCP_Name)]
              all_paths_extracted <- c(all_paths_extracted, matched_paths)
            }
          }
          
          cleaned_paths_array <- all_paths_extracted[!duplicated(all_paths_extracted)]
          
          manifest_list[[i]] <- data.frame(
            GROUP_CODE = row_item$GROUP_CODE,
            Scenario   = row_item$Scenario,
            KCP_Paths  = paste(cleaned_paths_array, collapse = "|"),
            KCP_Count  = length(cleaned_paths_array),
            stringsAsFactors = FALSE
          )
          setProgress(i / nrow(df))
        }
      })
      
      final_manifest_df <- do.call(rbind, manifest_list)
      
      m_file <- manifest_path()
      run_base_dir <- dirname(m_file)
      if (!dir.exists(run_base_dir)) dir.create(run_base_dir, recursive = TRUE, showWarnings = FALSE)
      
      write.csv(final_manifest_df, m_file, row.names = FALSE, na = "")
      
      # Explicitly signal to the reactive queue that a new manifest is on disk
      manifest_trigger(manifest_trigger() + 1)
      
      showModal(modalDialog(
        title = "Manifest Compiled Successfully",
        p("Configuration catalog exported to pipeline staging space:"),
        pre(style = "white-space: pre-wrap; word-break: break-all; max-height: 200px;", m_file),
        size = "m",
        easyClose = TRUE, 
        footer = modalButton("Dismiss")
      ))
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Validation Constraint Failure",
        p(e$message), easyClose = TRUE, footer = modalButton("Dismiss")
      ))
    })
  })
  
  output$pipeline_diagnostics <- renderText({
    full_db_path <- resolve_db_path(input$root_dir, input$master_db)
    m_file <- manifest_path()
    
    if (!file.exists(full_db_path)) {
      return("Status: Blocked. Root folder path or master SQL database file is unmapped.")
    }
    if (!file.exists(m_file)) {
      return("Status: Blocked. Active 'KCP_AddFile_Manifest.csv' not found in the rFVS_Runs directory. Save matrix settings on Panel 2 first.")
    }
    
    jq <- get_job_queue()
    if (is.null(jq)) {
      return("Status: Blocked. Job queue query to the database failed. Check the red error notification in the corner (Are STAND_ID and STAND_CN columns present?).")
    }
    if (nrow(jq) == 0) {
      return("Status: Blocked. Manifest file exists but exact GROUP_CODE cross-join with database targets returned 0 jobs.")
    }
    
    sprintf(
      "Pipeline Status: Active Ready Queue\n - Master Database Found: %s\n - Cross-Join Stands Queue: %d active tasks\n - Cores: %d threads requested\n - Project Time Interval: %d cycles (%d total years simulated)\n\n[Ready to generate standalone .key configurations.]",
      basename(full_db_path), nrow(jq), input$num_cores, input$num_cycles, (input$num_cycles * input$time_int)
    )
  })
  
  output$pipeline_diagnostics_rfvs <- renderText({
    full_db_path <- resolve_db_path(input$root_dir, input$master_db)
    m_file <- manifest_path()
    
    if (!file.exists(full_db_path)) {
      return("Status: Blocked. Root folder path or master SQL database file is unmapped.")
    }
    if (!file.exists(m_file)) {
      return("Status: Blocked. Active 'KCP_AddFile_Manifest.csv' not found. Save matrix settings on Panel 2 first.")
    }
    
    jq <- get_job_queue()
    if (is.null(jq)) {
      return("Status: Blocked. Job queue query to the database failed. Check the red error notification in the corner (Are STAND_ID and STAND_CN columns present?).")
    }
    if (nrow(jq) == 0) {
      return("Status: Blocked. Manifest file exists but exact GROUP_CODE cross-join with database targets returned 0 jobs.")
    }
    
    detected_variants <- if ("VARIANT" %in% names(jq)) {
      paste(unique(jq$VARIANT), collapse = ", ")
    } else {
      "None Detected (Ensure VARIANT column exists in Database)"
    }
    
    sprintf(
      "Pipeline Status: Active Ready Queue\n - Master Database Found: %s\n - Cross-Join Stands Queue: %d active tasks\n - Cores: %d threads requested\n - Detected FVS Variants: %s (%d total)\n\n[Ready to dispatch parallel rFVS simulations.]",
      basename(full_db_path), nrow(jq), input$num_cores_rfvs, detected_variants, if ("VARIANT" %in% names(jq)) length(unique(jq$VARIANT)) else 0
    )
  })
  
  output$pipeline_diagnostics_merge <- renderText({
    full_db_path <- resolve_db_path(input$root_dir, input$master_db)
    m_file <- manifest_path()
    
    if (!file.exists(full_db_path) || !file.exists(m_file)) {
      return("Status: Blocked. Setup incomplete.")
    }
    
    jq <- get_job_queue()
    if (is.null(jq) || nrow(jq) == 0) {
      return("Status: Blocked. No jobs available to merge.")
    }
    
    unique_combos <- unique(jq[, c("GROUP_CODE", "Scenario")])
    sprintf(
      "Pipeline Status: Ready to Consolidate\n - Scenarios to Merge: %d\n - Cores: %d threads requested\n - Target Outputs directory: %s\n\n[All scenario DBs will be compiled and then merged into a single master DB.]",
      nrow(unique_combos),
      input$num_cores_merge,
      file.path(input$root_dir, "Outputs")
    )
  })
  
  # ----------------- PIPELINE STEP 1: PARALLEL KEYFILE GENERATION -----------------
  observeEvent(input$gen_keyfiles, {
    shinyjs::disable("gen_keyfiles")
    shinyjs::show("kill_gen_btn")
    step_start_gen(Sys.time())
    
    job_queue <- get_job_queue()
    if (is.null(job_queue) || nrow(job_queue) == 0) {
      shinyjs::enable("gen_keyfiles")
      shinyjs::hide("kill_gen_btn")
      step_start_gen(NULL)
      showNotification("Pipeline Action Denied: No compiled project queue targets found.", type = "error")
      return()
    }
    
    keyfile_db_path <- resolve_db_path(input$root_dir, input$master_db, slash = "\\")
    p_inv_year   <- input$inv_year
    p_time_int   <- input$time_int
    p_num_cycles <- input$num_cycles
    p_cores      <- input$num_cores
    if (is.na(p_cores) || p_cores < 1) p_cores <- 1
    
    p_stand_tbl  <- input$stand_tbl
    p_tree_tbl   <- gsub("StandInit", "TreeInit", p_stand_tbl, ignore.case = TRUE)
    
    unique_scenarios <- unique(job_queue$Scenario)
    total_jobs <- nrow(job_queue)
    
    writeLines("0|Booting up compute cluster (this may take a moment)...", prog_file_gen)
    gen_prog <<- shiny::Progress$new(session, min=0, max=1)
    gen_prog$set(message = "Executing : Building Stand Keyfiles...", value=0, detail = "Booting up compute cluster (this may take a moment)...")
    
    p <- callr::r_bg(function(job_queue, p_cores, p_stand_tbl, p_tree_tbl, p_inv_year, p_time_int, p_num_cycles, keyfile_db_path, prog_file, unique_scenarios, total_jobs) {
      library(parallel)
      library(doSNOW)
      library(uuid)
      
      cl <- makeCluster(p_cores)
      registerDoSNOW(cl)
      on.exit(stopCluster(cl), add = TRUE)
      
      last_update <- Sys.time()
      progress_callback <- function(n) {
        if (as.numeric(difftime(Sys.time(), last_update, units="secs")) > 0.5 || n == total_jobs) {
          writeLines(sprintf("%d|Processed %d of %d keyfiles...", n, n, total_jobs), prog_file)
          last_update <<- Sys.time()
        }
      }
      
      foreach(i = seq_len(total_jobs), 
              .packages = c("uuid"),
              .options.snow = list(progress = progress_callback)) %dopar% {
                
                stand_id      <- job_queue$STAND_ID[i]
                stand_cn      <- job_queue$STAND_CN[i]
                grp_code      <- job_queue$GROUP_CODE[i]
                scenario      <- job_queue$Scenario[i]
                kcp_paths_raw <- job_queue$KCP_Paths[i]
                stand_dir_p   <- job_queue$stand_dir[i]
                
                mgmt_idx <- match(scenario, unique_scenarios)
                mgmt_id  <- sprintf("A%03d", mgmt_idx)
                run_name <- scenario
                
                dir.create(stand_dir_p, recursive = TRUE, showWarnings = FALSE)
                kcp_vector <- unlist(strsplit(kcp_paths_raw, "\\|"))
                
                stack_block <- c(
                  "*--- AUTO-GENERATED ADDFILE KCP STACK ---*",
                  paste0("* GROUP_CODE: ", grp_code),
                  paste0("* Scenario: ", scenario),
                  paste0("* Built: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
                  ""
                )
                
                if (length(kcp_vector) > 0 && nchar(kcp_paths_raw) > 0) {
                  for (k in seq_along(kcp_vector)) {
                    fnum <- 50 + k
                    open_line    <- sprintf("%-10s%10s%10s%10s%10s%10s", "OPEN", paste0(fnum, "."), "0.", "0.", "80.", "0.")
                    addfile_line <- sprintf("%-10s%10s", "ADDFILE", paste0(fnum, "."))
                    close_line   <- sprintf("%-10s%10s", "CLOSE", paste0(fnum, "."))
                    clean_path   <- normalizePath(kcp_vector[k], winslash = "\\", mustWork = FALSE)
                    stack_block  <- c(stack_block, open_line, clean_path, addfile_line, close_line)
                  }
                }
                stack_block <- c(stack_block, "*-----------------------------------------------*")
                
                kw_content <- c(
                  paste0("!!title: ", run_name, "_", stand_id),
                  paste0("!!uuid:  ", UUIDgenerate()),
                  paste0("!!built: ", format(Sys.time(), "%Y-%m-%d_%H:%M:%S")),
                  "StdIdent",
                  sprintf("%-40s%s", stand_id, run_name),
                  "StandCN        ",
                  stand_cn,
                  "MgmtId",
                  mgmt_id,
                  paste0("InvYear       ", p_inv_year),
                  paste0("TimeInt                   ", p_time_int), 
                  paste0("NumCycle     ", p_num_cycles),
                  "",
                  stack_block,
                  "",
                  "Database", 
                  "DSNin", keyfile_db_path, 
                  "StandSQL", paste0("SELECT * FROM ", p_stand_tbl, " WHERE STAND_ID = '", stand_id, "'"), "EndSQL",
                  "TreeSQL",  paste0("SELECT * FROM ", p_tree_tbl, " WHERE STAND_ID = '", stand_id, "'"), "EndSQL",
                  "End",
                  "",
                  "Process",
                  "Stop"
                )
                
                writeLines(kw_content, file.path(stand_dir_p, "run.key"), useBytes = TRUE)
                return(TRUE)
              }
      return(total_jobs)
    }, args = list(job_queue, p_cores, p_stand_tbl, p_tree_tbl, p_inv_year, p_time_int, p_num_cycles, keyfile_db_path, prog_file_gen, unique_scenarios, total_jobs), supervise = TRUE)
    
    bg_gen(p)
  })
  
  observe({
    p <- bg_gen()
    req(p)
    invalidateLater(500, session)
    
    if (p$is_alive()) {
      if (file.exists(prog_file_gen)) {
        l <- suppressWarnings(readLines(prog_file_gen))
        if (length(l) > 0) {
          parts <- strsplit(l[length(l)], "\\|")[[1]]
          if (length(parts) >= 2 && !is.null(gen_prog)) {
            val <- as.numeric(parts[1]) / nrow(isolate(get_job_queue()))
            gen_prog$set(value = val, detail = paste(parts[-1], collapse="|"))
          }
        }
      }
    } else {
      elapsed_gen <- format_elapsed(step_start_gen())
      step_start_gen(NULL)
      bg_gen(NULL)
      if (!is.null(gen_prog)) gen_prog$close()
      gen_prog <<- NULL
      shinyjs::enable("gen_keyfiles")
      shinyjs::hide("kill_gen_btn")
      updateActionButton(session, "kill_gen_btn", label = "Cancel", icon = icon("xmark"))
      
      if (p$get_exit_status() == 0) {
        showModal(modalDialog(
          title = "Step 1 Complete: Keyfiles Isolated",
          p(sprintf("Successfully deployed and structured %s FVS keyfiles across processing nodes.", p$get_result())),
          p(sprintf("Elapsed Time: %s", elapsed_gen)),
          easyClose = TRUE, footer = modalButton("Dismiss")
        ))
      } else {
        err <- p$read_error_lines()
        showModal(modalDialog(
          title = "Keyfile Engine Initialization Failure",
          p(paste(err, collapse = "\n")), easyClose = TRUE, footer = modalButton("Dismiss")
        ))
      }
    }
  })
  
  observeEvent(input$kill_gen_btn, priority=110, {
    p <- bg_gen()
    if (!is.null(p) && p$is_alive()) {
      p$kill_tree()
      bg_gen(NULL)
      if (!is.null(gen_prog)) gen_prog$close()
      gen_prog <<- NULL
      step_start_gen(NULL)
      shinyjs::enable("gen_keyfiles")
      shinyjs::hide("kill_gen_btn")
      updateActionButton(session, "kill_gen_btn", label = "Cancel", icon = icon("xmark"))
      showNotification("Keyfile generation cancelled by user.", type = "warning", duration = 5)
    }
  })
  
  # ----------------- PIPELINE STEP 2: PARALLEL rFVS ENGINE RUNS -----------------
  observeEvent(input$run_rfvs, {
    shinyjs::disable("run_rfvs")
    shinyjs::show("kill_run_btn")
    step_start_run(Sys.time())
    
    job_queue <- get_job_queue()
    if (is.null(job_queue) || nrow(job_queue) == 0) {
      shinyjs::enable("run_rfvs")
      shinyjs::hide("kill_run_btn")
      step_start_run(NULL)
      showNotification("Pipeline Action Denied: No execution targets map to the system queue.", type = "error")
      return()
    }
    
    p_bin_loc    <- input$bin_loc
    p_cores      <- input$num_cores_rfvs
    if (is.na(p_cores) || p_cores < 1) p_cores <- 1
    total_jobs   <- nrow(job_queue)
    
    if (!("VARIANT" %in% names(job_queue))) {
      shinyjs::enable("run_rfvs")
      shinyjs::hide("kill_run_btn")
      step_start_run(NULL)
      showNotification("Pipeline Action Denied: VARIANT column not found in database. Cannot auto-detect variant.", type = "error")
      return()
    }
    
    p_overwrite <- input$overwrite_scens
    if (is.null(p_overwrite)) p_overwrite <- character(0)
    
    writeLines("0|Booting up compute cluster (this may take a moment)...", prog_file_run)
    run_prog <<- shiny::Progress$new(session, min=0, max=1)
    run_prog$set(message = "Executing : Dispatching Parallel rFVS Runs...", value=0, detail = "Booting up compute cluster (this may take a moment)...")
    
    p <- callr::r_bg(function(job_queue, p_cores, p_bin_loc, p_overwrite, prog_file, total_jobs) {
      library(parallel)
      library(doSNOW)
      library(rFVS)

      # Keep workers in same-variant streaks to reduce repeated fvsLoad() calls.
      job_queue <- job_queue[order(job_queue$VARIANT, job_queue$Scenario, job_queue$STAND_ID), , drop = FALSE]

      writeLines("0|Initializing FVS binaries mapped to worker RAM...", prog_file)
      cl <- makeCluster(p_cores)
      on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
      registerDoSNOW(cl)

      clusterExport(cl, c("p_bin_loc", "p_overwrite"), envir = environment())
      clusterEvalQ(cl, {
        library(rFVS)
        .fvs_worker_state <- new.env(parent = emptyenv())
        .fvs_worker_state$active_variant <- NA_character_
        NULL
      })

      last_update <- Sys.time()
      progress_callback_rfvs <- function(n) {
        if (as.numeric(difftime(Sys.time(), last_update, units = "secs")) > 0.5 || n == total_jobs) {
          writeLines(sprintf("%d|Processed %d of %d simulation binaries via worker clusters...", n, n, total_jobs), prog_file)
          last_update <<- Sys.time()
        }
      }

      conns_before <- as.integer(rownames(showConnections(all = FALSE)))

      foreach(i = seq_len(total_jobs),
              .packages = c("rFVS"),
              .options.snow = list(progress = progress_callback_rfvs)) %dopar% {

                stand_dir_path <- job_queue$stand_dir[i]
                variant_i <- job_queue$VARIANT[i]
                scenario_i <- job_queue$Scenario[i]

                if (!dir.exists(stand_dir_path)) return(FALSE)

                # Each worker reloads rFVS only when variant changes on that worker.
                if (!exists(".fvs_worker_state", envir = .GlobalEnv, inherits = FALSE)) {
                  assign(".fvs_worker_state", new.env(parent = emptyenv()), envir = .GlobalEnv)
                  .GlobalEnv$.fvs_worker_state$active_variant <- NA_character_
                }
                if (!identical(.GlobalEnv$.fvs_worker_state$active_variant, variant_i)) {
                  rFVS::fvsLoad(bin = p_bin_loc, fvsProgram = variant_i)
                  .GlobalEnv$.fvs_worker_state$active_variant <- variant_i
                }

                is_overwrite <- ("ALL" %in% p_overwrite) || (scenario_i %in% p_overwrite)

                db_files <- list.files(stand_dir_path, pattern = "\\.db$", full.names = FALSE, ignore.case = TRUE)

                if (!is_overwrite) {
                  if (length(db_files) > 1) {
                    cat(sprintf("[%s] rFVS Runtime Error: Found multiple .db files in %s: %s\n", Sys.time(), stand_dir_path, paste(db_files, collapse = ", ")), file = file.path(stand_dir_path, "fvs_runtime_error.log"), append = TRUE)
                    return(FALSE)
                  }
                  if (length(db_files) == 1) {
                    return(TRUE)
                  }
                } else {
                  files_to_delete <- list.files(stand_dir_path, pattern = "\\.(db|out)$", full.names = TRUE, ignore.case = TRUE)
                  if (length(files_to_delete) > 0) {
                    unlink(files_to_delete)
                  }
                }

                orig_wd <- getwd()
                setwd(stand_dir_path)

                tryCatch({
                  rFVS::fvsSetCmdLine(cl = "--keywordfile=run.key")
                  rFVS::fvsRun()

                  post_db_files <- list.files(stand_dir_path, pattern = "\\.db$", full.names = FALSE, ignore.case = TRUE)
                  if (length(post_db_files) > 1) {
                    cat(sprintf("[%s] rFVS Runtime Error: Multiple .db files detected after run in %s: %s\n", Sys.time(), stand_dir_path, paste(post_db_files, collapse = ", ")), file = file.path(stand_dir_path, "fvs_runtime_error.log"), append = TRUE)
                    return(FALSE)
                  }
                  return(TRUE)
                }, error = function(e) {
                  cat(sprintf("[%s] rFVS Runtime Error: %s\n", Sys.time(), e$message), file = file.path(stand_dir_path, "fvs_runtime_error.log"))
                  return(FALSE)
                }, finally = {
                  setwd(orig_wd)
                })
              }

      conns_after <- as.integer(rownames(showConnections(all = FALSE)))
      for (cn in setdiff(conns_after, conns_before)) try(close(getConnection(cn)), silent = TRUE)
      return(total_jobs)
    }, args = list(job_queue, p_cores, p_bin_loc, p_overwrite, prog_file_run, total_jobs), supervise = TRUE)
    
    bg_run(p)
  })
  
  observe({
    p <- bg_run()
    req(p)
    invalidateLater(500, session)
    
    if (p$is_alive()) {
      if (file.exists(prog_file_run)) {
        l <- suppressWarnings(readLines(prog_file_run))
        if (length(l) > 0) {
          parts <- strsplit(l[length(l)], "\\|")[[1]]
          if (length(parts) >= 2 && !is.null(run_prog)) {
            val <- as.numeric(parts[1]) / nrow(isolate(get_job_queue()))
            run_prog$set(value = val, detail = paste(parts[-1], collapse="|"))
          }
        }
      }
    } else {
      elapsed_run <- format_elapsed(step_start_run())
      step_start_run(NULL)
      bg_run(NULL)
      if (!is.null(run_prog)) run_prog$close()
      run_prog <<- NULL
      shinyjs::enable("run_rfvs")
      shinyjs::hide("kill_run_btn")
      updateActionButton(session, "kill_run_btn", label = "Cancel", icon = icon("xmark"))
      
      if (p$get_exit_status() == 0) {
        showModal(modalDialog(
          title = "Step 2 Complete: Simulations Ran",
          p(sprintf("Successfully processed %s simulation binaries via worker clusters.", p$get_result())),
          p(sprintf("Elapsed Time: %s", elapsed_run)),
          easyClose = TRUE, footer = modalButton("Dismiss")
        ))
      } else {
        err <- p$read_error_lines()
        showModal(modalDialog(
          title = "rFVS Execution Failure",
          p(paste(err, collapse = "\n")), easyClose = TRUE, footer = modalButton("Dismiss")
        ))
      }
    }
  })
  
  observeEvent(input$kill_run_btn, priority=110, {
    p <- bg_run()
    if (!is.null(p) && p$is_alive()) {
      p$kill_tree()
      bg_run(NULL)
      if (!is.null(run_prog)) run_prog$close()
      run_prog <<- NULL
      step_start_run(NULL)
      shinyjs::enable("run_rfvs")
      shinyjs::hide("kill_run_btn")
      updateActionButton(session, "kill_run_btn", label = "Cancel", icon = icon("xmark"))
      showNotification("rFVS execution cancelled by user.", type = "warning", duration = 5)
    }
  })
  
  # ----------------- PIPELINE STEP 3: CONSOLIDATE MASTER OUTPUTS -----------------
  observeEvent(input$merge_outputs, {
    shinyjs::disable("merge_outputs")
    shinyjs::show("kill_merge_btn")
    step_start_merge(Sys.time())
    
    job_queue <- get_job_queue()
    if (is.null(job_queue) || nrow(job_queue) == 0) {
      shinyjs::enable("merge_outputs")
      shinyjs::hide("kill_merge_btn")
      step_start_merge(NULL)
      showNotification("Pipeline Action Denied: No execution targets map to the system queue.", type = "error")
      return()
    }
    
    output_base_dir <- file.path(input$root_dir, "Outputs")
    if (!dir.exists(output_base_dir)) dir.create(output_base_dir, recursive = TRUE, showWarnings = FALSE)
    
    unique_combos <- unique(job_queue[, c("GROUP_CODE", "Scenario")])
    total_combos  <- nrow(unique_combos)
    p_cores      <- input$num_cores_merge
    if (is.na(p_cores) || p_cores < 1) p_cores <- 1
    
    writeLines("0|Booting up compute cluster (this may take a moment)...", prog_file_merge)
    merge_prog <<- shiny::Progress$new(session, min=0, max=1)
    merge_prog$set(message = "Executing : Consolidating Master Outputs...", value=0, detail = "Booting up compute cluster (this may take a moment)...")
    
    p <- callr::r_bg(function(job_queue, p_cores, output_base_dir, unique_combos, total_combos, prog_file) {
      library(parallel)
      library(doSNOW)
      library(foreach)
      library(RSQLite)

      qid <- function(x) paste0('"', gsub('"', '""', x), '"')
      total_errors <- 0

      # Phase 1: merge each Group/Scenario independently in parallel.
      worker_cores <- max(1, min(as.integer(p_cores), as.integer(total_combos)))
      cl <- makeCluster(worker_cores)
      on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
      registerDoSNOW(cl)

      last_update <- Sys.time()
      progress_callback <- function(n) {
        if (as.numeric(difftime(Sys.time(), last_update, units = "secs")) > 0.5 || n == total_combos) {
          pct <- (n / total_combos) * 0.85
          writeLines(sprintf("%f|Merged %d of %d Group/Scenario databases...", pct, n, total_combos), prog_file)
          last_update <<- Sys.time()
        }
      }

      phase1_results <- foreach(
        combo_idx = seq_len(total_combos),
        .packages = c("RSQLite"),
        .options.snow = list(progress = progress_callback)
      ) %dopar% {
        qid_local <- function(x) paste0('"', gsub('"', '""', x), '"')
        grp  <- unique_combos$GROUP_CODE[combo_idx]
        scen <- unique_combos$Scenario[combo_idx]

        grp_out_dir <- file.path(output_base_dir, grp, scen)
        dir.create(grp_out_dir, recursive = TRUE, showWarnings = FALSE)
        scen_out_db <- file.path(grp_out_dir, sprintf("FVSOut_%s.db", scen))
        if (file.exists(scen_out_db)) unlink(scen_out_db)

        combo_jobs <- subset(job_queue, GROUP_CODE == grp & Scenario == scen)
        if (nrow(combo_jobs) == 0) {
          return(list(errors = 0L, scen_db = NA_character_))
        }

        local_errors <- 0L
        log_file <- file.path(grp_out_dir, "merge_errors.log")
        m_con <- dbConnect(SQLite(), scen_out_db)

        tryCatch({
          dbExecute(m_con, "PRAGMA synchronous = OFF")
          dbExecute(m_con, "PRAGMA journal_mode = MEMORY")
          dbExecute(m_con, "PRAGMA temp_store = MEMORY")
          dbExecute(m_con, "PRAGMA cache_size = -200000")

          created_tables <- dbListTables(m_con)

          for (j in seq_len(nrow(combo_jobs))) {
            sid <- combo_jobs$STAND_ID[j]
            s_dir <- combo_jobs$stand_dir[j]

            db_files <- list.files(s_dir, pattern = "\\.db$", full.names = FALSE, ignore.case = TRUE)
            if (length(db_files) == 0) next
            if (length(db_files) > 1) {
              local_errors <- local_errors + 1L
              cat(sprintf("[%s] Stand %s Error: Found multiple .db files in %s: %s.\n", Sys.time(), sid, s_dir, paste(db_files, collapse = ", ")), file = log_file, append = TRUE)
              next
            }

            stand_db <- file.path(s_dir, db_files[1])

            tryCatch({
              safe_stand_db_path <- normalizePath(stand_db, winslash = "/", mustWork = TRUE)
              try(dbExecute(m_con, "DETACH DATABASE srcdb"), silent = TRUE)
              dbExecute(m_con, sprintf("ATTACH DATABASE '%s' AS srcdb", safe_stand_db_path))

              dbBegin(m_con)
              tables_in_src <- dbGetQuery(m_con, "SELECT name FROM srcdb.sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%'")$name
              for (tbl_name in tables_in_src) {
                tbl_q <- qid_local(tbl_name)
                if (!(tbl_name %in% created_tables)) {
                  dbExecute(m_con, sprintf("CREATE TABLE %s AS SELECT * FROM srcdb.%s", tbl_q, tbl_q))
                  created_tables <- c(created_tables, tbl_name)
                } else {
                  dbExecute(m_con, sprintf("INSERT INTO %s SELECT * FROM srcdb.%s", tbl_q, tbl_q))
                }
              }
              dbCommit(m_con)
              dbExecute(m_con, "DETACH DATABASE srcdb")
            }, error = function(e) {
              try(dbRollback(m_con), silent = TRUE)
              try(dbExecute(m_con, "DETACH DATABASE srcdb"), silent = TRUE)
              local_errors <<- local_errors + 1L
              cat(sprintf("[%s] Stand %s Error: %s\n", Sys.time(), sid, e$message), file = log_file, append = TRUE)
            })
          }
        }, finally = {
          try(dbDisconnect(m_con), silent = TRUE)
        })

        list(errors = local_errors, scen_db = if (file.exists(scen_out_db)) scen_out_db else NA_character_)
      }

      total_errors <- total_errors + sum(vapply(phase1_results, function(x) x$errors, integer(1)), na.rm = TRUE)
      scen_db_paths <- unique(vapply(phase1_results, function(x) x$scen_db, character(1)))
      scen_db_paths <- scen_db_paths[!is.na(scen_db_paths) & nzchar(scen_db_paths)]

      # Phase 2: consolidate scenario DBs into master with SQLite streaming merges.
      writeLines(sprintf("%f|Creating single master output database...", 0.9), prog_file)
      mega_db_name <- sprintf("FVS_Out_%s.db", format(Sys.time(), "%Y%m%d_%H%M%S"))
      mega_db_path <- file.path(output_base_dir, mega_db_name)

      if (length(scen_db_paths) > 0) {
        all_rFVS_sims <- dbConnect(SQLite(), mega_db_path)
        on.exit(try(dbDisconnect(all_rFVS_sims), silent = TRUE), add = TRUE)
        dbExecute(all_rFVS_sims, "PRAGMA synchronous = OFF")
        dbExecute(all_rFVS_sims, "PRAGMA journal_mode = MEMORY")
        dbExecute(all_rFVS_sims, "PRAGMA temp_store = MEMORY")
        dbExecute(all_rFVS_sims, "PRAGMA cache_size = -200000")
        mega_created_tables <- character(0)

        for (k in seq_along(scen_db_paths)) {
          scen_db_path <- scen_db_paths[k]
          tryCatch({
            safe_path <- normalizePath(scen_db_path, winslash = "/", mustWork = TRUE)
            try(dbExecute(all_rFVS_sims, "DETACH DATABASE sDB"), silent = TRUE)
            dbExecute(all_rFVS_sims, sprintf("ATTACH DATABASE '%s' AS sDB", safe_path))
            dbBegin(all_rFVS_sims)
            tabs <- dbGetQuery(all_rFVS_sims, "SELECT name FROM sDB.sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%'")$name

            for (tbl in tabs) {
              tbl_q <- qid(tbl)
              if (!(tbl %in% mega_created_tables)) {
                dbExecute(all_rFVS_sims, sprintf("CREATE TABLE %s AS SELECT * FROM sDB.%s", tbl_q, tbl_q))
                mega_created_tables <- c(mega_created_tables, tbl)
              } else {
                dbExecute(all_rFVS_sims, sprintf("INSERT INTO %s SELECT * FROM sDB.%s", tbl_q, tbl_q))
              }
            }
            dbCommit(all_rFVS_sims)
            dbExecute(all_rFVS_sims, "DETACH DATABASE sDB")
          }, error = function(e) {
            try(dbRollback(all_rFVS_sims), silent = TRUE)
            try(dbExecute(all_rFVS_sims, "DETACH DATABASE sDB"), silent = TRUE)
            total_errors <<- total_errors + 1
            cat(sprintf("[%s] Full Project Merge Error for %s: %s\n", Sys.time(), scen_db_path, e$message), file = file.path(output_base_dir, "full_project_merge_errors.log"), append = TRUE)
          })

          if (k %% 5 == 0 || k == length(scen_db_paths)) {
            pct <- 0.9 + (k / length(scen_db_paths)) * 0.09
            writeLines(sprintf("%f|Building project master DB: %d of %d scenario DBs merged...", pct, k, length(scen_db_paths)), prog_file)
          }
        }
      }

      writeLines("1|Merge complete.", prog_file)
      return(total_errors)
    }, args = list(job_queue, p_cores, output_base_dir, unique_combos, total_combos, prog_file_merge), supervise = TRUE)
    
    bg_merge(p)
  })
  
  observe({
    p <- bg_merge()
    req(p)
    invalidateLater(500, session)
    
    if (p$is_alive()) {
      if (file.exists(prog_file_merge)) {
        l <- suppressWarnings(readLines(prog_file_merge))
        if (length(l) > 0) {
          parts <- strsplit(l[length(l)], "\\|")[[1]]
          if (length(parts) >= 2 && !is.null(merge_prog)) {
            val <- as.numeric(parts[1])
            merge_prog$set(value = val, detail = paste(parts[-1], collapse="|"))
          }
        }
      }
    } else {
      elapsed_merge <- format_elapsed(step_start_merge())
      step_start_merge(NULL)
      bg_merge(NULL)
      if (!is.null(merge_prog)) merge_prog$close()
      merge_prog <<- NULL
      shinyjs::enable("merge_outputs")
      shinyjs::hide("kill_merge_btn")
      updateActionButton(session, "kill_merge_btn", label = "Cancel", icon = icon("xmark"))
      
      if (p$get_exit_status() == 0) {
        total_errors <- p$get_result()
        if (total_errors > 0) {
          showModal(modalDialog(
            title = "Step 3 Complete (With Errors)",
            p(sprintf("Merge finalized, but %d individual databases failed to append.", total_errors)),
            p(sprintf("Elapsed Time: %s", elapsed_merge)),
            easyClose = TRUE, footer = modalButton("Dismiss")
          ))
        } else {
          showModal(modalDialog(
            title = "Step 3 Complete: Databases Merged",
            p("All database outputs successfully consolidated."),
            p(sprintf("Elapsed Time: %s", elapsed_merge)),
            easyClose = TRUE, footer = modalButton("Dismiss")
          ))
        }
      } else {
        err <- p$read_error_lines()
        showModal(modalDialog(
          title = "Database Merging Failure",
          p(paste(err, collapse = "\n")), easyClose = TRUE, footer = modalButton("Dismiss")
        ))
      }
    }
  })
  
  observeEvent(input$kill_merge_btn, priority=110, {
    p <- bg_merge()
    if (!is.null(p) && p$is_alive()) {
      p$kill_tree()
      bg_merge(NULL)
      if (!is.null(merge_prog)) merge_prog$close()
      merge_prog <<- NULL
      step_start_merge(NULL)
      shinyjs::enable("merge_outputs")
      shinyjs::hide("kill_merge_btn")
      updateActionButton(session, "kill_merge_btn", label = "Cancel", icon = icon("xmark"))
      showNotification("Database consolidation cancelled by user.", type = "warning", duration = 5)
    }
  })
}

# ------------------------------------------------------------------------------
# 4. APPLICATION INITIALIZATION
# ------------------------------------------------------------------------------
shinyApp(ui, server)
