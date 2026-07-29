# Auto-generated from rFVS_BatchProcessor_rShiny_v2.R
# Split for package structure on 2026-07-29 11:06:54

# 1. GLOBAL SETTINGS & UTILITIES
# ------------------------------------------------------------------------------
# Increase maximum upload size to 10GB for very large database/file transfers
options(shiny.maxRequestSize = 10000 * 1024^2)

# Determine root directory, usually one level up from this script's working dir
RootDir <- normalizePath(dirname(getwd()), winslash = "/", mustWork = FALSE)
# Define where simulation runs will be hosted
RunBaseDir <- file.path(RootDir, "rFVS_Runs")
# Set path for the KCP combinations manifest file
ManifestFile <- file.path(RunBaseDir, "KCP_AddFile_Manifest.csv")
# Set a visual icon/workflow diagram image name
WorkflowImageFile <- "FVS_BatchProcessing_WorkflowDiagram_v3.png"
WorkflowResourcePrefix <- "fvsbp_workflow_assets"

# Registers the workflow image directory under a stable Shiny resource prefix.
register_workflow_assets <- function() {
  workflow_img_dirs <- c(
    system.file("app/www", package = "FVSBatchProcessor"),
    file.path(getwd(), "inst", "app", "www"),
    file.path(getwd(), "app", "www"),
    file.path(getwd(), "www"),
    file.path(getwd(), "Scripts", "www"),
    file.path(RootDir, "Scripts", "www")
  )

  workflow_img_dir <- workflow_img_dirs[file.exists(file.path(workflow_img_dirs, WorkflowImageFile))][1]
  if (is.na(workflow_img_dir) || !nzchar(workflow_img_dir)) return(FALSE)

  normalized <- normalizePath(workflow_img_dir, winslash = "/", mustWork = TRUE)
  current_paths <- shiny::resourcePaths()

  if (!WorkflowResourcePrefix %in% names(current_paths)) {
    shiny::addResourcePath(WorkflowResourcePrefix, normalized)
    return(TRUE)
  }

  identical(normalizePath(current_paths[[WorkflowResourcePrefix]], winslash = "/", mustWork = FALSE), normalized)
}

register_workflow_assets()

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

