# Auto-generated from rFVS_BatchProcessor_rShiny_v2.R
# Split for package structure on 2026-07-29 11:06:54

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

  pick_directory <- function(default = ".", caption = "Select folder") {
    if (requireNamespace("rstudioapi", quietly = TRUE) && isTRUE(rstudioapi::isAvailable())) {
      dir <- tryCatch(rstudioapi::selectDirectory(path = default, caption = caption), error = function(e) NULL)
      if (!is.null(dir) && nzchar(dir)) return(dir)
    }

    if (.Platform$OS.type == "windows" && exists("choose.dir", where = asNamespace("utils"), mode = "function")) {
      dir <- tryCatch(utils::choose.dir(default = default, caption = caption), error = function(e) NA_character_)
      if (!is.na(dir) && nzchar(dir)) return(dir)
    }

    if (requireNamespace("tcltk", quietly = TRUE)) {
      dir <- tryCatch(tcltk::tk_choose.dir(default = default, caption = caption), error = function(e) NA_character_)
      if (!is.na(dir) && nzchar(dir)) return(dir)
    }

    NA_character_
  }

  pick_file <- function(default = ".", caption = "Select file") {
    if (requireNamespace("rstudioapi", quietly = TRUE) && isTRUE(rstudioapi::isAvailable())) {
      file <- tryCatch(rstudioapi::selectFile(path = default, caption = caption), error = function(e) NULL)
      if (!is.null(file) && nzchar(file)) return(file)
    }

    if (.Platform$OS.type == "windows" && exists("choose.files", where = asNamespace("utils"), mode = "function")) {
      file <- tryCatch(utils::choose.files(default = default, caption = caption, multi = FALSE), error = function(e) character(0))
      if (length(file) > 0 && !is.na(file[1]) && nzchar(file[1])) return(file[1])
    }

    if (exists("file.choose", where = asNamespace("base"), mode = "function")) {
      file <- tryCatch(base::file.choose(), error = function(e) "")
      if (nzchar(file)) return(file)
    }

    ""
  }

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
  observeEvent(input$browse_root, {
    dir <- pick_directory(default = input$root_dir, caption = "Select Root Folder Path")
    if (!is.na(dir) && nzchar(dir)) {
      updateTextInput(session, "root_dir", value = normalizePath(dir, winslash = "/", mustWork = FALSE))
    } else {
      showNotification("Folder picker is unavailable in this R session. Paste a full path manually.", type = "warning")
    }
  })
  
  observeEvent(input$browse_db, {
    file <- pick_file(default = input$root_dir, caption = "Select Master Database File")
    
    if (length(file) > 0 && !is.na(file) && nzchar(file)) {
      inputs_dir <- normalizePath(file.path(input$root_dir, "Inputs"), winslash = "/", mustWork = FALSE)
      file_dir <- normalizePath(dirname(file), winslash = "/", mustWork = FALSE)
      
      if (inputs_dir == file_dir) {
        updateTextInput(session, "master_db", value = basename(file))
      } else {
        updateTextInput(session, "master_db", value = normalizePath(file, winslash = "/", mustWork = FALSE))
      }
    } else {
      showNotification("File picker is unavailable in this R session. Paste a full file path manually.", type = "warning")
    }
  })
  
  observeEvent(input$browse_kcp, {
    default_path <- file.path(input$root_dir, "KCP_Catalog")
    dir <- pick_directory(default = default_path, caption = "Select KCP Directory")
    if (!is.na(dir) && nzchar(dir)) {
      updateTextInput(session, "kcp_dir", value = normalizePath(dir, winslash = "/", mustWork = FALSE))
    } else {
      showNotification("Folder picker is unavailable in this R session. Paste a full path manually.", type = "warning")
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

