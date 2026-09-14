server <- function(input, output, session) {
  
  bg_gen <- reactiveVal(NULL) # Holds Step 1 background process handle (`callr::r_bg` object).
  bg_run <- reactiveVal(NULL) # Holds Step 2 background process handle.
  bg_merge <- reactiveVal(NULL) # Holds Step 3 background process handle.

  step_start_gen <- reactiveVal(NULL) # Stores Step 1 start time for elapsed-time formatting.
  step_start_run <- reactiveVal(NULL) # Stores Step 2 start time.
  step_start_merge <- reactiveVal(NULL) # Stores Step 3 start time.
  active_gen_total <- reactiveVal(NULL) # Snapshots the Step 1 queue size for stable progress reporting.
  active_run_total <- reactiveVal(NULL) # Snapshots the Step 2 queue size for stable progress reporting.
  active_run_is_test <- reactiveVal(FALSE) # Records whether the active Step 2 process uses the sampled queue.
  
  gen_prog <- NULL # UI progress modal instance for Step 1.
  run_prog <- NULL # UI progress modal instance for Step 2.
  merge_prog <- NULL # UI progress modal instance for Step 3.
  
  prog_file_gen <- tempfile(pattern = "gen_", fileext = ".txt") # Step 1 progress handoff file (worker -> UI poller).
  prog_file_run <- tempfile(pattern = "run_", fileext = ".txt") # Step 2 progress handoff file.
  prog_file_merge <- tempfile(pattern = "merge_", fileext = ".txt") # Step 3 progress handoff file.

  format_elapsed <- function(start_time) { # Convert a start timestamp into HH:MM:SS elapsed text.
    if (is.null(start_time) || is.na(start_time)) return("N/A") # Guard unset/invalid timestamps.
    secs <- as.numeric(difftime(Sys.time(), start_time, units = "secs")) # Compute elapsed seconds.
    secs <- max(0, round(secs)) # Clamp/round to non-negative integer seconds.
    hh <- secs %/% 3600 # Extract hour component.
    mm <- (secs %% 3600) %/% 60 # Extract minute component.
    ss <- secs %% 60 # Extract second component.
    sprintf("%02d:%02d:%02d", hh, mm, ss) # Return zero-padded HH:MM:SS string.
  }

  resolve_worker_count <- function(total_tasks, requested_cores, workload = c("heavy", "light")) { # Size worker pools from useful work rather than the configured maximum alone.
    workload <- match.arg(workload)
    total_tasks <- suppressWarnings(as.integer(total_tasks))
    requested_cores <- suppressWarnings(as.integer(requested_cores))
    if (length(total_tasks) != 1L || is.na(total_tasks) || total_tasks < 1L) return(1L)
    if (length(requested_cores) != 1L || is.na(requested_cores) || requested_cores < 1L) requested_cores <- 1L

    useful_limit <- min(total_tasks, requested_cores) # Never provision more workers than tasks or the user's selected limit.
    if (useful_limit <= 1L || identical(workload, "heavy")) return(as.integer(useful_limit)) # Expensive rFVS tasks benefit from every useful worker.

    # Keyfile writes are lightweight and can become slower when PSOCK startup and
    # network-file contention exceed the work itself. Square-root scaling keeps
    # small queues small while increasing parallelism steadily for large queues.
    scaled_limit <- max(2L, as.integer(ceiling(sqrt(total_tasks))))
    as.integer(min(useful_limit, scaled_limit))
  }

  terminate_bg_process <- function(proc) { # Best-effort terminator for callr background process trees.
    if (is.null(proc)) return(FALSE) # No process object means nothing to terminate.

    is_alive <- tryCatch(isTRUE(proc$is_alive()), error = function(e) FALSE) # Safely check liveness.
    if (!is_alive) return(FALSE) # Already stopped/invalid process.

    killed <- tryCatch({ # Prefer killing full child process tree first.
      proc$kill_tree()
      TRUE
    }, error = function(e) FALSE)

    if (!isTRUE(killed)) { # Fallback to direct process kill when tree kill fails.
      killed <- tryCatch({
        proc$kill()
        TRUE
      }, error = function(e) FALSE)
    }

    isTRUE(killed) # Return explicit logical success flag.
  }

  # --- UI BUTTON BROWSER EVENT OBSERVERS ---
  
  # Sync compute cores across tabs so changing one updates the others
  observeEvent(input$num_cores, {
    val <- input$num_cores
    if (!is.null(val) && !is.na(val)) {
      if (!identical(val, isolate(input$num_cores_rfvs))) updateNumericInput(session, "num_cores_rfvs", value = val)
      if (!identical(val, isolate(input$num_cores_merge))) updateNumericInput(session, "num_cores_merge", value = val)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$num_cores_rfvs, {
    val <- input$num_cores_rfvs
    if (!is.null(val) && !is.na(val)) {
      if (!identical(val, isolate(input$num_cores))) updateNumericInput(session, "num_cores", value = val)
      if (!identical(val, isolate(input$num_cores_merge))) updateNumericInput(session, "num_cores_merge", value = val)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$num_cores_merge, {
    val <- input$num_cores_merge
    if (!is.null(val) && !is.na(val)) {
      if (!identical(val, isolate(input$num_cores))) updateNumericInput(session, "num_cores", value = val)
      if (!identical(val, isolate(input$num_cores_rfvs))) updateNumericInput(session, "num_cores_rfvs", value = val)
    }
  }, ignoreInit = TRUE)

  # Use the user's active session wd instead of the package directory.
  dynamic_wd <- getShinyOption("FVS_USER_WD", default = getwd())

  normalize_dir_input <- function(path, fallback = dynamic_wd) { # Normalize directory-like UI input into a safe absolute-ish path.
    if (is.null(path) || length(path) == 0 || is.na(path[1])) { # Handle NULL/empty/NA inputs from widgets.
      path <- ""
    } else {
      path <- path[1] # Use first value for scalar text-input semantics.
    }
    path <- trimws(as.character(path)) # Coerce to trimmed string.
    if (!nzchar(path)) path <- fallback # Replace blank input with runtime fallback directory.
    normalizePath(path, winslash = "/", mustWork = FALSE) # Normalize separators without requiring physical existence.
  }

  observeEvent(input$browse_db, { # Open native file picker for Database selection.
    req(input$browse_db > 0) # Ensure click event fired.
    working_dir <- normalize_dir_input(dynamic_wd) # Resolve the directory from which the application was launched.
    default_db_dir <- find_input_dir(working_dir) # Prefer an existing Input/Inputs-like project folder.
    if (!nzchar(default_db_dir)) default_db_dir <- working_dir # Fall back safely when the project has no input folder.
    selected_file <- get_native_file(default_path = default_db_dir, caption_text = "Select Master Database File") # Show platform file chooser.
    if (!is.null(selected_file)) { # Update only when user confirms a file.
      updateTextInput(session, "master_db", value = normalizePath(selected_file, winslash = "/", mustWork = FALSE)) # Persist normalized file path into UI.
    }
  }, ignoreInit = TRUE) # Do not run until user explicitly clicks.

  observeEvent(input$browse_root, { # Open native folder picker for Root directory selection.
    req(input$browse_root > 0) # Ensure click event fired.
    working_dir <- normalize_dir_input(dynamic_wd) # Resolve the directory from which the application was launched.
    default_root <- normalizePath(dirname(working_dir), winslash = "/", mustWork = FALSE) # Start one level up so the project folder is visible.
    selected_dir <- get_native_folder(default_path = default_root, caption_text = "Select Root Folder Path") # Show platform folder chooser.
    if (!is.null(selected_dir)) { # Update only when user confirms a folder.
      updateTextInput(session, "root_dir", value = normalizePath(selected_dir, winslash = "/", mustWork = FALSE)) # Persist normalized folder into UI.
    }
  }, ignoreInit = TRUE) # Do not run until user explicitly clicks.

  observeEvent(input$browse_kcp, { # Open native folder picker for KCP directory selection.
    req(input$browse_kcp > 0) # Ensure click event fired.
    default_path <- file.path(normalize_dir_input(input$root_dir, fallback = dynamic_wd), "KCP_Catalog") # Seed picker near expected KCP folder.
    selected_dir <- get_native_folder(default_path = default_path, caption_text = "Select KCP Directory") # Show platform folder chooser.
    if (!is.null(selected_dir)) { # Update only when user confirms a folder.
      updateTextInput(session, "kcp_dir", value = normalizePath(selected_dir, winslash = "/", mustWork = FALSE)) # Persist normalized KCP directory into UI.
    }
  }, ignoreInit = TRUE) # Do not run until user explicitly clicks.
  
  find_input_dir <- function(root_dir) { # Discover best matching top-level input directory under project root.
    top_dirs <- tryCatch(list.dirs(root_dir, full.names = TRUE, recursive = FALSE), error = function(e) character(0)) # List first-level directories safely.
    if (length(top_dirs) == 0) return("") # No candidates available.
    top_dirs <- unique(normalizePath(top_dirs, winslash = "/", mustWork = FALSE)) # Normalize/deduplicate candidate directories.

    candidate_input_dirs <- top_dirs[grepl("input", basename(top_dirs), ignore.case = TRUE)] # Find fuzzy matches containing "input".
    exact_input <- candidate_input_dirs[tolower(basename(candidate_input_dirs)) %in% c("input", "inputs")] # Prefer exact canonical names.
    if (length(exact_input) > 0) candidate_input_dirs <- exact_input # Narrow to exact matches when present.

    if (length(candidate_input_dirs) == 0) return("") # Return empty when no suitable directory exists.
    normalizePath(candidate_input_dirs[1], winslash = "/", mustWork = FALSE) # Return first preferred candidate path.
  }

  find_kcp_dir <- function(root_dir) { # Discover best matching top-level KCP catalog directory under project root.
    top_dirs <- tryCatch(list.dirs(root_dir, full.names = TRUE, recursive = FALSE), error = function(e) character(0)) # List first-level directories safely.
    if (length(top_dirs) == 0) return("") # No candidates available.
    top_dirs <- unique(normalizePath(top_dirs, winslash = "/", mustWork = FALSE)) # Normalize/deduplicate candidate directories.

    kcp_matches <- top_dirs[grepl("kcp", basename(top_dirs), ignore.case = TRUE)] # Find fuzzy matches containing "kcp".
    exact_kcp <- kcp_matches[tolower(basename(kcp_matches)) %in% c("kcp", "kcp_catalog")] # Prefer canonical KCP folder names.
    if (length(exact_kcp) > 0) kcp_matches <- exact_kcp # Narrow to exact matches when present.

    if (length(kcp_matches) == 0) return("") # Return empty when no suitable directory exists.
    normalizePath(kcp_matches[1], winslash = "/", mustWork = FALSE) # Return first preferred candidate path.
  }

  detect_single_db_name <- function(root_dir) { # Auto-detect DB filename when exactly one DB-like file exists.
    input_dir <- find_input_dir(root_dir) # Resolve candidate Inputs directory.
    if (!nzchar(input_dir)) return("<FVS_Input.db>") # Fall back to placeholder when Inputs cannot be found.

    available_dbs <- tryCatch( # Enumerate DB/SQLite files in Inputs directory.
      list.files(input_dir, pattern = "\\.(db|sqlite)$", ignore.case = TRUE, full.names = FALSE),
      error = function(e) character(0)
    )
    if (length(available_dbs) == 1) available_dbs[1] else "<FVS_Input.db>" # Return single match; otherwise keep placeholder.
  }

  resolve_db_path <- function(root_dir, db_name, slash = "/") { # Resolve effective DB path from root + UI DB value.
    db_name <- trimws(db_name) # Normalize user-supplied DB text.

    if (!nzchar(db_name) || grepl("^<.*>$", db_name)) { # Replace blank/placeholder labels with auto-detected DB name.
      db_name <- detect_single_db_name(root_dir)
    }

    if (grepl("^([A-Za-z]:|\\\\|/)", db_name)) { # Treat absolute paths as already rooted.
      normalizePath(db_name, winslash = slash, mustWork = FALSE)
    } else {
      input_dir <- find_input_dir(root_dir) # Resolve best Inputs directory for relative names.
      if (!nzchar(input_dir)) {
        input_dir <- normalizePath(file.path(root_dir, "Inputs"), winslash = "/", mustWork = FALSE) # Fall back to conventional `Inputs` path.
      }
      normalizePath(file.path(input_dir, db_name), winslash = slash, mustWork = FALSE) # Return normalized full path to selected DB.
    }
  }
  
  resolve_kcp_path <- function(root_dir, dir_name, slash = "/") { # Resolve effective KCP directory path from root + UI value.
    dir_name <- trimws(dir_name) # Normalize user-supplied KCP text.

    if (!nzchar(dir_name) || identical(dir_name, "KCP_Catalog")) { # Replace blank/default values with discovered KCP path when available.
      detected_kcp <- find_kcp_dir(root_dir)
      if (nzchar(detected_kcp)) dir_name <- detected_kcp
    }

    if (grepl("^([A-Za-z]:|\\\\|/)", dir_name)) { # Absolute path branch.
      normalizePath(dir_name, winslash = slash, mustWork = FALSE)
    } else {
      normalizePath(file.path(root_dir, dir_name), winslash = slash, mustWork = FALSE) # Relative path branch rooted at project root.
    }
  }

  # Runtime defaults must be derived at launch (not package install time).
  derive_runtime_defaults <- function(project_root) { # Build startup defaults for root/db/kcp from launch directory.
    root <- normalizePath(project_root, winslash = "/", mustWork = FALSE) # Normalize launch root.

    top_dirs <- tryCatch(list.dirs(root, full.names = TRUE, recursive = FALSE), error = function(e) character(0)) # Probe top-level directories (best-effort).
    top_dirs <- unique(normalizePath(top_dirs, winslash = "/", mustWork = FALSE)) # Normalize/deduplicate for consistent downstream matching.

    db_default <- detect_single_db_name(root) # Auto-select DB when possible.
    kcp_detected <- find_kcp_dir(root) # Auto-detect KCP directory when present.
    kcp_default <- if (nzchar(kcp_detected)) kcp_detected else "KCP_Catalog" # Fallback to canonical relative folder name.

    list(root = root, db = db_default, kcp = kcp_default) # Return startup defaults payload.
  }

  defaults_initialized <- reactiveVal(FALSE) # One-time gate to avoid repeatedly applying startup defaults.

  observe({ # Initialize root/db/kcp inputs once at app startup.
    if (isTRUE(defaults_initialized())) return() # Exit after first successful initialization.
    
    # Use FVS_USER_WD instead of getwd() to track where the script was launched from vs the package root
    caller_wd <- getShinyOption("FVS_USER_WD", default = getwd()) # Recover launch working directory preference.
    
    d <- derive_runtime_defaults(caller_wd) # Compute startup defaults from launch directory.
    if (!nzchar(trimws(as.character(d$db)))) d$db <- "<FVS_Input.db>" # Guarantee non-empty DB placeholder text.
    updateTextInput(session, "root_dir", value = d$root) # Seed root input.
    updateTextInput(session, "master_db", value = d$db) # Seed DB input.
    updateTextInput(session, "kcp_dir", value = d$kcp) # Seed KCP input.

    defaults_initialized(TRUE) # Mark initialization complete.
  })

  observeEvent(input$root_dir, { # Re-evaluate likely DB file whenever user changes root directory.
    runtime_root <- normalize_dir_input(input$root_dir, fallback = getShinyOption("FVS_USER_WD", default = getwd())) # Normalize updated root.
    detected_db <- detect_single_db_name(runtime_root) # Try automatic DB file detection under new root.
    if (!nzchar(trimws(as.character(detected_db)))) detected_db <- "<FVS_Input.db>" # Fall back to placeholder when detection fails.

    updateTextInput(session, "master_db", value = detected_db) # Push detected/placeholder DB value to UI.
  }, ignoreInit = TRUE) # Ignore initial load because startup observer handles defaulting.
  
  meta <- reactiveValues(groups = NULL, catalog = NULL, types = NULL) # Shared metadata cache for groups/catalog/type definitions.
  grid_data <- reactiveVal(data.frame()) # Central editable matrix backing the Handsontable UI.
  suppress_next_scenario_auto_recalc <- reactiveVal(FALSE) # One-shot guard against programmatic scenario-selector recalc.
  
  observeEvent(list(input$master_db, input$root_dir), { # Populate and auto-pick a preferred StandInit table when root/db changes or startup defaults arrive.
    req(input$master_db, input$root_dir) # Require both root and DB inputs before querying.
    full_db_path <- resolve_db_path(input$root_dir, input$master_db) # Resolve target DB path.
    if (!file.exists(full_db_path)) { # Do not retain a misleading placeholder when the current DB cannot be opened.
      updateSelectInput(session, "stand_tbl", choices = character(0), selected = character(0))
      return()
    }

    tryCatch({ # Keep discovery failures from terminating the session while making them visible for diagnosis.
        local({ # Scope connection lifecycle locally.
          con <- dbConnect(SQLite(), full_db_path) # Open SQLite connection.
          on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE) # Always close connection on exit.
          all_tables <- dbListTables(con) # Read physical table names exactly as stored in the selected database.
          tables <- all_tables[grepl("standinit", tolower(all_tables), fixed = TRUE)] # Include every table containing StandInit, regardless of capitalization or position.
          tables <- sort(unique(tables)) # Present stable, deduplicated dropdown choices.
          if (length(tables) > 0) { # Choose the preferred table while retaining all matches as user-selectable options.
            tables_lower <- tolower(tables) # Case-insensitive comparison vector used only for priority matching.
            if ("fvs_standinit_cond" %in% tables_lower) {
              target_tbl <- tables[tables_lower == "fvs_standinit_cond"][1] # Highest preference: conditional stand table.
            } else if ("fvs_standinit" %in% tables_lower) {
              target_tbl <- tables[tables_lower == "fvs_standinit"][1] # Second preference: base stand table.
            } else if ("fvs_standinit_plot" %in% tables_lower) {
              target_tbl <- tables[tables_lower == "fvs_standinit_plot"][1] # Third preference: plot stand table.
            } else {
              target_tbl <- tables[1] # Fallback: first matching stand table.
            }
            updateSelectInput(session, "stand_tbl", choices = tables, selected = target_tbl) # Populate all matches and auto-select the existing priority winner.
          } else {
            updateSelectInput(session, "stand_tbl", choices = character(0), selected = character(0)) # Clear stale choices when the database has no StandInit table.
          }
        })
      }, error = function(e) { # Clear stale values and report why discovery failed.
        updateSelectInput(session, "stand_tbl", choices = character(0), selected = character(0))
        showNotification(paste("Could not load StandInit table choices:", e$message), type = "error", duration = 8)
      })
  }, ignoreNULL = FALSE, ignoreInit = FALSE) # Run during initial binding and again when programmatic defaults or user inputs change.
  
  refresh_grouping_controls <- function() { # Rebuild group-column and excluded-group selectors from current DB context.
    req(input$master_db, input$root_dir, input$stand_tbl) # Require core DB/table inputs.
    full_db_path <- resolve_db_path(input$root_dir, input$master_db) # Resolve active DB path.
    if (!file.exists(full_db_path)) return() # Skip when DB is unavailable.
    
    use_grp <- isTRUE(isolate(input$use_groups_col)) # Determine whether GROUPS parsing mode is enabled.
    
    if (use_grp) { # Parse-mode: list valid keys from GROUPS column structure.
      cols <- tryCatch(
        parse_groups_column_keys(full_db_path, input$stand_tbl),
        error = function(e) character(0)
      )
    } else {
      cols <- tryCatch( # Direct-column mode: list usable grouping columns from stand table.
        get_group_column_choices(full_db_path, input$stand_tbl),
        error = function(e) character(0)
      )
      cols <- cols[!toupper(cols) %in% "GROUPS"] # Exclude raw GROUPS column in direct-column mode.
    }
    
    current_group_col <- isolate(input$group_col) # Snapshot current selected group column.
    current_group_match <- if (length(current_group_col) == 1 && nzchar(current_group_col)) { # Resolve an existing selection without requiring identical database casing.
      cols[toupper(cols) == toupper(current_group_col)][1]
    } else {
      NA_character_
    }
    variant_col <- if (!use_grp) cols[toupper(cols) == "VARIANT"][1] else NA_character_ # Preserve the database's actual spelling while locating VARIANT case-insensitively.
    selected_group_col <- if (length(cols) == 0) {
      character(0) # No available choices.
    } else if (!is.na(current_group_match)) {
      current_group_match # Retain the matching physical column name even when legacy casing differs.
    } else if (!is.na(variant_col)) {
      variant_col # Prefer the actual VARIANT column by default in direct-column mode.
    } else {
      cols[1] # Fallback to first available column.
    }
    
    updateSelectInput(session, "group_col", choices = cols, selected = selected_group_col) # Refresh group-column selector with preserved/default selection.
    
    group_values <- if (length(selected_group_col) == 1 && nzchar(selected_group_col)) { # Populate available group values for exclusion selector.
      tryCatch({
        if (use_grp) {
          get_unique_group_values_parsed(full_db_path, input$stand_tbl, selected_group_col) # Extract parsed GROUPS-key values.
        } else {
          get_unique_group_values(full_db_path, input$stand_tbl, selected_group_col) # Extract direct column distinct values.
        }
      }, error = function(e) character(0))
    } else {
      character(0) # No active group column, so no choices.
    }
    
    current_excluded <- normalize_excluded_groups(isolate(input$exclude_grps)) # Normalize existing excluded selections.
    retained_excluded <- current_excluded[current_excluded %in% group_values] # Keep only exclusions still valid under new choices.
    updateSelectizeInput(session, "exclude_grps", choices = group_values, selected = retained_excluded, server = TRUE) # Refresh exclusion selector with retained valid state.
  }
  
  observeEvent(list(input$master_db, input$root_dir, input$stand_tbl), { # On database-context or stand-table change, infer defaults and warn on multi-variant tables.
    req(input$master_db, input$root_dir, input$stand_tbl) # Require DB/table context before querying.
    full_db_path <- resolve_db_path(input$root_dir, input$master_db) # Resolve active DB path.
    if (file.exists(full_db_path)) { # Proceed only when DB file exists.
      tryCatch({ # Swallow query failures to keep UI responsive.
        local({ # Scope connection lifecycle.
          con <- dbConnect(SQLite(), full_db_path) # Open SQLite connection.
          on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE) # Ensure connection close.
          if (dbExistsTable(con, input$stand_tbl)) { # Continue only when selected stand table exists.
            cols <- dbListFields(con, input$stand_tbl) # Read available column names.
            
            if ("INV_YEAR" %in% toupper(cols)) { # Auto-fill the common start year from the latest non-empty database inventory year.
              inv_year_col <- cols[toupper(cols) == "INV_YEAR"][1] # Preserve the physical column spelling used by legacy databases.
              inv_query <- sprintf(
                "SELECT MAX(CAST(%s AS INTEGER)) AS INV_YEAR FROM %s WHERE %s IS NOT NULL AND TRIM(CAST(%s AS TEXT)) != ''",
                quote_sql_identifier(inv_year_col),
                quote_sql_identifier(input$stand_tbl),
                quote_sql_identifier(inv_year_col),
                quote_sql_identifier(inv_year_col)
              ) # Use the latest stand inventory year as the safe common-year default.
              inv_df <- dbGetQuery(con, inv_query) # Execute INV_YEAR lookup query.
              if (nrow(inv_df) > 0) { # Update only when a candidate value was returned.
                inv_val <- suppressWarnings(as.numeric(inv_df$INV_YEAR[1])) # Safely coerce to numeric.
                if (!is.na(inv_val)) {
                  updateNumericInput(session, "inv_year", value = inv_val) # Push detected inventory year into UI.
                }
              }
            }
            
            variant_col <- cols[toupper(cols) == "VARIANT"][1] # Resolve legacy Variant/variant spellings to the physical database column.
            if (!is.na(variant_col)) { # Notify user when multiple variants exist in selected stand table.
              var_query <- sprintf(
                "SELECT DISTINCT %s AS VARIANT FROM %s WHERE %s IS NOT NULL AND TRIM(CAST(%s AS TEXT)) != ''",
                quote_sql_identifier(variant_col),
                quote_sql_identifier(input$stand_tbl),
                quote_sql_identifier(variant_col),
                quote_sql_identifier(variant_col)
              ) # Alias the query result so downstream R code can always use the canonical VARIANT name.
              var_df <- dbGetQuery(con, var_query) # Execute variant inventory query.
              if (nrow(var_df) > 0) {
                unique_vars <- unique(tolower(trimws(var_df$VARIANT))) # Normalize variant labels for consistent counting.
                if (length(unique_vars) > 1) {
                  showNotification(sprintf("Multiple FVS Variants detected (%s).", paste(toupper(unique_vars), collapse = ", ")), type = "message") # Inform user of mixed-variant dataset.
                }
              }
            }
          }
        })
      }, error = function(e) {}) # Silent error handling to avoid noisy transient messages.
    }
  }, ignoreInit = FALSE) # Run at startup and whenever the active database context or stand table changes.
  
  refresh_merge_controls <- function() {
    req(input$master_db, input$root_dir, input$stand_tbl)
    full_db_path <- resolve_db_path(input$root_dir, input$master_db)
    if (!file.exists(full_db_path)) return()
    
    con_m <- tryCatch(dbConnect(SQLite(), full_db_path), error = function(e) NULL)
    if (is.null(con_m)) return()
    on.exit(try(dbDisconnect(con_m), silent = TRUE), add = TRUE)
    
    table_cols <- tryCatch(dbListFields(con_m, input$stand_tbl), error = function(e) character(0))
    if (length(table_cols) == 0) return()
    
    updateSelectizeInput(session, "merge_cols_select", choices = sort(table_cols))
    
    if ("GROUPS" %in% toupper(table_cols)) {
      grp_col_name <- table_cols[toupper(table_cols) == "GROUPS"][1]
      sql_grp <- sprintf("SELECT %s FROM %s WHERE %s IS NOT NULL AND TRIM(CAST(%s AS TEXT)) != ''",
                         quote_sql_identifier(grp_col_name), quote_sql_identifier(input$stand_tbl),
                         quote_sql_identifier(grp_col_name), quote_sql_identifier(grp_col_name))
      raw_groups <- tryCatch(dbGetQuery(con_m, sql_grp)[[1]], error = function(e) character(0))
      if (length(raw_groups) > 0) {
        words <- unlist(strsplit(raw_groups, "\\s+"))
        words <- words[nzchar(words)]
        keys <- sapply(strsplit(words, "="), `[`, 1)
        keys <- keys[!toupper(trimws(keys)) %in% c("NA", "<NA>", "NULL", "NONE")]
        updateSelectizeInput(session, "merge_groups_select", choices = sort(unique(keys)))
      } else {
        updateSelectizeInput(session, "merge_groups_select", choices = character(0))
      }
    } else {
      updateSelectizeInput(session, "merge_groups_select", choices = character(0))
    }
  }

  observeEvent(list(input$use_groups_col, input$master_db, input$root_dir, input$stand_tbl), { # Rebuild grouping widgets whenever grouping mode or DB context changes.
    refresh_grouping_controls() # Delegate full selector refresh logic.
    refresh_merge_controls() # Delegate merge selector refresh logic.
  }, ignoreInit = FALSE) # Execute on startup and subsequent input changes.
  
  observeEvent(input$group_col, { # Recompute exclude-group choices when selected group column changes.
    req(input$master_db, input$root_dir, input$stand_tbl) # Require DB/table context.
    full_db_path <- resolve_db_path(input$root_dir, input$master_db) # Resolve active DB path.
    if (!file.exists(full_db_path)) return() # Abort when DB path is unavailable.
    
    use_grp <- isTRUE(isolate(input$use_groups_col)) # Determine grouping mode.
    group_values <- tryCatch({ # Fetch candidate group values for selected column.
      if (use_grp) {
        get_unique_group_values_parsed(full_db_path, input$stand_tbl, input$group_col) # Parse values from GROUPS key.
      } else {
        get_unique_group_values(full_db_path, input$stand_tbl, input$group_col) # Pull direct distinct values.
      }
    }, error = function(e) character(0))
    
    current_excluded <- normalize_excluded_groups(isolate(input$exclude_grps)) # Normalize current excluded selections.
    retained_excluded <- current_excluded[current_excluded %in% group_values] # Keep only exclusions still represented.
    updateSelectizeInput(session, "exclude_grps", choices = group_values, selected = retained_excluded, server = TRUE) # Refresh exclusion list and retained selections.
  }, ignoreInit = FALSE) # Execute at startup and on column changes.
  
  manifest_path <- reactive({ # Compute canonical manifest CSV path from current root input.
    req(input$root_dir) # Require root directory input before path build.
    normalizePath(file.path(input$root_dir, "rFVS_Runs", "KCP_AddFile_Manifest.csv"), winslash = "/", mustWork = FALSE) # Return normalized manifest path.
  })
  
  # Reactive value to explicitly trigger job queue recalculation when the manifest is successfully written to disk
  manifest_trigger <- reactiveVal(0) # Post-write increment used to force safe queue recomputation.
  manifest_ready <- reactiveVal(FALSE) # Require an explicit successful manifest save for the current configuration and session.

  observeEvent(
    list(grid_data(), input$root_dir, input$master_db, input$stand_tbl, input$group_col, input$use_groups_col),
    manifest_ready(FALSE), # Any lookup or database-context change makes the previously saved manifest stale.
    ignoreInit = TRUE
  )
  
  observe({ # Keep overwrite-scenarios selector synchronized with currently available queue scenarios.
    jq <- get_job_queue() # Resolve active job queue snapshot.
    if (!is.null(jq) && nrow(jq) > 0) { # Update only when queue has runnable rows.
      valid_choices <- c("ALL", unique(jq$Scenario)) # Build valid overwrite choices with global shortcut.
      current_sel <- isolate(input$overwrite_scens) # Snapshot current overwrite selection.
      retained_sel <- current_sel[current_sel %in% valid_choices] # Drop selections no longer represented in queue.
      updateSelectInput(session, "overwrite_scens", choices = valid_choices, selected = retained_sel) # Refresh choices while preserving valid existing selections.
    }
  })
  
  # Retrieves job queue payload connecting active inputs, user manifests and physical directories
  get_job_queue <- reactive({ # Build the runnable stand-level queue from DB + manifest inputs.
    # To prevent reactive race conditions between file writing and UI updates
    # we explicitly wait for this trigger (fired only AFTER the file finishes saving)
    manifest_trigger() # Depend on explicit post-write trigger to avoid reading partial files.
    if (!isTRUE(manifest_ready())) return(NULL) # Never run from a pre-existing or stale manifest that was not saved for the current UI state.
    
    req(input$root_dir, input$master_db, input$group_col, input$stand_tbl) # Require key UI inputs before proceeding.
    
    full_db_path <- resolve_db_path(input$root_dir, input$master_db) # Resolve runtime path to selected master SQLite DB.
    m_file <- manifest_path() # Resolve runtime path to manifest CSV.
    
    if (!file.exists(full_db_path) || !file.exists(m_file)) return(NULL) # Abort until both DB and manifest exist.
    
    tryCatch({ # Catch DB/read/merge failures and report as a user notification.
      # Scan target database tables inside standard FVS SQLite structures
      stInitDF <- local({ # Isolate DB connection scope and return stand-init data frame.
        con <- dbConnect(SQLite(), full_db_path) # Open connection to master SQLite database.
        on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE) # Ensure DB connection closes even on error.
        if (!dbExistsTable(con, input$stand_tbl)) { # Validate selected stand table exists.
          return(NULL) # Stop queue build if stand table is missing.
        }
        
        cols <- dbListFields(con, input$stand_tbl) # Retrieve available columns from stand table.

        # Older FVS-ready databases use mixed-case names (for example Stand_ID, Stand_CN, and Variant).
        # Resolve physical names case-insensitively, then alias query results to the canonical names expected by R.
        stand_id_col <- cols[toupper(cols) == "STAND_ID"][1]
        stand_cn_col <- cols[toupper(cols) == "STAND_CN"][1]
        variant_col <- cols[toupper(cols) == "VARIANT"][1]
        inv_year_col <- cols[toupper(cols) == "INV_YEAR"][1]

        if (is.na(stand_id_col) || is.na(stand_cn_col)) { # Fail with a clear schema error only when the required fields are genuinely absent.
          missing_cols <- c("STAND_ID", "STAND_CN")[c(is.na(stand_id_col), is.na(stand_cn_col))]
          stop(sprintf("Required column(s) missing from table '%s': %s", input$stand_tbl, paste(missing_cols, collapse = ", ")))
        }

        stand_select <- sprintf(
          "%s AS STAND_ID, %s AS STAND_CN",
          quote_sql_identifier(stand_id_col),
          quote_sql_identifier(stand_cn_col)
        ) # Canonical aliases isolate all downstream R code from database column-name casing.
        has_var <- !is.na(variant_col) # Detect VARIANT using the case-insensitive physical-name lookup.
        var_str <- if (has_var) sprintf(", %s AS VARIANT", quote_sql_identifier(variant_col)) else "" # Return a canonical VARIANT field when available.
        inv_year_str <- if (!is.na(inv_year_col)) sprintf(", %s AS INV_YEAR", quote_sql_identifier(inv_year_col)) else "" # Carry each stand's database inventory year for optional pre-growth timing.
        
        if (isTRUE(isolate(input$use_groups_col))) { # Branch: derive groups by parsing GROUPS column.
          grp_col <- cols[toupper(cols) == "GROUPS"] # Locate the actual GROUPS column name (case-insensitive).
          if (length(grp_col) == 0) return(NULL) # Cannot parse groups if GROUPS column is absent.
          query <- sprintf("SELECT %s, %s AS GROUPS_RAW %s %s FROM %s", # Build SQL with canonical stand/variant/inventory-year aliases plus raw GROUPS.
                           stand_select, quote_sql_identifier(grp_col[1]), var_str, inv_year_str, quote_sql_identifier(input$stand_tbl)) # Insert resolved and escaped identifiers safely.
          res <- dbGetQuery(con, query) # Execute stand query.
          target_key <- isolate(input$group_col) # Selected GROUPS key to extract (for example, RX).
          
          res$GROUP_CODE <- extract_group_values_vectorized(res$GROUPS_RAW, target_key) # Parse GROUPS text into normalized GROUP_CODE values.
          res$GROUPS_RAW <- NULL # Drop temporary raw GROUPS text column after parsing.
          attr(res, "has_var") <- has_var # Preserve variant-presence metadata for downstream normalization.
          res # Return parsed stand-level result set.
        } else { # Branch: use selected column directly as GROUP_CODE.
          query <- sprintf("SELECT %s, %s AS GROUP_CODE %s %s FROM %s", # Build SQL with canonical stand/variant/inventory-year aliases and selected GROUP_CODE.
                           stand_select, quote_sql_identifier(input$group_col), var_str, inv_year_str, quote_sql_identifier(input$stand_tbl)) # Insert resolved and escaped identifiers safely.
          res <- dbGetQuery(con, query) # Execute stand query.
          attr(res, "has_var") <- has_var # Preserve variant-presence metadata for downstream normalization.
          res # Return direct-column stand-level result set.
        }
      })
      
      if (is.null(stInitDF)) return(NULL) # Exit if stand data could not be produced.
      has_var <- attr(stInitDF, "has_var") # Recover whether VARIANT existed in source data.
      
      stInitDF$GROUP_CODE <- trimws(as.character(stInitDF$GROUP_CODE)) # Normalize GROUP_CODE for reliable joins and filters.
      if (has_var) { # Only normalize VARIANT when column exists.
        stInitDF$VARIANT <- paste0("FVS", tolower(trimws(as.character(stInitDF$VARIANT)))) # Convert to expected rFVS program token (e.g., FVSbm).
      }
      
      
      ex_groups <- normalize_excluded_groups(input$exclude_grps) # Normalize user exclusions from selectize/text input.
      stInitDF <- subset(stInitDF, !is.na(GROUP_CODE) & nzchar(GROUP_CODE) & !(tolower(GROUP_CODE) %in% tolower(ex_groups))) # Remove NA/blank/excluded groups.
      
      manifest_df <- read.csv(m_file, stringsAsFactors = FALSE, colClasses = "character") # Load scenario/KCP manifest as character data.
      if (is.null(manifest_df) || nrow(manifest_df) == 0) return(NULL) # Exit if manifest has no rows.
      
      manifest_df$GROUP_CODE <- trimws(as.character(manifest_df$GROUP_CODE)) # Normalize manifest grouping values before join.
      manifest_df <- unique(manifest_df) # Drop duplicate manifest rows.
      
      job_queue <- merge(stInitDF, manifest_df, by = "GROUP_CODE", all = FALSE) # Inner-join stands to scenarios by GROUP_CODE.
      
      job_queue <- job_queue[!duplicated(job_queue[, c("STAND_ID", "GROUP_CODE", "Scenario")]), ] # Remove duplicate stand/group/scenario combos.
      
      run_base_dir <- normalizePath(file.path(input$root_dir, "rFVS_Runs"), winslash = "/", mustWork = FALSE) # Resolve base run output directory.
      job_queue$stand_dir <- file.path(run_base_dir, job_queue$GROUP_CODE, job_queue$Scenario, paste0("fvs_", job_queue$STAND_ID)) # Compute per-stand working directory path.
      
      return(job_queue) # Return final runnable queue for keyfile generation and rFVS dispatch.
    }, error = function(e) { # Handle failures from SQL, parsing, file IO, or joins.
      showNotification(paste("Database Query Error:", e$message), type = "error", duration = 8) # Surface error details to the UI.
      return(NULL) # Fail gracefully so downstream reactives can block cleanly.
    })
  })

  get_execution_queue <- reactive({ # Derive the Step 2 workload without changing the full project queue.
    job_queue <- get_job_queue()
    if (is.null(job_queue) || nrow(job_queue) == 0 || !isTRUE(input$test_run_mode)) {
      return(job_queue)
    }

    # Select the numerically lowest STAND_ID for every GROUP_CODE/Scenario pair
    # so all configured KCP scenarios are represented in the test workload.
    stand_id_text <- trimws(as.character(job_queue$STAND_ID))
    stand_id_numeric <- suppressWarnings(as.numeric(stand_id_text))
    stand_cn_sort <- if ("STAND_CN" %in% names(job_queue)) job_queue$STAND_CN else seq_len(nrow(job_queue))
    queue_order <- order(
      job_queue$GROUP_CODE,
      job_queue$Scenario,
      is.na(stand_id_numeric),
      stand_id_numeric,
      stand_id_text,
      stand_cn_sort,
      na.last = TRUE
    )
    ordered_queue <- job_queue[queue_order, , drop = FALSE]
    sample_columns <- ordered_queue[, c("GROUP_CODE", "Scenario"), drop = FALSE]
    ordered_queue[!duplicated(sample_columns), , drop = FALSE]
  })

  # --- SYSTEM SCANNING OBSERVER ---
  # Loads the available database context and dynamic KCP lookup combinations into a central reactive UI table
  observeEvent(input$load_metadata, { # Re-scan DB and KCP sources to rebuild UI metadata and defaults.
    req(input$root_dir) # Require a root directory before scanning.

    runtime_root <- normalizePath(trimws(input$root_dir), winslash = "/", mustWork = FALSE) # Normalize user-supplied root path.
    if (!nzchar(runtime_root)) runtime_root <- normalizePath(getShinyOption("FVS_USER_WD", default = getwd()), winslash = "/", mustWork = FALSE) # Fall back to launch working directory when root is blank.

    runtime_db <- trimws(as.character(input$master_db)) # Read current DB input as a trimmed string.
    if (!nzchar(runtime_db) || grepl("^<.*>$", runtime_db)) { # Detect blank or placeholder DB values.
      runtime_db <- detect_single_db_name(runtime_root) # Auto-detect a likely DB file name from Inputs.
      updateTextInput(session, "master_db", value = runtime_db) # Reflect detected DB back into the UI.
    }

    runtime_kcp <- trimws(as.character(input$kcp_dir)) # Read current KCP directory input as a trimmed string.
    if (!nzchar(runtime_kcp) || identical(runtime_kcp, "KCP_Catalog")) { # Detect blank/default KCP directory setting.
      detected_kcp <- find_kcp_dir(runtime_root) # Attempt to discover KCP folder under root.
      if (nzchar(detected_kcp)) { # Update only when detection succeeds.
        runtime_kcp <- detected_kcp # Use discovered KCP path for this scan.
        updateTextInput(session, "kcp_dir", value = runtime_kcp) # Reflect detected KCP path back into the UI.
      }
    }

    full_db_path <- resolve_db_path(runtime_root, runtime_db) # Resolve full absolute path to DB target.
    full_kcp_dir <- resolve_kcp_path(runtime_root, runtime_kcp) # Resolve full absolute path to KCP directory.
    
    if (!file.exists(full_db_path)) { # Guard: DB path must exist.
      showNotification("Database target not found at specified Root directory path.", type = "error") # Inform user about missing DB.
      return() # Stop scanning when DB is unavailable.
    }
    if (!dir.exists(full_kcp_dir)) { # Guard: KCP directory path must exist.
      showNotification("KCP directory mapping path could not be located.", type = "warning") # Inform user about missing KCP folder.
      return() # Stop scanning when KCP directory is unavailable.
    }
    
    err_msg <- NULL # Capture downstream errors without crashing observer.
    withProgress(message = "Extracting file indexing mappings & building DB indexes...", value = 0.5, { # Show progress during metadata refresh.
      ex_groups <- normalize_excluded_groups(input$exclude_grps) # Normalize user exclusions for group retrieval.
      tryCatch({ # Catch DB/index/catalog errors and surface a friendly message.
        local({ # Keep DB connection scoped to this block.
          con_m <- dbConnect(SQLite(), full_db_path) # Open metadata DB connection.
          on.exit(try(dbDisconnect(con_m), silent = TRUE), add = TRUE) # Ensure DB connection closes on exit.

          create_fvs_lookup_indexes <- function(con, table_name) { # Validate an FVS input table and create indexes using its physical column names.
            if (!dbExistsTable(con, table_name)) { # Fail clearly if a discovered or selected table disappeared before indexing.
              stop(sprintf("Cannot create indexes: table '%s' does not exist.", table_name))
            }

            physical_columns <- dbListFields(con, table_name) # Read exact database spelling, including legacy mixed-case names.
            required_columns <- c("STAND_CN", "STAND_ID") # Both lookup columns are required for indexed FVS stand/tree access.
            physical_matches <- vapply(required_columns, function(required_name) {
              matches <- physical_columns[toupper(physical_columns) == required_name] # Match canonical names without assuming physical casing.
              if (length(matches) == 0) NA_character_ else matches[1]
            }, character(1))

            missing_columns <- required_columns[is.na(physical_matches)] # Validate before issuing any CREATE INDEX statements.
            if (length(missing_columns) > 0) {
              stop(sprintf(
                "Cannot index table '%s': required column(s) missing: %s.",
                table_name,
                paste(missing_columns, collapse = ", ")
              ))
            }

            for (column_name in required_columns) { # Create one safely quoted index for each validated lookup column.
              physical_column <- unname(physical_matches[[column_name]])
              index_name <- sprintf("idx_%s_%s", table_name, tolower(column_name))
              dbExecute(
                con,
                sprintf(
                  "CREATE INDEX IF NOT EXISTS %s ON %s (%s)",
                  quote_sql_identifier(index_name),
                  quote_sql_identifier(table_name),
                  quote_sql_identifier(physical_column)
                )
              )
            }
          }
          
          # Build validated indexes on the selected Stand Init table.
          create_fvs_lookup_indexes(con_m, input$stand_tbl)
          
          # Index only the conventionally paired TreeInit table. Unrelated
          # TreeInit tables must not block metadata loading for this selection.
          expected_tree_tbl <- gsub("StandInit", "TreeInit", input$stand_tbl, ignore.case = TRUE)
          physical_tables <- dbListTables(con_m)
          tree_tbl_match <- physical_tables[toupper(physical_tables) == toupper(expected_tree_tbl)]
          if (length(tree_tbl_match) == 0L) {
            stop(sprintf("Expected paired TreeInit table '%s' does not exist.", expected_tree_tbl))
          }
          create_fvs_lookup_indexes(con_m, tree_tbl_match[1]) # Validate and index the exact expected partner using its physical spelling.
        })
        
        meta$groups <- get_groups_from_db(full_db_path, input$stand_tbl, input$group_col, ex_groups, isolate(input$use_groups_col)) # Load available groups for selected grouping mode.
        meta$catalog <- discover_kcp_catalog(full_kcp_dir) # Load KCP catalog from folder structure.
      }, error = function(e) { # Capture any failure from metadata build steps.
        err_msg <<- e$message # Store error text for post-progress handling.
      })
    })
    
    if (!is.null(err_msg)) { # Exit early when metadata build failed.
      showNotification(err_msg, type = "error") # Surface captured error to user.
      return() # Stop further processing on error.
    }
    
    if (is.null(meta$catalog) || nrow(meta$catalog) == 0) { # Validate that at least one KCP was discovered.
      showNotification("No .kcp files located inside designated catalog directories.", type = "warning") # Inform user that catalog is empty.
      return() # Stop if there are no KCP options to map.
    }
    
    meta$types <- unique(meta$catalog$KCP_Type[order(meta$catalog$TypeOrder)]) # Build ordered KCP type list for UI/grid columns.
    
    df <- data.frame(GROUP_CODE = meta$groups, Scenario = "", stringsAsFactors = FALSE) # Seed lookup grid with one row per group.
    for (t in meta$types) { # Add one column per KCP type.
      type_rows <- meta$catalog[meta$catalog$KCP_Type == t & !is.na(meta$catalog$KCP_Name), ] # Gather available KCP names for this type.
      is_singleton_default_type <- grepl("global|outputs?", t, ignore.case = TRUE) # Recognize Global/Output folder roles without requiring an exact singular name.
      df[[t]] <- if (is_singleton_default_type && nrow(type_rows) == 1) {
        type_rows$KCP_Name[1] # Use the sole KCP regardless of its filename.
      } else {
        ""
      }
    }

    # Allow scenario naming from any editable columns except reserved fields.
    scenario_col_choices <- setdiff(names(df), c("GROUP_CODE", "Scenario")) # Build selectable Scenario suffix columns.
    selected_cols <- isolate(input$scenario_add_cols) # Read current scenario-column selection without reactive dependency.
    selected_cols <- selected_cols[selected_cols %in% scenario_col_choices] # Keep only still-valid selected columns.
    updateSelectInput(session, "scenario_add_cols", choices = scenario_col_choices, selected = selected_cols) # Refresh selector choices and retained selection.

    df <- recalculate_scenarios(df, scenario_cols = selected_cols) # Compute Scenario names for seeded grid rows.
    grid_data(df) # Publish refreshed grid to table output/reactive state.
    
    # Auto-export the default setup files
    tryCatch({ # Best-effort export of helper files without blocking UI on failure.
      out_dir <- file.path(input$root_dir, "rFVS_Runs") # Define run output directory.
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE) # Create run directory if missing.
      
      auto_wb_path <- file.path(out_dir, "KCP_Lookup_Table.xlsx") # Define default lookup workbook path.
      if (!file.exists(auto_wb_path)) { # Only generate workbook when one does not already exist.
        wb <- create_lookup_wb(df, meta, selected_cols) # Build workbook with dropdowns and formulas.
        saveWorkbook(wb, auto_wb_path, overwrite = TRUE) # Save workbook to disk.
        strip_missing_drawing_relationships(auto_wb_path) # Clean workbook XML for compatibility.
      }
      
      opt_path <- file.path(out_dir, "KCP_Lookup_Options.csv") # Define options catalog export path.
      write.csv(meta$catalog[, c("KCP_Type", "Folder", "KCP_Name", "KCP_Path")], opt_path, row.names = FALSE, na = "") # Export detailed KCP options table.
      
    }, error = function(e) { # Handle non-fatal export errors.
      warning("Could not auto-generate default setup files: ", e$message) # Log warning without interrupting main workflow.
    })
    
    showNotification("Directory scan complete. Default matrix and options exported to rFVS_Runs.", type = "message") # Confirm successful metadata scan to user.
  })
  
  output$meta_status <- renderText({ # Render metadata summary text for the status panel.
    if (is.null(meta$groups)) { # If metadata has not been loaded yet, show waiting state.
      "System State: Awaiting initialization scan triggers." # Inform user that initialization scan has not run.
    } else { # Otherwise display current resolved runtime metadata details.
      full_db_path <- resolve_db_path(input$root_dir, input$master_db) # Resolve full DB path from current root + DB inputs.
      full_kcp_dir <- resolve_kcp_path(input$root_dir, input$kcp_dir) # Resolve full KCP directory path from current root + KCP inputs.
      
      sprintf( # Build multi-line status string shown in UI.
        "Active Catalog Status:\n - Database Path: %s\n - KCP Directory Path: %s\n - Discovered Groups: %d\n - Tracked .KCP Source Files: %d\n - Dynamic KCP Lookup Types: %s", # Template for active metadata diagnostics.
        full_db_path, full_kcp_dir, length(meta$groups), nrow(meta$catalog), paste(meta$types, collapse = ", ") # Populate template with current resolved values.
      )
    }
  })
  
  output$prescription_table <- renderRHandsontable({ # Render the editable lookup matrix in Handsontable.
    df <- grid_data() # Read current matrix from reactive storage.
    req(nrow(df) > 0) # Require at least one row before rendering.
    
    hot <- rhandsontable(df, rowHeaders = TRUE, stretchH = "all") # Initialize handsontable widget from data frame.
    
    if (!is.null(meta$catalog)) { # Add dropdown constraints when catalog metadata is available.
      hot <- hot %>% hot_col(col = "GROUP_CODE", type = "dropdown", source = c("", meta$groups), strict = FALSE) # Restrict GROUP_CODE to discovered groups while allowing manual edits.
      for (t in meta$types) { # Configure each KCP type column with allowed values.
        kcp_list <- meta$catalog$KCP_Name[meta$catalog$KCP_Type == t] # Collect KCP names for this specific type.
        kcp_list <- kcp_list[!is.na(kcp_list)] # Remove missing names from dropdown options.
        hot <- hot %>% hot_col(col = t, type = "dropdown", source = c("", "ALL", kcp_list), strict = FALSE) # Set dropdown values: blank, ALL, or explicit KCP name.
      }
    }
    hot # Return configured table widget to UI.
  })
  
  observeEvent(input$prescription_table, { # Sync edited table values back into reactive state.
    old_df <- grid_data() # Capture prior table state for scenario recalculation context.
    df <- hot_to_r(input$prescription_table) # Convert handsontable payload into an R data frame.
    df <- recalculate_scenarios(df, old_df, input$scenario_add_cols) # Rebuild Scenario strings after any cell edits.
    grid_data(df) # Persist updated matrix for downstream pipeline steps.
  })

  observeEvent(input$scenario_add_cols, { # Recompute Scenario values whenever scenario suffix column selection changes.
    if (isTRUE(suppress_next_scenario_auto_recalc())) { # Skip one forced recalc when selector changes were programmatic (for example upload sync).
      suppress_next_scenario_auto_recalc(FALSE) # Consume the suppression flag so subsequent user edits recalc normally.
      return()
    }
    df <- grid_data() # Read current matrix from reactive storage.
    if (!is.null(df) && nrow(df) > 0) {
      df <- recalculate_scenarios(df, scenario_cols = input$scenario_add_cols, force_auto = TRUE) # Force full Scenario regeneration using selected columns (or fallback naming rules).
      grid_data(df) # Persist updated matrix for downstream pipeline steps.
    }
  }, ignoreNULL = FALSE) # Run even when selection is NULL/empty so fallback naming (for example GROUP_CODE + _NG) is applied.

  observeEvent(input$btn_create_merged_col, {
    mode <- input$merge_col_mode
    standalone_table <- input$stand_tbl
    
    root <- normalizePath(trimws(input$root_dir), winslash = "/", mustWork = FALSE)
    if (!nzchar(root)) root <- normalizePath(getShinyOption("FVS_USER_WD", default = getwd()), winslash = "/", mustWork = FALSE)
    db_name <- trimws(as.character(input$master_db))
    db_path <- resolve_db_path(root, db_name)
    
    if (!file.exists(db_path)) {
      showNotification("Database not found. Please scan directory first.", type = "error")
      return()
    }
    
    if (mode == "cols") {
      cols <- input$merge_cols_select
      if (length(cols) < 2) {
        showNotification("Please select at least 2 columns to merge.", type = "warning")
        return()
      }
      new_col_name <- paste(cols, collapse = "_")
      
      con <- dbConnect(SQLite(), db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      
      if (new_col_name %in% dbListFields(con, standalone_table)) {
         showNotification(sprintf("Column '%s' already exists.", new_col_name), type = "warning")
         return()
      }
      
      tryCatch({
        dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s TEXT", quote_sql_identifier(standalone_table), quote_sql_identifier(new_col_name)))
        
        # Only populate the merged column when all source columns are non-NULL.
        concat_expr <- paste(sprintf("CAST(%s AS TEXT)", sapply(cols, quote_sql_identifier)), collapse = " || '_' || ")
        nonnull_condition <- paste(sprintf("%s IS NOT NULL", sapply(cols, quote_sql_identifier)), collapse = " AND ")

        sql_update <- sprintf(
          "UPDATE %s SET %s = CASE WHEN %s THEN %s ELSE NULL END",
          quote_sql_identifier(standalone_table),
          quote_sql_identifier(new_col_name),
          nonnull_condition,
          concat_expr
        )
        dbExecute(con, sql_update)
        
        showNotification(sprintf("Successfully created merged column: %s", new_col_name), type = "message")
        
        # Update dropdowns
        table_cols <- dbListFields(con, standalone_table)
        updateSelectInput(session, "group_col", choices = sort(table_cols), selected = new_col_name)
        updateSelectizeInput(session, "merge_cols_select", choices = sort(table_cols))
      }, error = function(e) {
        showNotification(paste("Error merging columns:", e$message), type = "error")
      })
      
    } else if (mode == "groups") {
      groups <- input$merge_groups_select
      if (length(groups) < 2) {
        showNotification("Please select at least 2 GROUP entries to merge.", type = "warning")
        return()
      }
      
      new_col_name <- paste(groups, collapse = "_")
      con <- dbConnect(SQLite(), db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      
      table_cols <- dbListFields(con, standalone_table)
      grp_col_candidates <- table_cols[toupper(table_cols) == "GROUPS"]
      if (length(grp_col_candidates) == 0) {
        showNotification("GROUPS column not found in Stand Initialization table.", type = "error")
        return()
      }
      grp_col_name <- grp_col_candidates[1]
      
      if (new_col_name %in% table_cols) {
         showNotification(sprintf("Column '%s' already exists.", new_col_name), type = "warning")
         return()
      }
      
      withProgress(message = "Merging GROUP entries...", value = 0.5, {
        tryCatch({
          df_stand <- dbGetQuery(con, sprintf("SELECT rowid, %s FROM %s", quote_sql_identifier(grp_col_name), quote_sql_identifier(standalone_table)))
          
          # Extract each selected GROUPS entry independently. Standalone tokens
          # (for example All_FIA_Conditions) return their token, while entries
          # such as BPS=110480 return 110480.
          extracted_values <- vapply(
            groups,
            function(group_name) {
              extract_group_values_vectorized(df_stand[[grp_col_name]], group_name)
            },
            character(nrow(df_stand))
          )
          if (length(groups) == 1) {
            extracted_values <- matrix(extracted_values, ncol = 1)
          }

          # Populate the derived value only when every selected entry exists on
          # that row. Missing tokens and explicit NULL/NA values remain NULL.
          complete_rows <- apply(
            extracted_values,
            1,
            function(values) all(!is.na(values) & nzchar(trimws(values)))
          )
          merged_vals <- rep(NA_character_, nrow(df_stand))
          if (any(complete_rows)) {
            merged_vals[complete_rows] <- apply(
              extracted_values[complete_rows, , drop = FALSE],
              1,
              paste,
              collapse = "_"
            )
          }
          
          dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s TEXT", quote_sql_identifier(standalone_table), quote_sql_identifier(new_col_name)))
          
          # Parameterized update back to the SQLite DB
          update_df <- data.frame(new_val = merged_vals, row_id = df_stand$rowid, stringsAsFactors = FALSE)
          update_df <- update_df[!is.na(update_df$new_val), ]
          
          if (nrow(update_df) > 0) {
             sql_update <- sprintf("UPDATE %s SET %s = :new_val WHERE rowid = :row_id", quote_sql_identifier(standalone_table), quote_sql_identifier(new_col_name))
             dbExecute(
              con,
              sql_update,
              params = list(new_val = update_df$new_val, row_id = update_df$row_id)
             )
          }
          
          showNotification(sprintf("Successfully created merged column: %s", new_col_name), type = "message")
          
          table_cols <- dbListFields(con, standalone_table)
          updateSelectInput(session, "group_col", choices = sort(table_cols), selected = new_col_name)
          updateSelectizeInput(session, "merge_cols_select", choices = sort(table_cols))
        }, error = function(e) {
          showNotification(paste("Error merging GROUPS entries:", e$message), type = "error")
        })
      })
    }
  })
  
  observeEvent(input$btn_expand_modal, { # Open modal for auto-populating KCP combinations
    req(meta$groups, meta$types) # Ensure metadata is available
    
    showModal(modalDialog(
      title = "Auto-Populate Scenario Combinations",
      p("Select one or more KCP folders. Every available value across the selected folders will be cross-joined for each target group and added to the grid below."),
      selectizeInput("expand_groups", "Target Group(s) [Leave blank for ALL]:", choices = meta$groups, multiple = TRUE, width = "100%"),
      selectizeInput(
        "expand_folders",
        "Target KCP Folders (Types):",
        choices = meta$types,
        multiple = TRUE,
        width = "100%",
        options = list(placeholder = "Select one or more KCP folders")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("do_expand", "Generate Rows", class = "btn-success", icon = icon("check"))
      ),
      size = "m",
      easyClose = TRUE
    ))
  })

  observeEvent(input$do_expand, { # Execute combination expansion cross-join logic
    df <- grid_data()
    if (nrow(df) == 0) {
      removeModal()
      return()
    }
    
    target_groups <- input$expand_groups
    if (length(target_groups) == 0) target_groups <- meta$groups # Empty implies all known groups

    target_folders <- intersect(as.character(input$expand_folders), as.character(meta$types))
    if (length(target_folders) == 0) {
      showNotification("Select at least one KCP folder to generate combinations.", type = "warning")
      return()
    }

    # Build one value vector per selected KCP folder, then calculate the full
    # Cartesian product (for example, every Prescription x Timing pairing).
    folder_values <- setNames(lapply(target_folders, function(folder_name) {
      values <- meta$catalog$KCP_Name[
        meta$catalog$KCP_Type == folder_name & !is.na(meta$catalog$KCP_Name)
      ]
      sort(unique(as.character(values[nzchar(trimws(as.character(values)))])))
    }), target_folders)

    empty_folders <- names(folder_values)[lengths(folder_values) == 0]
    if (length(empty_folders) > 0) {
      showNotification(
        sprintf("No KCP files found in folder(s): %s.", paste(empty_folders, collapse = ", ")),
        type = "warning"
      )
      removeModal()
      return()
    }

    kcp_combinations <- expand.grid(
      folder_values,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    
    # Build expanded row blocks for target groups
    expanded_rows <- lapply(target_groups, function(grp) {
      base_rows <- df[df$GROUP_CODE == grp, , drop = FALSE]
      if (nrow(base_rows) > 0) {
        base_row <- base_rows[1, , drop = FALSE] # Preserve the first existing row to carry over column selections like Global/Calib settings
      } else {
        base_row <- df[1, , drop = FALSE] # Fallback in case of missing group entirely
        base_row[1, ] <- NA
        base_row$GROUP_CODE <- grp
      }
      
      rep_df <- base_row[rep(1, nrow(kcp_combinations)), , drop = FALSE]
      for (folder_name in target_folders) {
        rep_df[[folder_name]] <- kcp_combinations[[folder_name]]
      }
      rep_df
    })
    
    # Combine back together: keep ALL of the original df (including blanks), then append the newly expanded rows
    expanded_df <- do.call(rbind, expanded_rows)
    final_df <- rbind(df, expanded_df)
    
    # Sort and re-index for display neatness
    final_df <- final_df[order(final_df$GROUP_CODE), ]
    rownames(final_df) <- NULL
    
    # Include all expanded folders in Scenario labels so each cross-product row
    # has a unique, descriptive scenario name (for example Group_Rx_Timing).
    scenario_cols <- unique(c(as.character(input$scenario_add_cols), target_folders))
    updateSelectInput(session, "scenario_add_cols", selected = scenario_cols)
    final_df <- recalculate_scenarios(final_df, scenario_cols = scenario_cols, force_auto = TRUE)
    
    grid_data(final_df)
    removeModal()
    showNotification(sprintf("Successfully generated %d row combinations.", nrow(expanded_df)), type = "message")
  })

  output$download_excel <- downloadHandler( # Export the current lookup matrix as an xlsx file.
    filename = function() { # Build a date-stamped default file name.
      paste0("FVS_KCP_Lookup_", Sys.Date(), ".xlsx") # Return download file name.
    },
    content = function(file) { # Write workbook content to the requested temp download path.
      df <- grid_data() # Read current editable matrix from reactive state.
      req(nrow(df) > 0) # Require at least one row before export.
      
      wb <- create_lookup_wb(df, meta, input$scenario_add_cols) # Build workbook with dropdown validations and scenario setup.
      saveWorkbook(wb, file, overwrite = TRUE) # Save workbook to the download destination.
      strip_missing_drawing_relationships(file) # Repair drawing relationships for spreadsheet compatibility.
    }
  )
  
  observeEvent(input$upload_excel, { # Import an uploaded CSV/XLSX layout into the editable grid.
    file_info <- input$upload_excel # Read uploaded file metadata and temp path.
    req(file_info) # Require an uploaded file before processing.
    
    tryCatch({ # Catch import and validation errors to show a user-friendly modal.
      ext <- tools::file_ext(file_info$name) # Detect uploaded file extension.
      uploaded_df <- if (tolower(ext) == "csv") { # Branch for CSV input.
        read.csv(file_info$datapath, stringsAsFactors = FALSE, check.names = FALSE) # Read CSV without factor conversion and preserve headers.
      } else {
        read.xlsx(
          file_info$datapath,
          sheet = 1,
          skipEmptyRows = FALSE,
          check.names = FALSE
        ) # Keep worksheet row positions so uncached constant formulas can be restored reliably.
      }
      
      if (!("GROUP_CODE" %in% names(uploaded_df))) { # Validate required grouping key column exists.
        stop("The uploaded spreadsheet is missing the required 'GROUP_CODE' header column.") # Stop import when required schema is missing.
      }

      # Newly downloaded workbooks may not yet have cached Excel formula values.
      # Recover constant GROUP_CODE formulas such as ="01" directly from xlsx XML
      # so users can immediately re-upload a file without opening it in Excel.
      if (tolower(ext) %in% c("xlsx", "xlsm")) {
        group_col_index <- match("GROUP_CODE", names(uploaded_df))
        formula_groups <- read_xlsx_constant_text_formulas(
          file_info$datapath,
          column_index = group_col_index,
          sheet_index = 1L
        )
        if (nrow(formula_groups) > 0) {
          data_rows <- formula_groups$excel_row - 1L # Worksheet row 1 contains headers.
          valid_formula_rows <- data_rows >= 1L
          data_rows <- data_rows[valid_formula_rows]
          formula_values <- formula_groups$value[valid_formula_rows]

          if (length(data_rows) > 0) {
            required_rows <- max(data_rows)
            if (nrow(uploaded_df) < required_rows) {
              uploaded_df[seq.int(nrow(uploaded_df) + 1L, required_rows), ] <- NA
            }
            current_groups <- trimws(as.character(uploaded_df$GROUP_CODE[data_rows]))
            restore_rows <- is.na(current_groups) | !nzchar(current_groups)
            uploaded_df$GROUP_CODE[data_rows[restore_rows]] <- formula_values[restore_rows]
          }
        }
      }
      
      if (!is.null(meta$types)) { # Ensure all expected KCP type columns exist in imported data.
        for (t in meta$types) { # Iterate every discovered KCP type column.
          if (!(t %in% names(uploaded_df))) uploaded_df[[t]] <- "" # Add missing type columns as blank strings.
        }
      }

      uploaded_df$GROUP_CODE <- trimws(as.character(uploaded_df$GROUP_CODE)) # Normalize GROUP_CODE values before filtering.
      valid_group_rows <- !is.na(uploaded_df$GROUP_CODE) & nzchar(uploaded_df$GROUP_CODE)
      if (!any(valid_group_rows)) {
        stop(
          paste(
            "The uploaded spreadsheet contains no populated GROUP_CODE values.",
            "The existing KCP Lookup grid was left unchanged."
          )
        )
      }
      uploaded_df <- uploaded_df[valid_group_rows, , drop = FALSE] # Remove only individual blank rows after validating the workbook.

      # Keep scenario selector aligned with imported structure.
      scenario_col_choices <- setdiff(names(uploaded_df), c("GROUP_CODE", "Scenario")) # Derive selectable scenario-suffix columns from imported headers.
      selected_cols <- isolate(input$scenario_add_cols) # Snapshot current scenario suffix selection.
      selected_cols <- selected_cols[selected_cols %in% scenario_col_choices] # Retain only selections that still exist in imported data.
      suppress_next_scenario_auto_recalc(TRUE) # Mark next scenario_add_cols observer tick as programmatic so uploaded labels are preserved.
      freezeReactiveValue(input, "scenario_add_cols") # Prevent programmatic selector updates from triggering forced Scenario regeneration.
      updateSelectInput(session, "scenario_add_cols", choices = scenario_col_choices, selected = selected_cols) # Refresh scenario suffix selector with imported column set.
      
      prior_uploaded_df <- if ("Scenario" %in% names(uploaded_df)) uploaded_df else NULL # Keep uploaded Scenario labels as preservation baseline when available.
      uploaded_df <- recalculate_scenarios(uploaded_df, old_df = prior_uploaded_df, scenario_cols = selected_cols, force_auto = FALSE) # Preserve uploaded Scenario labels; only auto-fill blanks from current suffix rules.
      
      grid_data(uploaded_df) # Publish imported/normalized data back to the grid.
      showNotification("Excel workbook layout imported successfully. Grid mirrors applied data.", type = "message") # Confirm successful import to user.
      
    }, error = function(e) { # Handle import failure with details dialog.
      showModal(modalDialog(
        title = "Spreadsheet Import Failure", # Modal title for import errors.
        p(e$message), easyClose = TRUE, footer = modalButton("Dismiss") # Display underlying error message.
      ))
    })
  })
  
  observeEvent(input$save_matrix, { # Compile current matrix into GROUP_CODE/Scenario manifest records.
    manifest_ready(FALSE) # Keep the pipeline blocked unless this save completes successfully.
    df <- grid_data() # Read current matrix state.
    if (nrow(df) == 0) { # Guard against empty table saves.
      showNotification("No data array available to build scenarios.", type = "error") # Notify user matrix has no rows.
      return() # Stop save flow for empty matrix.
    }
    
    df$GROUP_CODE <- trimws(as.character(df$GROUP_CODE)) # Normalize GROUP_CODE text before validation.
    df <- df[df$GROUP_CODE != "" & !is.na(df$GROUP_CODE), ] # Keep only rows with usable GROUP_CODE values.
    
    if (nrow(df) == 0) { # Guard when all rows were removed by GROUP_CODE filter.
      showNotification("Table does not contain valid populated GROUP_CODE rows.", type = "error") # Inform user no valid group rows remain.
      return() # Stop save flow when no valid groups exist.
    }
    
    # Prevent ambiguous runs by disallowing duplicate Scenario names.
    scenario_vals <- trimws(as.character(df$Scenario)) # Normalize Scenario values for duplicate checks.
    scenario_vals <- scenario_vals[!is.na(scenario_vals) & nzchar(scenario_vals)] # Ignore blank or missing Scenario entries.
    dup_scenarios <- sort(unique(scenario_vals[duplicated(scenario_vals)])) # Identify repeated Scenario names.
    if (length(dup_scenarios) > 0) { # Block manifest save when duplicate scenario labels exist.
      showModal(modalDialog(
        title = "Duplicate Scenario Names Found", # Explain why save is blocked.
        p("Each Scenario must be unique. Please edit or remove duplicates before saving."), # Guidance text for resolving duplicates.
        pre(style = "white-space: pre-wrap; word-break: break-all; max-height: 200px;", paste(dup_scenarios, collapse = "\n")), # Show duplicate Scenario list.
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
      return() # Abort save until duplicates are fixed.
    }
    
    manifest_list <- vector("list", nrow(df)) # Pre-allocate list to accumulate manifest rows.
    
    tryCatch({ # Catch validation/catalog mapping errors during manifest construction.
      withProgress(message = "Compiling paths manifest...", value = 0, { # Show progress while converting grid rows to manifest rows.
        for (i in seq_len(nrow(df))) { # Iterate each grid row as a manifest record.
          row_item <- df[i, , drop = FALSE] # Work with one row at a time.
          all_paths_extracted <- character(0) # Collect resolved KCP file paths for this row.
          
          for (t in meta$types) { # Resolve requested KCP entries for each KCP type column.
            cell_input <- row_item[[t]] # Read cell text for current KCP type.
            requested_names <- split_kcp_cell(cell_input) # Parse multi-value cell input into requested KCP names.
            if (length(requested_names) == 0) next # Skip empty selections for this type.
            
            type_catalog_rows <- meta$catalog[meta$catalog$KCP_Type == t, ] # Slice catalog rows for current KCP type.
            
            if (any(toupper(requested_names) == "ALL")) { # Expand ALL selection to every KCP path in this type.
              all_paths_extracted <- c(all_paths_extracted, type_catalog_rows$KCP_Path) # Append all paths for selected type.
            } else {
              missing_elements <- setdiff(requested_names, type_catalog_rows$KCP_Name) # Check for requested names not present in catalog.
              if (length(missing_elements) > 0) { # Fail fast when the row references unknown KCP names.
                stop(sprintf("Row %d ('%s') references unknown file entries inside '%s': %s", 
                             i, row_item$GROUP_CODE, t, paste(missing_elements, collapse = ", "))) # Provide detailed row/type validation message.
              }
              matched_paths <- type_catalog_rows$KCP_Path[match(requested_names, type_catalog_rows$KCP_Name)] # Resolve requested names to absolute KCP paths.
              all_paths_extracted <- c(all_paths_extracted, matched_paths) # Append resolved paths for this type.
            }
          }
          
          cleaned_paths_array <- all_paths_extracted[!duplicated(all_paths_extracted)] # Remove duplicate paths while preserving first-seen order.
          
          manifest_list[[i]] <- data.frame( # Store compiled manifest row for this group/scenario.
            GROUP_CODE = row_item$GROUP_CODE, # Group identifier used for queue join.
            Scenario   = row_item$Scenario, # Scenario label used for directory/run naming.
            KCP_Paths  = paste(cleaned_paths_array, collapse = "|"), # Pipe-delimited KCP path payload.
            KCP_Count  = length(cleaned_paths_array), # Count of unique KCP files for this row.
            stringsAsFactors = FALSE
          )
          setProgress(i / nrow(df)) # Update UI progress based on completed rows.
        }
      })
      
      final_manifest_df <- do.call(rbind, manifest_list) # Combine row-level manifest records into one data frame.
      
      m_file <- manifest_path() # Resolve manifest CSV output path.
      run_base_dir <- dirname(m_file) # Resolve containing output folder.
      if (!dir.exists(run_base_dir)) dir.create(run_base_dir, recursive = TRUE, showWarnings = FALSE) # Ensure output directory exists.
      
      tryCatch(
        write.csv(final_manifest_df, m_file, row.names = FALSE, na = ""), # Write compiled manifest to disk.
        error = function(e) {
          stop(
            paste(
              "The manifest CSV could not be saved. It is likely open or locked by Excel or another application.",
              "Close KCP_AddFile_Manifest.csv and try Save Matrix Settings again.",
              paste0("System message: ", e$message)
            ),
            call. = FALSE
          )
        }
      )

      manifest_ready(TRUE) # Unlock Steps 1–3 only after the current matrix is successfully written.
      
      # Explicitly signal to the reactive queue that a new manifest is on disk
      manifest_trigger(manifest_trigger() + 1) # Trigger queue recalculation after successful write.
      
      showModal(modalDialog(
        title = "Manifest Compiled Successfully", # Success title for manifest export.
        p("Configuration catalog exported to pipeline staging space:"), # Confirmation text.
        pre(style = "white-space: pre-wrap; word-break: break-all; max-height: 200px;", m_file), # Show written manifest file path.
        size = "m",
        easyClose = TRUE, 
        footer = modalButton("Dismiss")
      ))
      
    }, error = function(e) { # Surface manifest build/validation errors in a modal.
      showModal(modalDialog(
        title = "Validation Constraint Failure", # Error title for failed manifest compile.
        p(e$message), easyClose = TRUE, footer = modalButton("Dismiss") # Present exact validation error message.
      ))
    })
  })
  
  output$pipeline_diagnostics <- renderText({ # Render readiness summary for Step 1 keyfile generation.
    manifest_trigger() # Re-render when Save Matrix creates or replaces the manifest, even if it was missing during the prior render.
    full_db_path <- resolve_db_path(input$root_dir, input$master_db) # Resolve effective master DB path from current UI inputs.
    m_file <- manifest_path() # Resolve expected manifest CSV path.
    
    if (!file.exists(full_db_path)) { # Block when master DB path is missing.
      return("Status: Blocked. Root folder path or master SQL database file is unmapped.") # Explain missing DB prerequisite.
    }
    if (!file.exists(m_file)) { # Block when manifest has not yet been saved.
      return("Status: Blocked. Active 'KCP_AddFile_Manifest.csv' not found in the rFVS_Runs directory. Save matrix settings on Panel 2 first.") # Explain missing manifest prerequisite.
    }
    if (!isTRUE(manifest_ready())) {
      return("Status: Blocked. Run 'Save KCP Scenarios & Build Manifest' for the current KCP lookup table before generating keyfiles.")
    }
    
    jq <- get_job_queue() # Build joined stand/scenario queue for readiness checks.
    if (is.null(jq)) { # Block when queue construction failed.
      return("Status: Blocked. Job queue query to the database failed. Check the red error notification in the corner (Are STAND_ID and STAND_CN columns present?).") # Show queue failure guidance.
    }
    if (nrow(jq) == 0) { # Block when queue exists but no jobs matched.
      return("Status: Blocked. Manifest file exists but exact GROUP_CODE cross-join with database targets returned 0 jobs.") # Explain zero-job join condition.
    }
    planned_workers <- resolve_worker_count(nrow(jq), input$num_cores, workload = "light") # Preview the dynamically scaled key-generation pool.
    
    sprintf( # Return success summary with queue size and run settings.
      "Pipeline Status: Active Ready Queue\n - Master Database Found: %s\n - Cross-Join Stands Queue: %d active tasks\n - Workers Planned: %d of %d requested\n - Project Time Interval: %d cycles (%d total years simulated)\n\n[Ready to generate standalone .key configurations.]", # Template for step-1 readiness diagnostics.
      basename(full_db_path), nrow(jq), planned_workers, input$num_cores, input$num_cycles, (input$num_cycles * input$time_int) # Populate summary with resolved values.
    )
  })
  
  output$pipeline_diagnostics_rfvs <- renderText({ # Render readiness summary for Step 2 rFVS execution.
    manifest_trigger() # Re-render when Save Matrix creates or replaces the manifest, even if it was missing during the prior render.
    full_db_path <- resolve_db_path(input$root_dir, input$master_db) # Resolve effective master DB path from current UI inputs.
    m_file <- manifest_path() # Resolve expected manifest CSV path.
    
    if (!file.exists(full_db_path)) { # Block when master DB path is missing.
      return("Status: Blocked. Root folder path or master SQL database file is unmapped.") # Explain missing DB prerequisite.
    }
    if (!file.exists(m_file)) { # Block when manifest has not yet been saved.
      return("Status: Blocked. Active 'KCP_AddFile_Manifest.csv' not found. Save matrix settings on Panel 2 first.") # Explain missing manifest prerequisite.
    }
    if (!isTRUE(manifest_ready())) {
      return("Status: Blocked. Run 'Save KCP Scenarios & Build Manifest' for the current KCP lookup table before running rFVS.")
    }
    
    full_jq <- get_job_queue() # Build the complete joined stand/scenario queue for readiness checks.
    if (is.null(full_jq)) { # Block when queue construction failed.
      return("Status: Blocked. Job queue query to the database failed. Check the red error notification in the corner (Are STAND_ID and STAND_CN columns present?).") # Show queue failure guidance.
    }
    if (nrow(full_jq) == 0) { # Block when queue exists but no jobs matched.
      return("Status: Blocked. Manifest file exists but exact GROUP_CODE cross-join with database targets returned 0 jobs.") # Explain zero-job join condition.
    }

    jq <- get_execution_queue() # Apply the optional one-stand-per-group/scenario test filter.
    
    detected_variants <- if ("VARIANT" %in% names(jq)) { # Derive variant inventory only when column exists in queue.
      paste(unique(jq$VARIANT), collapse = ", ") # Build comma-delimited variant list for status text.
    } else {
      "None Detected (Ensure VARIANT column exists in Database)" # Fallback message when variant metadata is unavailable.
    }
    
    run_mode <- if (isTRUE(input$test_run_mode)) "TEST — first stand per GROUP_CODE/Scenario" else "FULL"
    planned_workers <- resolve_worker_count(nrow(jq), input$num_cores_rfvs, workload = "heavy") # Preview all useful workers based on the actual full or sampled execution queue.
    sprintf( # Return success summary with queue size, cores, and variant inventory.
      "Pipeline Status: Active Ready Queue\n - Master Database Found: %s\n - Execution Mode: %s\n - Full Cross-Join Queue: %d active tasks\n - Current Execution Queue: %d active tasks\n - Workers Planned: %d of %d requested\n - Detected FVS Variants: %s (%d total)\n\n[Ready to dispatch parallel rFVS simulations.]", # Template for lightweight step-2 readiness diagnostics.
      basename(full_db_path), run_mode, nrow(full_jq), nrow(jq), planned_workers, input$num_cores_rfvs, detected_variants, if ("VARIANT" %in% names(jq)) length(unique(jq$VARIANT)) else 0 # Populate summary with queue and worker details.
    )
  })
  
  output$pipeline_diagnostics_merge <- renderText({ # Render readiness summary for Step 3 output merge.
    manifest_trigger() # Re-render when Save Matrix creates or replaces the manifest, even if it was missing during the prior render.
    full_db_path <- resolve_db_path(input$root_dir, input$master_db) # Resolve effective master DB path from current UI inputs.
    m_file <- manifest_path() # Resolve expected manifest CSV path.
    
    if (!file.exists(full_db_path) || !file.exists(m_file)) { # Block when DB or manifest prerequisite is missing.
      return("Status: Blocked. Setup incomplete.") # Explain missing prerequisites for merge stage.
    }
    if (!isTRUE(manifest_ready())) {
      return("Status: Blocked. Run 'Save KCP Scenarios & Build Manifest' for the current KCP lookup table before merging outputs.")
    }
    
    jq <- get_job_queue() # Build joined stand/scenario queue used to derive merge targets.
    if (is.null(jq) || nrow(jq) == 0) { # Block when there are no jobs available to merge.
      return("Status: Blocked. No jobs available to merge.") # Explain empty merge queue.
    }
    
    unique_combos <- unique(jq[, c("GROUP_CODE", "Scenario")]) # Count unique Group/Scenario output databases to merge.
    sprintf( # Return success summary with merge workload and destination.
      "Pipeline Status: Ready to Consolidate\n - Scenarios to Merge: %d\n - Cores: %d threads requested\n - Target Outputs directory: %s\n\n[All scenario DBs will be compiled and then merged into a single master DB.]", # Template for lightweight step-3 readiness diagnostics.
      nrow(unique_combos), # Number of unique scenario databases expected in merge.
      input$num_cores_merge, # User-requested core count for merge workers.
      file.path(input$root_dir, "Outputs") # Final output directory path.
    )
  })
  
  # ----------------- PIPELINE STEP 1: PARALLEL KEYFILE GENERATION -----------------
  observeEvent(input$gen_keyfiles, { # Start Step 1: build per-stand `run.key` files in parallel.
    shinyjs::disable("gen_keyfiles") # Prevent duplicate launches while generation is active.
    step_start_gen(Sys.time()) # Record start timestamp for elapsed-time reporting.

    if (!isTRUE(manifest_ready())) {
      shinyjs::enable("gen_keyfiles")
      step_start_gen(NULL)
      showNotification("Run 'Save KCP Scenarios & Build Manifest' for the current KCP lookup table before generating keyfiles.", type = "warning", duration = 8)
      return()
    }
    
    job_queue <- get_job_queue() # Always generate keyfiles for the complete configured queue.
    if (is.null(job_queue) || nrow(job_queue) == 0) { # Block run when no executable jobs exist.
      shinyjs::enable("gen_keyfiles") # Re-enable launch button because nothing ran.
      shinyjs::hide("kill_gen_btn_wrap") # Keep cancel UI hidden when no background process exists.
      step_start_gen(NULL) # Clear start time because execution did not begin.
      showNotification("Pipeline Action Denied: No compiled project queue targets found.", type = "error") # Explain why launch was denied.
      return() # Exit observer early.
    }
    
    keyfile_db_path <- resolve_db_path(input$root_dir, input$master_db, slash = "\\") # Resolve Windows-style DB path inserted into keyfiles.
    p_inv_year   <- as.integer(input$inv_year) # Snapshot the requested common simulation start year.
    p_inv_year_mode <- input$inv_year_mode # Snapshot whether to reset InvYear or preserve it and add pre-growth cycles.
    p_time_int   <- input$time_int # Snapshot cycle interval setting.
    p_num_cycles <- input$num_cycles # Snapshot total cycle count setting.
    p_cores      <- input$num_cores # Snapshot requested worker core count.
    if (is.na(p_cores) || p_cores < 1) p_cores <- 1 # Clamp to at least one worker.
    
    p_stand_tbl  <- input$stand_tbl # Source stand table.
    expected_tree_tbl <- gsub("StandInit", "TreeInit", p_stand_tbl, ignore.case = TRUE) # Derive the required convention-based tree-table name.
    key_sql_schema <- tryCatch({ # Resolve exact physical names before any workers or output directories are started.
      con_key <- dbConnect(SQLite(), resolve_db_path(input$root_dir, input$master_db))
      on.exit(try(dbDisconnect(con_key), silent = TRUE), add = TRUE)
      physical_tables <- dbListTables(con_key)
      stand_match <- physical_tables[toupper(physical_tables) == toupper(p_stand_tbl)]
      tree_match <- physical_tables[toupper(physical_tables) == toupper(expected_tree_tbl)]
      if (length(stand_match) == 0L) stop(sprintf("Selected StandInit table '%s' does not exist.", p_stand_tbl))
      if (length(tree_match) == 0L) stop(sprintf("Expected paired TreeInit table '%s' does not exist.", expected_tree_tbl))

      physical_stand_tbl <- stand_match[1]
      physical_tree_tbl <- tree_match[1]
      stand_columns <- dbListFields(con_key, physical_stand_tbl)
      tree_columns <- dbListFields(con_key, physical_tree_tbl)
      stand_id_column <- stand_columns[toupper(stand_columns) == "STAND_ID"]
      tree_id_column <- tree_columns[toupper(tree_columns) == "STAND_ID"]
      if (length(stand_id_column) == 0L) stop(sprintf("StandInit table '%s' is missing STAND_ID.", physical_stand_tbl))
      if (length(tree_id_column) == 0L) stop(sprintf("TreeInit table '%s' is missing STAND_ID.", physical_tree_tbl))

      list(
        stand_table = quote_sql_identifier(physical_stand_tbl),
        tree_table = quote_sql_identifier(physical_tree_tbl),
        stand_id = quote_sql_identifier(stand_id_column[1]),
        tree_id = quote_sql_identifier(tree_id_column[1])
      )
    }, error = function(e) e)
    if (inherits(key_sql_schema, "error")) {
      shinyjs::enable("gen_keyfiles")
      step_start_gen(NULL)
      showNotification(paste("Cannot generate keyfiles:", key_sql_schema$message), type = "error", duration = 10)
      return()
    }
    p_stand_tbl_sql <- key_sql_schema$stand_table # Safely quoted physical StandInit table name for generated SQL.
    p_tree_tbl_sql <- key_sql_schema$tree_table # Safely quoted physical TreeInit table name for generated SQL.
    p_stand_id_sql <- key_sql_schema$stand_id # Safely quoted physical StandInit identifier column.
    p_tree_id_sql <- key_sql_schema$tree_id # Safely quoted physical TreeInit identifier column.

    if (isTRUE(identical(p_inv_year_mode, "grow"))) { # Pre-growth requires a valid inventory year for every queued stand.
      if (!("INV_YEAR" %in% names(job_queue))) {
        shinyjs::enable("gen_keyfiles")
        step_start_gen(NULL)
        showNotification("Cannot preserve and grow inventory years: the selected stand table has no INV_YEAR column.", type = "error", duration = 8)
        return()
      }
      stand_inv_years <- suppressWarnings(as.integer(job_queue$INV_YEAR))
      if (any(is.na(stand_inv_years))) {
        shinyjs::enable("gen_keyfiles")
        step_start_gen(NULL)
        showNotification("Cannot preserve and grow inventory years: one or more queued stands have a missing or invalid INV_YEAR.", type = "error", duration = 8)
        return()
      }
      if (any(stand_inv_years > p_inv_year)) {
        shinyjs::enable("gen_keyfiles")
        step_start_gen(NULL)
        showNotification(sprintf("Common start year must be at least %d, the latest INV_YEAR in the active queue.", max(stand_inv_years)), type = "error", duration = 8)
        return()
      }
    }
    
    unique_scenarios <- unique(job_queue$Scenario) # Build stable scenario index for MgmtId assignment.
    total_jobs <- nrow(job_queue) # Total stands to process in parallel loop.
    p_cores <- resolve_worker_count(total_jobs, p_cores, workload = "light") # Scale lightweight key generation without paying for an oversized cluster.
    active_gen_total(total_jobs) # Freeze the progress denominator for this background process.

    shinyjs::show("kill_gen_btn_wrap") # Reveal cancel control once work starts.
    
    writeLines("0|Booting up compute cluster (this may take a moment)...", prog_file_gen) # Seed progress file read by the UI poller.
    gen_prog <<- shiny::Progress$new(session, min=0, max=1) # Open modal progress bar for this step.
    gen_prog$set(message = "Executing : Building Stand Keyfiles...", value=0, detail = "Booting up compute cluster (this may take a moment)...") # Initialize user-facing progress text.
    
    p <- callr::r_bg(function(job_queue, p_cores, p_stand_tbl_sql, p_tree_tbl_sql, p_stand_id_sql, p_tree_id_sql, p_inv_year, p_inv_year_mode, p_time_int, p_num_cycles, keyfile_db_path, prog_file, unique_scenarios, total_jobs) { # Fork a background R session so Shiny stays responsive.
      library(parallel) # Provides PSOCK cluster workers.
      library(doSNOW) # Registers foreach backend with progress callback support.
      library(uuid) # Generates run UUID headers for keyfiles.
      
      cl <- NULL # Hold an optional PSOCK cluster when more than one worker is useful.
      if (p_cores > 1L) {
        cl <- makeCluster(p_cores) # Spawn only the capped number of useful workers.
        registerDoSNOW(cl) # Route `%dopar%` iterations to this cluster.
        on.exit(stopCluster(cl), add = TRUE) # Guarantee worker shutdown on success/error.
      } else {
        foreach::registerDoSEQ() # Avoid PSOCK startup entirely for a single sampled task.
        writeLines(sprintf("0|Generating %d keyfile without cluster startup...", total_jobs), prog_file)
      }
      
      last_update <- Sys.time() # Throttle progress-file writes to avoid excessive disk churn.
      progress_callback <- function(n) { # Called by doSNOW after completed loop iterations.
        if (as.numeric(difftime(Sys.time(), last_update, units="secs")) > 0.5 || n == total_jobs) { # Emit update every ~0.5s or at completion.
          writeLines(sprintf("%d|Processed %d of %d keyfiles...", n, n, total_jobs), prog_file) # Persist machine-readable progress for UI poller.
          last_update <<- Sys.time() # Reset throttle timer after write.
        }
      }
      
            foreach(i = seq_len(total_jobs),  # Parallel outer loop: one iteration per stand/scenario queue record.
              .packages = c("uuid"), # Ensure worker can call UUIDgenerate().
              .options.snow = if (p_cores > 1L) list(progress = progress_callback) else NULL) %dopar% { # Wire progress only when the snow backend is active.
                
                stand_id      <- job_queue$STAND_ID[i] # Stand identifier used in SQL and title.
                stand_cn      <- job_queue$STAND_CN[i] # StandCN block value.
                grp_code      <- job_queue$GROUP_CODE[i] # Group code carried into comment header.
                scenario      <- job_queue$Scenario[i] # Scenario label used in names and MgmtId mapping.
                kcp_paths_raw <- job_queue$KCP_Paths[i] # Pipe-delimited KCP file path payload.
                stand_dir_p   <- job_queue$stand_dir[i] # Target output directory for this stand.
                
                mgmt_idx <- match(scenario, unique_scenarios) # Convert scenario to stable 1-based index.
                mgmt_id  <- sprintf("A%03d", mgmt_idx) # Format management ID token (A001, A002, ...).
                run_name <- scenario # Keep run label aligned to Scenario value.
                
                dir.create(stand_dir_p, recursive = TRUE, showWarnings = FALSE) # Ensure stand output directory exists.
                kcp_vector <- unlist(strsplit(kcp_paths_raw, "\\|")) # Split serialized KCP list into individual file paths.
                
                stack_block <- c( # Seed AddFile stack header comments inserted into keyfile body.
                  "*--- AUTO-GENERATED ADDFILE KCP STACK ---*",
                  paste0("* GROUP_CODE: ", grp_code),
                  paste0("* Scenario: ", scenario),
                  paste0("* Built: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
                  ""
                )
                
                if (length(kcp_vector) > 0 && nchar(kcp_paths_raw) > 0) { # Build OPEN/ADDFILE/CLOSE sections only when KCP paths exist.
                  for (k in seq_along(kcp_vector)) { # Serial inner loop: append one file handle block per KCP path.
                    fnum <- 50 + k # Assign deterministic file unit numbers starting at 51.
                    open_line    <- sprintf("%-10s%10s%10s%10s%10s%10s", "OPEN", paste0(fnum, "."), "0.", "0.", "80.", "0.") # OPEN command line.
                    addfile_line <- sprintf("%-10s%10s", "ADDFILE", paste0(fnum, ".")) # ADDFILE command referencing this unit.
                    close_line   <- sprintf("%-10s%10s", "CLOSE", paste0(fnum, ".")) # CLOSE command for this unit.
                    clean_path   <- normalizePath(kcp_vector[k], winslash = "\\", mustWork = FALSE) # Normalize to Windows path separators for keyfile compatibility.
                    stack_block  <- c(stack_block, open_line, clean_path, addfile_line, close_line) # Append this KCP's mini-stack to the full block.
                  }
                }
                stack_block <- c(stack_block, "*-----------------------------------------------*") # Close stack comment block.

                # In preserve/grow mode, retain the stand's database InvYear and
                # insert any bridge cycles needed to reach the common start year.
                # The requested number of regular cycles begins only after that year.
                timing_keywords <- if (identical(p_inv_year_mode, "grow")) {
                  stand_inv_year <- as.integer(job_queue$INV_YEAR[i])
                  bridge_years <- seq(stand_inv_year, p_inv_year, by = p_time_int)
                  regular_years <- seq(p_inv_year, p_inv_year + p_num_cycles * p_time_int, by = p_time_int)
                  cycle_years <- sort(unique(c(bridge_years, p_inv_year, regular_years)))
                  cycle_intervals <- diff(cycle_years)
                  interval_overrides <- vapply(
                    which(cycle_intervals != p_time_int),
                    function(cycle_index) sprintf("%-10s%10d%10d", "TimeInt", cycle_index, cycle_intervals[cycle_index]),
                    character(1)
                  )
                  c(
                    sprintf("%-10s%10d", "TimeInt", p_time_int),
                    interval_overrides,
                    sprintf("%-10s%10d", "NumCycle", length(cycle_intervals))
                  )
                } else {
                  c(
                    sprintf("%-10s%10d", "TimeInt", p_time_int),
                    sprintf("%-10s%10d", "NumCycle", p_num_cycles)
                  )
                }

                database_keywords <- c(
                  "Database",
                  "DSNin", keyfile_db_path,
                  "StandSQL", paste0("SELECT * FROM ", p_stand_tbl_sql, " WHERE ", p_stand_id_sql, " = '", gsub("'", "''", stand_id, fixed = TRUE), "'"), "EndSQL",
                  "TreeSQL",  paste0("SELECT * FROM ", p_tree_tbl_sql, " WHERE ", p_tree_id_sql, " = '", gsub("'", "''", stand_id, fixed = TRUE), "'"), "EndSQL",
                  "End"
                )
                if (identical(p_inv_year_mode, "reset")) {
                  database_keywords <- c(database_keywords, paste0("InvYear       ", p_inv_year)) # Override the value loaded from the database without modifying the source table.
                }
                
                kw_content <- c( # Assemble final run.key content in expected FVS keyword order.
                  paste0("!!title: ", run_name, "_", stand_id),
                  paste0("!!uuid:  ", UUIDgenerate()),
                  paste0("!!built: ", format(Sys.time(), "%Y-%m-%d_%H:%M:%S")),
                  "StdIdent",
                  sprintf("%-40s%s", stand_id, run_name),
                  "StandCN        ",
                  stand_cn,
                  "MgmtId",
                  mgmt_id,
                  timing_keywords,
                  "",
                  stack_block,
                  "",
                  database_keywords,
                  "",
                  "Process",
                  "Stop"
                )
                
                writeLines(kw_content, file.path(stand_dir_p, "run.key"), useBytes = TRUE) # Persist generated keyfile for this stand.
                return(TRUE) # Mark this iteration as successful.
              }
      return(total_jobs) # Return processed count to parent process on successful completion.
    }, args = list(job_queue, p_cores, p_stand_tbl_sql, p_tree_tbl_sql, p_stand_id_sql, p_tree_id_sql, p_inv_year, p_inv_year_mode, p_time_int, p_num_cycles, keyfile_db_path, prog_file_gen, unique_scenarios, total_jobs), supervise = TRUE) # Pass immutable args into background scope with supervision enabled.
    
    bg_gen(p) # Store process handle for progress polling, cancellation, and completion handling.
  })
  
  observe({ # Poll Step 1 background process state and keep progress UI synchronized.
    p <- bg_gen() # Retrieve active keyfile-generation process handle.
    req(p) # Run this observer only while a process handle exists.
    invalidateLater(500, session) # Re-check process/progress every 500 ms.
    
    if (p$is_alive()) { # Live branch: background process is still running.
      shinyjs::show("kill_gen_btn_wrap") # Keep cancel control visible during execution.
      if (file.exists(prog_file_gen)) { # Parse latest progress marker emitted by background worker.
        l <- suppressWarnings(readLines(prog_file_gen)) # Read progress lines without warning on transient file races.
        if (length(l) > 0) { # Proceed only when at least one progress line exists.
          parts <- strsplit(l[length(l)], "\\|")[[1]] # Use most recent `count|detail` payload.
          if (length(parts) >= 2 && !is.null(gen_prog)) { # Update UI only when payload is valid and progress modal exists.
            total_jobs <- isolate(active_gen_total()) # Use the queue size captured when this run started.
            val <- if (!is.null(total_jobs) && total_jobs > 0) as.numeric(parts[1]) / total_jobs else 0 # Convert processed-count to 0..1 progress fraction.
            gen_prog$set(value = val, detail = paste(parts[-1], collapse="|")) # Refresh modal bar value and status text.
          }
        }
      }
    } else { # Completion branch: process exited (success or failure).
      elapsed_gen <- format_elapsed(step_start_gen()) # Compute elapsed wall-clock duration for completion dialog.
      step_start_gen(NULL) # Clear step start timestamp.
      bg_gen(NULL) # Clear stored process handle.
      if (!is.null(gen_prog)) gen_prog$close() # Close progress modal if still open.
      gen_prog <<- NULL # Reset progress object state.
      shinyjs::enable("gen_keyfiles") # Re-enable launch button for next run.
      shinyjs::hide("kill_gen_btn_wrap") # Hide cancel control because no process is active.
      updateActionButton(session, "kill_gen_btn", label = "Cancel", icon = icon("xmark")) # Restore default cancel button appearance.
      
      if (p$get_exit_status() == 0) { # Successful process exit.
        showModal(modalDialog(
          title = "Step 1 Complete: Keyfiles Isolated",
          p(sprintf("Successfully deployed and structured %s FVS keyfiles across processing nodes.", p$get_result())),
          p(sprintf("Elapsed Time: %s", elapsed_gen)),
          easyClose = TRUE, footer = modalButton("Dismiss")
        ))
      } else { # Non-zero exit; surface captured background errors.
        err <- p$read_error_lines() # Read stderr lines from failed background process.
        showModal(modalDialog(
          title = "Keyfile Engine Initialization Failure",
          p(paste(err, collapse = "\n")), easyClose = TRUE, footer = modalButton("Dismiss")
        ))
      }
      active_gen_total(NULL) # Clear the frozen progress denominator.
    }
  })
  
  observeEvent(input$kill_gen_btn, priority=110, { # Handle user-requested cancellation of Step 1 background job.
    p <- bg_gen() # Retrieve active process handle (if any).
    if (!is.null(p) && isTRUE(p$is_alive())) { # Cancel only when process exists and is currently alive.
      terminate_bg_process(p) # Attempt graceful tree kill, then fallback kill.
      bg_gen(NULL) # Clear stored process reference after cancellation.
      if (!is.null(gen_prog)) gen_prog$close() # Close visible progress modal.
      gen_prog <<- NULL # Reset progress object.
      step_start_gen(NULL) # Clear elapsed-time start marker.
      active_gen_total(NULL) # Clear the frozen progress denominator.
      shinyjs::enable("gen_keyfiles") # Re-enable Step 1 launch button.
      shinyjs::hide("kill_gen_btn_wrap") # Hide cancel control since process is no longer running.
      updateActionButton(session, "kill_gen_btn", label = "Cancel", icon = icon("xmark")) # Reset cancel button label/icon to default.
      showNotification("Keyfile generation cancelled by user.", type = "warning", duration = 5) # Confirm cancellation to user.
    }
  })
  
  # ----------------- PIPELINE STEP 2: PARALLEL rFVS ENGINE RUNS -----------------
  observeEvent(input$run_rfvs, { # Start Step 2: execute generated keyfiles through rFVS in parallel.
    shinyjs::disable("run_rfvs") # Prevent duplicate launches while workers are active.
    step_start_run(Sys.time()) # Record start timestamp for elapsed-time reporting.

    if (!isTRUE(manifest_ready())) {
      shinyjs::enable("run_rfvs")
      step_start_run(NULL)
      showNotification("Run 'Save KCP Scenarios & Build Manifest' for the current KCP lookup table before running rFVS.", type = "warning", duration = 8)
      return()
    }
    
    job_queue <- get_execution_queue() # Resolve the full or sampled execution queue according to Step 2 test mode.
    if (is.null(job_queue) || nrow(job_queue) == 0) { # Block run when no executable stand records exist.
      shinyjs::enable("run_rfvs") # Re-enable launch button because nothing started.
      shinyjs::hide("kill_run_btn_wrap") # Ensure cancel control remains hidden.
      step_start_run(NULL) # Clear timer because run did not begin.
      showNotification("Pipeline Action Denied: No execution targets map to the system queue.", type = "error") # Explain denial reason.
      return() # Exit observer early.
    }
    
    p_bin_loc    <- input$bin_loc # Base directory containing FVS binaries.
    p_cores      <- input$num_cores_rfvs # Requested parallel worker count for simulations.
    if (is.na(p_cores) || p_cores < 1) p_cores <- 1 # Clamp core count to a valid minimum.
    total_jobs   <- nrow(job_queue) # Total stand runs to dispatch.
    p_is_test    <- isTRUE(input$test_run_mode) # Snapshot test mode before launching the background process.
    p_cores      <- resolve_worker_count(total_jobs, p_cores, workload = "heavy") # Use every useful configured worker for expensive full or sampled rFVS tasks.
    
    if (!("VARIANT" %in% names(job_queue))) { # Each run requires variant-specific binary loading.
      shinyjs::enable("run_rfvs") # Restore launch control because run is blocked.
      shinyjs::hide("kill_run_btn_wrap") # Keep cancel hidden when no process exists.
      step_start_run(NULL) # Reset elapsed timer state.
      showNotification("Pipeline Action Denied: VARIANT column not found in database. Cannot auto-detect variant.", type = "error") # Report missing prerequisite column.
      return() # Exit observer early.
    }
    
    p_overwrite <- input$overwrite_scens # User-selected scenarios to force rerun/overwrite.
    if (is.null(p_overwrite)) p_overwrite <- character(0) # Normalize NULL input to empty character vector.
    active_run_total(total_jobs) # Freeze the progress denominator for this background process.
    active_run_is_test(p_is_test) # Freeze the mode label for completion reporting.

    shinyjs::show("kill_run_btn_wrap") # Show cancel button once background process starts.
    
    writeLines("0|Booting up compute cluster (this may take a moment)...", prog_file_run) # Seed progress file consumed by UI poller.
    run_prog <<- shiny::Progress$new(session, min=0, max=1) # Open progress modal for Step 2.
    run_prog$set(message = "Executing : Dispatching Parallel rFVS Runs...", value=0, detail = "Booting up compute cluster (this may take a moment)...") # Initialize visible progress text.
    
    p <- callr::r_bg(function(job_queue, p_cores, p_bin_loc, p_overwrite, prog_file, total_jobs) { # Execute compute-heavy simulation loop in background R process.
      library(parallel) # Provides cluster worker creation.
      library(doSNOW) # Provides foreach backend with progress hooks.
      library(rFVS) # Provides FVS runtime interface.

      # Keep workers in same-variant streaks to reduce repeated fvsLoad() calls.
      job_queue <- job_queue[order(job_queue$VARIANT, job_queue$Scenario, job_queue$STAND_ID), , drop = FALSE]

      writeLines("0|Initializing FVS binaries mapped to worker RAM...", prog_file) # Emit startup status before dispatch.
      cl <- NULL # Hold an optional PSOCK cluster when the queue contains multiple runs.
      if (p_cores > 1L) {
        cl <- makeCluster(p_cores) # Start no more workers than there are executable jobs.
        on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE) # Always tear down workers on exit.
        registerDoSNOW(cl) # Route foreach iterations to active cluster.

        clusterExport(cl, c("p_bin_loc", "p_overwrite"), envir = environment()) # Share binary path and overwrite preferences with workers.
        clusterEvalQ(cl, { # Initialize worker-local state once per worker process.
          library(rFVS) # Load rFVS inside each worker.
          .fvs_worker_state <- new.env(parent = emptyenv()) # Create lightweight worker cache.
          .fvs_worker_state$active_variant <- NA_character_ # Track currently loaded variant per worker.
          NULL # Return nothing from initialization expression.
        })
      } else {
        foreach::registerDoSEQ() # Run a one-job test directly in the background process without PSOCK startup.
        .fvs_worker_state <- new.env(parent = emptyenv()) # Initialize the same variant cache used by cluster workers.
        .fvs_worker_state$active_variant <- NA_character_
        writeLines("0|Running the single simulation without cluster startup...", prog_file)
      }

      last_update <- Sys.time() # Throttle progress-file writes.
      progress_callback_rfvs <- function(n) { # Called by doSNOW as iterations complete.
        if (as.numeric(difftime(Sys.time(), last_update, units = "secs")) > 0.5 || n == total_jobs) { # Update about twice per second or on final iteration.
          writeLines(sprintf("%d|Processed %d of %d simulation binaries via worker clusters...", n, n, total_jobs), prog_file) # Persist progress for UI polling observer.
          last_update <<- Sys.time() # Reset throttle timestamp.
        }
      }

      conns_before <- as.integer(rownames(showConnections(all = FALSE))) # Snapshot open R connections for cleanup bookkeeping.

            run_results <- foreach(i = seq_len(total_jobs), # Parallel outer loop: one iteration per stand run directory.
              .packages = c("rFVS"), # Ensure worker has required package namespace.
              .options.snow = if (p_cores > 1L) list(progress = progress_callback_rfvs) else NULL) %dopar% { # Attach progress only when the snow backend is active.

                stand_dir_path <- job_queue$stand_dir[i] # Stand-specific working directory containing `run.key`.
                variant_i <- job_queue$VARIANT[i] # Variant program token for rFVS::fvsLoad.
                scenario_i <- job_queue$Scenario[i] # Scenario label used for overwrite filtering.

                if (!dir.exists(stand_dir_path)) { # Preserve a visible error record even when Step 1 never created this stand directory.
                  dir.create(stand_dir_path, recursive = TRUE, showWarnings = FALSE)
                  cat(sprintf("[%s] rFVS Runtime Error: Expected stand run directory and run.key were not created before Step 2.\n", Sys.time()), file = file.path(stand_dir_path, "fvs_runtime_error.log"), append = TRUE)
                  return(FALSE)
                }
                runtime_error_log <- file.path(stand_dir_path, "fvs_runtime_error.log") # Use one current-attempt error log per stand run.
                clear_runtime_error_log <- function() { # Remove stale/current error state whenever this stand begins or finishes successfully.
                  if (file.exists(runtime_error_log)) unlink(runtime_error_log, force = TRUE)
                  if (file.exists(runtime_error_log)) try(file.remove(runtime_error_log), silent = TRUE) # Retry through the alternate file-removal API on Windows.
                  !file.exists(runtime_error_log)
                }
                clear_runtime_error_log() # Clear errors from a prior attempt before validating or retrying this queued run.
                if (!file.exists(file.path(stand_dir_path, "run.key"))) { # Fail clearly before invoking rFVS when the expected keyfile is absent.
                  cat(sprintf("[%s] rFVS Runtime Error: Expected run.key was not found in %s.\n", Sys.time(), stand_dir_path), file = runtime_error_log, append = TRUE)
                  return(FALSE)
                }

                # Each worker reloads rFVS only when variant changes on that worker.
                if (!exists(".fvs_worker_state", envir = .GlobalEnv, inherits = FALSE)) { # Defensive fallback if worker state was not initialized.
                  assign(".fvs_worker_state", new.env(parent = emptyenv()), envir = .GlobalEnv) # Recreate worker cache.
                  .GlobalEnv$.fvs_worker_state$active_variant <- NA_character_ # Reset cached active variant.
                }
                if (!identical(.GlobalEnv$.fvs_worker_state$active_variant, variant_i)) { # Reload binary only when worker switches variant.
                  variant_load_error <- tryCatch({
                    rFVS::fvsLoad(bin = p_bin_loc, fvsProgram = variant_i) # Map target variant binary into worker process.
                    NULL
                  }, error = function(e) e)
                  if (inherits(variant_load_error, "error")) {
                    cat(sprintf("[%s] rFVS Variant Load Error (%s): %s\n", Sys.time(), variant_i, variant_load_error$message), file = runtime_error_log, append = TRUE)
                    return(FALSE) # Isolate a bad variant/load failure to this task instead of terminating the entire queue.
                  }
                  .GlobalEnv$.fvs_worker_state$active_variant <- variant_i # Cache loaded variant to avoid redundant fvsLoad.
                }

                is_overwrite <- ("ALL" %in% p_overwrite) || (scenario_i %in% p_overwrite) # Determine whether existing outputs should be replaced.

                db_files <- list.files(stand_dir_path, pattern = "\\.db$", full.names = FALSE, ignore.case = TRUE) # Inspect existing DB outputs in stand directory.

                if (!is_overwrite) { # Reuse mode: do not rerun when a valid single DB already exists.
                  if (length(db_files) > 1) { # Multiple DBs indicate ambiguous prior output state.
                    cat(sprintf("[%s] rFVS Runtime Error: Found multiple .db files in %s: %s\n", Sys.time(), stand_dir_path, paste(db_files, collapse = ", ")), file = runtime_error_log, append = TRUE)
                    return(FALSE)
                  }
                  if (length(db_files) == 1) { # Skip run because output already exists and overwrite is not requested.
                    clear_runtime_error_log() # Existing valid output is a successful result, so no stale failure log should remain.
                    return(TRUE)
                  }
                } else { # Overwrite mode: remove prior DB/OUT artifacts before executing.
                  files_to_delete <- list.files(stand_dir_path, pattern = "\\.(db|out)$", full.names = TRUE, ignore.case = TRUE) # Gather replaceable output files.
                  if (length(files_to_delete) > 0) { # Delete stale outputs when present.
                    unlink(files_to_delete)
                  }
                }

                orig_wd <- getwd() # Preserve worker cwd so we can restore it safely.
                setwd(stand_dir_path) # Run rFVS from stand directory so relative files resolve.

                tryCatch({ # Run FVS simulation and validate resulting output state.
                  rFVS::fvsSetCmdLine(cl = "--keywordfile=run.key") # Point runtime to generated keyfile.
                  rFVS::fvsRun() # Execute FVS simulation.

                  post_db_files <- list.files(stand_dir_path, pattern = "\\.db$", full.names = FALSE, ignore.case = TRUE) # Re-check DB outputs after run.
                  if (length(post_db_files) == 0) { # A simulation is not successful when it produced no database for Step 3.
                    cat(sprintf("[%s] rFVS Runtime Error: No .db output was created after fvsRun completed.\n", Sys.time()), file = runtime_error_log, append = TRUE)
                    return(FALSE)
                  }
                  if (length(post_db_files) > 1) { # Treat multiple post-run DBs as failure.
                    cat(sprintf("[%s] rFVS Runtime Error: Multiple .db files detected after run in %s: %s\n", Sys.time(), stand_dir_path, paste(post_db_files, collapse = ", ")), file = runtime_error_log, append = TRUE)
                    return(FALSE)
                  }
                  clear_runtime_error_log() # Remove any stale/current error file before recording a successful completion.
                  return(TRUE) # Mark iteration success.
                }, error = function(e) { # Capture simulation errors to per-stand log.
                  cat(sprintf("[%s] rFVS Runtime Error: %s\n", Sys.time(), e$message), file = runtime_error_log, append = TRUE)
                  return(FALSE) # Mark iteration failure.
                }, finally = { # Always restore original working directory.
                  setwd(orig_wd)
                })
              }

      conns_after <- as.integer(rownames(showConnections(all = FALSE))) # Snapshot open connections after parallel work.
      for (cn in setdiff(conns_after, conns_before)) try(close(getConnection(cn)), silent = TRUE) # Close newly opened connections to prevent descriptor leaks.
      successful_runs <- sum(vapply(run_results, isTRUE, logical(1))) # Count only tasks that actually completed or validly reused output.
      return(list(total = total_jobs, successful = successful_runs, failed = total_jobs - successful_runs)) # Return explicit outcome counts to the parent process.
    }, args = list(job_queue, p_cores, p_bin_loc, p_overwrite, prog_file_run, total_jobs), supervise = TRUE) # Launch supervised background process with immutable arguments.
    
    bg_run(p) # Store process handle for progress polling and cancellation observers.
  })
  
  observe({
    p <- bg_run()
    req(p)
    invalidateLater(500, session)
    
    if (p$is_alive()) {
      shinyjs::show("kill_run_btn_wrap")
      if (file.exists(prog_file_run)) {
        l <- suppressWarnings(readLines(prog_file_run))
        if (length(l) > 0) {
          parts <- strsplit(l[length(l)], "\\|")[[1]]
          if (length(parts) >= 2 && !is.null(run_prog)) {
            total_jobs <- isolate(active_run_total())
            val <- if (!is.null(total_jobs) && total_jobs > 0) as.numeric(parts[1]) / total_jobs else 0
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
      shinyjs::hide("kill_run_btn_wrap")
      updateActionButton(session, "kill_run_btn", label = "Cancel", icon = icon("xmark"))
      
      if (p$get_exit_status() == 0) {
        run_result <- p$get_result()
        completion_title <- if (isTRUE(active_run_is_test())) "Step 2 Test Complete: Sample Simulations Ran" else "Step 2 Complete: Simulations Ran"
        if (run_result$failed > 0L) {
          showModal(modalDialog(
            title = paste0(completion_title, " (With Errors)"),
            p(sprintf("%d of %d simulations succeeded; %d failed. Review stand-level fvs_runtime_error.log files for details.", run_result$successful, run_result$total, run_result$failed)),
            p(sprintf("Elapsed Time: %s", elapsed_run)),
            easyClose = TRUE, footer = modalButton("Dismiss")
          ))
        } else {
          showModal(modalDialog(
            title = completion_title,
            p(sprintf("Successfully processed %d simulation binaries.", run_result$successful)),
            p(sprintf("Elapsed Time: %s", elapsed_run)),
            easyClose = TRUE, footer = modalButton("Dismiss")
          ))
        }
      } else {
        err <- p$read_error_lines()
        showModal(modalDialog(
          title = "rFVS Execution Failure",
          p(paste(err, collapse = "\n")), easyClose = TRUE, footer = modalButton("Dismiss")
        ))
      }
      active_run_total(NULL)
      active_run_is_test(FALSE)
    }
  })
  
  observeEvent(input$kill_run_btn, priority=110, {
    p <- bg_run()
    if (!is.null(p) && isTRUE(p$is_alive())) {
      terminate_bg_process(p)
      bg_run(NULL)
      if (!is.null(run_prog)) run_prog$close()
      run_prog <<- NULL
      step_start_run(NULL)
      active_run_total(NULL)
      active_run_is_test(FALSE)
      shinyjs::enable("run_rfvs")
      shinyjs::hide("kill_run_btn_wrap")
      updateActionButton(session, "kill_run_btn", label = "Cancel", icon = icon("xmark"))
      showNotification("rFVS execution cancelled by user.", type = "warning", duration = 5)
    }
  })
  
  # ----------------- PIPELINE STEP 3: CONSOLIDATE MASTER OUTPUTS -----------------
  observeEvent(input$merge_outputs, { # Start Step 3: merge stand/scenario DB outputs into consolidated project DBs.
    shinyjs::disable("merge_outputs") # Prevent duplicate merge launches while process is active.
    step_start_merge(Sys.time()) # Record merge start time for elapsed reporting.

    if (!isTRUE(manifest_ready())) {
      shinyjs::enable("merge_outputs")
      step_start_merge(NULL)
      showNotification("Run 'Save KCP Scenarios & Build Manifest' for the current KCP lookup table before merging outputs.", type = "warning", duration = 8)
      return()
    }
    
    job_queue <- get_job_queue() # Resolve stand/scenario outputs expected for merge.
    if (is.null(job_queue) || nrow(job_queue) == 0) { # Block merge when queue is empty.
      shinyjs::enable("merge_outputs") # Restore launch button because nothing started.
      shinyjs::hide("kill_merge_btn_wrap") # Keep cancel control hidden.
      step_start_merge(NULL) # Clear timer because merge did not begin.
      showNotification("Pipeline Action Denied: No execution targets map to the system queue.", type = "error") # Explain blocked condition.
      return() # Exit observer early.
    }
    
    output_base_dir <- file.path(input$root_dir, "Outputs") # Root directory for merged output databases.
    if (!dir.exists(output_base_dir)) dir.create(output_base_dir, recursive = TRUE, showWarnings = FALSE) # Ensure output root exists.
    
    unique_combos <- unique(job_queue[, c("GROUP_CODE", "Scenario")]) # Identify unique Group/Scenario databases to build in phase 1.
    total_combos  <- nrow(unique_combos) # Total independent scenario-merge tasks.
    p_cores      <- input$num_cores_merge # Requested worker count for merge phase.
    if (is.na(p_cores) || p_cores < 1) p_cores <- 1 # Clamp core count to valid minimum.

    shinyjs::show("kill_merge_btn_wrap") # Show cancel control once merge process launches.
    
    writeLines("0|Booting up compute cluster (this may take a moment)...", prog_file_merge) # Seed merge progress file for UI polling.
    merge_prog <<- shiny::Progress$new(session, min=0, max=1) # Open progress modal for merge stage.
    merge_prog$set(message = "Executing : Consolidating Master Outputs...", value=0, detail = "Booting up compute cluster (this may take a moment)...") # Initialize progress message/details.
    
    p <- callr::r_bg(function(job_queue, p_cores, output_base_dir, unique_combos, total_combos, prog_file) { # Run merge pipeline in background process to keep UI responsive.
      library(parallel) # Worker-cluster utilities.
      library(doSNOW) # Foreach backend with progress callback support.
      library(foreach) # Parallel iteration API.
      library(RSQLite) # SQLite read/attach/merge operations.

      qid <- function(x) paste0('"', gsub('"', '""', x), '"') # Quote SQL identifiers defensively.
      sqlite_affinity <- function(declared_type) { # Reduce source declarations to safe SQLite affinities for added columns.
        declared_type <- toupper(ifelse(is.na(declared_type), "", declared_type))
        if (grepl("INT", declared_type)) return("INTEGER")
        if (grepl("CHAR|CLOB|TEXT", declared_type)) return("TEXT")
        if (grepl("REAL|FLOA|DOUB", declared_type)) return("REAL")
        if (!nzchar(declared_type) || grepl("BLOB", declared_type)) return("BLOB")
        "NUMERIC"
      }
      append_attached_table <- function(con, source_schema, table_name) { # Union schemas and append source rows by column name in either merge phase.
        table_q <- qid(table_name)
        source_info <- dbGetQuery(con, sprintf("PRAGMA %s.table_info(%s)", source_schema, table_q))
        if (nrow(source_info) == 0) return(invisible(NULL))

        destination_exists <- toupper(table_name) %in% toupper(dbListTables(con))
        if (!destination_exists) {
          dbExecute(con, sprintf("CREATE TABLE %s AS SELECT * FROM %s.%s", table_q, source_schema, table_q))
          return(invisible(NULL))
        }

        destination_info <- dbGetQuery(con, sprintf("PRAGMA main.table_info(%s)", table_q))
        destination_keys <- toupper(destination_info$name)
        new_source_rows <- source_info[!(toupper(source_info$name) %in% destination_keys), , drop = FALSE]
        if (nrow(new_source_rows) > 0) {
          for (column_idx in seq_len(nrow(new_source_rows))) {
            column_name <- new_source_rows$name[column_idx]
            column_type <- sqlite_affinity(new_source_rows$type[column_idx])
            dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s %s", table_q, qid(column_name), column_type))
          }
          destination_info <- dbGetQuery(con, sprintf("PRAGMA main.table_info(%s)", table_q))
        }

        source_match <- match(toupper(destination_info$name), toupper(source_info$name))
        select_expressions <- vapply(seq_len(nrow(destination_info)), function(column_idx) {
          if (is.na(source_match[column_idx])) {
            sprintf("NULL AS %s", qid(destination_info$name[column_idx]))
          } else {
            qid(source_info$name[source_match[column_idx]])
          }
        }, character(1))
        destination_columns <- paste(vapply(destination_info$name, qid, character(1)), collapse = ", ")
        dbExecute(
          con,
          sprintf(
            "INSERT INTO %s (%s) SELECT %s FROM %s.%s",
            table_q,
            destination_columns,
            paste(select_expressions, collapse = ", "),
            source_schema,
            table_q
          )
        )
        invisible(NULL)
      }
      total_errors <- 0 # Aggregate failures across both merge phases.

      # Phase 1: merge each Group/Scenario independently in parallel.
      worker_cores <- max(1, min(as.integer(p_cores), as.integer(total_combos))) # Avoid provisioning more workers than combo tasks.
      cl <- makeCluster(worker_cores) # Create cluster for phase-1 Group/Scenario merges.
      on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE) # Ensure workers are shut down on exit.
      registerDoSNOW(cl) # Bind foreach to active worker cluster.

      last_update <- Sys.time() # Track timestamp for throttled progress writes.
      progress_callback <- function(n) { # Called as each phase-1 combo merge completes.
        if (as.numeric(difftime(Sys.time(), last_update, units = "secs")) > 0.5 || n == total_combos) { # Emit updates every ~0.5s or at completion.
          pct <- (n / total_combos) * 0.85 # Reserve first 85% of progress bar for phase 1.
          writeLines(sprintf("%f|Merged %d of %d Group/Scenario databases...", pct, n, total_combos), prog_file) # Persist progress marker for UI polling observer.
          last_update <<- Sys.time() # Reset throttle timestamp after write.
        }
      }

      phase1_results <- foreach( # Parallel outer loop: each iteration merges one Group/Scenario stack.
        combo_idx = seq_len(total_combos), # Iterate all unique Group/Scenario combinations.
        .packages = c("RSQLite"), # Ensure SQLite APIs are available in workers.
        .export = c("qid", "sqlite_affinity", "append_attached_table"), # Share the same schema-union implementation used later by phase 2.
        .options.snow = list(progress = progress_callback) # Wire completion callback for UI progress.
      ) %dopar% {
        grp  <- unique_combos$GROUP_CODE[combo_idx] # Current group code for this combo task.
        scen <- unique_combos$Scenario[combo_idx] # Current scenario label for this combo task.

        grp_out_dir <- file.path(output_base_dir, grp, scen) # Scenario-specific output directory.
        dir.create(grp_out_dir, recursive = TRUE, showWarnings = FALSE) # Ensure scenario directory exists.
        scen_out_db <- file.path(grp_out_dir, sprintf("FVSOut_%s.db", scen)) # Target merged DB for this combo.
        if (file.exists(scen_out_db)) unlink(scen_out_db) # Remove prior merged DB so rebuild is clean.

        combo_jobs <- subset(job_queue, GROUP_CODE == grp & Scenario == scen) # Filter stand rows belonging to this combo.
        if (nrow(combo_jobs) == 0) { # Defensive guard when combo has no matching stands.
          return(list(errors = 0L, scen_db = NA_character_)) # Return empty-success payload.
        }

        local_errors <- 0L # Count failures within this combo merge task.
        log_file <- file.path(grp_out_dir, "merge_errors.log") # Per-combo merge error log file.
        m_con <- dbConnect(SQLite(), scen_out_db) # Open destination DB connection for this combo.

        tryCatch({ # Merge all stand DBs for this combo into one scenario DB.
          dbExecute(m_con, "PRAGMA synchronous = OFF") # Speed up bulk inserts (less durable until completion).
          dbExecute(m_con, "PRAGMA journal_mode = MEMORY") # Keep journal in memory for throughput.
          dbExecute(m_con, "PRAGMA temp_store = MEMORY") # Keep temp data in memory.
          dbExecute(m_con, "PRAGMA cache_size = -200000") # Expand page cache for larger streaming merges.

          created_tables <- dbListTables(m_con) # Track tables already created in destination DB.

          for (j in seq_len(nrow(combo_jobs))) { # Serial inner loop: append each stand DB into combo DB.
            sid <- combo_jobs$STAND_ID[j] # Stand identifier for logging context.
            s_dir <- combo_jobs$stand_dir[j] # Directory expected to contain stand output DB.

            db_files <- list.files(s_dir, pattern = "\\.db$", full.names = FALSE, ignore.case = TRUE) # Discover produced DB files for this stand.
            if (length(db_files) == 0) { # Missing output must be reported so an incomplete merge cannot appear successful.
              local_errors <- local_errors + 1L
              cat(sprintf("[%s] Stand %s Error: No .db output found in %s.\n", Sys.time(), sid, s_dir), file = log_file, append = TRUE)
              next
            }
            if (length(db_files) > 1) { # Multiple DB files are ambiguous; log and skip.
              local_errors <- local_errors + 1L # Increment combo-local error count.
              cat(sprintf("[%s] Stand %s Error: Found multiple .db files in %s: %s.\n", Sys.time(), sid, s_dir, paste(db_files, collapse = ", ")), file = log_file, append = TRUE)
              next
            }

            stand_db <- file.path(s_dir, db_files[1]) # Resolve full path to stand DB file.

            tryCatch({ # Attach stand DB and stream its tables into combo destination DB.
              safe_stand_db_path <- normalizePath(stand_db, winslash = "/", mustWork = TRUE) # Validate and normalize source DB path.
              try(dbExecute(m_con, "DETACH DATABASE srcdb"), silent = TRUE) # Clean stale attachment from prior iterations.
              safe_stand_db_sql <- gsub("'", "''", safe_stand_db_path, fixed = TRUE) # Escape apostrophes for SQLite string-literal safety.
              dbExecute(m_con, sprintf("ATTACH DATABASE '%s' AS srcdb", safe_stand_db_sql)) # Attach stand DB as `srcdb` alias.

              dbBegin(m_con) # Use transaction for atomic per-stand append.
              tables_in_src <- dbGetQuery(m_con, "SELECT name FROM srcdb.sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%'")$name # Enumerate user tables from stand DB.
              for (tbl_name in tables_in_src) { # Serial table loop: union schemas and append rows by matching column names.
                append_attached_table(m_con, "srcdb", tbl_name)
                if (!(tbl_name %in% created_tables)) created_tables <- c(created_tables, tbl_name)
              }
              dbCommit(m_con) # Commit successful per-stand append.
              dbExecute(m_con, "DETACH DATABASE srcdb") # Detach source DB before next stand.
            }, error = function(e) { # Roll back and log stand-level merge failures.
              try(dbRollback(m_con), silent = TRUE) # Undo partial writes for this stand.
              try(dbExecute(m_con, "DETACH DATABASE srcdb"), silent = TRUE) # Best-effort cleanup of attachment.
              local_errors <<- local_errors + 1L # Increment error count for this combo.
              cat(sprintf("[%s] Stand %s Error: %s\n", Sys.time(), sid, e$message), file = log_file, append = TRUE)
            })
          }
        }, finally = { # Always close destination connection for this combo.
          try(dbDisconnect(m_con), silent = TRUE)
        })

        list(errors = local_errors, scen_db = if (file.exists(scen_out_db)) scen_out_db else NA_character_) # Return combo error count and produced scenario DB path.
      }

      total_errors <- total_errors + sum(vapply(phase1_results, function(x) x$errors, integer(1)), na.rm = TRUE) # Aggregate phase-1 failures across all combos.
      scen_db_paths <- unique(vapply(phase1_results, function(x) x$scen_db, character(1))) # Collect produced scenario DB paths.
      scen_db_paths <- scen_db_paths[!is.na(scen_db_paths) & nzchar(scen_db_paths)] # Keep only valid non-empty paths.

      # Phase 2: consolidate scenario DBs into master with SQLite streaming merges.
      writeLines(sprintf("%f|Creating single master output database...", 0.9), prog_file) # Enter phase 2 (remaining 10% of progress bar).
      mega_db_name <- sprintf("FVS_Out_%s.db", format(Sys.time(), "%Y%m%d_%H%M%S")) # Timestamped final consolidated DB name.
      mega_db_path <- file.path(output_base_dir, mega_db_name) # Full destination path for project-level merged DB.

      if (length(scen_db_paths) > 0) { # Only build master DB when phase-1 scenario DBs exist.
        all_rFVS_sims <- dbConnect(SQLite(), mega_db_path) # Open project-level destination DB.
        on.exit(try(dbDisconnect(all_rFVS_sims), silent = TRUE), add = TRUE) # Ensure project DB connection closes on exit.
        dbExecute(all_rFVS_sims, "PRAGMA synchronous = OFF") # Optimize bulk merge writes.
        dbExecute(all_rFVS_sims, "PRAGMA journal_mode = MEMORY") # Keep journal in memory for speed.
        dbExecute(all_rFVS_sims, "PRAGMA temp_store = MEMORY") # Keep temp data in memory.
        dbExecute(all_rFVS_sims, "PRAGMA cache_size = -200000") # Increase cache for streaming inserts.
        mega_created_tables <- character(0) # Track tables created in project-level destination DB.

        for (k in seq_along(scen_db_paths)) { # Serial loop: append each scenario DB into one project master DB.
          scen_db_path <- scen_db_paths[k] # Current scenario DB path to attach.
          tryCatch({ # Attach scenario DB and stream its tables into project DB.
            safe_path <- normalizePath(scen_db_path, winslash = "/", mustWork = TRUE) # Validate/normalize source DB path.
            try(dbExecute(all_rFVS_sims, "DETACH DATABASE sDB"), silent = TRUE) # Clear stale attachment alias.
            safe_path_sql <- gsub("'", "''", safe_path, fixed = TRUE) # Escape apostrophes for SQLite string-literal safety.
            dbExecute(all_rFVS_sims, sprintf("ATTACH DATABASE '%s' AS sDB", safe_path_sql)) # Attach source scenario DB.
            dbBegin(all_rFVS_sims) # Wrap each scenario append in a transaction.
            tabs <- dbGetQuery(all_rFVS_sims, "SELECT name FROM sDB.sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%'")$name # Enumerate source user tables.

            for (tbl in tabs) { # Serial table loop: union schemas and append rows by matching column names.
              append_attached_table(all_rFVS_sims, "sDB", tbl)
              if (!(tbl %in% mega_created_tables)) mega_created_tables <- c(mega_created_tables, tbl)
            }
            dbCommit(all_rFVS_sims) # Commit successful scenario append.
            dbExecute(all_rFVS_sims, "DETACH DATABASE sDB") # Detach source before next iteration.
          }, error = function(e) { # Roll back and log any scenario-level merge failure.
            try(dbRollback(all_rFVS_sims), silent = TRUE) # Undo partial writes for current scenario append.
            try(dbExecute(all_rFVS_sims, "DETACH DATABASE sDB"), silent = TRUE) # Best-effort detach cleanup.
            total_errors <<- total_errors + 1 # Increment global error counter.
            cat(sprintf("[%s] Full Project Merge Error for %s: %s\n", Sys.time(), scen_db_path, e$message), file = file.path(output_base_dir, "full_project_merge_errors.log"), append = TRUE)
          })

          if (k %% 5 == 0 || k == length(scen_db_paths)) { # Emit periodic phase-2 progress updates.
            pct <- 0.9 + (k / length(scen_db_paths)) * 0.09 # Map phase-2 iterations into 90%-99% progress range.
            writeLines(sprintf("%f|Building project master DB: %d of %d scenario DBs merged...", pct, k, length(scen_db_paths)), prog_file) # Persist progress for UI poller.
          }
        }
      }

      writeLines("1|Merge complete.", prog_file) # Signal completion to polling observer.
      return(total_errors) # Return aggregate error count to parent process.
    }, args = list(job_queue, p_cores, output_base_dir, unique_combos, total_combos, prog_file_merge), supervise = TRUE) # Launch supervised background merge process.
    
    bg_merge(p) # Store process handle for polling/cancel/completion observers.
  })
  
  observe({
    p <- bg_merge()
    req(p)
    invalidateLater(500, session)
    
    if (p$is_alive()) {
      shinyjs::show("kill_merge_btn_wrap")
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
      shinyjs::hide("kill_merge_btn_wrap")
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
    if (!is.null(p) && isTRUE(p$is_alive())) {
      terminate_bg_process(p)
      bg_merge(NULL)
      if (!is.null(merge_prog)) merge_prog$close()
      merge_prog <<- NULL
      step_start_merge(NULL)
      shinyjs::enable("merge_outputs")
      shinyjs::hide("kill_merge_btn_wrap")
      updateActionButton(session, "kill_merge_btn", label = "Cancel", icon = icon("xmark"))
      showNotification("Database consolidation cancelled by user.", type = "warning", duration = 5)
    }
  })
}

