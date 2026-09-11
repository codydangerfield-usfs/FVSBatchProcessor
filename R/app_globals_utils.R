
# 1. GLOBAL SETTINGS & UTILITIES
# ------------------------------------------------------------------------------
# Robust native OS folder picker wrapper
#
# Purpose:
#   Opens a native directory picker for the host OS and returns a normalized
#   path string for downstream file operations.
#
# Inputs:
#   default_path : initial folder shown when dialog opens.
#   caption_text : title/prompt text shown in the picker.
#
# Returns:
#   Normalized folder path (with forward slashes) or NULL when canceled/failing.
#
# Platform behavior:
#   Windows : PowerShell + embedded C# COM IFileOpenDialog.
#   macOS   : AppleScript choose folder via osascript.
#   Linux   : zenity directory picker, then tcltk fallback.
get_native_folder <- function(default_path = getwd(), caption_text = "Select a Directory") {
  os <- Sys.info()[["sysname"]]
  path <- NULL

  default_path <- path.expand(default_path)

  if (os == "Windows") {
    win_path <- gsub("/", "\\\\", default_path)

    ps_script <- paste0(
      '$code = @"\n',
      'using System;\n',
      'using System.Runtime.InteropServices;\n',
      'public class NativeFolderPicker {\n',
      '    [DllImport("user32.dll")]\n',
      '    private static extern IntPtr GetForegroundWindow();\n',
      '    [DllImport("shell32.dll", CharSet = CharSet.Unicode, PreserveSig = false)]\n',
      '    private static extern void SHCreateItemFromParsingName(\n',
      '        [MarshalAs(UnmanagedType.LPWStr)] string pszPath,\n',
      '        IntPtr pbc,\n',
      '        ref Guid riid,\n',
      '        [MarshalAs(UnmanagedType.Interface)] out IShellItem ppv);\n',
      '    [ComImport, Guid("42f85136-db7e-439c-85f1-e4075d135fc8"), InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]\n',
      '    private interface IFileOpenDialog {\n',
      '        [PreserveSig] int Show(IntPtr parent);\n',
      '        void SetFileTypes(uint cFileTypes, IntPtr rgFilterSpec);\n',
      '        void SetFileTypeIndex(uint iFileType);\n',
      '        void GetFileTypeIndex(out uint piFileType);\n',
      '        void Advise(IntPtr pfde, out uint pdwCookie);\n',
      '        void Unadvise(uint dwCookie);\n',
      '        void SetOptions(uint dwFlags);\n',
      '        void GetOptions(out uint pdwFlags);\n',
      '        void SetDefaultFolder(IShellItem psi);\n',
      '        void SetFolder(IShellItem psi);\n',
      '        void GetFolder(out IShellItem ppsi);\n',
      '        void GetCurrentSelection(out IShellItem ppsi);\n',
      '        void SetFileName([MarshalAs(UnmanagedType.LPWStr)] string pszName);\n',
      '        void GetFileName([MarshalAs(UnmanagedType.LPWStr)] out string pszName);\n',
      '        void SetTitle([MarshalAs(UnmanagedType.LPWStr)] string pszTitle);\n',
      '        void SetOkButtonLabel([MarshalAs(UnmanagedType.LPWStr)] string pszText);\n',
      '        void SetFileNameLabel([MarshalAs(UnmanagedType.LPWStr)] string pszLabel);\n',
      '        void GetResult(out IShellItem ppsi);\n',
      '        void AddPlace(IShellItem psi, int fdap);\n',
      '        void SetDefaultExtension([MarshalAs(UnmanagedType.LPWStr)] string pszDefaultExtension);\n',
      '        void Close(int hr);\n',
      '        void SetClientGuid(ref Guid guid);\n',
      '        void ClearClientData();\n',
      '        void SetFilter(IntPtr pFilter);\n',
      '        void GetResults(out IntPtr ppenum);\n',
      '        void GetSelectedItems(out IntPtr ppsai);\n',
      '    }\n',
      '    [ComImport, Guid("43826d1e-e718-42ee-bc55-a1e261c37bfe"), InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]\n',
      '    private interface IShellItem {\n',
      '        void BindToHandler(IntPtr pbc, ref Guid bhid, ref Guid riid, out IntPtr ppv);\n',
      '        void GetParent(out IShellItem ppsi);\n',
      '        void GetDisplayName(uint sigdnName, [MarshalAs(UnmanagedType.LPWStr)] out string ppszName);\n',
      '        void GetAttributes(uint sfgaoMask, out uint psfgaoAttribs);\n',
      '        void Compare(IShellItem psi, uint hint, out int piOrder);\n',
      '    }\n',
      '    public static string Show(string initialPath, string title) {\n',
      '        try {\n',
      '            var dialog = (IFileOpenDialog)Activator.CreateInstance(Type.GetTypeFromCLSID(new Guid("DC1C5A9C-E88A-4dde-A5A1-60F82A20AEF7")));\n',
      '            uint options;\n',
      '            dialog.GetOptions(out options);\n',
      '            dialog.SetOptions(options | 0x00000020);\n',
      '            if (!string.IsNullOrEmpty(title)) dialog.SetTitle(title);\n',
      '            if (!string.IsNullOrEmpty(initialPath) && System.IO.Directory.Exists(initialPath)) {\n',
      '                try {\n',
      '                    Guid riid = new Guid("43826d1e-e718-42ee-bc55-a1e261c37bfe");\n',
      '                    IShellItem item;\n',
      '                    SHCreateItemFromParsingName(initialPath, IntPtr.Zero, ref riid, out item);\n',
      '                    if (item != null) dialog.SetFolder(item);\n',
      '                } catch {}\n',
      '            }\n',
      '            IntPtr owner = GetForegroundWindow();\n',
      '            if (dialog.Show(owner) == 0) {\n',
      '                IShellItem item;\n',
      '                dialog.GetResult(out item);\n',
      '                string pickedPath;\n',
      '                item.GetDisplayName(0x80058000, out pickedPath);\n',
      '                return pickedPath;\n',
      '            }\n',
      '        } catch {}\n',
      '        return null;\n',
      '    }\n',
      '}\n',
      '"@\n',
      'Add-Type -TypeDefinition $code\n',
      '$res = [NativeFolderPicker]::Show("', gsub('"', '`"', win_path), '", "', gsub('"', '`"', caption_text), '")\n',
      'if ($res) { Write-Output $res }\n'
    )

    tf <- tempfile(fileext = ".ps1")
    writeLines(ps_script, tf)

    res <- tryCatch(
      system2(
        "powershell",
        args = c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", shQuote(tf)),
        stdout = TRUE,
        stderr = FALSE
      ),
      error = function(e) NULL
    )

    unlink(tf)
    if (length(res) > 0 && nzchar(res[1])) path <- res[1]
  } else if (os == "Darwin") {
    script <- sprintf('
      try
        tell application (path to frontmost application as text)
          activate
          set myFolder to choose folder with prompt "%s" default location POSIX file "%s"
        end tell
        POSIX path of myFolder
      end try
    ', caption_text, default_path)

    res <- tryCatch(
      system2("osascript", args = c("-e", shQuote(script)), stdout = TRUE, stderr = FALSE),
      error = function(e) NULL
    )
    if (length(res) > 0 && !grepl("user canceled", res[1], ignore.case = TRUE)) path <- res[1]
  } else {
    if (nzchar(Sys.which("zenity"))) {
      res <- tryCatch(
        system2(
          "zenity",
          args = c(
            "--file-selection", "--directory", "--modal",
            sprintf('--title="%s"', caption_text),
            sprintf('--filename="%s/"', default_path)
          ),
          stdout = TRUE,
          stderr = FALSE
        ),
        error = function(e) NULL
      )
      if (length(res) > 0 && nzchar(res[1])) path <- res[1]
    } else if (requireNamespace("tcltk", quietly = TRUE)) {
      path <- tryCatch(tcltk::tk_chooseDir(default = default_path, caption = caption_text), error = function(e) NULL)
    }
  }

  if (is.null(path) || length(path) == 0 || path == "" || is.na(path)) {
    return(NULL)
  }

  normalizePath(as.character(path), winslash = "/", mustWork = FALSE)
}

# Robust native OS file picker wrapper
#
# Purpose:
#   Opens a native file picker for the host OS and returns a normalized
#   path string for downstream file operations.
#
# Inputs:
#   default_path : initial directory shown when dialog opens.
#   caption_text : title/prompt text shown in the picker.
#
# Returns:
#   Normalized file path (with forward slashes) or NULL when canceled/failing.
get_native_file <- function(default_path = getwd(), caption_text = "Select a File") {
  os <- Sys.info()[["sysname"]]
  path <- NULL

  default_path <- path.expand(default_path)

  if (os == "Windows") {
    win_path <- gsub("/", "\\\\", default_path)

    ps_script <- paste0(
      '$code = @"\n',
      'using System;\n',
      'using System.Runtime.InteropServices;\n',
      'public class NativeFilePicker {\n',
      '    [DllImport("user32.dll")]\n',
      '    private static extern IntPtr GetForegroundWindow();\n',
      '    [DllImport("shell32.dll", CharSet = CharSet.Unicode, PreserveSig = false)]\n',
      '    private static extern void SHCreateItemFromParsingName(\n',
      '        [MarshalAs(UnmanagedType.LPWStr)] string pszPath,\n',
      '        IntPtr pbc,\n',
      '        ref Guid riid,\n',
      '        [MarshalAs(UnmanagedType.Interface)] out IShellItem ppv);\n',
      '    [ComImport, Guid("42f85136-db7e-439c-85f1-e4075d135fc8"), InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]\n',
      '    private interface IFileOpenDialog {\n',
      '        [PreserveSig] int Show(IntPtr parent);\n',
      '        void SetFileTypes(uint cFileTypes, IntPtr rgFilterSpec);\n',
      '        void SetFileTypeIndex(uint iFileType);\n',
      '        void GetFileTypeIndex(out uint piFileType);\n',
      '        void Advise(IntPtr pfde, out uint pdwCookie);\n',
      '        void Unadvise(uint dwCookie);\n',
      '        void SetOptions(uint dwFlags);\n',
      '        void GetOptions(out uint pdwFlags);\n',
      '        void SetDefaultFolder(IShellItem psi);\n',
      '        void SetFolder(IShellItem psi);\n',
      '        void GetFolder(out IShellItem ppsi);\n',
      '        void GetCurrentSelection(out IShellItem ppsi);\n',
      '        void SetFileName([MarshalAs(UnmanagedType.LPWStr)] string pszName);\n',
      '        void GetFileName([MarshalAs(UnmanagedType.LPWStr)] out string pszName);\n',
      '        void SetTitle([MarshalAs(UnmanagedType.LPWStr)] string pszTitle);\n',
      '        void SetOkButtonLabel([MarshalAs(UnmanagedType.LPWStr)] string pszText);\n',
      '        void SetFileNameLabel([MarshalAs(UnmanagedType.LPWStr)] string pszLabel);\n',
      '        void GetResult(out IShellItem ppsi);\n',
      '        void AddPlace(IShellItem psi, int fdap);\n',
      '        void SetDefaultExtension([MarshalAs(UnmanagedType.LPWStr)] string pszDefaultExtension);\n',
      '        void Close(int hr);\n',
      '        void SetClientGuid(ref Guid guid);\n',
      '        void ClearClientData();\n',
      '        void SetFilter(IntPtr pFilter);\n',
      '        void GetResults(out IntPtr ppenum);\n',
      '        void GetSelectedItems(out IntPtr ppsai);\n',
      '    }\n',
      '    [ComImport, Guid("43826d1e-e718-42ee-bc55-a1e261c37bfe"), InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]\n',
      '    private interface IShellItem {\n',
      '        void BindToHandler(IntPtr pbc, ref Guid bhid, ref Guid riid, out IntPtr ppv);\n',
      '        void GetParent(out IShellItem ppsi);\n',
      '        void GetDisplayName(uint sigdnName, [MarshalAs(UnmanagedType.LPWStr)] out string ppszName);\n',
      '        void GetAttributes(uint sfgaoMask, out uint psfgaoAttribs);\n',
      '        void Compare(IShellItem psi, uint hint, out int piOrder);\n',
      '    }\n',
      '    public static string Show(string initialPath, string title) {\n',
      '        try {\n',
      '            var dialog = (IFileOpenDialog)Activator.CreateInstance(Type.GetTypeFromCLSID(new Guid("DC1C5A9C-E88A-4dde-A5A1-60F82A20AEF7")));\n',
      '            uint options;\n',
      '            dialog.GetOptions(out options);\n',
      '            dialog.SetOptions(options | 0x00001000); // 0x00001000 = FOS_FILEMUSTEXIST\n',
      '            if (!string.IsNullOrEmpty(title)) dialog.SetTitle(title);\n',
      '            if (!string.IsNullOrEmpty(initialPath)) {\n',
      '                try {\n',
      '                    string dirPath = System.IO.Directory.Exists(initialPath) ? initialPath : System.IO.Path.GetDirectoryName(initialPath);\n',
      '                    if (System.IO.Directory.Exists(dirPath)) {\n',
      '                        Guid riid = new Guid("43826d1e-e718-42ee-bc55-a1e261c37bfe");\n',
      '                        IShellItem item;\n',
      '                        SHCreateItemFromParsingName(dirPath, IntPtr.Zero, ref riid, out item);\n',
      '                        if (item != null) dialog.SetFolder(item);\n',
      '                    }\n',
      '                } catch {}\n',
      '            }\n',
      '            IntPtr owner = GetForegroundWindow();\n',
      '            if (dialog.Show(owner) == 0) {\n',
      '                IShellItem item;\n',
      '                dialog.GetResult(out item);\n',
      '                string pickedPath;\n',
      '                item.GetDisplayName(0x80058000, out pickedPath);\n',
      '                return pickedPath;\n',
      '            }\n',
      '        } catch {}\n',
      '        return null;\n',
      '    }\n',
      '}\n',
      '"@\n',
      'Add-Type -TypeDefinition $code\n',
      '$res = [NativeFilePicker]::Show("', gsub('"', '`"', win_path), '", "', gsub('"', '`"', caption_text), '")\n',
      'if ($res) { Write-Output $res }\n'
    )

    tf <- tempfile(fileext = ".ps1")
    writeLines(ps_script, tf)

    res <- tryCatch(
      system2(
        "powershell",
        args = c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", shQuote(tf)),
        stdout = TRUE,
        stderr = FALSE
      ),
      error = function(e) NULL
    )

    unlink(tf)
    if (length(res) > 0 && nzchar(res[1])) path <- res[1]
  } else if (os == "Darwin") {
    script <- sprintf('
      try
        tell application (path to frontmost application as text)
          activate
          set myFile to choose file with prompt "%s" default location POSIX file "%s"
        end tell
        POSIX path of myFile
      end try
    ', caption_text, default_path)

    res <- tryCatch(
      system2("osascript", args = c("-e", shQuote(script)), stdout = TRUE, stderr = FALSE),
      error = function(e) NULL
    )
    if (length(res) > 0 && !grepl("user canceled", res[1], ignore.case = TRUE)) path <- res[1]
  } else {
    if (nzchar(Sys.which("zenity"))) {
      res <- tryCatch(
        system2(
          "zenity",
          args = c(
            "--file-selection", "--modal",
            sprintf('--title="%s"', caption_text),
            sprintf('--filename="%s/"', default_path)
          ),
          stdout = TRUE,
          stderr = FALSE
        ),
        error = function(e) NULL
      )
      if (length(res) > 0 && nzchar(res[1])) path <- res[1]
    } else if (requireNamespace("tcltk", quietly = TRUE)) {
      path <- tryCatch(tcltk::tk_getOpenFile(initialdir = default_path, title = caption_text), error = function(e) NULL)
    }
  }

  if (is.null(path) || length(path) == 0 || path == "" || is.na(path)) {
    return(NULL)
  }

  normalizePath(as.character(path), winslash = "/", mustWork = FALSE)
}

# Increase maximum upload size to 10GB for very large database/file transfers
options(shiny.maxRequestSize = 10000 * 1024^2)

# Set a visual icon/workflow diagram image name
WorkflowImageFile <- "FVS_BatchProcessing_WorkflowDiagram.png"
WorkflowResourcePrefix <- "workflow_assets"

# Registers the workflow image directory for the Shiny UI.
#
# Purpose:
#   Registers installed static assets (package www directory) under a stable
#   Shiny URL prefix so UI code can reference the workflow diagram.
#
# Returns:
#   TRUE when assets are available and registered; FALSE otherwise.
register_workflow_assets <- function() {
  workflow_img_dir <- system.file("www", package = "FVSBatchProcessor")
  workflow_img_path <- file.path(workflow_img_dir, WorkflowImageFile)

  if (!nzchar(workflow_img_dir) || !file.exists(workflow_img_path)) {
    return(FALSE)
  }

  shiny::addResourcePath(
    WorkflowResourcePrefix,
    normalizePath(workflow_img_dir, winslash = "/", mustWork = TRUE)
  )

  TRUE
}
register_workflow_assets()

# Helper function to remove leading numbers and special characters from a folder name
#
# Purpose:
#   Converts ordered folder labels such as "01_Region" into clean type keys
#   suitable for table columns and data-frame names.
clean_kcp_type <- function(folder_name) {
  cleaned <- sub("^\\d+[_ -]*", "", folder_name)
  make.names(cleaned, unique = TRUE)
}

# Helper function to wrap identifiers in quotes safely for SQL syntax
#
# Purpose:
#   Escapes embedded quotes and surrounds identifiers with double quotes.
#   This avoids SQL issues with reserved words and special characters.
quote_sql_identifier <- function(x) {
  paste0("\"", gsub("\"", "\"\"", x), "\"")
}

# Recursively scans the master KCP directory structure to build a catalog dataframe of all .kcp files
#
# Purpose:
#   Creates a catalog of all first-level KCP type folders and their .kcp files.
#
# Input:
#   master_dir : root folder containing one subfolder per KCP type.
#
# Returns:
#   data.frame containing TypeOrder, KCP_Type, Folder, KCP_Name, KCP_Path;
#   returns NULL when root/types are missing.
#
# Notes:
#   Type folders with no .kcp files are still included with NA name/path rows.
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

# Splits concatenated multiple string KCP entries inside a cell
#
# Purpose:
#   Parses user-entered multi-select KCP cell values using comma/semicolon/pipe
#   delimiters, trims whitespace, and strips file extensions.
#
# Returns:
#   Character vector of KCP names, or character(0) for empty input.
split_kcp_cell <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(x))) return(character(0))
  values <- unlist(strsplit(as.character(x), "\\s*[;,|]\\s*", perl = TRUE))
  values <- values[nzchar(trimws(values))]
  tools::file_path_sans_ext(values)
}

# Utility function that defines Scenario from GROUP_CODE plus either:
# 1) user-selected columns, or 2) default Prescription-like column fallback.
#
# Purpose:
#   Keeps Scenario values synchronized with grouping and selected metadata while
#   preserving deliberate user edits when fields are unchanged.
#
# Inputs:
#   df            : current lookup table.
#   old_df        : prior lookup table snapshot for row-level change detection.
#   scenario_cols : optional columns appended to GROUP_CODE in Scenario labels.
#   force_auto    : when TRUE, regenerate all Scenario values from rules.
#
# Returns:
#   Updated data frame with Scenario column applied.
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
#
# Purpose:
#   Sanitizes exported xlsx files by removing stale drawing relationship XML
#   entries that can break round-trips through spreadsheet tooling.
#
# Input:
#   xlsx_path : workbook path to repair in place.
#
# Side effects:
#   Unzips, mutates XML, and re-zips to the original file path.
strip_missing_drawing_relationships <- function(xlsx_path) {
  tmp_dir <- tempfile("xlsx_clean_"); dir.create(tmp_dir); on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  unzip(xlsx_path, exdir = tmp_dir)
  rel_dir <- file.path(tmp_dir, "xl", "worksheets", "_rels")
  if (dir.exists(rel_dir)) {
    rel_files <- list.files(rel_dir, pattern = "\\.rels$", full.names = TRUE)
    for (rel_file in rel_files) {
      rel_xml <- paste(readLines(rel_file, warn = FALSE), collapse = "")
      rel_xml <- gsub('<Relationship[^>]+Type="[^"]+/drawing"[^>]+Target="\\.\\./drawings/drawing[0-9]+\\.xml"[^>]*/>', "", rel_xml)
      rel_xml <- gsub('<Relationship[^>]+Type="[^"]+/vmlDrawing"[^>]+Target="\\.\\./drawings/vmlDrawing[0-9]+\\.vml"[^>]*/>', "", rel_xml)
      writeLines(rel_xml, rel_file, useBytes = TRUE)
    }
  }
  sheet_files <- list.files(file.path(tmp_dir, "xl", "worksheets"), pattern = "^sheet[0-9]+\\.xml$", full.names = TRUE)
  for (sheet_file in sheet_files) {
    sheet_xml <- paste(readLines(sheet_file, warn = FALSE), collapse = "")
    sheet_xml <- gsub('<drawing[^>]*/>', "", sheet_xml)
    sheet_xml <- gsub('<legacyDrawing[^>]*/>', "", sheet_xml)

    # openxlsx writes formulas without cached results. Cache constant string
    # formulas (for example, ="01") so protected-view and immediate re-upload
    # workflows can read GROUP_CODE values before Excel recalculates the file.
    worksheet_cells <- regmatches(
      sheet_xml,
      gregexpr("<c\\b[^>]*>.*?</c>", sheet_xml, perl = TRUE)
    )[[1]]
    formula_cells <- worksheet_cells[
      grepl("<f(?:\\s[^>]*)?>.*?</f>", worksheet_cells, perl = TRUE)
    ]
    if (length(formula_cells) > 0) {
      for (formula_cell in formula_cells) {
        formula_xml <- sub("(?s).*?<f(?:\\s[^>]*)?>(.*?)</f>.*", "\\1", formula_cell, perl = TRUE)
        formula_text <- gsub("&quot;", '"', formula_xml, fixed = TRUE)
        formula_text <- gsub("&apos;", "'", formula_text, fixed = TRUE)
        formula_text <- gsub("&lt;", "<", formula_text, fixed = TRUE)
        formula_text <- gsub("&gt;", ">", formula_text, fixed = TRUE)
        formula_text <- gsub("&amp;", "&", formula_text, fixed = TRUE)
        formula_text <- sub("^=", "", trimws(formula_text))

        if (grepl('^"(?:[^"]|"")*"$', formula_text, perl = TRUE)) {
          cached_value <- substring(formula_text, 2L, nchar(formula_text) - 1L)
          cached_value <- gsub('""', '"', cached_value, fixed = TRUE)
          cached_xml <- gsub("&", "&amp;", cached_value, fixed = TRUE)
          cached_xml <- gsub("<", "&lt;", cached_xml, fixed = TRUE)
          cached_xml <- gsub(">", "&gt;", cached_xml, fixed = TRUE)

          updated_cell <- if (grepl("<v(?:\\s[^>]*)?>.*?</v>", formula_cell, perl = TRUE)) {
            sub("<v(?:\\s[^>]*)?>.*?</v>", paste0("<v>", cached_xml, "</v>"), formula_cell, perl = TRUE)
          } else {
            sub("</f>", paste0("</f><v>", cached_xml, "</v>"), formula_cell, fixed = TRUE)
          }
          updated_cell <- if (grepl('^<c\\b[^>]*\\bt="[^"]*"', updated_cell, perl = TRUE)) {
            sub('(^<c\\b[^>]*\\b)t="[^"]*"', '\\1t="str"', updated_cell, perl = TRUE)
          } else {
            sub("^<c\\b([^>]*)>", '<c\\1 t="str">', updated_cell, perl = TRUE)
          }
          sheet_xml <- sub(formula_cell, updated_cell, sheet_xml, fixed = TRUE)
        }
      }
    }
    writeLines(sheet_xml, sheet_file, useBytes = TRUE)
  }
  zip::zipr(zipfile = xlsx_path, files = list.files(tmp_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE), root = tmp_dir, mode = "mirror")
}

# Reads constant text formulas such as ="01" directly from an xlsx worksheet.
# This avoids depending on Excel's cached formula results, which are absent until
# a downloaded workbook has been opened with editing enabled and recalculated.
read_xlsx_constant_text_formulas <- function(xlsx_path, column_index, sheet_index = 1L) {
  if (!file.exists(xlsx_path) || is.na(column_index) || column_index < 1) {
    return(data.frame(excel_row = integer(0), value = character(0)))
  }

  sheet_path <- sprintf("xl/worksheets/sheet%d.xml", as.integer(sheet_index))
  archive_files <- tryCatch(unzip(xlsx_path, list = TRUE)$Name, error = function(e) character(0))
  if (!(sheet_path %in% archive_files)) {
    return(data.frame(excel_row = integer(0), value = character(0)))
  }

  sheet_connection <- unz(xlsx_path, sheet_path, open = "rb")
  on.exit(close(sheet_connection), add = TRUE)
  sheet_xml <- paste(readLines(sheet_connection, warn = FALSE, encoding = "UTF-8"), collapse = "")

  cell_matches <- regmatches(
    sheet_xml,
    gregexpr("<c\\b[^>]*>.*?</c>", sheet_xml, perl = TRUE)
  )[[1]]
  if (length(cell_matches) == 0 || identical(cell_matches, character(0))) {
    return(data.frame(excel_row = integer(0), value = character(0)))
  }

  has_reference <- grepl('\\br="[A-Z]+[0-9]+"', cell_matches, perl = TRUE)
  has_formula <- grepl("<f(?:\\s[^>]*)?>.*?</f>", cell_matches, perl = TRUE)
  cell_matches <- cell_matches[has_reference & has_formula]
  if (length(cell_matches) == 0) {
    return(data.frame(excel_row = integer(0), value = character(0)))
  }

  references <- sub('.*?\\br="([A-Z]+[0-9]+)".*', "\\1", cell_matches, perl = TRUE)
  target_column <- openxlsx::int2col(as.integer(column_index))
  target_cells <- sub("[0-9]+$", "", references) == target_column
  cell_matches <- cell_matches[target_cells]
  references <- references[target_cells]
  if (length(cell_matches) == 0) {
    return(data.frame(excel_row = integer(0), value = character(0)))
  }

  formulas <- sub(".*?<f(?:\\s[^>]*)?>(.*?)</f>.*", "\\1", cell_matches, perl = TRUE)
  formulas <- gsub("&quot;", '"', formulas, fixed = TRUE)
  formulas <- gsub("&apos;", "'", formulas, fixed = TRUE)
  formulas <- gsub("&lt;", "<", formulas, fixed = TRUE)
  formulas <- gsub("&gt;", ">", formulas, fixed = TRUE)
  formulas <- gsub("&amp;", "&", formulas, fixed = TRUE)
  formulas <- sub("^=", "", trimws(formulas))

  is_constant_text <- grepl('^"(?:[^"]|"")*"$', formulas, perl = TRUE)
  formulas <- formulas[is_constant_text]
  references <- references[is_constant_text]
  if (length(formulas) == 0) {
    return(data.frame(excel_row = integer(0), value = character(0)))
  }

  values <- substring(formulas, 2L, nchar(formulas) - 1L)
  values <- gsub('""', '"', values, fixed = TRUE)
  data.frame(
    excel_row = as.integer(sub("^[A-Z]+", "", references)),
    value = values,
    stringsAsFactors = FALSE
  )
}

# Function to construct a formatted Excel workbook with predefined drop-down options for mapped KCP scenarios
#
# Purpose:
#   Creates the user-facing lookup workbook with dropdown validations for KCP
#   selections and formula-driven Scenario labels.
#
# Inputs:
#   df_export     : lookup rows (GROUP_CODE and KCP type columns).
#   meta_info     : metadata bundle with catalog, types, and groups.
#   scenario_cols : optional columns included in Scenario suffix generation.
#
# Returns:
#   An openxlsx workbook object containing Lookup and hidden Options sheets.
create_lookup_wb <- function(df_export, meta_info, scenario_cols = NULL) {
  df_export$Scenario <- ""
  wb <- createWorkbook()
  addWorksheet(wb, "Lookup", gridLines = TRUE)
  addWorksheet(wb, "Options", gridLines = FALSE)
  
  # Preserve GROUP_CODE as an identifier so leading zeros survive export.
  df_export$GROUP_CODE <- as.character(df_export$GROUP_CODE)
  
  # Output the template structure layout into the primary mapped worksheet 
  writeDataTable(wb, sheet = "Lookup", x = df_export, tableName = "KCP_Lookup", withFilter = TRUE, tableStyle = "TableStyleMedium2")
  
  lookup_columns <- names(df_export)
  group_col_idx <- match("GROUP_CODE", lookup_columns)

  # Replace populated group cells with constant text formulas (for example,
  # ="01"). Because these formulas contain no relative references, dragging
  # an initial cell copies its exact value instead of creating a number series.
  if (!is.na(group_col_idx) && nrow(df_export) > 0) {
    group_values <- df_export$GROUP_CODE
    escaped_group_values <- gsub('"', '""', group_values, fixed = TRUE)
    group_formulas <- ifelse(
      is.na(group_values) | !nzchar(group_values),
      '=""',
      paste0('="', escaped_group_values, '"')
    )
    writeFormula(
      wb,
      sheet = "Lookup",
      x = group_formulas,
      startCol = group_col_idx,
      startRow = 2
    )
  }

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
    group_values <- as.character(meta_info$groups)
    group_start <- options_row + 1
    writeData(wb, "Options", x = "GROUP_CODE", startCol = 1, startRow = group_start)
    writeData(wb, "Options", x = data.frame(GROUP_CODE = group_values, stringsAsFactors = FALSE), startCol = 1, startRow = group_start + 1)
    
    dataValidation(wb, sheet = "Lookup", cols = group_col_idx, rows = 2:500, 
                   type = "list", value = sprintf("'Options'!$A$%d:$A$%d", group_start + 2, group_start + 1 + length(group_values)), allowBlank = FALSE)
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



# Extracts values for a requested key from GROUPS-style text vectors.
#
# Behavior:
#   - Matches both key=value tokens and standalone key tokens.
#   - Returns a vector aligned to input rows.
#   - Converts explicit NA-like strings (NA, <NA>, NULL, NONE) to NA.
extract_group_values_vectorized <- function(vec, target_key) {
  target_key <- trimws(as.character(target_key)[1])
  if (is.na(target_key) || !nzchar(target_key)) return(rep(NA_character_, length(vec)))

  equal_prefix <- paste0(target_key, "=")
  parsed_vals <- vapply(vec, function(raw_value) {
    if (is.na(raw_value) || !nzchar(trimws(as.character(raw_value)))) return(NA_character_)

    tokens <- strsplit(trimws(as.character(raw_value)), "\\s+")[[1]]
    keyed_tokens <- tokens[startsWith(tokens, equal_prefix)]

    # Prefer key=value when both forms appear in the same GROUPS string.
    if (length(keyed_tokens) > 0) {
      value <- substring(keyed_tokens[1], nchar(equal_prefix) + 1)
    } else if (target_key %in% tokens) {
      value <- target_key
    } else {
      return(NA_character_)
    }

    value <- trimws(value)
    if (!nzchar(value) || toupper(value) %in% c("NA", "<NA>", "NULL", "NONE")) {
      return(NA_character_)
    }
    value
  }, character(1), USE.NAMES = FALSE)

  parsed_vals
}

# Connects to SQLite DB and retrieves unique group strings, ignoring 'excluded_groups'
#
# Purpose:
#   Retrieves available grouping values from SQLite using one of two modes:
#   direct-column mode or parsed GROUPS-column mode.
#
# Inputs:
#   db_path         : path to SQLite database.
#   table_name      : stand initialization table name.
#   group_col       : direct column name or GROUPS key name.
#   excluded_groups : values to remove from returned groups.
#   use_groups      : FALSE for direct-column mode; TRUE for GROUPS parsing mode.
#
# Returns:
#   Sorted unique character vector of group values.
get_groups_from_db <- function(db_path, table_name, group_col, excluded_groups, use_groups = FALSE) {
  if (!file.exists(db_path)) return(character(0))
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE) # Ensure we close the DB connection automatically 
  
  # Ensure the target table actually exists
  if (!dbExistsTable(con, table_name)) {
    stop(sprintf("Table '%s' could not be found in the database. Please check the 'Stand Initialization Table' name.", table_name))
  }
  
  table_cols <- dbListFields(con, table_name)
  
  if (isTRUE(use_groups)) {
    grp_col_candidates <- table_cols[toupper(table_cols) == "GROUPS"]
    if (length(grp_col_candidates) == 0) {
      stop(sprintf("Column 'GROUPS' could not be found in table '%s'.", table_name))
    }
    grp_col_name <- grp_col_candidates[1]
    
    sql <- sprintf("SELECT %s FROM %s WHERE %s IS NOT NULL AND TRIM(CAST(%s AS TEXT)) != ''",
                   quote_sql_identifier(grp_col_name), quote_sql_identifier(table_name),
                   quote_sql_identifier(grp_col_name), quote_sql_identifier(grp_col_name))
    vals <- dbGetQuery(con, sql)[[1]]
    if (length(vals) == 0) return(character(0))
    
    parsed_unique <- unique(extract_group_values_vectorized(vals, group_col))
    groups <- as.character(parsed_unique[!is.na(parsed_unique)])
  } else {
    if (!(group_col %in% table_cols)) {
      stop(sprintf("Column '%s' could not be found in the table '%s'. Please check the 'Database Grouping Column' name.", group_col, table_name))
    }
    sql <- sprintf("SELECT DISTINCT %s AS GROUP_CODE FROM %s", quote_sql_identifier(group_col), quote_sql_identifier(table_name))
    groups <- dbGetQuery(con, sql)$GROUP_CODE
    groups <- as.character(groups)
  }
  
  # Filter out empty strings, NA, or user excluded groups (e.g. 'Riparian')
  groups <- groups[!is.na(groups) & nzchar(trimws(groups))]
  groups <- groups[!(tolower(groups) %in% tolower(excluded_groups))]
  sort(unique(groups))
}


  
# Normalizes excluded-group inputs from text or multi-select controls.
#
# Input:
#   values : character vector or comma-delimited single string.
#
# Returns:
#   Cleaned character vector with whitespace removed and empties dropped.
normalize_excluded_groups <- function(values) {
  if (is.null(values) || length(values) == 0) return(character(0))
  if (length(values) == 1) {
    values <- unlist(strsplit(values, "\\s*,\\s*"))
  }
  values <- trimws(as.character(values))
  values[!is.na(values) & nzchar(values)]
}

# Returns available stand table columns for direct-column grouping mode.
#
# Inputs:
#   db_path   : path to SQLite database.
#   stand_tbl : stand initialization table name.
#
# Returns:
#   Character vector of column names, or character(0) when unavailable.
get_group_column_choices <- function(db_path, stand_tbl) {
  if (!file.exists(db_path) || !nzchar(trimws(stand_tbl))) return(character(0))
  con <- dbConnect(SQLite(), db_path)
  on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)
  if (!dbExistsTable(con, stand_tbl)) return(character(0))
  dbListFields(con, stand_tbl)
}

# Returns unique non-empty values from a selected direct grouping column.
#
# Inputs:
#   db_path   : path to SQLite database.
#   stand_tbl : stand initialization table name.
#   group_col : grouping column name.
#
# Returns:
#   Sorted unique values from the selected column.
get_unique_group_values <- function(db_path, stand_tbl, group_col) {
  if (!file.exists(db_path) || !nzchar(trimws(stand_tbl)) || !nzchar(trimws(group_col))) return(character(0))
  con <- dbConnect(SQLite(), db_path)
  on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)
  if (!dbExistsTable(con, stand_tbl)) return(character(0))
  table_cols <- dbListFields(con, stand_tbl)
  if (!(group_col %in% table_cols)) return(character(0))

  sql <- sprintf(
    "SELECT DISTINCT %s AS GROUP_VALUE FROM %s WHERE %s IS NOT NULL AND TRIM(CAST(%s AS TEXT)) != ''",
    quote_sql_identifier(group_col),
    quote_sql_identifier(stand_tbl),
    quote_sql_identifier(group_col),
    quote_sql_identifier(group_col)
  )
  values <- dbGetQuery(con, sql)$GROUP_VALUE
  values <- trimws(as.character(values))
  sort(unique(values[!is.na(values) & nzchar(values)]))
}

# Parses unique key names present in GROUPS text tokens across all rows.
#
# Inputs:
#   db_path   : path to SQLite database.
#   stand_tbl : stand initialization table name.
#
# Returns:
#   Sorted unique GROUPS keys (left side of key=value tokens).
parse_groups_column_keys <- function(db_path, stand_tbl) {
  if (!file.exists(db_path) || !nzchar(trimws(stand_tbl))) return(character(0))
  con <- dbConnect(SQLite(), db_path)
  on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)
  if (!dbExistsTable(con, stand_tbl)) return(character(0))
  cols <- dbListFields(con, stand_tbl)
  grp_col <- cols[toupper(cols) == "GROUPS"]
  if (length(grp_col) == 0) return(character(0))

  sql <- sprintf("SELECT %s FROM %s WHERE %s IS NOT NULL AND TRIM(CAST(%s AS TEXT)) != ''",
                 quote_sql_identifier(grp_col[1]), quote_sql_identifier(stand_tbl),
                 quote_sql_identifier(grp_col[1]), quote_sql_identifier(grp_col[1]))
  vals <- dbGetQuery(con, sql)[[1]]
  if (length(vals) == 0) return(character(0))

  parts <- unlist(strsplit(vals[!is.na(vals)], "\\s+"))
  parts <- parts[nzchar(parts)]
  keys <- sub("=.*$", "", parts)

  # Filter explicit null placeholders without removing real keys whose
  # key=value entries may contain a null value on only some rows.
  keys <- keys[!toupper(trimws(keys)) %in% c("NA", "<NA>", "NULL", "NONE")]

  sort(unique(keys))
}

# Returns unique values for one parsed GROUPS key.
#
# Inputs:
#   db_path    : path to SQLite database.
#   stand_tbl  : stand initialization table name.
#   parsed_key : GROUPS key to extract values for.
#
# Returns:
#   Sorted unique values associated with parsed_key.
get_unique_group_values_parsed <- function(db_path, stand_tbl, parsed_key) {
  if (!file.exists(db_path) || !nzchar(trimws(stand_tbl)) || !nzchar(trimws(parsed_key))) return(character(0))
  con <- dbConnect(SQLite(), db_path)
  on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)
  if (!dbExistsTable(con, stand_tbl)) return(character(0))
  cols <- dbListFields(con, stand_tbl)
  grp_col <- cols[toupper(cols) == "GROUPS"]
  if (length(grp_col) == 0) return(character(0))

  sql <- sprintf("SELECT %s FROM %s WHERE %s IS NOT NULL AND TRIM(CAST(%s AS TEXT)) != ''",
                 quote_sql_identifier(grp_col[1]), quote_sql_identifier(stand_tbl),
                 quote_sql_identifier(grp_col[1]), quote_sql_identifier(grp_col[1]))
  vals <- dbGetQuery(con, sql)[[1]]
  if (length(vals) == 0) return(character(0))

  all_vals <- unique(extract_group_values_vectorized(vals, parsed_key))
  all_vals <- all_vals[!is.na(all_vals)]
  sort(all_vals[nzchar(all_vals)])
}