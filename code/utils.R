# Helper functions reused across scripts in this project.



# save_latex_table()
#
# Port of arpcNetFarmIncome's data-raw save_latex_table() with the numeric
# formatting stripped out: df arrives already formatted as character.
#
# df                already-formatted character data frame
# col_spec          character vector of column specs for the non-first columns
# first_col_spec    column spec for the first column
# size_cmd          e.g. "\\small" or "\\footnotesize"
# tabcolsep_pt      optional \setlength{\tabcolsep}{Xpt}
# extrarowheight_pt optional \setlength{\extrarowheight}{Xpt}
# bold_rows         regex matched against the first cell; matching rows get a
#                   \midrule before them and \textbf{} around every cell
save_latex_table <- function(df, file_path, caption, label, note, col_spec,
                             first_col_spec = "L{1.8cm}", size_cmd = "\\small",
                             tabcolsep_pt = NULL, extrarowheight_pt = NULL,
                             bold_rows = "Total") {

  df <- as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE,
                      check.names = FALSE)

  esc <- function(x) gsub("&", "\\\\&", gsub("%", "\\\\%", x))
  headers <- esc(colnames(df))

  header_row <- paste0(
    paste0("{\\color{white}\\bfseries ", headers, "}", collapse = " & "), " \\\\"
  )

  body_rows <- apply(df, 1, function(row) {
    if (nzchar(bold_rows) && grepl(bold_rows, row[1])) {
      paste0("\\midrule\n\\textbf{", paste(row, collapse = "} & \\textbf{"), "} \\\\")
    } else {
      paste0(paste(row, collapse = " & "), " \\\\")
    }
  })

  spacing <- c(
    if (!is.null(tabcolsep_pt))
      paste0("\\setlength{\\tabcolsep}{", tabcolsep_pt, "pt}"),
    if (!is.null(extrarowheight_pt))
      paste0("\\setlength{\\extrarowheight}{", extrarowheight_pt, "pt}")
  )

  latex_code <- c(
    "\\begin{table}[htpb!]", "\\centering", "\\begin{threeparttable}",
    "\\captionsetup{justification=raggedright,singlelinecheck=true}",
    paste0("\\caption{", caption, "}\\label{tab:", label, "}"),
    "\\rowcolors{2}{lightgray}{white}",
    size_cmd,
    spacing,
    "",
    paste0("\\begin{tabular}{", first_col_spec, " ",
           paste(col_spec, collapse = " "), "}"),
    "\\toprule", "\\rowcolor{ndsu}", header_row, "\\midrule",
    body_rows, "\\bottomrule", "\\end{tabular}",
    "\\vspace{1.5pt}", "\\begin{tablenotes}[flushleft]", "\\footnotesize",
    "\\setlength{\\leftmargin}{0pt}\\setlength{\\labelsep}{0pt}\\setlength{\\itemsep}{4pt}",
    paste0("\\item\\textit{Note:} ", note, "\\par"),
    "\\item\\textit{Source:} ARPC calculations using data from USDA.",
    "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}"
  )

  writeLines(latex_code, file_path)
  invisible(latex_code)
}

# ========================================
# helpers_tables.R
# LaTeX table helpers: NDSU seamless header bands
# ========================================
#
# Sourced at the top of ./code/tables.R. Nothing here depends on the rest of
# the pipeline, so tables.R can be run on its own after master.R's config block
# plus helpers.R.
#
# The header of every table built here is ONE \multicolumn cell painted with
# \columncolor{ndsu}, containing a nested tabular whose column spec is
# byte-identical to the body spec. Because the colored background is a single
# paint rectangle, seams, hairline gaps, and header/body misalignment cannot
# happen. See the fix-ndsu-tables skill for the diagnosis of the failure modes
# this avoids.
#
# ========================================

# ----------------------------------------
# header_band()
#
# Verbatim from fix-ndsu-tables/references/r-helpers.md.
#
# Header band: the whole header is one \multicolumn cell painted green, with a
# nested tabular (identical column spec as the body) carrying super-headers,
# white cmidrules, and column labels. One paint rectangle -> no seams, and no
# @{}-gap tricks that desync header and body columns.
# ----------------------------------------
header_band <- function(colspec, supers, labels) {
  n <- length(labels)
  lab_cells <- sprintf("{\\color{white}\\bfseries %s}", labels)
  # no super-headers -> no super row and no cmidrules, just the labels
  sup_rows <- character(0)
  if (length(supers) > 0) {
    sup_cells <- vapply(supers, function(s) {
      if (s$label == "") {
        if (s$span == 1) "" else sprintf("\\multicolumn{%d}{c}{}", s$span)
      } else {
        sprintf("\\multicolumn{%d}{c}{\\color{white}\\bfseries %s}", s$span, s$label)
      }
    }, character(1))
    spans <- vapply(supers, function(s) s$span, numeric(1))
    last  <- cumsum(spans)
    first <- c(1, head(last, -1) + 1)
    cmids <- unlist(mapply(function(s, a, b) {
      if (s$label == "") character(0) else sprintf("\\cmidrule(lr){%d-%d}", a, b)
    }, supers, first, last))
    sup_rows <- c(paste0(paste(sup_cells, collapse = " & "), " \\\\"),
                  paste0("\\arrayrulecolor{white}", paste(cmids, collapse = "")))
  }
  c(sprintf("\\multicolumn{%d}{@{}>{\\columncolor{ndsu}[0pt][0pt]}c@{}}{%%", n),
    sprintf("\\begin{tabular}{%s}", colspec),
    sup_rows,
    paste0(paste(lab_cells, collapse = " & "), " \\\\"),
    "\\end{tabular}%",
    "} \\\\",
    "\\arrayrulecolor{black}\\midrule")
}

# ----------------------------------------
# stripe()
#
# Verbatim from fix-ndsu-tables/references/r-helpers.md.
#
# Explicit zebra striping (the nested header tabular corrupts \rowcolors'
# row counter, so alternate colors deterministically instead).
# ----------------------------------------
stripe <- function(rows, start_index = 1) {
  idx <- seq_along(rows) + start_index - 1
  ifelse(idx %% 2 == 1, paste0("\\rowcolor{lightgray}", rows), rows)
}

# ----------------------------------------
# tex_escape()
#
# Escapes % and & for LaTeX, but leaves an already-escaped \% or \& alone so
# that callers may write either convention. Dollar signs are never touched:
# callers write \$ themselves.
# ----------------------------------------
tex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("(?<!\\\\)%", "\\\\%", x, perl = TRUE)
  x <- gsub("(?<!\\\\)&", "\\\\&", x, perl = TRUE)
  x
}

# ----------------------------------------
# save_latex_table_bands()
#
# Same wrapper as helpers.R's save_latex_table() (table float, threeparttable,
# caption, size, tablenotes), but the header is a seamless NDSU band and the
# striping is explicit rather than \rowcolors.
#
# df                already-formatted character data frame
# colspec           ONE string, e.g. "L{1.7cm} C{1.0cm} C{1.0cm}". It is used
#                   for BOTH the outer tabular and the nested header tabular.
#                   That identity is what guarantees column alignment.
# supers            ordered list of list(label=, span=); label = "" draws no
#                   cmidrule. Spans must sum to ncol(df). NULL (the default)
#                   drops the super-header row entirely.
# labels            column labels; defaults to names(df)
# size_cmd          e.g. "\\small" or "\\footnotesize"
# tabcolsep_pt      optional \setlength{\tabcolsep}{Xpt}
# extrarowheight_pt optional \setlength{\extrarowheight}{Xpt}
# rule_rows         regex on the first cell; matching rows get a \midrule
#                   before them
# bold_rows         regex on the first cell; matching rows are bolded cell by
#                   cell
# source            text of the Source: line in the table notes
#
# Striping is applied to the full body vector FIRST and the \midrule lines are
# spliced in afterwards, so the stripe parity does not depend on how many rules
# are inserted.
# ----------------------------------------
save_latex_table_bands <- function(df, file_path, caption, label, note,
                                   colspec, supers = NULL, labels = names(df),
                                   size_cmd = "\\footnotesize",
                                   tabcolsep_pt = 3,
                                   extrarowheight_pt = NULL,
                                   rule_rows = "^(Total|Average)$",
                                   bold_rows = "^Total$",
                                   source = "ARPC calculations using data from USDA.") {

  df <- as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE,
                      check.names = FALSE)

  n_col <- ncol(df)
  stopifnot(length(labels) == n_col)
  stopifnot(is.null(supers) ||
              sum(vapply(supers, function(s) s$span, numeric(1))) == n_col)

  labels <- tex_escape(labels)
  supers <- lapply(supers, function(s) {
    s$label <- tex_escape(s$label)
    s
  })

  first_cells <- as.character(df[[1]])
  cells <- as.matrix(as.data.frame(lapply(df, tex_escape),
                                   stringsAsFactors = FALSE,
                                   check.names = FALSE))

  body_rows <- vapply(seq_len(nrow(cells)), function(i) {
    row <- cells[i, ]
    if (nzchar(bold_rows) && grepl(bold_rows, first_cells[i])) {
      paste0(paste0("\\textbf{", row, "}", collapse = " & "), " \\\\")
    } else {
      paste0(paste(row, collapse = " & "), " \\\\")
    }
  }, character(1))

  # stripe first, then splice the rules in, so parity stays stable
  body_rows <- stripe(body_rows)
  body_out  <- character(0)
  for (i in seq_along(body_rows)) {
    if (nzchar(rule_rows) && grepl(rule_rows, first_cells[i])) {
      body_out <- c(body_out, "\\midrule")
    }
    body_out <- c(body_out, body_rows[i])
  }

  spacing <- c(
    if (!is.null(tabcolsep_pt))
      paste0("\\setlength{\\tabcolsep}{", tabcolsep_pt, "pt}"),
    if (!is.null(extrarowheight_pt))
      paste0("\\setlength{\\extrarowheight}{", extrarowheight_pt, "pt}")
  )

  latex_code <- c(
    "\\begin{table}[htpb!]", "\\centering", "\\begin{threeparttable}",
    "\\captionsetup{justification=raggedright,singlelinecheck=true}",
    paste0("\\caption{", caption, "}\\label{tab:", label, "}"),
    size_cmd,
    spacing,
    "",
    paste0("\\begin{tabular}{", colspec, "}"),
    "\\toprule",
    header_band(colspec, supers, labels),
    body_out,
    "\\bottomrule", "\\end{tabular}",
    "\\vspace{1.5pt}", "\\begin{tablenotes}[flushleft]", "\\footnotesize",
    "\\setlength{\\leftmargin}{0pt}\\setlength{\\labelsep}{0pt}\\setlength{\\itemsep}{4pt}",
    paste0("\\item\\textit{Note:} ", note, "\\par"),
    paste0("\\item\\textit{Source:} ", source),
    "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}"
  )

  writeLines(latex_code, file_path)
  invisible(latex_code)
}

