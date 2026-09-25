# Project-wide constants: file paths, parameters, shared settings.

NDSU_COLORS <- c(
ndsu_green  = "#00583D",
ndsu_yellow = "#FFC425",
ndsu_rust   = "#B83E27",
ndsu_night  = "#0F374B",
ndsu_teal   = "#51ABD0",
ndsu_sage   = "#8ABD78"
)


# From Dylan Turner

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

