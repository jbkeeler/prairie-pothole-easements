% % % --------------------------------------------------------------------------------------
% % % Class Information (ADD HERE ANY PACKAGES YOU MIGHT NEED)
% % % --------------------------------------------------------------------------------------

\NeedsTeXFormat{LaTeX2e}

\ProvidesClass{arpc-memo}[2025/11/11 ARPC Memo Class]

\RequirePackage{expl3} % Required for writing LaTeX3 code in this class.

\RequirePackage[dvipsnames, table, xcdraw]{xcolor} % Extended color support.
\DeclareTextFontCommand{\boldnd}{\color{ndsu}\bfseries}
\usepackage{graphicx}    % standard, good to have

%%% Handle Class Options %%%

\ExplSyntaxOn
% Default internal values for ARPC Brief
\tl_new:N \g_arpcmemo_school_tl
\tl_new:N \g_arpcmemo_cover_tl
\tl_new:N \g_arpcmemo_language_tl
\tl_new:N \g_arpcmemo_style_tl
\tl_new:N \g_arpcmemo_docstage_tl
\tl_new:N \g_arpcmemo_media_tl
\tl_new:N \g_arpcmemo_linkcolor_tl
\tl_new:N \g_arpcmemo_doctype_tl
\tl_new:N \g_arpcmemo_bookprint_tl
\tl_new:N \g_arpcmemo_aiack_tl

% Assign all default values
\tl_gset:Nn \g_arpcmemo_school_tl     { ATM }
\tl_gset:Nn \g_arpcmemo_cover_tl      { classic }
\tl_gset:Nn \g_arpcmemo_language_tl   { english }
\tl_gset:Nn \g_arpcmemo_style_tl      { classic }
\tl_gset:Nn \g_arpcmemo_docstage_tl   { working }
\tl_gset:Nn \g_arpcmemo_media_tl      { paper }
\tl_gset:Nn \g_arpcmemo_linkcolor_tl  { ndsu!45!black }
\tl_gset:Nn \g_arpcmemo_doctype_tl    { thesis }
\tl_gset:Nn \g_arpcmemo_bookprint_tl  { false }
\tl_gset:Nn \g_arpcmemo_aiack_tl      { true }

% Define NDSU brand color

\RequirePackage{xcolor}
\definecolor{ndsu}{HTML}{005643}

% Helper function for color validation (if ever needed)

\cs_new_protected:Npn \arpcmemo_check_color:n #1 {
  \color_set:nn {arpcmemotemp}{#1}
}
\ExplSyntaxOff

%%% Load Master Class %%%
\LoadClass[letter,11pt,oneside]{article}

%%% Required Packages %%%
\RequirePackage{ifthen} % Provides conditional statements.
\RequirePackage{etoolbox} % Advanced programming tools for LaTeX.
\RequirePackage{graphicx} % For including and manipulating images.
\RequirePackage{subcaption} % Subfigures and side-by-side captions.
\RequirePackage{tabularx} % Flexible-width tables.
\RequirePackage{xltabular} % Same as tabularx but to be used in long tables.
\RequirePackage{booktabs} % Professional table styling.
\RequirePackage{multirow,multicol} % Support for multirow cells in tables.
\RequirePackage{longtable} % Tables spanning multiple pages.
\RequirePackage{caption} % Customisation of captions.
\RequirePackage{enumitem} % Customisation of lists.
\RequirePackage{amsmath} % Advanced mathematical expressions.
\RequirePackage{amssymb} % Extended mathematical symbols.
\RequirePackage{varwidth} % Allows variable-width boxes.
\RequirePackage{titlesec} % Customisation of section and chapter titles.
\RequirePackage{titling} % Control over title and titlepage formatting.
\RequirePackage[toc,page]{appendix} % Formatting for appendices.
\RequirePackage[acronym]{glossaries} % Create glossaries and acronyms.
\usepackage[useregional]{datetime2}
\RequirePackage[bookmarks,pdfusetitle]{hyperref} % Hyperlinks and PDF metadata.
\RequirePackage{xparse} % Advanced argument parsing for custom macros.
\RequirePackage{typearea} % Page layout adjustments (KOMA-Script).
\RequirePackage{eso-pic} % Adding elements to the page background.
\RequirePackage{setspace} % Adjust line spacing.
%\RequirePackage[newfloat]{minted} % Syntax highlighting for code.
%\RequirePackage{silence} % Suppress specific package warnings.
%\RequirePackage{fontawesome5} % Font awesome icons.
%\RequirePackage{calc} % Infix notation arithmetic.
%\RequirePackage{soul} % A better underline.
%\RequirePackage{mfirstuc} % Capitalise words in a sentence.
%\RequirePackage{tikz} % Create high-quality graphics programmatically.
%\RequirePackage{psvectorian} % To include in-text ornamatentions. 
%\RequirePackage{blindtext} % Generate placeholder text.
\usepackage{lipsum}
\usepackage{wrapfig}
\usepackage{tikz}
\usepackage{calc} % for real dimension handling
\usepackage{pdflscape}
\usepackage{qrcode} % Generates QR codes as TikZ pictures
\usepackage{tcolorbox} % Creating colored and highlighted boxes.
\usepackage{changepage}
%\usepackage[table]{xcolor}
\usepackage{colortbl}

\tcbuselibrary{most} % Load most libraries from the tcolorbox package.
%\WarningsOff[blindtext] % Suppress warnings from the blindtext package.
\usepackage{ragged2e}

\usepackage{enumitem}
\usepackage{pifont}
\setlist[itemize]{
  label=\textcolor{ndsu}{\ding{233}},
  topsep=5pt,      
  itemsep=5pt,       
  parsep=3pt,       
  leftmargin=0.25in     
}

\renewcommand{\labelitemi}{$\textcolor{ndsu}{\bullet}$}
\renewcommand{\labelitemii}{$\textcolor{ndsu}{\cdot}$}
\renewcommand{\labelitemiii}{$\textcolor{ndsu}{\diamond}$}
\renewcommand{\labelitemiv}{$\textcolor{ndsu}{\ast}$}

%%% Import Configurations - Maintain the Specified Order! %%%


\usepackage{theme/configuration/00-Fonts}
\usepackage{theme/configuration/01-Colors}
\usepackage{theme/configuration/02-Margins}
\usepackage{theme/configuration/03-References}
\usepackage{theme/configuration/04-Headers}
\usepackage{theme/configuration/05-Contents}
\usepackage{theme/configuration/06-Glossary}
\usepackage{theme/configuration/07-Sections}
\usepackage{theme/configuration/08-Tables}
\usepackage{theme/configuration/09-Code}
\usepackage{theme/configuration/10-Macros}
\usepackage{theme/configuration/11-Metadata}

\newcommand{\raisedarrows}{\raisebox{0.25ex}{\textgreater\textgreater\textgreater}}

\newcommand{\standardfigwidth}{0.95\linewidth}
\usepackage{rotating}

\newcommand{\authorbio}[3]{%
  \begin{minipage}{\textwidth}
    \begin{wrapfigure}{l}{3cm}
      \vspace*{-32pt}
      \begin{tikzpicture}
        \clip (1.25cm,1.25cm) circle (1.25cm);
        \node at (1.25cm,1.25cm) {\includegraphics[width=2.5cm]{#1}}; % adjust vertical centering
      \end{tikzpicture}
    \end{wrapfigure}%
    \vspace{2pt}%
    \textcolor{ndsu}{\textbf{#2}} \\[0.5ex]
    #3
  \end{minipage}%
  \vspace{3em}
}

\captionsetup[figure]{
  name=Figure,
  labelfont={bf,color=ndsu},   % Bold + italic label (“Figure 1”) in NDSU green
  textfont={it,color=ndsu},       % Italic caption text in NDSU green
  labelsep=colon,                 % Colon separator (“Figure 1: …”)
  justification=centering,        % Centered captions (looks cleaner for briefs)
  singlelinecheck=false,          % Center even one-line captions
  font=normal                      % Slightly smaller than body text
}

\captionsetup[table]{
  name=Table,
  labelfont={bf,color=ndsu},   % Bold + italic label (“Figure 1”) in NDSU green
  textfont={it,color=ndsu},       % Italic caption text in NDSU green
  labelsep=colon,                 % Colon separator (“Figure 1: …”)
  justification=centering,        % Centered captions (looks cleaner for briefs)
  singlelinecheck=false,          % Center even one-line captions
  font=normal                      % Slightly smaller than body text
}

\newtcolorbox{highlight}{
  enhanced,
  colback=ndsu,
  colframe=ndsu,
  coltext=white,
  boxrule=0.5pt,
  arc=2pt,
  left=5pt,
  right=5pt,
  top=5pt,
  bottom=5pt,
  width=1.025\linewidth,
  valign=center,
  halign=center
}


\usepackage{threeparttable} % footnotes under tables
\definecolor{lightgray}{gray}{0.95}

\newif\ifatm@embargo
\DeclareOption{embargo}{\atm@embargotrue}
\ProcessOptions\relax

\ifatm@embargo
  \RequirePackage{draftwatermark}
  \SetWatermarkText{EMBARGOED}
  \SetWatermarkColor[gray]{0.85}
  \SetWatermarkFontSize{22pt}
  \SetWatermarkScale{1.2}
\fi

\makeatletter
\AtBeginDocument{%
  \renewcommand{\today}{%
    \ifcase\month\or
      January\or February\or March\or April\or May\or June\or
      July\or August\or September\or October\or November\or December%
    \fi\space\number\day, \number\year%
  }%
}
\makeatother

