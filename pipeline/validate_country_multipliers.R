################################################################################
#
# Validation: Country Multiplier at Age 60, All RCP Scenarios
#
# Generates a multi-page PDF with one page per country.
# Each page shows the total mortality multiplier for age 60 (expressed as %
# change vs baseline) across all three RCP scenarios (2.6 / 4.5 / 7.0),
# with year on the x-axis.
#
# Input:  results_csv/country_multiplier_matrices/<country>_<rcp>.csv
# Output: plots/validate_multiplier_age60.pdf
#
################################################################################

library(data.table)
library(ggplot2)
library(showtext)

# ── Fonts & palette (matching plot_country_multipliers.R) ────────────────────

font_add("Montserrat",
         regular    = "fonts/Montserrat-Regular.ttf",
         bold       = "fonts/Montserrat-Bold.ttf",
         italic     = "fonts/Montserrat-Italic.ttf",
         bolditalic = "fonts/Montserrat-BoldItalic.ttf")
showtext_auto()
showtext_opts(dpi = 150)

OceanBlue <- "#000066"

rcp_palette <- c(
  "RCP 2.6" = "#4575b4",
  "RCP 4.5" = "#f4a582",
  "RCP 7.0" = "#d73027"
)

theme_pub <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "Montserrat") +
    theme(
      text             = element_text(color = OceanBlue),
      plot.title       = element_text(face = "bold", size = base_size * 1.35,
                                      color = OceanBlue, margin = margin(b = 4)),
      plot.subtitle    = element_text(size = base_size * 0.95, color = OceanBlue,
                                      margin = margin(b = 8)),
      plot.caption     = element_text(hjust = 0, size = base_size * 0.72,
                                      color = "#666666", margin = margin(t = 6)),
      axis.title       = element_text(face = "bold", color = OceanBlue,
                                      size = base_size * 0.88),
      axis.text        = element_text(color = OceanBlue, size = base_size * 0.82),
      legend.title     = element_text(face = "bold", color = OceanBlue,
                                      size = base_size * 0.88),
      legend.text      = element_text(color = OceanBlue, size = base_size * 0.82),
      panel.grid.major = element_line(color = "#e8e8e8", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
}

# ── Country lookup (code → display name) ─────────────────────────────────────

country_names <- c(
  AT = "Austria",    BE = "Belgium",    BG = "Bulgaria",   CH = "Switzerland",
  CY = "Cyprus",     CZ = "Czechia",    DE = "Germany",    DK = "Denmark",
  EE = "Estonia",    EL = "Greece",     ES = "Spain",      FI = "Finland",
  FR = "France",     HR = "Croatia",    HU = "Hungary",    IE = "Ireland",
  IT = "Italy",      LT = "Lithuania",  LU = "Luxembourg", LV = "Latvia",
  MT = "Malta",      NL = "Netherlands",NO = "Norway",     PL = "Poland",
  PT = "Portugal",   RO = "Romania",    SE = "Sweden",     SI = "Slovenia",
  SK = "Slovakia",   UK = "United Kingdom"
)

rcp_labels <- c("rcp26" = "RCP 2.6", "rcp45" = "RCP 4.5", "rcp70" = "RCP 7.0")

# ── Load all matrix files ─────────────────────────────────────────────────────

mat_dir <- "results_csv/country_multiplier_matrices"
files   <- list.files(mat_dir, pattern = "\\.csv$", full.names = TRUE)
cat(sprintf("Found %d files in %s\n", length(files), mat_dir))

dt_list <- lapply(files, function(f) {
  bname  <- tools::file_path_sans_ext(basename(f))
  # filename pattern: <country_name>_<rcp_suffix>
  # split on last underscore group matching rcp__
  m      <- regmatches(bname, regexpr("^(.+)_(rcp\\d+)$", bname, perl = TRUE))
  if (length(m) == 0L) {
    warning(sprintf("Could not parse filename: %s", basename(f)))
    return(NULL)
  }
  parts     <- strsplit(m, "_(?=rcp)", perl = TRUE)[[1]]
  cname_raw <- parts[1]
  rcp_suf   <- parts[2]

  wide <- fread(f)
  # pivot to long
  long <- melt(wide, id.vars = "age", variable.name = "year",
               value.name = "multiplier")
  long[, year        := as.integer(as.character(year))]
  long[, rcp_label   := rcp_labels[rcp_suf]]
  long[, country_raw := cname_raw]
  long
})

dt <- rbindlist(dt_list[!sapply(dt_list, is.null)])

# Attach display country name (look up via country_raw → code → display name)
name_to_code <- setNames(
  names(country_names),
  gsub("_", " ", tolower(country_names))
)
dt[, country_display := {
  code <- name_to_code[country_raw]
  ifelse(!is.na(code), country_names[code], tools::toTitleCase(gsub("_", " ", country_raw)))
}]

countries_ordered <- sort(unique(dt$country_display))
cat(sprintf("Countries: %d | RCP scenarios: %s\n",
            length(countries_ordered),
            paste(unique(dt$rcp_label), collapse = ", ")))

# ── Subset to age 60 and convert multiplier to % change ──────────────────────

target_age <- 60L
plot_dt <- dt[age == target_age][
  order(country_display, rcp_label, year)
]
plot_dt[, pct_change := (multiplier - 1) * 100]

cat(sprintf("Rows for age %d: %s\n", target_age, format(nrow(plot_dt), big.mark = ",")))

# ── Common Y-axis limits across all countries ─────────────────────────────────

y_all   <- range(plot_dt$pct_change, na.rm = TRUE)
y_pad   <- diff(y_all) * 0.06
y_lim   <- c(y_all[1] - y_pad, y_all[2] + y_pad)
y_breaks <- seq(floor((y_lim[1]) / 2) * 2, ceiling((y_lim[2]) / 2) * 2, by = 2)

# ── Build one ggplot per country ──────────────────────────────────────────────

if (!dir.exists("plots")) dir.create("plots")
out_pdf <- "plots/validate_multiplier_age60.pdf"
cat(sprintf("Writing PDF: %s\n", out_pdf))

pdf(out_pdf, width = 8, height = 5.5, onefile = TRUE)

for (cntry in countries_ordered) {
  sub <- plot_dt[country_display == cntry]

  p <- ggplot(sub, aes(x = year, y = pct_change,
                       colour = rcp_label, group = rcp_label)) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "#aaaaaa", linewidth = 0.5) +
    geom_line(linewidth = 0.9) +
    scale_colour_manual(values = rcp_palette, name = "Scenario") +
    scale_x_continuous(
      breaks = seq(2020, 2099, by = 10),
      expand = expansion(add = c(1, 1))
    ) +
    scale_y_continuous(
      limits = y_lim,
      breaks = y_breaks,
      labels = function(x) paste0(ifelse(x >= 0, "+", ""), sprintf("%.0f", x), "%")
    ) +
    labs(
      title    = cntry,
      subtitle = "Mortality multiplier at age 60 relative to 1990\u20132019 baseline",
      x        = "Year",
      y        = "Multiplier (% change)",
      caption  = paste0(
        "Multiplier = GCM-ensemble mean RR for projected temperatures / ",
        "mean RR for baseline temperatures (1990\u20132019). ",
        "Total (heat + cold) component. Y-axis fixed across all countries."
      )
    ) +
    theme_pub(base_size = 12) +
    guides(colour = guide_legend(title.position = "top"))

  # Render the plot to the PDF device
  print(p)
}

dev.off()
cat(sprintf("Done. Pages: %d\n", length(countries_ordered)))
