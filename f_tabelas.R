
# manuel.ribeiro@tecnico.ulisboa.pt
# created: 2025-06-05
# last revision : 2025-06-05

# Mortalidade infantil 2015-2024

# funções para preparar tabelas de resultados RR e SMR

library(ggplot2)
library(gridExtra)
library(dplyr)
library(reshape2)
library(tidyr) # para o pivot_wider()
library(knitr)
library(kableExtra)


# ----- funcoes ------

tabrr_val <- function(rr, rrLL, rrUL, digits = 2) {
  # coluna nomes dos municipios
  county_col_name <- names(rr)[1]

  # converte para formato longo, mantendo o nome
  # media
  rr_long <- rr %>%
    pivot_longer(-!!sym(county_col_name), names_to = "year", values_to = "rr")
  # IC95
  rrLL_long <- rrLL %>%
    pivot_longer(-!!sym(county_col_name), names_to = "year", values_to = "ll")
  rrUL_long <- rrUL %>%
    pivot_longer(-!!sym(county_col_name), names_to = "year", values_to = "ul")

  # combina valores e formata
  rrout <- rr_long %>%
    left_join(rrLL_long, by = c(county_col_name, "year")) %>%
    left_join(rrUL_long, by = c(county_col_name, "year")) %>%
    mutate(formato = sprintf("%.2f (%.2f, %.2f)",
                             round(rr, digits),
                             round(ll, digits),
                             round(ul, digits)),
           # estilo HTML para celulas onde LL do RR > 1
           formato = ifelse(ll > 1,
                            paste0('<span style="background-color: #ffb3d9; padding: 2px 4px; border-radius: 3px;">',
                                   formato, '</span>'), formato)) %>%
    dplyr::select(!!sym(county_col_name), year, formato) %>%
    pivot_wider(names_from = year, values_from = formato)

  return(rrout)
}

# kaggle
tabrr <- function(rr_table, max_rows = 278) {
  rr_table %>%
    head(max_rows) %>%
    kable(format = "html", escape = FALSE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  full_width = FALSE,
                  font_size = 12) %>%
    scroll_box(width = "100%", height = "400px")
}
