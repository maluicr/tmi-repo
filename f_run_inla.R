#' Objetivo:
#'
#' Implementa várias funções - workflow - para explorar o uso de
#' diferentes distribuições à priori com Penalized Complexity (PC)
#' e diferentes modelos INLA.
#'
#' 1) create_hyperparameter_grid(), cria uma grid de hiperparametros a serem avaliados
#' 2) get_hyperpriors(), extrai os hiperparamteros para uma combinação (efeitos aleatorios BYM2, RW1 e fixos EDI)
#' 3) fit_model(), ajusta modelo INLA com combinação de hiperparamteros
#' 4) fit_all_models(), ajusta todos os modelos INLA com todas as config (gera um RDS com os resultados todos)
#' 5) summarize_results(), função para extrair resumo dos resultados (gera "results_balanced.rds" com os resultados todos)
#'
#' os resultados ficam todos armazenados num RDS gerado na função fit_all_models()
#'
#' criado: 2025-12-10
#' ultima revisao: 2025-12-10
#' autor: manuel.ribeiro at tecnico.ulisboa.pt + claude.ia





# ----- função para criar grelha de parametros -----


create_hyperparameter_grid <- function(
  # Parâmetros BYM2
  bym2_prec_u = c(0.5),
  bym2_prec_alpha = c(0.8),
  bym2_phi_u = c(0.5),
  bym2_phi_alpha = c(0.1),

  # Parâmetros RW1 temporal
  rw1_prec_u = c(0.5),
  rw1_prec_alpha = c(0.1),

  # Parâmetros interação espaço-temporal
  st_prec_u = c(1),
  st_prec_alpha = c(0.1),

  # Parâmetros efeito fixo EDI
  edi_mean = c(0.1),
  edi_prec = c(0.5)
) {

  grid <- expand.grid(
    bym2_prec_u = bym2_prec_u,
    bym2_prec_alpha = bym2_prec_alpha,
    bym2_phi_u = bym2_phi_u,
    bym2_phi_alpha = bym2_phi_alpha,
    rw1_prec_u = rw1_prec_u,
    rw1_prec_alpha = rw1_prec_alpha,
    st_prec_u = st_prec_u,
    st_prec_alpha = st_prec_alpha,
    edi_mean = edi_mean,
    edi_prec = edi_prec,
    stringsAsFactors = FALSE
  )

  grid$config_id <- seq_len(nrow(grid))
  grid <- grid[, c("config_id", setdiff(names(grid), "config_id"))]

  return(grid)
}


# ----- função para extrair os hiperparametros das priors numa configuracao -----

get_hyperpriors <- function(grid_row) {

  hyper.bym2 <- list(
    prec = list(prior = "pc.prec", param = c(grid_row$bym2_prec_u, grid_row$bym2_prec_alpha)),
    phi = list(prior = "pc", param = c(grid_row$bym2_phi_u, grid_row$bym2_phi_alpha))
  )

  hyper.temporal <- list(
    prec = list(prior = "pc.prec", param = c(grid_row$rw1_prec_u, grid_row$rw1_prec_alpha))
  )

  hyper.st <- list(
    prec = list(prior = "pc.prec", param = c(grid_row$st_prec_u, grid_row$st_prec_alpha))
  )

  control.fixed <- list(
    mean.intercept = 0,
    prec.intercept = 0.01,
    mean = grid_row$edi_mean,
    prec = grid_row$edi_prec
  )

  return(list(
    hyper.bym2 = hyper.bym2,
    hyper.temporal = hyper.temporal,
    hyper.st = hyper.st,
    control.fixed = control.fixed
  ))
}

# ----- funcao para ajustar um modelo com config especifica -----

fit_model <- function(model_id, grid_row, d, E, g, verbose = TRUE) {

  # Extrair hiperpriors
  hp <- get_hyperpriors(grid_row)

  if (verbose) {
    cat(sprintf("Ajustando modelo %s (config %d)...\n", model_id, grid_row$config_id))
  }

  # Definir fórmulas para cada modelo
  if (model_id == "m1") {
    formula <- y ~ 1 + idtime
    result <- inla(formula,
                   family = "poisson",
                   data = d,
                   E = E,
                   control.fixed = hp$control.fixed,
                   control.predictor = list(compute = TRUE),
                   control.compute = list(dic = TRUE, waic = TRUE))

  } else if (model_id == "m2") {
    formula <- y ~ 1 + idtime +
      f(idarea, model = "bym2", graph = g, hyper = hp$hyper.bym2, constr = TRUE) +
      f(idarea1, idtime, model = "iid", hyper = hp$hyper.st, constr = TRUE)
    result <- inla(formula,
                   family = "poisson",
                   data = d,
                   E = E,
                   control.fixed = hp$control.fixed,
                   control.predictor = list(compute = TRUE),
                   control.compute = list(dic = TRUE, waic = TRUE))

  } else if (model_id == "m3") {
    formula <- y ~ 1 +
      f(idarea, model = "bym2", graph = g, hyper = hp$hyper.bym2) +
      f(idtime, model = "rw1", hyper = hp$hyper.temporal) +
      f(idtime1, model = "iid")
    lcs <- inla.make.lincombs(idtime = diag(10), idtime1 = diag(10))
    result <- inla(formula,
                   family = "poisson",
                   data = d,
                   E = E,
                   control.predictor = list(compute = TRUE),
                   lincomb = lcs,
                   control.compute = list(dic = TRUE, waic = TRUE))

  } else if (model_id == "m4") {
    formula <- y ~ 1 +
      f(idarea, model = "bym2", hyper = hp$hyper.bym2, graph = g) +
      f(idtime, model = "rw1", hyper = hp$hyper.temporal) +
      f(idtime1, model = "iid") +
      f(idareatime, model = "iid", hyper = hp$hyper.st)
    result <- inla(formula,
                   family = "poisson",
                   data = d,
                   E = E,
                   control.fixed = hp$control.fixed,
                   control.predictor = list(compute = TRUE),
                   control.compute = list(dic = TRUE, waic = TRUE))

  } else if (model_id == "m5") {
    formula <- y ~ 1 + idtime + EDI_SCORE +
      f(idarea, model = "bym2", graph = g, hyper = hp$hyper.bym2) +
      f(idarea1, idtime, model = "iid", hyper = hp$hyper.st)
    result <- inla(formula,
                   family = "poisson",
                   data = d,
                   E = E,
                   control.fixed = hp$control.fixed,
                   control.predictor = list(compute = TRUE),
                   control.compute = list(dic = TRUE, waic = TRUE))

  } else if (model_id == "m6") {
    formula <- y ~ 1 + EDI_SCORE +
      f(idarea, model = "bym2", graph = g, hyper = hp$hyper.bym2) +
      f(idtime, model = "rw1", hyper = hp$hyper.temporal) +
      f(idtime1, model = "iid")
    lcs <- inla.make.lincombs(idtime = diag(10), idtime1 = diag(10))
    result <- inla(formula,
                   family = "poisson",
                   data = d,
                   E = E,
                   control.predictor = list(compute = TRUE),
                   lincomb = lcs,
                   control.compute = list(dic = TRUE, waic = TRUE))

  } else if (model_id == "m7") {
    formula <- y ~ 1 + EDI_SCORE +
      f(idarea, model = "bym2", hyper = hp$hyper.bym2, graph = g) +
      f(idtime, model = "rw1", hyper = hp$hyper.temporal) +
      f(idtime1, model = "iid") +
      f(idareatime, model = "iid", hyper = hp$hyper.st)
    result <- inla(formula,
                   family = "poisson",
                   data = d,
                   E = E,
                   control.fixed = hp$control.fixed,
                   control.predictor = list(compute = TRUE),
                   control.compute = list(dic = TRUE, waic = TRUE))

  } else {
    stop("Modelo desconhecido: ", model_id)
  }

  return(result)
}

# ----- funcao para ajustar todos os modelos com todas as config -----

fit_all_models <- function(grid, d, E, g,
                           models = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
                           save_file = NULL,
                           verbose = TRUE) {

  results <- list()
  total_fits <- nrow(grid) * length(models)
  current_fit <- 0

  for (i in 1:nrow(grid)) {
    for (model_id in models) {
      current_fit <- current_fit + 1

      if (verbose) {
        cat(sprintf("\n[%d/%d] Modelo: %s | Config: %d/%d\n",
                    current_fit, total_fits, model_id, i, nrow(grid)))
      }

      tryCatch({
        model_fit <- fit_model(model_id, grid[i, ], d, E, g, verbose = FALSE)

        results[[length(results) + 1]] <- list(
          model_id = model_id,
          config_id = grid[i, "config_id"],
          config = grid[i, ],
          fit = model_fit,
          dic = model_fit$dic$dic,
          waic = model_fit$waic$waic,
          cpo_fail = sum(model_fit$cpo$failure, na.rm = TRUE),
          mlik = model_fit$mlik[1]
        )

        if (verbose) {
          cat(sprintf("  DIC: %.2f | WAIC: %.2f | CPO failures: %d\n",
                      model_fit$dic$dic, model_fit$waic$waic,
                      sum(model_fit$cpo$failure, na.rm = TRUE)))
        }

      }, error = function(e) {
        if (verbose) {
          cat(sprintf("  ERRO: %s\n", e$message))
        }
        results[[length(results) + 1]] <- list(
          model_id = model_id,
          config_id = grid[i, "config_id"],
          config = grid[i, ],
          fit = NULL,
          error = e$message
        )
      })

      # Guardar periodicamente
      if (!is.null(save_file) && current_fit %% 10 == 0) {
        saveRDS(results, save_file)
        if (verbose) cat("  Progresso guardado.\n")
      }
    }
  }

  # Guardar resultado final
  if (!is.null(save_file)) {
    saveRDS(results, save_file)
    if (verbose) cat("\nResultados finais guardados em:", save_file, "\n")
  }

  return(results)
}

# ----- função para extrair resumo dos resultados ------

summarize_results <- function(results) {

  summary_df <- data.frame()

  for (res in results) {
    if (!is.null(res$fit)) {
      summary_df <- rbind(summary_df, data.frame(
        model_id = res$model_id,
        config_id = res$config_id,
        dic = res$dic,
        waic = res$waic,
        cpo_fail = res$cpo_fail,
        mlik = res$mlik,
        bym2_prec_u = res$config$bym2_prec_u,
        bym2_prec_alpha = res$config$bym2_prec_alpha,
        bym2_phi_u = res$config$bym2_phi_u,
        bym2_phi_alpha = res$config$bym2_phi_alpha,
        rw1_prec_u = res$config$rw1_prec_u,
        rw1_prec_alpha = res$config$rw1_prec_alpha,
        st_prec_u = res$config$st_prec_u,
        st_prec_alpha = res$config$st_prec_alpha,
        edi_mean = res$config$edi_mean,
        edi_prec = res$config$edi_prec
      ))
    }
  }

  return(summary_df)
}

# ----- executar o workflow -----

# # Equilíbrio entre exploração e tempo
# grid_balanced <- create_hyperparameter_grid(
#   bym2_prec_u = c(0.5),
#   bym2_prec_alpha = c(0.8),          # manter valor conservador
#   bym2_phi_u = c(0.5),
#   bym2_phi_alpha = c(0.1),           # manter estrutura moderada
#   rw1_prec_u = c(0.5),
#   rw1_prec_alpha = c(0.1, 0.15),     # 2 valores - temporal
#   st_prec_u = c(1, 1.5),             # 2 valores - interação
#   st_prec_alpha = c(0.1),
#   edi_mean = c(0.1, 0.15),           # 2 valores - magnitude EDI
#   edi_prec = c(0.5, 0.7)             # 2 valores - certeza EDI
# )
#
# # 2 * 2 * 2 * 2 = 16 configurações × 7 modelos = 112 ajustes
# # tempo de processamento: 50-60 min
#
# cat("Configurações:", nrow(grid_balanced), "\n")
# cat("Ajustes totais:", nrow(grid_balanced) * 7, "\n")
#
# # Executar
# results <- fit_all_models(
#   grid = grid_balanced,
#   d = d, E = E, g = g,
#   models = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
#   save_file = "results_balanced.rds",
#   verbose = TRUE
# )

results <- readRDS("results_balanced.rds")

# Analisar
summary_df <- summarize_results(results)
summary_df <- summary_df[order(summary_df$waic), ]
head(summary_df, 10)
