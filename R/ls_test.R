#' ls_test
#'
#' Helper function that returns parameter estimates, effect sizes, pair-wise
#' comparisons and power t-test calculations.
#'
#' @param model
#' a model
#' @param contrast
#' vector used for least square mean estimates and contrasts.
#' @param comp
#' string containing the two levels for use in the power calculations
#' @param sig.level
#' signficance level used in power calculations
#' @param power
#' power used in power calculations
#'
#'
#' @return
#' A list consisting of the following:
#' \describe{
#' \item{estimates}{summary(model)}
#' \item{ls_means}{least square means table (emmGrid object) using proportional
#' weighting}
#' \item{contrasts}{pair-wise contrast table (emmGrid object) against control
#' using no multiplicity correction}
#' \item{summary}{summary table of contrasts and power calculations}
#' }
#' @export
#'
#' @examples
ls_test <-
  function(model, contrast, comp = NULL, sig.level = .05, power = .9) {
    contrast <- rlang::enexpr(contrast)
    mod_contrast <- rlang::eval_tidy(contrast, model$model)
    if(!is.factor(mod_contrast)){
      mod_contrast <- as.factor(mod_contrast)
    }
    sigma <- summary(model)$sigma
    specs <- rlang::eval_tidy(rlang::expr(`~`(pairwise, !!contrast)))
    emm <- emmeans::emmeans(
      model,
      weights = "prop",
      specs = specs,
      adjust = "None"
    )
    em1 <- tidyr::as_tibble(summary(emm$emmeans))
    em1_contrast <- rlang::eval_tidy(contrast, em1)
    contrasts <- summary(emm)$contrasts
    contrasts <- dplyr::mutate(
      contrasts,
      pwr = purrr::map(
        contrasts$estimate,
        ~ power.t.test(
          delta = .x,
          sd = sigma,
          sig.level = sig.level,
          power = power
        )
      ),
      sig.level = purrr::map_dbl(pwr, "sig.level"),
      power = purrr::map_dbl(pwr, "power"),
      power = scales::percent(power),
      sigma = purrr::map_dbl(pwr, "sd"),
      N = ceiling(purrr::map_dbl(pwr, "n")) * 2,
      C1 = stringr::str_split(contrast, " - ", simplify = TRUE)[,1],
      C1 = purrr::map_dbl(C1, ~ em1$emmean[em1_contrast %in% .x]),
      C2 = stringr::str_split(contrast, " - ", simplify = TRUE)[,2],
      C2 = purrr::map_dbl(C2, ~ em1$emmean[em1_contrast %in% .x]),
    )
    contrasts <- dplyr::select(
      contrasts,
      contrast, l.contr = C1, r.contr = C2, delta = estimate, SE, df,
      t.ratio, p.value, sig.level, power,
      sigma, N
    )
    if (!is.null(comp)){
      contrasts <- dplyr::filter(contrasts, grepl(comp, contrast))
    }
    results = list("estimates" = summary(model),
                   "ls_means" = emm$emmeans,
                   "contrasts" = emm$contrasts,
                   "summary" = contrasts
    )
    results <- structure(results, class = "modelj")
    return(results)
  }
