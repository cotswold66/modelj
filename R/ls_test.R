#' ls_test
#'
#' Helper function that returns parameter estimates, effect sizes, pair-wise
#' comparisons and power t-test calculations.
#'
#' @param model
#' a model
#' @param contrast
#' variable used for least square mean estimates and contrasts.
#' @param comp
#' vector containing the two factor levels for use in the power calculations
#' @param sig.level
#' signficance level used in power calculations
#' @param power
#' power used in power calculations
#'
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
#' \item{power}{power.t.test result}
#' }
#' @export
#'
#' @examples
ls_test <-
  function(model, contrast, comp, sig.level = .05, power = .9) {
    sigma <- summary(model)$sigma
    emm <- emmeans::emmeans(
      model,
      weights = "prop",
      specs = stats::as.formula(paste0("pairwise ~ ", contrast)),
      # specs = stats::as.formula(paste0("trt.vs.ctrl ~ ", contrast)),
      # ref = control,
      adjust = "None"
    )
    emm2 <- emmeans::emmeans(
      model,
      weights = "prop",
      specs = stats::as.formula(paste0("pairwise ~ ", contrast)),
      at = eval(parse(text = paste0("list(", contrast," = comp)")))
    )
    effect_size = summary(emm2)$contrasts$estimate
    power_res <- stats::power.t.test(
      delta = effect_size,
      sd = sigma,
      sig.level = .05,
      power = .9
    )
    results = list("estimates" = summary(model),
                   "ls_means" = emm$emmeans,
                   "contrasts" = emm$contrasts,
                   "power" = power_res
                   )
    return(results)
  }
