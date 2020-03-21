#' ls_test
#'
#' Helper function that returns parameter estimates, effect sizes, pair-wise
#' comparisons and power t-test calculations.
#'
#' @param model
#' a model
#' @param contrast
#' variable used for least square mean estimates and contrasts.
#' @param control
#' factor level used as the control value for contrasts
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
  function(model, contrast, control, sig.level = .05, power = .9) {
    sigma <- summary(model)$sigma
    emm <- emmeans::emmeans(
      model,
      weights = "prop",
      specs = as.formula(paste0("trt.vs.ctrl ~ ", contrast)),
      ref = control,
      adjust = "None"
    )
    effect_size = summary(emm)$contrasts[1, ]$estimate
    power_res <- power.t.test(
      delta = effect_size,
      sd = sigma,
      sig.level = sig.level,
      power = power
    )
    results = list("estimates" = summary(model),
                   "ls_means" = emm$emmeans,
                   "contrasts" = emm$contrasts,
                   "power" = power_res
                   )
    return(results)
  }
