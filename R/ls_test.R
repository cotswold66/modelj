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
