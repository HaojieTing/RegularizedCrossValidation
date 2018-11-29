
testMx2BCVSeqTestPower <- function() {
  source("tasks/mx2_t_test_power.R", encoding="UTF-8")
  task_config <- list(
    delta0 = 0.01,
    delta1 = 0.08,
    m = 40,
    alpha  = 0.05,
    beta   = 0.05,
    rho1   = 0.1,
    rho2   = 0.5,
    sigma  = 0.25,
    type   = "mx2cv_seq_significant_test",
    step   = 0.01,
    low_bound = -1,
    up_bound  = 1,
    plot = TRUE,
    plot_reflines = TRUE,
    loss_costs = list(
      "liber" = 0.6,
      "cons" = 0.4,
      "crit" = 1,
      "acc" = 0
    )
  )
  task_config <- mx2_t_test_power.task_config_validation(task_config)
  mx2_t_test_power.perform_task(task_config)
}


testDietterichSeqTestPower <- function() {
  source("tasks/mx2_t_test_power.R", encoding="UTF-8")
  task_config <- list(
    delta0 = 0.01,
    delta1 = 0.08,
    m = 40,
    alpha  = 0.05,
    beta   = 0.05,
    rho1   = 0.1,
    rho2   = 0.5,
    sigma  = 0.25,
    type   = "diet_mx2cv_seq_significant_test",
    step   = 0.01,
    low_bound = -1,
    up_bound  = 1,
    plot = TRUE,
    plot_reflines = TRUE,
    loss_costs = list(
      "liber" = 0.6,
      "cons" = 0.4,
      "crit" = 1,
      "acc" = 0
    )
  )
  task_config <- mx2_t_test_power.task_config_validation(task_config)
  mx2_t_test_power.perform_task(task_config)
}


testCombinedSeqTestPower <- function() {
  source("tasks/mx2_t_test_power.R", encoding="UTF-8")
  task_config <- list(
    delta0 = 0.01,
    delta1 = 0.08,
    m = 20,
    alpha  = 0.05,
    beta   = 0.05,
    rho1   = 0.1,
    rho2   = 0.5,
    sigma  = 0.25,
    type   = "combined_mx2cv_seq_t_test",
    step   = 0.01,
    low_bound = -1,
    up_bound  = 1,
    plot = TRUE,
    plot_reflines = TRUE,
    loss_costs = list(
      "liber" = 0.6,
      "cons" = 0.4,
      "crit" = 1,
      "acc" = 0
    )
  )
  task_config <- mx2_t_test_power.task_config_validation(task_config)
  mx2_t_test_power.perform_task(task_config)
}