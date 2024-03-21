## Bland-Altman (Mean-Difference) Plot code
## Input:
##  x      comparison variable
##  ref    reference variable
##  data  (optional) data frame
##
##  Output:
##    A list containing
##      -  a data frame containing raw values, means, and differences
##      -  summary statistics about the differences
##      -  histogram of the differences
##      -  Bland-Altman plot
##      -  Shapiro-Wilks test of normality of the differences
##      -  regression output (differences ~ means)

ba <- function(x, ref, data = NULL) {
  if (length(x) > 1 & length(ref) > 1) {
    if (!is.null(data)){
      warning("data argument ignored due to supplying values to ref and x")
    }
    d <- tibble(x, ref) %>%
      rowwise() %>%
      mutate(M = mean(c(x, ref)),
             D = ref - x) %>%
      ungroup()
    
    cap <- NULL
  }
  
  if (is.character(x) & is.character(ref) & !is.null(data)) {
    d <- data %>%
      select(x = !!x,ref = !!ref) %>%
      rowwise() %>%
      mutate(M = mean(c(x, ref)),
             D = ref - x) %>%
      ungroup()
    
    cap <- sprintf("Ref: %s\nX: %s",ref,x)
  }
  
  diffs <- d %>%
    summarise(Mean = mean(D), SD = sd(D), ucl95 = Mean + 1.96*SD, lcl95 = Mean - 1.96*SD)
  
  shapiro = broom::tidy(shapiro.test(d$D))
  
  reg <- lm(D ~ M, data = d) |> broom::tidy()
  
  diff_hist <- ggplot(d, aes(x = D))+
    geom_histogram(bins = 50) +
    geom_vline(xintercept = diffs$Mean) +
    geom_vline(xintercept = diffs$ucl95, linetype = 'dashed') +
    geom_vline(xintercept = diffs$lcl95, linetype = 'dashed') +
    geom_label(data = diffs, aes(x= Mean, y = -5, label = sprintf("Mean\n%.2f",Mean)))+
    geom_label(data = diffs, aes(x= ucl95, y = -5, label = sprintf("+CI(95%%)\n%.2f",ucl95)))+
    geom_label(data = diffs, aes(x= lcl95, y = -5, label = sprintf("-CI(95%%)\n%.2f",lcl95))) +
    labs(x="Difference", y = "Count", title="Histogram of Differences (Ref - X)")
  
  p <- ggplot(d, aes(x = M, y = D)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ x) +
    geom_hline(data = diffs, aes(yintercept = Mean)) +
    geom_hline(data = diffs, aes(yintercept = ucl95), linetype ='dashed') +
    geom_hline(data = diffs, aes(yintercept = lcl95), linetype = 'dashed') +
    labs(x = "Mean(Ref, X)", y = "Difference (Ref - X)", 
         title = "Mean-Difference Calibration Plot", caption = cap) +
    theme_minimal()
  
  ## note: shapiro test is for the normality of the differences
  return(list(
    "plot" = p,
    "data" = d,
    "diffs" = diffs,
    "shapiro" = shapiro,
    "diff_hist" = diff_hist,
    "regression" = reg
  ))
}
b <- ba(x = "raga_tot_sev", ref = "raga_tot_frq",data = raga)
# b <- ba(x = raga$raga_tot_sev, ref = raga$raga_tot_frq, data = raga)
b$plot
