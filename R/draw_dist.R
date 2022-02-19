draw_geom <- function(n, p, type='pmf', marked_n=-1) {
  library(tidyverse)
  library(ggplot2)
  X <- 0:n
  if (type == 'pmf') {
    data.frame(x = X, prob = dgeom(x = X, prob = p)) %>%
      mutate(Failures = ifelse(x == marked_n, marked_n, "other")) %>%
      ggplot(aes(x = factor(x), y = prob, fill = Failures)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,2), y = prob + 0.01),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      labs(
        title = "Probability Mass Function",
        subtitle = paste0("Geometric (", p, ")"),
        x = "Failures before first success",
        y = "Probability") +
      guides(fill='none') +
      scale_fill_manual(values=c("#48cbd1","brown3"))
  }
  else if (type == 'cdf') {
    data.frame(x = X, prob = pgeom(q = X, prob = p)) %>%
      mutate(Failures = ifelse(x <= marked_n, marked_n, "other")) %>%
      ggplot(aes(x = factor(x), y = prob, fill = Failures)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,2), y = prob + 0.01),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      labs(
        title = "Cumulative Distribution Function",
        subtitle = paste0("Geometric (", p, ")"),
        x = "Failures before first success",
        y = "Probability") +
      guides(fill='none') +
      scale_fill_manual(values=c("#48cbd1","brown3"))
  }
}


draw_binom <- function(n, p, type='pmf', marked_n=-1) {
  library(tidyverse)
  library(ggplot2)
  X <- 1:n
  if (type == 'pmf') {
    data.frame(x = X, prob = dbinom(x = X, size=n, prob = p)) %>%
      mutate(Success = ifelse(x == marked_n, marked_n, "other")) %>%
      ggplot(aes(x = factor(x), y = prob, fill = Success)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,2), y = prob + 0.01),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      labs(
        title = "Probability Mass Function",
        subtitle = paste0("Binominal (", p, ")"),
        x = "Successes",
        y = "Probability") +
      guides(fill='none') +
      scale_fill_manual(values=c("brown3","#48cbd1"))
  }
  else if (type == 'cdf') {
    data.frame(x = X, prob = pbinom(q = X, size=n, prob = p)) %>%
      mutate(Success = ifelse(x <= marked_n, marked_n, "other")) %>%
      ggplot(aes(x = factor(x), y = prob, fill = Success)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,2), y = prob + 0.01),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      labs(
        title = "Cumulative Distribution Function",
        subtitle = paste0("Binominal (", p, ")"),
        x = "Successes",
        y = "Probability") +
      guides(fill='none') +
      scale_fill_manual(values=c("#48cbd1","brown3"))
  }
}


draw_norm <- function(mean, sd) {
  library(tidyverse)
  library(ggplot2)
  xvalues <- data.frame(x = c(mean-3.5*sd, mean+3.5*sd))
  
  ggplot(xvalues, aes(x=x)) +
    stat_function(fun = dnorm, 
                  args = list(mean = mean,
                              sd = sd),
                  geom = "area", fill = "#48cbd1", alpha = 0.9)+
    geom_vline(xintercept = mean, linetype='dashed', color='brown3') +
      labs(
        title = "Probability Density Function",
        subtitle = paste0("Normal (", mean,', ', sd, ")"),
        x = "Value",
        y = "Density") +
      guides(fill='none') +
      scale_fill_manual(values=c("brown3","#48cbd1"))
}


draw_exp <- function(t, rate, type='pdf') {
  library(tidyverse)
  library(ggplot2)
  xvalues <- data.frame(x = c(0, t))
  
  if (type == 'pdf') {
    ggplot(xvalues,aes(x=x)) +
      stat_function(fun = dexp, args = c(rate=rate),
                    color = "#48cbd1", size=2)+
      labs(
        title = "Probability Density Function",
        subtitle = paste0("Exponential (", round(rate,2), ")"),
        x = "Value",
        y = "Density") +
      guides(fill='none') +
      scale_fill_manual(values=c("brown3","#48cbd1"))
  }
  else if (type == 'cdf') {
    ggplot(xvalues,aes(x=x)) +
      stat_function(fun = pexp, args = c(rate=rate),
                    color = "#48cbd1", size=2)+      labs(
        title = "Cumulative Distribution Function",
        subtitle = paste0("Exponential (", round(rate,2), ")"),
        x = "Value",
        y = "Propability") +
      guides(fill='none') +
      scale_fill_manual(values=c("brown3","#48cbd1"))
  }
}
