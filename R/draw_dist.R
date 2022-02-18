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
        subtitle = paste("Geometric (", p, ")"),
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
        subtitle = paste("Geometric (", p, ")"),
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
        subtitle = paste("Binominal (", p, ")"),
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
        subtitle = paste("Binominal (", p, ")"),
        x = "Successes",
        y = "Probability") +
      guides(fill='none') +
      scale_fill_manual(values=c("#48cbd1","brown3"))
  }
}

