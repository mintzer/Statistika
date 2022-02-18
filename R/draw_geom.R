draw_geom <- function(X, p, n=-1) {
  library(tidyverse)
  library(tidylog)
  library(ggplot2)
  data.frame(x = X, prob = dgeom(x = X, prob = p)) %>%
    mutate(Failures = ifelse(x == n, n, "other")) %>%
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
    scale_fill_manual(values=c("brown3","#48cbd1"))
}
