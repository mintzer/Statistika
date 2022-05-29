draw_geom <- function(n, p, type='pmf', marked_n=-1) {
  library(tidyverse)
  library(ggplot2)
  X <- 1:n
  if (type == 'pmf') {
    data.frame(x = X, prob = dgeom(x = X - 1, prob = p)) %>%
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
        x = "Trials Until First Success",
        y = "Probability") +
      guides(fill='none') +
      scale_fill_manual(values=c("brown3","#48cbd1"))
  }
  else if (type == 'cdf') {
    data.frame(x = X, prob = pgeom(q = X - 1, prob = p)) %>%
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
        x = "Trials Until First Success",
        y = "Probability") +
      guides(fill='none') +
      scale_fill_manual(values=c("brown3","#48cbd1"))
  }
}


draw_binom <- function(n, p, type='pmf', marked_n=-1) {
  library(tidyverse)
  library(ggplot2)
  X <- 0:n
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
        subtitle = paste0("Binominal (",n, ', ' ,p, ")"),
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
        subtitle = paste0("Binominal (",n, ', ' ,p, ")"),
        x = "Successes",
        y = "Probability") +
      guides(fill='none') +
      scale_fill_manual(values=c("#48cbd1","brown3"))
  }
}


dnorm_marked <- function(x, mean, sd, n){
  norm_marked <- dnorm(x, mean, sd)
  norm_marked[x > n + sd / 17] <- NA
  norm_marked[x < n - sd / 17] <- NA
  return(norm_marked)
}

pnorm_marked <- function(x, mean, sd, n){
  norm_marked <- pnorm(x, mean, sd)
  norm_marked[x > n] <- NA
  return(norm_marked)
}

draw_norm <- function(mean = 0, sd = 1, type='pdf', marked_n=-Inf) {
  library(tidyverse)
  library(ggplot2)
  xvalues <- data.frame(x = c(mean-3.5*sd, mean+3.5*sd)) %>% 
    mutate(mark = ifelse(x <= marked_n, marked_n, "other"))
  
  if (type == 'pdf') {
  g <- ggplot(xvalues, aes(x=x)) +
    stat_function(fun = dnorm, 
                  args = list(mean = mean,
                              sd = sd),
                  geom = "area", alpha = 0.9, fill='#48cbd1')+ 
    stat_function(fun = dnorm_marked,
                  args = list(mean = mean,
                              sd = sd, n=marked_n),
                    geom = "area", alpha = 0.9, fill='brown3') +
    geom_vline(xintercept = mean, linetype='dashed', color='brown3') +
      labs(
        title = "Probability Density Function",
        subtitle = paste0("Normal (", mean,', ', sd, ")"),
        x = "Value",
        y = "Density")
  if (marked_n != -Inf) {
    g <- g + geom_text(
      aes(label = round(dnorm(marked_n, mean, sd),2), 
          x = marked_n,
          y = dnorm(marked_n, mean, sd)),
      size = 3, color='brown3',
      vjust = 0
    ) 
  }
  
  }
  else if (type == 'cdf') {
    g <- ggplot(xvalues,aes(x=x)) +
      stat_function(fun = pnorm, 
                    args = list(mean = mean,
                                sd = sd),
                    geom = "area", alpha = 0.9, fill="#48cbd1")+
      stat_function(fun = pnorm_marked, 
                    args = list(mean = mean,
                                sd = sd, n=marked_n),
                    geom = "area", alpha = 0.9, fill="brown3")+
      geom_vline(xintercept = mean, linetype='dashed', color='brown3') +
      labs(title = "Cumulative Distribution Function",
            subtitle = paste0("Normal (", mean,', ', sd, ")"),
            x = "Value",
            y = "Probability")
    
    if (marked_n != -Inf) {
      g <- g + geom_text(
        aes(label = round(pnorm(marked_n, mean, sd),2), 
            x = marked_n,
            y = pnorm(marked_n, mean, sd)),
        size = 3, color='brown3',
        vjust = 0
      ) 
    }

    g <- g + guides(fill='none') +
      scale_fill_manual(values=c("brown3","#48cbd1"))

  }
  
  print(g)
}




dexp_marked <- function(x, rate, n, range){
  exp_marked <- dexp(x, rate)
  exp_marked[x > n + range/100] <- NA
  exp_marked[x < n - range/100] <- NA
  return(exp_marked)
}

pexp_marked <- function(x, rate, n){
  exp_marked <- pexp(x, rate)
  exp_marked[x > n] <- NA
  return(exp_marked)
}


draw_exp <- function(t, rate, type='pdf', marked_n=-Inf) {
  library(tidyverse)
  library(ggplot2)
  xvalues <- data.frame(x = c(0, t))
  
  if (type == 'pdf') {
    g <- ggplot(xvalues,aes(x=x)) +
      stat_function(fun = dexp, args = c(rate=rate),
                    geom = "area", fill = "#48cbd1")+
      stat_function(fun = dexp_marked, args = c(rate=rate, n=marked_n, range=t),
                    geom = "area", fill = "brown3")+
      labs(
        title = "Probability Density Function",
        subtitle = paste0("Exponential (", round(rate,2), ")"),
        x = "Value",
        y = "Density") +
      guides(fill='none') +
      scale_fill_manual(values=c("brown3","#48cbd1"))
    if (marked_n != -Inf) {
      g <- g + geom_text(
        aes(label = round(dexp(marked_n, rate),2), 
            x = marked_n,
            y = dexp(marked_n, rate)),
        size = 3, color='brown3',
        vjust = 0
      ) 
    }
  }
  else if (type == 'cdf') {
    g <- ggplot(xvalues,aes(x=x)) +
      stat_function(fun = pexp, args = c(rate=rate),
                    geom = "area", alpha=0.9, fill = "#48cbd1", size=2)+      
      stat_function(fun = pexp_marked, args = c(rate=rate, n=marked_n),
                    geom = "area", alpha=0.9, fill = "brown3")+
      labs(
        title = "Cumulative Distribution Function",
        subtitle = paste0("Exponential (", round(rate,2), ")"),
        x = "Value",
        y = "Propability") +
      guides(fill='none') +
      scale_fill_manual(values=c("brown3","#48cbd1"))
    scale_fill_manual(values=c("brown3","#48cbd1"))
    if (marked_n != -Inf) {
      g <- g + geom_text(
        aes(label = round(pexp(marked_n, rate),2), 
            x = marked_n,
            y = pexp(marked_n, rate)),
        size = 3, color='brown3',
        vjust = 0
      ) 
    }
  }
  print(g)
}


z_marked <- function(x, n1=-Inf,n2=Inf, mean=0, sd=1){
  z <- dnorm(x, mean, sd)
  z[x >= n1 & x <= n2] <- NA
  return(z)
}

draw_z <- function(alpha = 0.05, type = "two.sided") {
  library(tidyverse)
  library(ggplot2)
  marked_n = -Inf
  marked_n2 = Inf
  if (type == "two.sided") {
    marked_n = -qnorm(1-(alpha/2))
    marked_n2 = qnorm(1-(alpha/2))}
  else if (type == "right") {marked_n2 = qnorm(1-alpha)}
  else if (type == "left") {marked_n = -qnorm(1-alpha)}
  
  xvalues <- data.frame(x = c(-3.5, 3.5)) 
    g <- ggplot(xvalues, aes(x=x)) +
      stat_function(fun = dnorm, 
                    args = list(mean = 0,
                                sd = 1),
                    geom = "area", alpha = 0.9, fill='#48cbd1')+ 
      stat_function(fun = z_marked,
                    args = list(n1=marked_n, n2=marked_n2),
                    geom = "area", alpha = 0.9, fill='brown3') +
      geom_vline(xintercept = 0, linetype='dashed', color='brown3') +
      labs(
        title = "Z Distribuiton",
        subtitle = "Normal (0, 1)",
        x = "Value",
        y = "Density")
    if (type == "two.sided") {
      g <- g + geom_text(
            aes(label = paste0(as.character(alpha*50), '%'),
            x = marked_n - 0.75,
            y = dnorm(marked_n - 0.25)), size = 4, color='brown3', vjust = 0) + 
        geom_text(
              aes(label = paste0(as.character(alpha*50), '%'),
                  x = marked_n2 + 0.75,
                  y = dnorm(marked_n2 + 0.25)), size = 4, color='brown3', vjust = 0)
    }
    if (type == "right") {
      g <- g + geom_text(
        aes(label = paste0(as.character(alpha*100), '%'),
            x = marked_n2 + 0.85,
            y = dnorm(marked_n2 + 0.5)), size = 4, color='brown3', vjust = 0) 
    }
    if (type == "left") {
      g <- g + geom_text(
        aes(label = paste0(as.character(alpha*100), '%'),
            x = marked_n - 0.85,
            y = dnorm(marked_n - 0.5)), size = 4, color='brown3', vjust = 0) 
    }
  
    g <- g + guides(fill='none') +
      scale_fill_manual(values=c("brown3","#48cbd1"))
    
  
  print(g)
}



t_marked <- function(x, df, n1=-Inf,n2=Inf){
  t <- dt(x, df)
  t[x >= n1 & x <= n2] <- NA
  return(t)
}

draw_t <- function(alpha=0.05, df=29, type = "two.sided") {
  library(tidyverse)
  library(ggplot2)
  marked_n = -Inf
  marked_n2 = Inf
  if (type == "two.sided") {
    marked_n = -qt(1-(alpha/2), df)
    marked_n2 = qt(1-(alpha/2), df)}
  else if (type == "right") {marked_n2 = qt(1-alpha,df)}
  else if (type == "left") {marked_n = -qt(1-alpha,df)}
  
  xvalues <- data.frame(x = c(-4, 4)) 
  g <- ggplot(xvalues, aes(x=x)) +
    stat_function(fun = dt, 
                  args = list(df=df),
                  geom = "area", alpha = 0.9, fill='#48cbd1')+ 
    stat_function(fun = t_marked,
                  args = list(n1=marked_n, n2=marked_n2, df=df),
                  geom = "area", alpha = 0.9, fill='brown3') +
    geom_vline(xintercept = 0, linetype='dashed', color='brown3') +
    labs(
      title = "t Distribuiton",
      subtitle = paste(df,"degrees of freedom"),
      x = "Value",
      y = "Density")
  if (type == "two.sided") {
    g <- g + geom_text(
      aes(label = paste0(as.character(alpha*50), '%'),
          x = marked_n - 1,
          y = dt(marked_n - 0.5, df)), size = 4, color='brown3', vjust = 0) + 
      geom_text(
        aes(label = paste0(as.character(alpha*50), '%'),
            x = marked_n2 + 1,
            y = dt(marked_n2 + 0.5, df)), size = 4, color='brown3', vjust = 0)
  }
  if (type == "right") {
    g <- g + geom_text(
      aes(label = paste0(as.character(alpha*100), '%'),
          x = marked_n2 + 1,
          y = dt(marked_n2 + 0.5, df)), size = 4, color='brown3', vjust = 0) 
  }
  if (type == "left") {
    g <- g + geom_text(
      aes(label = paste0(as.character(alpha*100), '%'),
          x = marked_n - 1,
          y = dt(marked_n - 0.5, df)), size = 4, color='brown3', vjust = 0) 
  }
  
  g <- g + guides(fill='none') +
    scale_fill_manual(values=c("brown3","#48cbd1"))
  
  
  print(g)
}


draw_test_power_z = function(mu0, mu1, sd, alpha, type='power') {
  pacman::p_load(ggplot2, dplyr)
  marked_n = qnorm(1-alpha, mu0, sd)
  beta = pnorm(marked_n, mu1, sd)
  power = 1 - beta
  min_x = min(mu0, mu1) - (3.5*sd)
  max_x = max(mu0, mu1) + (3.5*sd)
  max_y = dnorm(mu0, mu0, sd)
  xvalues <- data.frame(x = c(min_x, max_x)) 
  g <- ggplot(xvalues, aes(x=x)) +
    stat_function(fun = dnorm, args = list(mean = mu0, sd = sd), 
                  geom = "area", alpha = 0.7, fill='#48cbd1', color='black') +
    stat_function(fun = dnorm, args = list(mean = mu1, sd = sd), 
                  geom = "area", alpha = 0.7, fill='#48cbd1', color='black') +
    geom_segment(x=mu0, xend=mu1, y=max_y, yend=max_y, linetype = 'dashed', color='brown3') +
    geom_text(x=(mu1 + mu0)/2, y=max_y, label=paste0("cohen's d\n", round((mu1-mu0)/sd,2)), color='brown3')

  
  if (type == 'power') {
      g = g + stat_function(fun = z_marked, args = list(mean = mu1, sd = sd, n2=marked_n), 
                    geom = "area", alpha = 0.7, fill='brown3') +
      geom_text(color='white', size=5, x=(marked_n + max_x) * (7/16), y = (max_y * 1/8), 
                label = paste0('1- \u03b2\n ', (round(power * 100, 2)), '%')) +
        geom_vline(xintercept = round(marked_n + 0.501), size=1.5)
  }
  else if (type == 'beta') {
    g = g + stat_function(fun = z_marked, args = list(mean = mu1, sd = sd, n1=marked_n), 
                          geom = "area", alpha = 0.7, fill='brown3') +
      geom_text(color='white', size=5, x=(mu1 - (3.5*sd) + marked_n) * (5/7), y = (max_y * 1/8), 
                label = paste0('\u03b2\n', (round(beta * 100, 2)), '%')) +
      geom_vline(xintercept = round(marked_n - 0.501), size=1.5)
  }
  g = g + geom_text(x = mu0, y=max_y*20/19, label = 'H0',size=5) +
    geom_text(x = mu1, y=max_y*20/19, label = 'H1',size=5) +
    ylim(c(0,max_y*15/14)) +
    labs(x = "Value", y = "Density") 
  print(g)
}


