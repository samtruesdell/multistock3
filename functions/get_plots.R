

get_plots <- function(res, pth){
  
  # Harvest by # of stocks sampled
  hr_stocks <- res %>%
    group_by(egs, nstockSamp) %>%
    summarize(meanRun = mean(meanRun),
              meanH = mean(meanH),
              .groups = 'drop') %>%
    pivot_longer(-c(egs, nstockSamp), names_to = 'metric', values_to = 'value') %>%
    ggplot(aes(x = egs, y = value, color = metric, group = metric)) +
    geom_line() +
    facet_wrap('nstockSamp') +
    ggtitle('Harvest by number of stocks sampled')
  
  
  # Harvest by number of weirs
  hr_weirs <- res %>%
    group_by(egs, nweir) %>%
    summarize(meanRun = mean(meanRun),
              meanH = mean(meanH),
              .groups = 'drop') %>%
    pivot_longer(-c(egs, nweir), names_to = 'metric', values_to = 'value') %>%
    ggplot(aes(x = egs, y = value, color = metric, group = metric)) +
    geom_line() +
    facet_wrap('nweir') +
    ggtitle('Harvest and run size by # weirs')
  
  
  # Harvest and run size under the different options
  hr_stocksWeirs <- res %>%
    group_by(egs, nweir, nstockSamp) %>%
    summarize(meanRun = mean(meanRun),
              meanH = mean(meanH),
              .groups = 'drop') %>%
    pivot_longer(-c(egs, nweir, nstockSamp), names_to = 'metric', values_to = 'value') %>%
    ggplot(aes(x = egs, y = value, color = metric, group = metric)) +
    geom_line() +
    facet_grid(cols=vars(nstockSamp), rows=vars(nweir)) +
    ggtitle('Harvest and run size')
  
  
  # Percent overfished
  pover <- res %>%
    mutate(nstockSamp = factor(nstockSamp),
           nweir = factor(nweir)) %>%
    group_by(egs, nweir, nstockSamp) %>%
    summarize(meanOF = mean(pctOF),
              .groups = 'drop') %>%
    pivot_longer(-c(egs, nweir, nstockSamp), names_to = 'metric', values_to = 'value') %>%
    ggplot(aes(x = egs, y = value, linetype = nweir, color = nstockSamp)) +
    geom_line() +
    ggtitle('Percent overfished')
  
  
  # Percent extirpated
  pextr <- res %>%
    mutate(nstockSamp = factor(nstockSamp),
           nweir = factor(nweir)) %>%
    group_by(egs, nweir, nstockSamp) %>%
    summarize(meanOF = mean(pctEX),
              .groups = 'drop') %>%
    pivot_longer(-c(egs, nweir, nstockSamp), names_to = 'metric', values_to = 'value') %>%
    ggplot(aes(x = egs, y = value, linetype = nweir, color = nstockSamp)) +
    geom_line() +
    ggtitle('Percent extirpated')
  
  
  # Average Smsy
  meanSmsy <- res %>%
    ggplot(aes(y = meanSmsy, x=nstockSamp, fill = factor(nweir))) +
    geom_col(position = 'dodge') +
    ggtitle('Average Smsy')
  
  
  # Average harvest
  meanH <- res %>%
    group_by(nweir, nstockSamp) %>%
    summarize(meanH = mean(meanH),
              .groups = 'drop') %>%
    ggplot(aes(y = meanH, x=nstockSamp, fill = factor(nweir))) +
    geom_col(position = 'dodge') +
    ggtitle('Average harvest (trimmed mean)')
  
  # Average run
  meanRun <- res %>%
    group_by(nweir, nstockSamp) %>%
    summarize(meanRun = mean(meanRun),
              .groups = 'drop') %>%
    ggplot(aes(y = meanRun, x=nstockSamp, fill = factor(nweir))) +
    geom_col(position = 'dodge') +
    ggtitle('Average run size (trimmed mean)')
  

  
  ggsave(filename = file.path(pth, 'hr_stocks.png'),
         plot = hr_stocks, height = 7, width = 12, unit = 'in')
  ggsave(filename = file.path(pth, 'hr_weirs.png'),
         plot = hr_weirs, height = 7, width = 12, unit = 'in')
  ggsave(filename = file.path(pth, 'hr_stocksWeirs.png'),
         plot = hr_stocksWeirs, height = 7, width = 12, unit = 'in')
  ggsave(filename = file.path(pth, 'pover.png'),
         plot = pover)
  ggsave(filename = file.path(pth, 'pextr.png'),
         plot = pextr)
  ggsave(filename = file.path(pth, 'meanSmsy.png'),
         plot = meanSmsy)
  ggsave(filename = file.path(pth, 'meanH.png'),
         plot = meanH)
  ggsave(filename = file.path(pth, 'meanRun.png'),
         plot = meanRun)
  
  
}


