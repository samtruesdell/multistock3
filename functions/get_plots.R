

get_plots <- function(res, pth){

  # Harvest by # of stocks sampled
  # hr_stocks <- res %>%
  #   group_by(egs, nstockSampTxt) %>%
  #   summarize(meanRun = mean(meanRun),
  #             meanH = mean(meanH),
  #             .groups = 'drop') %>%
  #   pivot_longer(-c(egs, nstockSampTxt), names_to = 'metric', values_to = 'value') %>%
  #   ggplot(aes(x = egs, y = value, color = metric, group = metric)) +
  #   geom_line() +
  #   facet_wrap('nstockSampTxt') +
  #   ggtitle('Harvest by number of stocks sampled')
  
  
  # Harvest by number of weirs
  # hr_weirs <- res %>%
  #   group_by(egs, nweirTxt) %>%
  #   summarize(meanRun = mean(meanRun),
  #             meanH = mean(meanH),
  #             .groups = 'drop') %>%
  #   pivot_longer(-c(egs, nweirTxt), names_to = 'metric', values_to = 'value') %>%
  #   ggplot(aes(x = egs, y = value, color = metric, group = metric)) +
  #   geom_line() +
  #   facet_wrap('nweirTxt') +
  #   ggtitle('Harvest and run size by # weirs')
  
  
  # Harvest and run size under the different options
  # hr_stocksWeirs <- res %>%
  #   group_by(egs, nweirTxt, nstockSampTxt) %>%
  #   summarize(meanRun = mean(meanRun),
  #             meanH = mean(meanH),
  #             .groups = 'drop') %>%
  #   pivot_longer(-c(egs, nweirTxt, nstockSampTxt), 
  #                names_to = 'metric', values_to = 'value') %>%
  #   ggplot(aes(x = egs, y = value, color = metric, group = metric)) +
  #   geom_line() +
  #   facet_grid(cols=vars(nstockSampTxt), rows=vars(nweirTxt)) +
  #   ggtitle('Harvest and run size')
  
  
  # Percent overfished
  # pover <- res %>%
  #   mutate(nstockSampTxt = factor(nstockSampTxt),
  #          nweirTxt = factor(nweirTxt)) %>%
  #   group_by(egs, nweirTxt, nstockSampTxt) %>%
  #   summarize(meanOF = mean(pctOF),
  #             .groups = 'drop') %>%
  #   pivot_longer(-c(egs, nweirTxt, nstockSampTxt), names_to = 'metric', values_to = 'value') %>%
  #   ggplot(aes(x = egs, y = value, linetype = nweirTxt, color = nstockSampTxt)) +
  #   geom_line() +
  #   ggtitle('Percent overfished')
  
  
  # Percent extirpated
  # pextr <- res %>%
  #   mutate(nstockSampTxt = factor(nstockSampTxt),
  #          nweirTxt = factor(nweirTxt)) %>%
  #   group_by(egs, nweirTxt, nstockSampTxt) %>%
  #   summarize(meanOF = mean(pctEX),
  #             .groups = 'drop') %>%
  #   pivot_longer(-c(egs, nweirTxt, nstockSampTxt), names_to = 'metric', values_to = 'value') %>%
  #   ggplot(aes(x = egs, y = value, linetype = nweirTxt, color = nstockSampTxt)) +
  #   geom_line() +
  #   ggtitle('Percent extirpated')
  
  
  # Plots of H and run by nstock and propWeir
  tileDat <- res %>%
    group_by(nweir, nstockSamp, egs, scenario) %>%
    summarize(meanRun = mean(meanRun),
              meanH = mean(meanH),
              meanE = mean(meanE),
              meanPctOF = mean(pctOF),
              meanPctEX = mean(pctEX),
              meanSmsyBias = mean(meanSmsyBias),
              .groups = 'drop') %>%
    group_by(nweir, nstockSamp, scenario) %>%
    summarize(maxMeanRun = meanRun[which.max(meanH)],
              maxMeanH = meanH[which.max(meanH)],
              maxMeanE = meanE[which.max(meanH)],
              maxMeanPctOF = meanPctOF[which.max(meanH)],
              maxMeanPctEX = meanPctEX[which.max(meanH)],
              maxmeanSmsyBias = meanSmsyBias[which.max(meanH)],
              .groups = 'drop') %>%
    group_by(scenario) %>%
    mutate(maxMeanRunPct = maxMeanRun / max(maxMeanRun),
           maxMeanHPct = maxMeanH / max(maxMeanH),
           maxMeanEPct = maxMeanE / max(maxMeanE))


  tileColN <- scale_fill_gradient(low = 'gray20', high = 'gold')
  tileColP <- scale_fill_gradient(low = 'green', high = 'firebrick1')
    
  hTile <- tileDat %>%
    ggplot(aes(x = nstockSamp, y = nweir, fill = maxMeanHPct)) +
    geom_tile() +
    tileColN +
    ggtitle('Harvest') +
    facet_wrap(~scenario)
  
  runTile <- tileDat %>%
    ggplot(aes(x = nstockSamp, y = nweir, fill = maxMeanRunPct)) +
    geom_tile() +
    tileColN +
    ggtitle('Run size') +
    facet_wrap(~scenario)
  
  eTile <- tileDat %>%
    ggplot(aes(x = nstockSamp, y = nweir, fill = maxMeanEPct)) +
    geom_tile() +
    tileColN +
    ggtitle('Escapement') +
    facet_wrap(~scenario)
  
  pctOFTile <- tileDat %>%
    ggplot(aes(x = nstockSamp, y = nweir, fill = maxMeanPctOF)) +
    geom_tile() +
    tileColP +
    ggtitle('Percent overfished') +
    facet_wrap(~scenario)
  
  pctEXTile <- tileDat %>%
    ggplot(aes(x = nstockSamp, y = nweir, fill = maxMeanPctEX)) +
    geom_tile() +
    tileColP +
    ggtitle('Percent extirpated') +
    facet_wrap(~scenario)

  SmsyBiasTile <- tileDat %>%
    ggplot(aes(x = nstockSamp, y = nweir, fill = maxmeanSmsyBias)) +
    geom_tile() +
    tileColP +
    ggtitle('Percent bias in Smsy (mean trimmed by 10%)') +
    facet_wrap(~scenario)
    
  
  
  # Average Smsy bias
  meanSmsyBias <- res %>%
    ggplot(aes(y = meanSmsyBias, x=nstockSampTxt, fill = factor(nweirTxt))) +
    geom_col(position = 'dodge') +
    ggtitle('Average Smsy bias (trim = 0.1)')
  
  
  # Average harvest
  meanH <- res %>%
    group_by(nweirTxt, nstockSampTxt) %>%
    summarize(meanH = mean(meanH),
              .groups = 'drop') %>%
    ggplot(aes(y = meanH, x=nstockSampTxt, fill = factor(nweirTxt))) +
    geom_col(position = 'dodge') +
    ggtitle('Average harvest (trimmed mean)')
  
  # Average run
  meanRun <- res %>%
    group_by(nweirTxt, nstockSampTxt) %>%
    summarize(meanRun = mean(meanRun),
              .groups = 'drop') %>%
    ggplot(aes(y = meanRun, x=nstockSampTxt, fill = factor(nweirTxt))) +
    geom_col(position = 'dodge') +
    ggtitle('Average run size (trimmed mean)')
  

  
  # ggsave(filename = file.path(pth, 'hr_stocks.png'),
  #        plot = hr_stocks, height = 7, width = 12, unit = 'in')
  # ggsave(filename = file.path(pth, 'hr_weirs.png'),
  #        plot = hr_weirs, height = 7, width = 12, unit = 'in')
  # ggsave(filename = file.path(pth, 'hr_stocksWeirs.png'),
  #        plot = hr_stocksWeirs, height = 15, width = 15, unit = 'in')
  # ggsave(filename = file.path(pth, 'pover.png'),
  #        plot = pover)
  # ggsave(filename = file.path(pth, 'pextr.png'),
  #        plot = pextr)
  # ggsave(filename = file.path(pth, 'meanSmsyBias.png'),
  #        plot = meanSmsyBias)
  # ggsave(filename = file.path(pth, 'meanH.png'),
  #        plot = meanH)
  # ggsave(filename = file.path(pth, 'meanRun.png'),
  #        plot = meanRun)
  ggsave(filename = file.path(pth, 'hTile.png'),
         plot = hTile)
  ggsave(filename = file.path(pth, 'runTile.png'),
         plot = runTile)
  ggsave(filename = file.path(pth, 'eTile.png'),
         plot = eTile)
  ggsave(filename = file.path(pth, 'pctOFTile.png'),
         plot = pctOFTile)
  ggsave(filename = file.path(pth, 'pctEXTile.png'),
         plot = pctEXTile)
  ggsave(filename = file.path(pth, 'SmsyBiasTile.png'),
         plot = SmsyBiasTile)
  
  
}


