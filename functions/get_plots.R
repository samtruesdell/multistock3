

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
  
  tmean <- function(x) mean(x, trim = 0.1)
  
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
  
  
  w2plot <- round(seq(1, max(res$nweir)-1, length.out = 4))
  
  linePlotDat <- res %>%
    filter(nweir %in% w2plot) %>%
    mutate(nweir = factor(nweir)) %>%
    group_by(nstockSamp, nweir, scenario) %>%
    summarize(meanH = tmean(meanH),
              sdH = sqrt((1/n())^2 * sum(sdH^2)),
              meanRun = tmean(meanRun),
              sdRun = sqrt((1/n())^2 * sum(sdRun^2)),
              meanSmsyBias = tmean(meanSmsyBias),
              sdSmsyBias = sqrt((1/n())^2 * sum(sdSmsyBias^2)),
              pctOF = tmean(pctOF),
              sdpctOF = sqrt((1/n())^2 * sum(sdpctOF^2)),
              pctEX = tmean(pctEX),
              sdpctEX = sqrt((1/n())^2 * sum(sdpctEX^2)),
              meanE = tmean(meanE),
              sdE = NA,
              sdE = sqrt((1/n())^2 * sum(sdE^2)),
              .groups = 'drop')

  lpFun <- function(x, sdx){
    linePlotDat %>%
      select(all_of(c(x, sdx)), nstockSamp, nweir, scenario) %>%
      rename(x = 1, sdx = 2) %>%
      ggplot(aes(x = nstockSamp, y = x,
                 ymin = x - sdx, ymax = x + sdx,
                 group = nweir, col = nweir)) +
      geom_linerange(position = position_dodge(width = 0.25)) +
      geom_point(position = position_dodge(width = 0.25)) +
      geom_line(position = position_dodge(width = 0.25),
                linetype = 'dotted', alpha = 0.75) +
      facet_wrap(vars(scenario)) +
      ggtitle(x)
  }


  linePlotList <- list(
    lpFun(x = 'meanH', sdx = 'sdH'),
    lpFun(x = 'meanRun', sdx = 'sdRun'),
    lpFun(x = 'meanSmsyBias', sdx = 'sdSmsyBias'),
    lpFun(x = 'pctOF', sdx = 'sdpctOF'),
    lpFun(x = 'pctEX', sdx = 'sdpctEX'),
    lpFun(x = 'meanE', sdx = 'sdE')
  )


  # Marginal increase plots
  mDat <- res %>%
    mutate(naerial = nstockSamp - nweir)
  
  margList <- list()
  nstock <- min(mDat$nstockSamp):(max(mDat$nstockSamp)-1)
  for(i in 1:length(nstock)){
   

    baseSurveys <- expand.grid(nstockSamp = nstock[i], 
                               nweir = 1:nstock[i], 
                               naerial = 0:nstock[i]) %>%
      rowwise() %>%
      mutate(rs = sum(c_across(-nstockSamp))) %>%
      ungroup() %>%
      filter(rs == nstockSamp)
    
    base <- mDat %>%
      filter(nstockSamp %in% baseSurveys$nstockSamp,
             nweir %in% baseSurveys$nweir,
             naerial %in% baseSurveys$naerial) %>%
      group_by(scenario) %>%
      summarize(meanH = tmean(meanH), meanE = tmean(meanE), 
                meanRun = tmean(meanRun), meanSmsyBias = tmean(meanSmsyBias),
                meanpctEx = tmean(pctEX), meanpctOF = tmean(pctOF)) %>%
      mutate(Type = 'Base', .before = 1)
    
    margW <- mDat %>%
      filter(nstockSamp %in% (baseSurveys$nstockSamp + 1),
             nweir %in% (baseSurveys$nweir + 1),
             naerial %in% (baseSurveys$naerial)) %>%
      group_by(scenario) %>%
      summarize(meanH = tmean(meanH), meanE = tmean(meanE), 
                meanRun = tmean(meanRun), meanSmsyBias = tmean(meanSmsyBias),
                meanpctEx = tmean(pctEX), meanpctOF = tmean(pctOF)) %>%
      mutate(Type = 'addWeir', .before = 1)
    
    margA <- mDat %>%
      filter(nstockSamp %in% (baseSurveys$nstockSamp + 1),
             nweir %in% (baseSurveys$nweir ),
             naerial %in% (baseSurveys$naerial + 1)) %>%
      group_by(scenario) %>%
      summarize(meanH = tmean(meanH), meanE = tmean(meanE), 
                meanRun = tmean(meanRun), meanSmsyBias = tmean(meanSmsyBias),
                meanpctEx = tmean(pctEX), meanpctOF = tmean(pctOF)) %>%
      mutate(Type = 'addAerial', .before = 1)
    
    margList[[i]] <- bind_rows(base, margW, margA) %>%
      mutate(nStockSamp = i, .before = 3)
     
  }
  marg <- bind_rows(margList) %>%
    mutate(Type = factor(Type, levels = c('Base', 'addAerial', 'addWeir')))
  
  grps <- list(c('meanH', 'meanE'),
               c('meanpctEx', 'meanpctOF'),
               c('meanSmsyBias'))
  
  margPlots <- list()
  inc <- 1
  for(i in 1:max(mDat$scenario)){
    p1 <- marg %>%
      select(c(Type, scenario, nStockSamp, grps[[1]])) %>%
      filter(scenario == i) %>%
      pivot_longer(-c(Type, scenario, nStockSamp),
                   names_to = 'Metric', values_to = 'Value') %>%
      ggplot(aes(x = Metric, y = Value, fill = Type)) +
      geom_col(position = 'dodge') +
      facet_wrap(vars(nStockSamp)) +
      ggtitle(paste('Marginal increase from base average | Scenario =', i), 
              'Number of stocks sampled')
    
    p2 <- marg %>%
      select(c(Type, scenario, nStockSamp, grps[[2]])) %>%
      filter(scenario == i) %>%
      pivot_longer(-c(Type, scenario, nStockSamp),
                   names_to = 'Metric', values_to = 'Value') %>%
      ggplot(aes(x = Metric, y = Value, fill = Type)) +
      geom_col(position = 'dodge') +
      facet_wrap(vars(nStockSamp)) +
      ggtitle(paste('Marginal increase from base average | Scenario =', i), 
              'Number of stocks sampled')
    
    p3 <- marg %>%
      select(c(Type, scenario, nStockSamp, grps[[3]])) %>%
      filter(scenario == i) %>%
      pivot_longer(-c(Type, scenario, nStockSamp),
                   names_to = 'Metric', values_to = 'Value') %>%
      ggplot(aes(x = Metric, y = Value, fill = Type)) +
      geom_col(position = 'dodge') +
      facet_wrap(vars(nStockSamp)) +
      ggtitle(paste('Marginal increase from base average | Scenario =', i), 
              'Number of stocks sampled')
    
    margPlots[[inc]] <- p1
    margPlots[[inc+1]] <- p2
    margPlots[[inc+2]] <- p3
    inc <- inc + 3
  }
    
  
  
  

  h = 8
  w = 12
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
         plot = hTile, height = h, width = w)
  ggsave(filename = file.path(pth, 'runTile.png'),
         plot = runTile, height = h, width = w)
  ggsave(filename = file.path(pth, 'eTile.png'),
         plot = eTile, height = h, width = w)
  ggsave(filename = file.path(pth, 'pctOFTile.png'),
         plot = pctOFTile, height = h, width = w)
  ggsave(filename = file.path(pth, 'pctEXTile.png'),
         plot = pctEXTile, height = h, width = w)
  ggsave(filename = file.path(pth, 'SmsyBiasTile.png'),
         plot = SmsyBiasTile, height = h, width = w)
  
  return(list(linePlotList, margPlots))
  
}


