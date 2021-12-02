

get_recStats <- function(rec){

  scen <- sort(unique(rec$scenario))
  ns <- sort(unique(rec$nstockSamp))
  
  rec <- rModRecord %>%
    mutate(id = paste(scenario, s2s, egs, sep = 'x'),
           scenario2 = paste('Scenario', scenario))
  
  ### Number of stocks sampled
  
  # Residual boxplot
  boxNStock <- rec %>%
    group_by(nstockSamp) %>%
    mutate(stdResid = resid / sd(resid)) %>%
    filter(stdResid > -10 & stdResid < 10) %>%
    ggplot(aes(x = nstockSamp, group = nstockSamp, y = stdResid)) +
    geom_boxplot() +
    facet_wrap(vars(scenario2))
    
  ### Number weirs, assuming sampling all stocks
    
  boxNWeir <- rec %>%
    group_by(nweir) %>%
    mutate(stdResid = resid / sd(resid)) %>%
    filter(stdResid > -10 & stdResid < 10) %>%
    ggplot(aes(x = nweir, group = nweir, y = stdResid)) +
    geom_boxplot() +
    facet_wrap(vars(scenario2), ncol = 1) +
    xlab('Number of weirs') +
    ylab('Standardized residuals (resid / sd)')
  
  
  # Randomly selected plots by number weirs sampled (assuming all sampled)
  weirRecPlot <- list()
  inc <- 1
  for(i in 1:length(scen)){
    for(j in 1:length(ns)){
      recSelect <- rec %>%
        filter(scenario == scen[i],
               nstockSamp == j) %>%
        group_by(nweir, id) %>% 
        nest() %>%
        group_by(nweir) %>%
        slice_sample(n = 4) %>%
        ungroup() %>%
        unnest(cols = c(data)) %>%
        select(nweir, id, y, R_lm, S_lm, aprime, bprime) %>%
        group_by(nweir) %>%
        mutate(plotID = as.numeric(factor(id))) %>%
        ungroup()
      
      estData <- recSelect %>% select(id, nweir, aprime, bprime) %>%
        slice(rep(1:n(), each = 100)) %>%
        mutate(S_lm = rep(seq(0, max(recSelect$S_lm), length.out = 100),
                          times = nrow(recSelect))) %>%
        group_by(nweir) %>%
        mutate(plotID = as.numeric(factor(id)),
               R_lm = ricker(alpha = aprime, beta = bprime, S = S_lm))
      
      weirRecPlot[[inc]] <- recSelect %>%
        ggplot(aes(x = S_lm, y = R_lm)) +
        geom_point() +
        geom_line(data = estData) +
        facet_grid(rows = vars(nweir), cols = vars(plotID),
                   scales = 'free_y') +
        # facet_wrap(vars(nweir), ncol = 4) +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              plot.tag = element_text(angle=-90),
              plot.tag.position = c(1.02, 0.5),
              plot.margin = unit(c(5.5, 25, 5.5, 5.5), "points")) +
        ggtitle(paste('N stocks =', j,
                      '| Scenario =', scen[i]), '(4 random samples)') +
        labs(tag = 'Number of weirs sampled')
      inc <- inc + 1
    }
  }
  
  return(list(boxNStock = boxNStock,
              boxNWeir = boxNWeir,
              weirRecPlot = weirRecPlot))
}










# Randomly selected plots by n stocks sampled
# nstockRecPlot <- list()
# for(i in 1:length(scen)){
#   
#   recSelect <- rec %>%
#     filter(scenario == scen[i]) %>%
#     group_by(nstockSamp, id) %>% 
#     nest() %>%
#     group_by(nstockSamp) %>%
#     slice_sample(n = 4) %>%
#     ungroup() %>%
#     unnest(cols = c(data)) %>%
#     select(nstockSamp, id, y, R_lm, S_lm, aprime, bprime) %>%
#     group_by(nstockSamp) %>%
#     mutate(plotID = as.numeric(factor(id))) %>%
#     ungroup()
#   
#   estData <- recSelect %>% select(id, nstockSamp, aprime, bprime) %>%
#     slice(rep(1:n(), each = 100)) %>%
#     mutate(S_lm = rep(seq(0, max(recSelect$S_lm), length.out = 100),
#                       times = nrow(recSelect))) %>%
#     group_by(nstockSamp) %>%
#     mutate(plotID = as.numeric(factor(id)),
#            R_lm = ricker(alpha = aprime, beta = bprime, S = S_lm))
#   
#   nstockRecPlot[[i]] <- recSelect %>%
#     ggplot(aes(x = S_lm, y = R_lm)) +
#     geom_point() +
#     geom_line(data = estData) +
#     facet_grid(rows = vars(nstockSamp), cols = vars(plotID),
#                scales = 'free_y') +
#     # facet_wrap(vars(nstockSamp), ncol = 4) +
#     theme(axis.text = element_blank(),
#           axis.ticks = element_blank()) +
#     ggtitle(paste('Scenario =', scen[i]))
# }

