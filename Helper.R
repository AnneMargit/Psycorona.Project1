# Functies enzo

# Corona emoties descriptives berekenen
emotion.descriptives <- function(coronadataBSfill) {
  coronadataBSfill <- as_tibble(coronadataBSfill)
  head(coronadataBSfill, 5)
  
  countrytableaff <- coronadataBSfill %>%
    dplyr::filter(!is.na(country)) %>% 
    dplyr::group_by(country) %>%
    dplyr::summarise_each(funs(mean(., na.rm=TRUE), sd(., na.rm=TRUE)), 
                          affAnx, affBor, affCalm, affContent, affDepr, affEnerg, 
                          affExc, affNerv, affExh, affInsp, affRel, happy, N)
  
  ordered <- countrytableaff[c("country", "affAnx_mean", "affAnx_sd", "affBor_mean", "affBor_sd", 
                               "affCalm_mean", "affCalm_sd", "affContent_mean", "affContent_sd", "affDepr_mean", 
                               "affDepr_sd", "affEnerg_mean", "affEnerg_sd", 
                               "affExc_mean", "affExc_sd", "affNerv_mean", "affNerv_sd", 
                               "affExh_mean", "affExh_sd", "affInsp_mean", "affInsp_sd", "affRel_mean",
                               "affRel_sd", "happy_mean", "happy_sd")] 
  ordered[,-1] <- round(ordered[,-1], digits=2)
  return(ordered)
}

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
