# plot right singular

source("https://raw.githubusercontent.com/toddwschneider/ballr/master/plot_court.R")
source("https://raw.githubusercontent.com/toddwschneider/ballr/master/court_themes.R")
plot_court() # created the court_points object we need
court_points <- court_points %>% mutate_if(is.numeric,~.*10)

# right singular

plot_structure = function(v, component){
  
  print(component) # si saco el print no funciona (??????????)
 
  ggplot(data ={{v}}, aes(x = biny*20, y = binx*20)) + # invierto coord para alinear con plot de fondo
    geom_path(data = court_points,
              aes(x = x + 250, y = y, group = desc), 
              color = "black") + 
    geom_point(aes(color = scales::rescale(abs({{component}}),to = c(0,10)))) +
    scale_color_distiller(palette = "Spectral") +
    theme_dark() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position = "none") + 
    theme(axis.title.x =  element_blank(),
          axis.title.y =  element_blank()) + 
    labs(color = "Shot frequency")
  
}





rank_svd = function(var){    
  print(var)
  svdu_ranked = svdu %>%
    arrange({{var}}) %>%
    head(100)
}


rank_nmf = function(var){    
  print(var)
  nmf_ranked = player_nmf %>%
    arrange(desc({{var}}))
}
