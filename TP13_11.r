
library(ggplot2)

set.seed(42)
time <- seq(0, 24, by = 0.5)
K <- 1000  
r <- 0.4  


growth <- function(t, K, r) {
  K / (1 + (K - 1) * exp(-r * t))
}


bacteria_count <- growth(time, K, r) + rnorm(length(time), sd = 30)


data <- data.frame(time = time, bacteria_count = bacteria_count)


ggplot(data, aes(x = time, y = bacteria_count)) +
  geom_point(color = "blue", size = 3) +  
  geom_line(color = "green", linewidth = 1) +  
  stat_smooth(method = "nls", formula = y ~ K / (1 + (K - 1) * exp(-r * x)), 
              method.args = list(start = list(K = 1000, r = 0.4)),
              se = FALSE, color = "red") +  
  labs(title = "Croissance de la population bactérienne",
       x = "Temps (heures)",
       y = "Nombre de bactéries") +  
  theme_minimal() +  
  annotate("text", x = 15, y = 900, label = "Courbe de croissance", color = "red")  