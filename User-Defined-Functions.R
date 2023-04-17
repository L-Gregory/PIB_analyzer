# i. findMin(): Finds max PIB and the time when it occurs
findMin <- function(x, y){
  data.frame(min_Time = x, min_A = y)%>%
    mutate(lagA1 = stats::filter(min_A, rep(1/10, 10), sides = 2) %>% as.numeric()) %>%
    ##^ remove some noise prior to find the valley
    filter(lagA1 == min(lagA1, na.rm = T)) %>%
    select(-lagA1) #.$y
}

# ii. findA(): estimates assimilation at steady state in the light
#           Steady state is arbitrary considered as the last 50 observations, but
#           could be user controlled,
findA <- function(x, y){
  data.frame(Time = x, A = y) %>%
  filter(A > 0) %>% 
  summarize(steady_Assimilation = mean(tail(A, 50), na.rm = TRUE))
}

# iii. findRd(): estimates dark respiration at steady state in the dark.
#           Steady state is arbitrary considered as the last 50 observations, but
#           could be user controlled,
findRd <- function(x, y, LAST = 50, ...){
  data.frame(Time = x, A = y) %>%
    summarize(end_Burst = min(tail(Time, LAST), na.rm = TRUE),
              steady_darkRespiration = mean(tail(A, LAST), na.rm = TRUE))
}

# iiii. length_amount_Burst(): Estiamtes the length of the PIB and the amount of CO2 released during the burst.
#                             The function first identifies the points of the dataset that occured during the burst.
#                             This is, the observations below Rd and before steady state dark respiration.
#                             The steady state is considered to be the last 50 observations, but it can be user defined.
length_amount_Burst <- function(x, y, LAST = 50){
  data.frame(Time = x, A = y) %>%
    mutate(Rd = findRd(x, y, LAST) %>% .$steady_darkRespiration %>% as.numeric,
           Burst = Rd - A,
           start_PIB = min(Time[Burst > 0]),
           end_PIB = findRd(x, y, LAST) %>% .$end_Burst %>% as.numeric) %>%
    filter(Time > start_PIB & Time < end_PIB) %>%
    
    summarize(PIBlength_min = diff(range(Time)),
              # ^ Length of Burst: difference between last and first point
              PIB = sum(Burst))
  # ^ sum of all the differences between A and the linear trend
  
}

# iv. data_polygonBurst():  Creates the dataframe needed to represent the burst under Rd
#                           It's a closed loop (polygon) that goes (as time increases) through Rd
#                           and returns through A (as time decreases). That's why we reverse time and A
data_polygonBurst <- function(x, y, LAST = 50){
  data.frame(Time = x, A = y) %>%
    mutate(Rd = findRd(x, y, LAST) %>% .$steady_darkRespiration %>% as.numeric,
           Burst = Rd - A,
           start_PIB = min(Time[Burst > 0]),
           end_PIB = findRd(x, y, LAST) %>% .$end_Burst %>% as.numeric,
           lagA1 = stats::filter(A, rep(1/10, 10), sides = 2) %>% as.numeric()) %>%
    filter(Time > start_PIB & Time < end_PIB) %>%
    {rbind(select(., Time, ypos = Rd),
           select(., Time, ypos = lagA1) %>% mutate(Time = rev(Time), ypos = rev(ypos)) )}
  
}

# iiiii. plotPIB(): 
plotPIB <- function(x, y, LAST) {
  
  RD <- findRd(x, y, LAST)$steady_darkRespiration
  maxBurst.A <- findMin(x, y)$min_A
  maxBurst.time <- findMin(x, y)$min_Time
  polygon <- data_polygonBurst(x, y, LAST) 
  
  data.frame(Time = x, A = y) %>%
  mutate(Rd = findRd(x, y, LAST) %>% .$steady_darkRespiration %>% as.numeric,
         Burst = Rd - A,
         start_PIB = min(Time[Burst > 0]),
         end_PIB = findRd(x, y, LAST) %>% .$end_Burst %>% as.numeric,
         Burst.logical = Time > start_PIB & Time < end_PIB) %>%
  ggplot(aes(x = Time)) +
  geom_polygon(data = polygon, aes(y = ypos), fill = "#22A884FF", alpha = 0.45) +
  geom_point(aes(x = Time, y = A, fill = Burst.logical), 
             shape=21, alpha = 0.5, size = 2, show.legend = F) + 
  scale_fill_manual(values = c("#39568CFF", "#22A884FF")) +
  geom_line(aes(y = RD), color = "black", lty = 2) +
  geom_segment(aes(x = maxBurst.time, xend = maxBurst.time,
                                y = maxBurst.A, yend = RD),
                                col = "black", lwd = 1) +
  labs(title = " ", 
       x = expression(paste("Time")), 
       y = expression(paste(italic(A), " (", mu, "mol ", m^-2, " ", s^-1, ")" ))) +
  theme_bw(base_size = 20) 

}

# ii. 
PIBEstimation_table <- function(x, y, LAST) {
  params <- data.frame(x, y) %>%
  summarize(A = findA(x, y) %>% .$steady_Assimilation %>% as.numeric %>% round(2), 
            Rd = findRd(x, y, LAST) %>% .$steady_darkRespiration %>% as.numeric %>% round(3),
            Burst = length_amount_Burst(x, y, LAST) %>% .$PIB %>% as.numeric %>% round(1),
            Time = length_amount_Burst(x, y, LAST) %>% .$PIBlength_min %>% as.numeric %>% round(0),
            maxBurst = findMin(x, y) %>% .$min_A %>% as.numeric %>% round(2))
  
  colnames(params) <- c("Assimilation", "Dark Respiration", "Burst", "Time", "maxBurst")
  # units <- c("umol m-2 s-1", "umol m-2 s-1", "umol m-2", " ", "umol m-2 s-1")
  # paramsTable <- params %>% map2_dfc(units, ~set_units(.x, .y, mode = "standard"))
  return(params)
  }