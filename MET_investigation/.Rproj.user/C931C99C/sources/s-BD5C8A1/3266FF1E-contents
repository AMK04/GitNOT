source('./sourceLibraries.R')


activities <- read.csv2('C:\\Users\\a.kinkel\\Desktop\\2019-03-13_example_classification_17213_raw.csv', stringsAsFactors = FALSE)
activities <- activities %>% mutate(start = as.POSIXct(start))
activities <- addInterval(activities, 'day')

weight <- 79
height <- 186
age    <- 24

rmrFactor <- (9.99*weight + 6.25*height - 4.92*age + 5) / (60*60*24)

met_table <- data.frame(class = c('walking','standing', 'lying', 'shuffling', 'cycling', 'sitting', 'stair_walking', 'not_worn'),
                        met_factor = c(2.5, 1.3, 1.0, 1.8, 6.8, 1.3, 4.0, 0.0),
                        stringsAsFactors = F)



activities_ext <- left_join(activities, met_table, by = 'class') %>% 
  mutate(MET_table = met_factor * (duration / 60),
         TEE_McR   = (tee * duration) / 4186.8,
         TEE_table = (((duration / 60) * 3.5 * weight) / 1000) * 5 * met_factor,
         AEE_McR   = (aee * duration) / 4186.8,
         AEE_table = TEE_table - (rmrFactor * duration))



McR_vs_table_interval <- activities_ext %>% 
  group_by(interval, class) %>%  
  summarize(sum_dur          = sum(duration)/60,
            MET_factor_McR   = weighted.mean(met, duration),
            MET_factor_table = sum(MET_table) / sum_dur,
            MET_McR          = weighted.mean(met, duration) * sum_dur,
            MET_table        = sum(MET_table),
            TEE_McR          = sum(TEE_McR),
            TEE_table        = sum(TEE_table),
            AEE_McR          = sum(AEE_McR),
            AEE_table        = sum(AEE_table)) %>%
  mutate(MET_factor_diff = MET_factor_McR - MET_factor_table,
         MET_diff        = MET_McR - MET_table,
         TEE_diff        = TEE_McR - TEE_table,
         AEE_diff        = AEE_McR - AEE_table) %>%
  select(-sum_dur)

McR_vs_table_class <- activities_ext %>% 
  group_by(class) %>%  
  summarize(sum_dur          = sum(duration)/60,
            MET_factor_McR   = weighted.mean(met, duration),
            MET_factor_table = sum(MET_table) / sum_dur,
            MET_McR          = weighted.mean(met, duration) * sum_dur,
            MET_table        = sum(MET_table),
            TEE_McR          = sum(TEE_McR),
            TEE_table        = sum(TEE_table),
            AEE_McR          = sum(AEE_McR),
            AEE_table        = sum(AEE_table)) %>%
  mutate(MET_factor_diff = MET_factor_McR - MET_factor_table,
         MET_diff        = MET_McR - MET_table,
         TEE_diff        = TEE_McR - TEE_table,
         AEE_diff        = AEE_McR - AEE_table) %>%
  select(-sum_dur)


# Overview 
totalMET_table <- sum(activities_ext$MET_table)
totalMET_McR   <- sum(McR_vs_table$MET_McR)
totalTEE_table <- sum(activities_ext$TEE_table)
totalTEE_McR   <- sum(activities_ext$tee * activities_ext$duration) / 4186.8
totalAEE_table <- sum(activities_ext$AEE_table)
totalAEE_McR   <- sum(activities_ext$aee * activities_ext$duration) / 4186.8




# Make graphs
custom.col <- c("#94C93D", "#156332", "#8B684E", 
                "#D4D4D4", "#585C5C", "#333333", "#CAE49E", "#8AB199")

ggplot(McR_vs_table_interval, aes(x = interval, y = AEE_diff, color = class, size = TEE_diff)) + # class was interval
                geom_point() + 
                facet_wrap(~interval, scales = "free") +
                scale_color_manual(values = custom.col) + 
                labs (title = "AEE difference (McR vs. METs table) per day (interval)", subtitle = "Size is indication TEE difference", 
                       y = "AEE difference (kcal)", x = "interval(day)") + 
                 theme_classic()

ggplot(McR_vs_table_class) +
     geom_col(aes(x = class, y = MET_factor_McR, fill = class)) +
     scale_fill_manual(values = setNames(custom.col, c("cycling", "lying", "not_worn", "shuffling", "sitting", "stair_walking", "standing", "walking"))) +
     geom_point(aes(x = class, y = MET_factor_table, size = MET_factor_diff), color = 'deepskyblue4') + 
     theme_classic()
 
ggplot(activities, aes(x = mi, y = aee, color = class)) +
  geom_point() + 
  theme_classic()

McR_vs_table %>% gather('key','value', ends_with('McR')) %>% data.frame()

