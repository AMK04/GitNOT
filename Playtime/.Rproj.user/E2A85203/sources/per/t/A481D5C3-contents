source('./sourceLibraries.R')
# library('tidyverse')

mm_classification <- read.csv2("P:\\20200212_Playtime\\20200212_Playtime_classification_day.csv")

mm_distinct  <- mm_classification %>% 
  rename_(.dots = setNames(names(.), gsub("subject_", "", names(.)))) %>%
  distinct(measurement_id, interval, .keep_all = TRUE) 

# GGZE
# Filter
# Delete code  5 ivm meting van 1 dag, NA as zero in WALKING_steps
mm_ggze <- mm_distinct %>%
  filter(grepl('Linda Burgmans', user_name), grepl('^[13]{1}$', visit_name), !grepl('^[05]{2}$', code)) %>%
  arrange (measurement_id, interval) %>%
  # mutate(WALKING_steps = coalesce(WALKING_steps, 0))
  mutate(WALKING_steps = coalesce(WALKING_steps, 0L)) # L achter 0 @McR
  

# Number of days measured per measurement_id
mm_ggze %>% group_by(measurement_id, code) %>% summarise(n = n()) %>% arrange(n)

# Wearing compliance
mm_ggze_compl <- mm_ggze %>% 
  group_by(measurement_id, code, interval, visit_name) %>% 
  summarise(mean_worn = mean(PERC_day_worn)) %>% 
  mutate(n_days = n()) %>%
  arrange(code) 
 
custom.col <- c("#94C93D", "#156332", "#8B684E", 
                "#D4D4D4", "#585C5C", "#333333", "#CAE49E", "#8AB199")

ggplot(mm_ggze_compl, aes(x = interval, y = mean_worn, color = visit_name), size = interval) +
  facet_wrap(~ code, scales = "free") +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(limits = c(0, NA)) + 
  geom_point() +
  scale_color_manual(values = custom.col) +
  labs (title = "Wearing compliance per day per visit per subject", subtitle = "Visit 1 and Visit 3 (MM)", 
        y = "Wearing compliance per day (%)", x = "Days (number)") +
  theme_light()

# Total steps
mm_ggze_steps <- mm_ggze %>% 
  group_by(code, visit_name) %>% 
  summarise(sum_steps = sum(WALKING_steps)) %>% 
  arrange(code)

ggplot(mm_ggze_steps) +
  geom_col(position = "dodge") +
  aes(x = code, y = sum_steps, factor(code), fill = factor(visit_name)) + 
  scale_fill_manual(values = c(rep(c("#94C93D", "#585C5C"))))+
  labs(title = "Steps per visit per subject", subtitle = "Visit 1 and Visit 3 (MM)", 
       y = "Steps (number)", x = "Subject (code)", fill = "Visit") +  
  theme_light()

# SVD
# Filter
mm_svd <- mm_distinct %>%
  filter(grepl('Mariella Panagl', user_name) | 
         grepl('Sophie Gindl', user_name) , 
         grepl('^[0-9]{3}$', code )) %>%
  arrange (measurement_id, interval) %>%
  # mutate(WALKING_steps = coalesce(WALKING_steps, 0))
  mutate(WALKING_steps = coalesce(WALKING_steps, 0L)) # L achter 0 @McR

# Number of days measured per measurement_id 
mm_svd %>% group_by(measurement_id, code) %>% summarise(n = n()) %>% arrange(n)

# Wearing compliance
mm_svd_compl <- mm_svd %>% 
  group_by(measurement_id, code, interval, visit_name) %>% 
  summarise(mean_worn = mean(PERC_day_worn)) %>% 
  mutate(n_days = n()) %>%
  arrange(code) 

ggplot(mm_svd_compl, aes(x = interval, y = mean_worn, color = visit_name), size = interval) +
  facet_wrap(~ code, scales = "free") +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(limits = c(0, NA)) + 
  geom_point() +
  scale_color_manual(values = custom.col) +
  labs (title = "Wearing compliance per day per visit per subject", subtitle = "Visit 1 and Visit 3 (MM)", 
        y = "Wearing compliance per day (%)", x = "Days (number)") +
  theme_light()

# Total steps
mm_svd_steps <- mm_svd %>% 
  group_by(code, visit_name) %>% 
  summarise(sum_steps = sum(WALKING_steps)) %>% 
  arrange(code)

ggplot(mm_svd_steps) +
  geom_col(position = "dodge") +
  aes(x = code, y = sum_steps, factor(code), fill = factor(visit_name)) + 
  scale_fill_manual(values = c(rep(c("#94C93D", "#585C5C", "#8B684E"))))+
  labs(title = "Steps per visit per subject", subtitle = "Visit 1 and Visit 3 (MM)", 
       y = "Steps (number)", x = "Subject (code)", fill = "Visit") +  
  theme_light()
 