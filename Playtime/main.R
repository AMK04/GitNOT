source('./sourceLibraries.R')
# 
# installDir = "C:\\Program Files\\R\\R-3.3.1\\library"
# install.packages("tidyverse", dir=installDir)
# install.packages("dplyr")
# library('dplyr')
# library('ggpubr')


sppb <- readRDS("P:/20200204_PLAYTIME_MT_SPPB_STS_SWAY/20200204_PLAYTIME_MT_SPPB_STS_SWAY_sppb.Rds")
sts  <- readRDS("P:/20200204_PLAYTIME_MT_SPPB_STS_SWAY/20200204_PLAYTIME_MT_SPPB_STS_SWAY_ststest.Rds")
sway <- readRDS("P:/20200204_PLAYTIME_MT_SPPB_STS_SWAY/20200204_PLAYTIME_MT_SPPB_STS_SWAY_swaytest.Rds")

# Distinct, filter code and visit_name
sppb_distinct  <- sppb %>% 
  rename_(.dots = setNames(names(.), gsub("subject_", "", names(.)))) %>%
  distinct(measurement_id, .keep_all = TRUE) %>% 
  filter(grepl('^[0-9]{3}$', code) | grepl('^[14]{1}$', visit_name))

sway_distinct  <- sway %>% 
  rename_(.dots = setNames(names(.), gsub("subject_", "", names(.)))) %>%
  distinct(measurement_id, condition, .keep_all = TRUE) %>%
  filter(grepl('^[0-9]{3}$', code) | grepl('^[14]{1}$', visit_name))

sts_distinct  <- sts %>% 
  rename_(.dots = setNames(names(.), gsub("subject_", "", names(.)))) %>%
  distinct(measurement_id, repetition, .keep_all = TRUE) %>%
  filter(grepl('^[0-9]{3}$', code) | grepl('^[14]{1}$', visit_name))

custom.col <- c("#94C93D", "#156332", "#8B684E", 
                "#D4D4D4", "#585C5C", "#333333", "#CAE49E", "#8AB199")

# SPPB
# Visit 1
sppb_distinct_visit1 <- sppb_distinct %>%
  filter(!grepl('[Tt]+est', visit_name),
         !grepl('4|Visit End|Visit 2', visit_name))
# Correlation
# Age / total score
ggscatter(sppb_distinct_visit1, x = "age", y = "prepSPPB_s_total", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age (year)", ylab = "SPPB score (total)")

# Visit 2
sppb_distinct_visit2 <- sppb_distinct %>%
  filter(!grepl('[Tt]+est', visit_name),
         grepl('4|Visit End|Visit 2', visit_name))
# Correlation
# Age / total score
ggscatter(sppb_distinct_visit2, x = "age", y = "prepSPPB_s_total", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age (year)", ylab = "SPPB score (total)")

# 2 visits
# Filter SVD (code) and GGZ (visit name), filter test measurements, filter 104 and 105 (2 metingen met 'Visit End')
sppb_distinct_2visits <- sppb_distinct %>% 
  filter(!grepl('test', visit_name)) %>%
  filter(!code %in% c('105', '104')) %>% # Moet gecheckt worden bij s.gindl. Er zijn 2 metingen met Visit 'Visit End'.
  group_by(code) %>%
  mutate(n = n(),
         visit_name = tolower(visit_name),
         group = visit_name != 'visit end') %>%
  filter(n >= 2) %>% 
  ungroup() %>% 
  select(-n) %>% 
  arrange(code, visit_name) %>%
  filter (!grepl('29785', measurement_id)) # 113 had 3 metingen (middelste verwijderd)
# Difference visit 1 vs visit 2
ggplot(sppb_distinct_2visits) +
  geom_col(position = "dodge") +
  aes(x = code, y = prepSPPB_s_total, factor(visit_name), fill = factor(visit_name)) + 
  scale_fill_manual(values = c(rep(c("#94C93D", "#585C5C", "#8B684E", 
                                     "#D4D4D4", "#585C5C", "#333333", "#CAE49E", "#8AB199"))))+
  labs(title = "SPPB score per visit per subject", subtitle = "Visit 1 and Visit 3 (MT)", 
       y = "SPPB (total score)", x = "Subject (code)", fill = "Visit") +  
  theme_light()


#sppb_distinct_2visits %>% group_by(code) %>% summarise(n = n()) %>% arrange(n)


# SWAY
# 2 visits
# Subject 100 and 105 contain one test visit that is excluded by this filter.
sway_distinct <- sway_distinct %>% filter(!grepl('test', visit_name))

# Renames 1 visit for subjects: 112, 113, 118
# Excluding subjects: 104, 105
# Excluding due to only having 1 visit: 115, 117, 100, 101, 103, 109, 110, 114, 116, 119, 120
sway_distinct <- sway_distinct %>%
  mutate(visit_name = replace(visit_name, which(results_id %in% c(26564, 26182, 26186)), 'Visit 1')) %>% 
  filter(!code %in% c('105', '104'))  # Moet gecheckt worden bij s.gindl. Er zijn 2 metingen met Visit 'Visit End'.

sway_distinct <- sway_distinct %>%
  filter(tolower(visit_name) %in% c('1', '4', 'erstbesuch visit 1', 'visit 1', 'visit end')) %>% 
  group_by(code) %>%
  mutate(n = n(),
         visit_name = tolower(visit_name),
         group = visit_name != 'visit end') %>%
  filter(n >= 5) %>% 
  ungroup() %>% 
  select(-n) %>% 
  arrange(code, visit_name)
  
# sway_distinct %>% group_by(code) %>% summarise(n = n()) %>% arrange(n)

# STS
# 2 visits
sts_distinct_2visits <- sts_distinct %>% filter(!grepl('test', visit_name)) %>%
  filter(tolower(visit_name) %in% c('1', '4', 'erstbesuch visit 1', 'visit 1', 'visit end')) %>% 
  group_by(code) %>%
  mutate(n = n(),
         visit_name = tolower(visit_name),
         group = visit_name != 'visit end') %>%
  filter(n >= 7) %>% 
  ungroup() %>% 
  select(-n) %>% 
  arrange(code, visit_name)

# numbertrial <- sts_distinct_2visits %>% group_by(code) %>% summarise(n = n()) %>% arrange(n)






