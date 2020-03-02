source('./sourceLibraries.R')


activities <- read.csv2('C:\\Users\\a.kinkel\\Desktop\\2019-03-13_example_classification_17213_raw.csv', stringsAsFactors = FALSE)
activities <- activities %>% mutate(start = as.POSIXct(start))
activities <- addInterval(activities, 'day')

## total METs McR
# total_McR <- sum(activities$met)

## METs McR per class
met_total <- activities %>% 
  group_by( class) %>% 
  summarize(avg_met = weighted.mean(met, duration),
            sum_duration = sum(duration)/60)

# mean METs per class McR
# ?? met_total/met_total_dur > maar hoe nu dit te combineren in 1 tabel incl class? (bovenstaande berekening in 1 keer...)
met_total_class <- met_total %>%
  mutate(mean_met_class_McR = avg_met / sum_duration)


## METs table
# met_table <- c(walking = 2.5, standing = 1.3, lying = 1.0, shuffling = 1.8, cycling = 6.8, sitting = 1.3, stair_walking = 4.0, not_worn = 0.0)
met_table <- data.frame(class = c('walking','standing', 'lying', 'shuffling', 'cycling', 'sitting', 'stair_walking', 'not_worn'),
                        met_factor = c(2.5, 1.3, 1.0, 1.8, 6.8, 1.3, 4.0, 0.0),
                        stringsAsFactors = F)

activities2 <- activities %>%
  left_join(met_table, by = 'class') %>% 
  mutate(met_calc_table = met_factor * (duration/60))


## total METs table
total_table <- sum(activities2$met_calc_table)

## METS table per class 
# tabelwaarden * duration 
met_total_table <- activities2 %>% 
  group_by(class) %>% 
  summarize(sum_met = sum(met_calc_table))


## kCal TEE total measurement
# McR
# tee activities in Joule/sec, 1kcal = 4186.8J 
totalTEE_McR <- (sum(activities$tee*activities$duration))/4186.8

tee_total <- activities2 %>% 
  mutate(sum_tee_kcal = (tee*duration)/4186.8)%>%
  group_by(class) %>% 
  summarize(sum_tee = sum(sum_tee_kcal))


# MET table
# ((3.5 * kg * duration(min)) /1000) * 5(kCal/L) * MET 
weight <- 79
calc_TEE <- ((met_total$sum_duration*3.5*weight)/1000)*5 
# toevoegen aan met_table, groeperen op dezelfde manier als met_total_table
tee_total_table <- met_total %>%
  left_join(met_table, by = 'class') %>%
  mutate(calc_TEE_start = ((sum_duration*3.5*weight)/1000)*5,
         TEE_class_table = calc_TEE_start * met_factor)

totalTEE_table <- sum(tee_total_table$TEE_class_table)

## Difference total TEE McR vs table (kCal)
diff_tee = totalTEE_McR - totalTEE_table

tee_total_McRvstable <- tee_total_table%>%
 transmute(tee_total$class, tee_total$sum_tee, TEE_class_table)

## AEE via METs vs. AEE via McR
# McR
# aee activities in Joule/sec, 1kcal = 4186.8J 
totalAEE_McR <- (sum(activities$aee*activities$duration))/4186.8

aee_total <- activities2 %>%
  mutate(aee_kcal = (aee*duration)/4186.8) %>%
  group_by(class) %>%
  summarize(sum_aee_McR = sum(aee_kcal))

# via METs
height <- 186
age <- 24
rmrFactor <- (9.99*weight + 6.25*height - 4.92*age + 5) / (60*60*24)
# AEE per activiteit = TEE - RMR
# RMR per activiteit * duration
aee <- tee_total_McRvstable %>%
  mutate(duration_sec = tee_total_table$sum_duration*60)%>%
  mutate(aee_class_table = TEE_class_table - (rmrFactor*duration_sec))

## Difference total AEE McR vs table (kCal)
total_AEE_table <- sum(aee$aee_class_table)
diff_aee = totalAEE_McR - total_AEE_table

# aee_clas_table into aee_total table??

## Overig
# cntrl + shift + c
# met_total$sum_met
# met_total_class <- select(met_total, 2)
# met_mean_McR <- mean(activities$met)
# RANGE: handig voor sortering