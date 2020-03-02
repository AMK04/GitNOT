source('./sourceLibraries.R')


data <- read.csv('C:\\Users\\a.kinkel\\Desktop\\energy data analysis - csv.csv', stringsAsFactors = FALSE)

# AEE in kJ/min
# MET = TEE/REE
# O2 in L/min
# acce data *1000 voor vergelijking mi

# Arrange by age +  MET berekening walk normal
data_age <- arrange(data, age)%>% 
  mutate(id_age           = 1:185,
         MET_walk_calc    = O2_l.min_walk_all / O2_lmin_rest,
         AEE_Brandes      = -18.61 + 0.24*weight + 53.97*acc_walk.normal,
         AEE_Brandes2     = (-40.19 + 816.11*acc_walk.normal) * weight / 1000,
         delta_HF_cycling = HF.Fahrrad - HF.Ruhe)

ggplot(data_age, aes(x = MET, y = MET_walk_calc)) +
  geom_point() + 
  theme_classic()

# Plots rest/sitting
ggplot(data_age, aes(x = age, y = MET_rest)) + 
  geom_point() + 
  theme_classic()

ggplot(data_age, aes(x = id_age, y = HF.Ruhe, size = MET_rest)) + 
  geom_point() + 
  theme_classic()

ggplot(data_age, aes(x = age, y = HF.Ruhe, size = MET_rest)) + 
  geom_point() + 
  theme_classic()

ggplot(data_age, aes(x = age, y = HF.sitzen, size = MET.sitzen)) + 
  geom_point() + 
  theme_classic()


# Plots walking normal
ggplot(data_age, aes(x = id_age, y = MET_walk_normal, size  = acc_walk.normal)) + 
  geom_point() + 
  theme_classic()

ggplot(data_age, aes(x = age, y = acc_walk.normal, size = HF.normal)) + 
  geom_point() + 
  theme_classic()

ggplot(data_age, aes(x = id_age, y = acc_walk.normal, size = aee_kjmin_walk_normal)) + 
  geom_point() + 
  theme_classic()

ggplot(data_age, aes(x = weight, y = acc_walk.normal, size = aee_kjmin_walk_normal)) + 
  geom_point() + 
  theme_classic()

ggplot(data_age, aes(x = weight, y = aee_kjmin_walk_normal, size = acc_walk.normal)) + 
  geom_point() + 
  theme_classic()

ggplot(data_age, aes(x = age, y = acc_walk.normal, size = aee_kjmin_walk_normal)) + 
  geom_point() + 
  theme_classic()

stats <- lm(age ~ aee_kjmin_walk_normal, data = data_age)
summary(stats)

ggplot(data_age, aes(x = age, y = aee_kjmin_walk_normal)) + 
  geom_point() + 
  stat_smooth(method = "lm", col = "red") +
  theme_classic()

stats <- lm(weight ~ aee_kjmin_walk_normal, data = data_age)
summary(stats)

ggplot(data_age, aes(x = weight, y = aee_kjmin_walk_normal)) + 
  geom_point() + 
  theme_classic()


# MET walk met accelerometer data
stats <- lm(acc_walk.normal ~ MET_walk_normal, data = data_age)
summary(stats)

ggplot(data_age, aes(x = acc_walk.normal, y = MET_walk_normal)) + 
  geom_point() + 
  stat_smooth(method = "lm", col = "red") +
  theme_classic()

ggplot(data_age, aes(x = weight, y= MET_walk_normal)) + 
  geom_point() + 
  theme_classic()


# MET walk vs MET calculated 
ggplot(data_age, aes(x = MET_walk_calc , y= MET_walk_normal)) + 
  geom_point() +
  coord_cartesian(xlim = c(2, 10), ylim = c(2, 10)) +
  theme_classic()


# Brandes 
stats <- lm(acc_walk.normal ~ aee_kjmin_walk_normal, data = data_age)
summary(stats)

ggplot(data_age, aes(x = acc_walk.normal, y = aee_kjmin_walk_normal)) + 
  geom_point() + 
  theme_classic()

stats_Brandes <- lm(AEE_Brandes ~ aee_kjmin_walk_normal, data = data_age)
summary(stats_Brandes)

ggplot(data_age, aes(x = AEE_Brandes , y = aee_kjmin_walk_normal)) + 
  geom_point() + 
  coord_cartesian(xlim = c(-1, 40), ylim = c(0, 40)) +
  theme_classic()

stats_Brandes2 <- lm(AEE_Brandes2 ~ aee_kjmin_walk_normal, data = data_age)
summary(stats_Brandes2)

ggplot(data_age, aes(x = AEE_Brandes2 , y = aee_kjmin_walk_normal)) + 
  geom_point() + 
  coord_cartesian(xlim = c(-1, 40), ylim = c(0, 40)) +
  theme_classic()


# Cycling
ggplot(data_age, aes(x = delta_HF_cycling, y = acce_cycle)) +
  geom_point() + 
  theme_classic()

