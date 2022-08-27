
library(readr)
library(tidyverse)
shots_data <- read.csv("shots_data.csv")
View(shots_data)


#Two Teams
shots_A <- shots_data %>% filter(team == "Team A") #Team A
shots_B <- shots_data %>% filter(team == "Team B") # Team B


#For all “Non Corner” 3’s (where Y > 7.8), the 3PT line is 23.75 ft from the center of the hoop 
#For all “Corner” 3’s (where Y <= 7.8), the 3PT line is 22 feet from the court’s Y-axis at all  points (note the definition of “Corner” 3 is not determined by the “break” in the arc) 
#Zones: Two Point (2PT), Non Corner 3 (NC3) and Corner 3 (C3).

####Team A

#Corner 3
shots_C3A <- shots_A %>%
  filter((x>=22 & y<=7.8) | (x<=-22 & y<=7.8)) #18 shots
shots.C3A = 18/280 *100 #6.429

sum(shots_C3A$fgmade) #9

shots.C3AeFG = (9 + (0.5*9))/nrow(shots_C3A) *100 #75

#Non Corner 3
shots_NC3A <- shots_A %>%
  filter((x>=23.75 & y>7.8) | (x<=-23.75 & y>7.8)) #0 shots
shots.NC3A = 0/280 *100 #0

sum(shots_NC3A$fgmade) #0

shots.NC3AeFG = 0 #0

#Two PT zone
shots_2PTA <- shots_A %>%
  filter(((x<22 & y<=7.8) & (x>-22 & y<=7.8)) | ((x<23.75 & y>7.8) & (x>-23.75 & y>7.8))) #262 shots
shots.2PTA = 262/280 * 100 #93.571

sum(shots_2PTA$fgmade) #116

shots.2PTAeFG = 116/nrow(shots_2PTA)*100 #44.275


####Team B

#Corner 3
shots_C3B <- shots_B %>%
  filter((x>=22 & y<=7.8) | (x<=-22 & y<=7.8)) #12 shots
shots.C3B = 12/224 *100 #5.357

sum(shots_C3B$fgmade) #4

shots.C3BeFG = (4 + (0.5*4))/nrow(shots_C3B) *100 #50

#Non Corner 3
shots_NC3B <- shots_B %>%
  filter((x>=23.75 & y>7.8) | (x<=-23.75 & y>7.8)) #0 shots
shots.NC3B = 0/224 *100 #0

sum(shots_NC3A$fgmade) #0

shots.NC3AeFG = 0 #0

#Two Points Zone
view(shots_2PTB)
shots_2PTB <- shots_B %>%
  filter((x < 22 & y<= 7.8)  & (x > -22 & y <= 7.8) | ((x < 23.75 & y > 7.8) & (x> -23.75 & y > 7.8))) #212 shots
shots.2PTB = 212/224 * 100 #94.643

sum(shots_2PTB$fgmade) # 88

shots.2PTBeFG = 88 / nrow(shots_2PTB) * 100 #41.509

#library("writexl")
#write_xlsx(shots_2PTB,"debug.xlsx")

