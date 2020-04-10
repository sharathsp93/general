

# R version ---------------------------------------------------------------

# sessionInfo()
# R version 3.6.3 (2020-02-29)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Mojave 10.14.6


# Load the library --------------------------------------------------------


# lets have the library here and we can just source this script everytime. in the begining of every scripts
# these are all not necessary just my favoriate packages that i use commonly
# install.packages("pacman) # please install this once and you dont have to install any other packages, it just has to be written inside "p_load" function
library(pacman) 
p_load(dplyr, tidyverse, rio, readxl, janitor,  lubridate, flextable, officer, broom) #  other packages needed just have to mentioned inside no need to install it 


# Load the data -----------------------------------------------------------


# loading the data, then I choose sheet number 1, as we just work with that.
data <- read_excel("data/2020-04-1-sohelia-merging-help/Diversity Turn Cyclone Damage Assessment - latest version - labels - 2019-12-16-12-04-47 (1).xlsx", sheet = 1)

# now I seperate the measurememnt and interview part, so later we can merge them, its more efficient way (i just filtered by observer)
measurement <- data %>% filter(Observer == "Thorien")
interview <- data %>% filter(Observer == "Arnaud")

# I wanted to take a clean look at the data so I just exported them and visualised them in excel
export(measurement, "data/2020-04-1-sohelia-merging-help/measurement.csv")
export(interview, "data/2020-04-1-sohelia-merging-help/interview.csv")

# I edited some of the rows, which had no data, for example i removed "subscribed", the next one to subscribed, "other village",
# "informed consent", "mbola_tsara", "agnavako", "please save", 
# and shortened "choose which part of the survey you wanna complete". As they were either completely blank or was not I feel was not important.
# I am sorry if there was something important. We can fix it anyway


# re-read modified data ---------------------------------------------------



# I re-read the modified data
ed_measurement <- read_csv("data/2020-04-1-sohelia-merging-help/measurement.csv") %>% 
  set_names(paste0(names(.), "_measurement")) %>% # I added suffix by the type of recording data by "interview" or "measurement"
  rename("Baseline household ID" = "Baseline household ID_measurement") # We just want one varaiable to merge the datasets again so I saved "Baseline household ID_measurement"

ed_interview <- read_csv("data/2020-04-1-sohelia-merging-help/interview.csv") %>% 
  set_names(paste0(names(.), "_interview")) %>% 
  rename("Baseline household ID" = "Baseline household ID_interview")


# Well, now we just combine the data and export it. 

comb_data <- left_join(ed_measurement, ed_interview)

export(comb_data, "data/2020-04-1-sohelia-merging-help/comb_data.csv")


# The below code takes out the columns that is completely empy (i mean if there is no observation in that complete column), if you want this data you just have to assign a varible name for this

comb_data[colSums(!is.na(comb_data)) > 0] 

