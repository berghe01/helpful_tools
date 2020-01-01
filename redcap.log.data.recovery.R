# script to salvage test data from a database which has been accidentally overwritten in REDCap and 
# may not yet be in PRODUCTION mode: uses logfile

# assumes realtively simple database structure, no repeated instruments, 
# some modifications might be necessary depending on the structure of your data (i.e. you have survey responses, see line 23), 
# but this should get you pretty close
# bottom line, follow best practices...move to production before entering any data you wish to keep 

library(tidyverse)
library(tibble)
library(lubridate)

#download log file and save into working directory as logfile.csv
#import log file downloaded from REDCap
df <- read.csv("logfile.csv", header = TRUE, sep = ",")

#rename columns to something easy to work with
colnames(df) <- c("date_time",# time of each change that was saved to db
                  "user", # user making the change 
                  "ID", # record ID
                  "changes") # actual changes that were made to each variable, delimited by ","

df <- df %>%
  filter(str_detect(ID,  # grab only changes for record creation OR modification
                    "Created Record|Updated Record")) %>%  # could add `|Updated Response` if survey data as well
  select(ID, date_time, changes) %>%
  mutate(ID = str_sub(ID, start = 15)) # remove excess text so that only record ID number remains
                                       # potentially change "start" parameter if `|Updated Reponse` included

df <- df %>%
  separate_rows(changes, sep=",\\s*\\n*") %>% # isolate individual changes per log event
  mutate(changes = str_remove_all(changes, "\""), # remove "
         changes = str_remove_all(changes, "\'")) %>% # remove '
  separate(changes, into = c("vars1", "vars2"), 
           sep="\\s*=\\s*") # isolate variable names for each change

df <- df %>%
  filter(vars1 != "", # remove any empty variables
         vars1 != "ID") %>% # remove any changes that included just record creation
  # this step assumes you named your "record ID" variable == "ID" - in reality, you may need 
  # to change this to whatever name you originally applied to your "record ID" field
  mutate(rn = row_number()) %>% # create a sequence column
  pivot_wider(names_from = vars1, values_from = vars2, 
              values_fill = list(vars2 = '')) %>% # make a long database -> wide
  select(-rn) %>% 
  arrange(ID)

# rename any variable with (#) to ___#
# ... these tend to be the multiple choice variables 
names(df) <- gsub(x = names(df), pattern = "\\(", replacement = "___") 
names(df) <- gsub(x = names(df), pattern = "\\)", replacement = "")

# convert "checked" to a 1
df[] <- lapply(df, gsub, pattern = "checked", replacement = "1", fixed = TRUE)

# convert date_time to POSIXct
df <- df %>%
  mutate(date_time = mdy_hm(date_time)) 

# there may be cases where an entry was made into a variable and 
# subsequently, this was changed and a new entry was made in that variable,
# we will filter using date_time so that only the most recent variable is selected

# in cases where 2 different values were entered into a variable and saved within
# the same minute, the date_time will be the same. The following pipeline will
# identify those variables, which need to be removed via anti_join and entered into 
# redcap manually later

remove_records <- df %>% 
  gather(var, val, -ID, -date_time, na.rm = TRUE) %>%
  arrange(desc(val)) %>% 
  filter(!is.na(val), 
         val != "") %>% 
  group_by(ID, var) %>% 
  filter(date_time == max(date_time)) %>%
  count() %>%
  filter(n > 1) %>%
  select(ID, var)

# if remove_records shows 0 rows, then you are good, move along
# otherwise, go back to remove_records later to see which variables need to be entered
# into REDCap manually (usually a low number, typically the "section complete" checkoff, can be
# checked manually later) 

# next step...
# the dataframe has many NA and blank ("") values
# need to get rid of these and collpase grouped rows so that each record is only 
# on one (not multiple) rows

# again, use the time stamp (date_time) to select only the most recent entry

df <- df %>%
  gather(var, val, -ID, -date_time, na.rm = TRUE) %>% # long database
  arrange(desc(val)) %>% 
  filter(!is.na(val), 
         val != "") %>% 
  group_by(ID, var) %>% 
  anti_join(remove) %>%
  filter(date_time == max(date_time)) %>%
  select(-date_time) %>%
  spread(var, val) # wide database

df[is.na(df)] <- "" # remove all NA and replace with ""

# write the file to csv - as long as the structure of the database is intact, you can upload 
# via the data import tool and hopefully you have recovered your data

write.csv(df, file = "salvaged_df.csv", row.names = FALSE)


# thanks to stackoverflow for some useful advice with this
