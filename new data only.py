import re
import os
import pandas as pd

path = "C:/Users/Joel/Dropbox/Capstone/Data/"
day_data = os.path.join(path, "MPO/MPO_2009_2017.txt")
pattern = re.compile("[a-zA-Z0-9.-]+")

all_date = []
all_new_retail = []
all_new_medicaid = []
all_new_third = []

all_refill_retail = []
all_refill_medicaid = []
all_refill_third = []

all_total = []

###FOR EPIC REPORTS
all_date_epic = []
all_total_epic = []

####START OF EPIC REPORTS
path_epic = "C:/Users/Joel/Dropbox/Capstone/Data/"
day_data_epic = os.path.join(path_epic, "MPO/MPO_new_data_USE.txt")
pattern_epic = re.compile("[a-zA-Z0-9/,]+")


with open(day_data_epic) as e:
    line_epic = e.readline()
    while line_epic:
        line_list_epic = pattern_epic.findall(line_epic)

        if line_list_epic != []:
            if len(line_list_epic[0]) == 10:
                date_epic = line_list_epic[0]; all_date_epic.append(date_epic)
                total_epic = line_list_epic[1]; all_total_epic.append(total_epic)
                line_epic = e.readline()
            else:
                line_epic = e.readline()
        else:
            line_epic = e.readline()
        #print(line_list)

myDF_epic = pd.DataFrame({"Date": all_date_epic, "Total": all_total_epic})

myDF_epic.to_csv(path_or_buf="C:/Users/Joel/Dropbox/Capstone/Data/MPO_NEW.csv", sep=",")