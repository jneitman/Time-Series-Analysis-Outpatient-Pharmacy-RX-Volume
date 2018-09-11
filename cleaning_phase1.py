#Cleaning text files generated from reports in QS1/NRX


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


with open(day_data) as r:
    line = r.readline()
    while line:
        line_list = pattern.findall(line)
        #print(line_list)
        if line_list != []:
            if line_list[0] == "TX-Date":
                date = "/".join(line_list[2:5])
                all_date.append(date); all_date_epic.append(date)
                #print(date)
            elif line_list[0] == "----------------":
                next(r)
                RETAIL = pattern.findall(r.readline())
                MEDICAID = pattern.findall(r.readline())
                THIRD = pattern.findall(r.readline())

                NEW_RETAIL = RETAIL[1]; all_new_retail.append(NEW_RETAIL)
                NEW_MEDICAID = MEDICAID[1]; all_new_medicaid.append(NEW_MEDICAID)
                NEW_THIRD = THIRD[1]; all_new_third.append(NEW_THIRD)

                if RETAIL[1] == "0":
                    REFILL_RETAIL = RETAIL[5]; all_refill_retail.append(REFILL_RETAIL)
                else:
                    REFILL_RETAIL = RETAIL[6]; all_refill_retail.append(REFILL_RETAIL)

                if MEDICAID[1] == "0":
                    REFILL_MEDICAID = MEDICAID[5]; all_refill_medicaid.append(REFILL_MEDICAID)
                else:
                    REFILL_MEDICAID = MEDICAID[6]; all_refill_medicaid.append(REFILL_MEDICAID)

                if THIRD[1] == "0":
                    REFILL_THIRD = THIRD[5]; all_refill_third.append(REFILL_THIRD)
                else:
                    REFILL_THIRD = THIRD[6]; all_refill_third.append(REFILL_THIRD)

            elif line_list[0] == "----------------------------------------------":
                next(r)
                next(r)
                next(r)
                next(r)
                TOTAL = pattern.findall(r.readline())[1]; all_total.append(TOTAL); all_total_epic.append(TOTAL)
                #print(TOTAL)
        line = r.readline()
# print(all_date)
# print(all_new_retail)
# print(all_new_medicaid)
# print(all_new_third)
# print(all_refill_retail)
# print(all_refill_medicaid)
# print(all_refill_third)
# print(all_total)


myTS = pd.DataFrame({"Date": all_date,
                     "New Retail": all_new_retail,
                     "New Medicaid": all_new_medicaid,
                     "New Third": all_new_third,
                     "Refill Retail": all_refill_retail,
                     "Refill Medicaid": all_refill_medicaid,
                     "Refill Third": all_refill_third,
                     "Total": all_total})
myTS.to_csv(path_or_buf="C:/Users/Joel/Dropbox/Capstone/Data/MPO.csv", sep=",")

####START OF EPIC REPORTS
path_epic = "C:/Users/Joel/Dropbox/Capstone/Data/"
day_data_epic = os.path.join(path_epic, "MPO/MPO_epic.txt")
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

myDF_epic.to_csv(path_or_buf="C:/Users/Joel/Dropbox/Capstone/Data/MPO_totals_only.csv", sep=",")
