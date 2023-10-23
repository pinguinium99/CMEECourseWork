# Average UK Rainfall (mm) for 1910 by month
# http://www.metoffice.gov.uk/climate/uk/datasets
rainfall = (('JAN',111.4),
            ('FEB',126.1),
            ('MAR', 49.9),
            ('APR', 95.3),
            ('MAY', 71.8),
            ('JUN', 70.2),
            ('JUL', 97.1),
            ('AUG',140.2),
            ('SEP', 27.0),
            ('OCT', 89.4),
            ('NOV',128.4),
            ('DEC',142.2),
           )

# (1) Use a list comprehension to create a list of month,rainfall tuples where
# the amount of rain was greater than 100 mm.

rfallgt100 = [rain_mm for rain_mm in rainfall if rain_mm[1] > 100.0]
print("Months and rainfall values when the amount of rain was greater than 100mm:", rfallgt100)
 
# (2) Use a list comprehension to create a list of just month names where the
# amount of rain was less than 50 mm. 


rfalllt50 = [rain_mm[0] for rain_mm in rainfall if rain_mm[1] < 50.0]
print("Months when the amount of rain was less than 50mm:", rfalllt50)
 

# (3) Now do (1) and (2) using conventional loops (you can choose to do 
# this before 1 and 2 !). 
RainFgt100 = []

for rain_mm in rainfall:
    if rain_mm[1] > 100.0:
        RainFgt100.append(rain_mm)
print("Months and rainfall values when the amount of rain was greater than 100mm:", RainFgt100)

RainFlt50 = []

for rain_mm in rainfall:
    if rain_mm[1] < 50.0:
        RainFlt50.append(rain_mm[0])
print("Months when the amount of rain was less than 50mm:", RainFlt50)




# A good example output is:
#
# Step #1:
# Months and rainfall values when the amount of rain was greater than 100mm:
# [('JAN', 111.4), ('FEB', 126.1), ('AUG', 140.2), ('NOV', 128.4), ('DEC', 142.2)]
# ... etc.
