birds = ( ('Passerculus sandwichensis','Savannah sparrow',18.7),
          ('Delichon urbica','House martin',19),
          ('Junco phaeonotus','Yellow-eyed junco',19.5),
          ('Junco hyemalis','Dark-eyed junco',19.6),
          ('Tachycineata bicolor','Tree swallow',20.2),
         )

#(1) Write three separate list comprehensions that create three different
# lists containing the latin names, common names and mean body masses for
# each species in birds, respectively. 

Latin_names = [bird[0] for bird in birds]
print("latin names:",Latin_names)

Common_names = [bird[1] for bird in birds]
print("common names:",Common_names)

mean_bm = [bird[2] for bird in birds]
print("Mean bodymass:",mean_bm)

Latinnames4loop = []
Commonnames4loop = []
Meanbm4loop = []

for i in birds:
    Latinnames4loop.append(i[0])
    Commonnames4loop.append(i[1])
    Meanbm4loop.append(i[2])
print("latin names:",Latinnames4loop)
print("common names:",Commonnames4loop)
print("Mean bodymass:",Meanbm4loop)




# (2) Now do the same using conventional loops (you can choose to do this 
# before 1 !). 

# A nice example out out is:
# Step #1:
# Latin names:
# ['Passerculus sandwichensis', 'Delichon urbica', 'Junco phaeonotus', 'Junco hyemalis', 'Tachycineata bicolor']
# ... etc.
 