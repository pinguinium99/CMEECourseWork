###########################
def hello_1(x):
    for j in range(x):
        if j % 3 == 0:
            print('hello')
    print(' ')

hello_1(12)

###########################
def hello_2(x):
    for j in range(x):
        if j % 5 == 3:
            print('hello')
        elif j % 4 == 3:
            print('hello')
            print(' ')

hello_2(12)
############################
def hello_3(x, y):
    for i in range(x, y):
        print('hello')
    print(' ')
 
hello_3(3, 17)
############################
def hello_4(x):
    while x != 15:
        print('hello')
        x = x + 3
    print(' ')

hello_4(6)
############################
def hello_5(x):
    while x < 100:
        if x == 31:
            for k in range(7):
                print('hello')
        elif x == 18:
                print('hello')
        x = x + 1
    print(' ')

hello_5(12)
###########################
def hello_6(x, y):
    while x:
        print("hello! " + str(y))
        y += 1
        if y == 6:
            break
    print(' ')

hello_6(True, 7)

##############################################################


x = [i for i in range(10)]
print(x)

x = []
for i in range(10):
    x.append(i)

print(x)

x = [i.lower() for i in ["LIST","COMPERHENSIONS","ARE","COOL"]]
print(x)

x = ["LIST","COMPERHENSIONS","ARE","COOL"]
for i in range(len(x)):
    x[i] = x[i].lower()
print(x)

matrix = [[1,2,3],[4,5,6],[7,8,9]]
flattened_matrix = []
for row in matrix:
    for n in row:
        flattened_matrix.append(n)
print(flattened_matrix)

matrix = [[1,2,3],[4,5,6],[7,8,9]]
flattened_matrix = [n for row in matrix for n in row]
print(flattened_matrix)

words = ["these", "are", "some","words"]

first_letters = set()
for w in words:
    first_letters.add(w[0])

print(first_letters)

type(first_letters)
