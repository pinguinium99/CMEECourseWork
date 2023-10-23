def buggyfunc(x):
    y = x
    for i in range(x):
        try:
            y = y-1
            z = x/y
        except ZeroDivisionError:
            print("The result of dividing a number by zero is undefined")
        except:
            print(f"This didn't work;{x = }; {y = }")
    else:
        return z

buggyfunc(20)