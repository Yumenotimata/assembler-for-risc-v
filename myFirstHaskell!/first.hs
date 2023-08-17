doubleUs x y = doubleMe x + doubleMe y
doubleMe x = x + x
doubleSmaller x = (if x == 2 then x else 3)
numbers = [1,2,3,4]
number = [3,4,5]
length' xs = sum [1 | _ <- xs]