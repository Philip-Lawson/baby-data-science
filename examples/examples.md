``` python
def add(a, b):
    return a + b
add :: Num a => a -> a -> a
add x y = x + y

def less_than_tens(list):
    new_list = []
    for x in list:
        if x < 10:
            new_list.append(10)
    return new_list

def less_than_tens(list):
    return filter(lambda x: x < 10, list)
```
``` haskell
lessThanTens :: Num a => [a] -> [a]
lessThanTens list = filter (<10) list
```
``` python
def add_ones(list):
    new_list = []
    for x in list:
        new_list.append(x + 1)
    return new_list

def add_ones(list):
    return map(lambda x: x + 1, list)
```
``` haskell
addOnes :: Num a => [a] -> [a]
addOnes list = map succ list
```
``` python
def sum(num_list):
    total = 0
    for x in num_list:
        total += x
    return total

def sum(num_list):
    return reduce(lambda x, y: x + y, num_list)
```
``` haskell
foldr :: Reducer -> Default -> [a] -> Reduced or Default
foldr :: (a -> b -> b) -> b -> [a] -> b
sum :: Num a => [a] -> a
sum list = foldr (+) 0 list  
```
PatternMatching

``` python
def say_hello():
    friend = input("What is your name?")
    if friend == "Sheila":
        print "Hello!"
    else: 
        print "Feck off!"
```
``` haskell
sayHello :: IO ()
sayHello = do
    name <- getLine
    case name of
      "Sheila" -> putStrLn "Hello!"
      _ -> putStrLn "Feck off!"
```
Currying
``` python
def add_three(a, b, c):
    return a + b + c
```
``` haskell
addThree :: Num a => a -> a -> a -> a 
addThree a b c = a + b + c
```
``` python
def add(a, b):
    return add_three(0, a, b)
```
``` haskell
add :: Num a => a -> a -> a
add = addThree 0
```
