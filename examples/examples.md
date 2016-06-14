### Syntax and Function Definitions

#### Declaring a method/function

##### Python
``` python
def add(a, b):
    return a + b
```
##### Haskell
``` haskell
add :: Num a => a -> a -> a
add x y = x + y
```

#### Filtering from a list

##### Python
``` python
def less_than_tens(list):
    new_list = []
    for x in list:
        if x < 10:
            new_list.append(10)
    return new_list

def less_than_tens(list):
    return filter(lambda x: x < 10, list)
```
##### Haskell
``` haskell
lessThanTens :: Num a => [a] -> [a]
lessThanTens list = filter (<10) list
```

#### Performing an operation on a list

##### Python
``` python
def add_ones(list):
    new_list = []
    for x in list:
        new_list.append(x + 1)
    return new_list

def add_ones(list):
    return map(lambda x: x + 1, list)
```
##### Haskell
``` haskell
addOnes :: Num a => [a] -> [a]
addOnes list = map succ list
```

#### Performing a reduce

##### Python
``` python
def sum(num_list):
    total = 0
    for x in num_list:
        total += x
    return total

def sum(num_list):
    return reduce(lambda x, y: x + y, num_list)
```
##### Haskell
``` haskell
foldr :: Reducer -> Default -> [a] -> Reduced or Default
foldr :: (a -> b -> b) -> b -> [a] -> b
sum :: Num a => [a] -> a
sum list = foldr (+) 0 list  
```

#### PatternMatching

##### Python
``` python
def say_hello():
    friend = input("What is your name?")
    if friend == "Sheila":
        print "Hello!"
    else: 
        print "Feck off!"
```
##### Haskell
``` haskell
sayHello :: IO ()
sayHello = do
    name <- getLine
    case name of
      "Sheila" -> putStrLn "Hello!"
      _ -> putStrLn "Feck off!"
```

#### Currying

##### Python
``` python
def add_three(a, b, c):
    return a + b + c
```
##### Haskell
``` haskell
addThree :: Num a => a -> a -> a -> a 
addThree a b c = a + b + c
```
##### Python
``` python
def add(a, b):
    return add_three(0, a, b)
```
##### Haskell
``` haskell
add :: Num a => a -> a -> a
add = addThree 0
```

#### Piping

##### Elixir
``` elixir
def do_thingy(list) do
  Enum.map list, (fn (x) -> x + 1 end)
  |> Enum.filter (fn (x) -> x < 10 end)
  |> Enum.reduce (fn (x, y) -> x + y end)
end
```

##### F#
``` f#
let do_thingy list =
    List.map list (fun x -> x + 1)
    |> List.filter (fun x -> x < 10)
    |> List.reduce (fun x y -> x + y)
```

#### Haskell
``` haskell
do_thingy list = foldr (+) 0 . filter (<10) . map (+1) list
```
