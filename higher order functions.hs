

data Optional a = Empty | Present a deriving Show

{-
receives the function f as the first argument and the value o as the second argument.
Empty is returned if the second argument is Empty. Otherwise,
the first argument is applied to the value contained in the second argument
and the result of it is encapsulated in a present and returned.
-}

mapOptional :: (a -> b) -> Optional a -> Optional b
mapOptional _ Empty = Empty
mapOptional f ( Present x) = Present (f x)

{-
receives a function as the first argument that returns either True or False for a given value.
The second argument is a possibly empty value.
The return is the second argument if it is not empty
and the first argument applied to the value contained in the second argument is True.
Otherwise Empty is returned.
-}

filterOptional :: (a -> Bool ) -> Optional a -> Optional a
filterOptional p Empty = Empty
filterOptional p ( Present x) | p x = Present x
                              | otherwise = Empty

{-
maps an existing value with a given function and returns the result of it.
-}
foldOptional :: (a -> b) -> b -> Optional a -> b
foldOptional _ c Empty = c
foldOptional f _ ( Present x) = f x

{-
receives an item and returns False if and only if the transferred item has the name Dog Food.
-}

data Product = Article String Int deriving Show
isHumanEatable :: Product -> Bool
isHumanEatable ( Article name price ) = name /= "Dog Food"

{-
receives an article and returns an article with the same name.
If the transferred item costs less than 10 euros, i.e. less than 1000 cents,
the price is doubled. Otherwise the price remains the same.
-}

adjustPrice :: Product -> Product
adjustPrice ( Article name price ) | price < 1000 = Article name ( price *2)
                                   | otherwise = Article name price

{-
receives an article and returns a string.
-}

stringify :: Product -> String
stringify ( Article name price ) = " The Article named '" ++ name ++ "' costs " ++ ( show price ) ++ " Cent ."

{-
uses the filterOptional function to apply isHumanEatable to its argument
-}

filterHumanEatable :: Product -> Optional Product
filterHumanEatable a = filterOptional isHumanEatable ( Present a)

{-
adjustPriceO
uses the mapOptional function to apply adjustPrice to its argument.
-}
adjustPriceO :: Optional Product -> Optional Product
adjustPriceO a = mapOptional adjustPrice a

{-
stringifyO
uses the function foldOptional to convert its argument into a string with the help of stringify.
-}
stringifyO :: Optional Product -> String
stringifyO a = foldOptional stringify " This Article is unavailable ." a

{-
passes its argument to the function filterHumanEatable,
passes its return value to adjustPrice (),
passes its return value to stringify () and returns its return value
-}
toPriceTag :: Product -> String
toPriceTag a = stringifyO ( adjustPriceO ( filterHumanEatable a))
