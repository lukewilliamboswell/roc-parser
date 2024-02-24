interface Utils
    exposes []
    imports []

min = \(a, b) -> if a < b then a else b
max = \(a, b) -> if a > b then a else b

Range a : (Int a, Int a)
# Interval a : [Int a, Range a]

# # intervalToRange : Interval a -> Range a
# intervalToRange = \tag ->
#     when tag is
#         Int n -> (n, n)
#         Range r -> r

# Sort a range by its values
sortSingle : Range a -> Range a
sortSingle = \a ->
    if (a.1 - a.0) < 0 then
        (a.1, a.0)
    else
        a

expect sortSingle (3, 7) == (3, 7)
expect sortSingle (7, 3) == (3, 7)
expect sortSingle (7, 7) == (7, 7)

# Sort a list of ranges by their values
sort : List (Range a) -> List (Range a)
sort = \a -> a
    |> List.map sortSingle
    |> List.sortWith \x, y ->
        if x.0 < y.0 then
            LT
        else if x.0 > y.0 then
            GT
        else if x.1 < y.1 then
            LT
        else if x.1 > y.1 then
            GT
        else
            EQ

expect sort [(4, 2), (3, 1)] == [(1, 3), (2, 4)]
expect sort [(2, 5), (4, 2), (3, 1)] == [(1, 3), (2, 4), (2, 5)]
expect
    sort [(30, 30), (7, 7), (7, 2), (3, 4), (1, 2), (11, 5), (21, 15)]
    == [(1, 2), (2, 7), (3, 4), (5, 11), (7, 7), (15, 21), (30, 30)]

# Check if two ranges intersect
hasIntersection : (Range a, Range a) -> Bool
hasIntersection = \(a, b) -> a.0 <= b.1 && b.0 <= a.1

# Find the union of two sorted ranges
unionPair : (Range a, Range a) -> [Nothing, Single (Range a)]
unionPair = \(a, b) ->
    if hasIntersection (a, b) then
        Single (min (a.0, b.0), max (a.1, b.1))
    else
        Nothing

expect unionPair ((2, 5), (3, 8)) == Single (2, 8)
expect unionPair ((1, 2), (2, 7)) == Single (1, 7)
expect unionPair ((2, 4), (5, 8)) == Nothing

# I accidentally recreated List.walk XD
recursive = \list, accumulator, function ->
    when list is
        [] -> accumulator
        [element, .. as rest] -> recursive rest (function accumulator element) function
# Find the union of two sorted lists of ranges
union : List (Range a) -> List (Range a)
union = \a -> recursive a [] unionHelp

unionHelp = \x, aa ->
    when x is
        [] -> List.append x aa
        [.., last] ->
            when unionPair (last, aa) is
                Nothing -> List.append x aa
                Single u -> List.set x ((List.len x) - 1) u

# Using List.walk
# union2 = \a -> List.walk a [] unionHelp

expect
    actual = union [(1, 2), (2, 7), (3, 4), (5, 11), (7, 7), (15, 21), (30, 30)]
    actual == [(1, 11), (15, 21), (30, 30)]

# Find the difference between two sorted ranges
diffPair : (Range a, Range a) -> [Nothing, Single (Range a), Pair (Range a, Range a)]
diffPair = \(a, b) ->
    if !(hasIntersection (a, b)) then
        Single a
    else if a.0 < b.0 && a.1 > b.1 then
        Pair ((a.0, b.0 - 1), (b.1 + 1, a.1))
    else if a.0 < b.0 then
        Single (a.0, min (b.0 - 1, a.1))
    else if a.1 > b.1 then
        Single (max (b.1 + 1, a.0), a.1)
    else
        Nothing

expect diffPair ((3, 7), (11, 13)) == Single (3, 7)
expect diffPair ((3, 7), (5, 7)) == Single (3, 4)
expect diffPair ((3, 11), (5, 7)) == Pair ((3, 4), (8, 11))
expect diffPair ((5, 7), (3, 7)) == Nothing

# Find the difference between two sorted lists of ranges
diff : List (Range a), List (Range a) -> List (Range a)
diff = \a, b ->
    recursive a [] (\x, aa -> diffHelp1 x (recursive b [aa] diffHelp2))

diffHelp1 = \x, fn -> x |> List.concat fn

diffHelp2 = \y, bb -> y
    |> List.joinMap \yy ->
        when diffPair (yy, bb) is
            Single d -> [d]
            Pair d -> [d.0, d.1]
            Nothing -> []

# Using List.walk:
# diff2 = \a, b -> List.walk a [] \x, aa -> diffHelp1 x (List.walk b [aa] diffHelp2)

expect diff [(1, 11), (15, 21), (30, 30)] [] == [(1, 11), (15, 21), (30, 30)]
expect diff [(1, 11), (15, 21), (30, 30)] [(4, 7), (13, 13)] == [(1, 3), (8, 11), (15, 21), (30, 30)]
expect diff [(4, 7), (13, 13)] [(1, 11), (15, 21), (30, 30)] == [(13, 13)]

expect
    [(30, 30), (7, 7), (2, 7), (3, 4), (1, 2), (5, 11), (21, 15)]
    |> sort
    |> union
    == [(1, 11), (15, 21), (30, 30)]

expect
    [(4, 4), (4, 7), (13, 13)]
    |> sort
    |> union
    == [(4, 7), (13, 13)]

expect
    [[(30, 30), (7, 7), (2, 7), (3, 4), (1, 2), (5, 11), (21, 15)], [(4, 4), (4, 7), (13, 13)]]
    |> List.map (\x -> x |> sort |> union)
    |> \items ->
        when items is
            [a, b] -> a |> diff b
            _ -> [] |> diff []
    == [(1, 3), (8, 11), (15, 21), (30, 30)]
