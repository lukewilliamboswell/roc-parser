interface Utils
    exposes [sortedMergeDiff]
    imports []

# TODO
# Create a function takes a list of ranges that are valid ex in A-C
# And takes a list of ranges that are invalid ex out D-E

# TODO: Rename to CollapsedRange? Idk
Range a : (Int a, Int a)
Interval a : [Point (Int a), Range (Range a)]

intervalToRange : Interval a -> Range a
intervalToRange = \i ->
    when i is
        Range r -> r
        Point n -> (n, n)

expect
    input = List.concat (List.map [(5, 7), (2, 6)] Range) (List.map [2, 34, 623] Point)
    expected = [(5, 7), (2, 6), (2, 2), (34, 34), (623, 623)]
    actual = input |> List.map intervalToRange
    actual == expected

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

# Find the merge of two sorted ranges if are at least adjacent
mergePair : (Range a, Range a) -> [Nothing, Single (Range a)]
mergePair = \(a, b) ->
    if hasIntersection (a, (b.0 - 1, b.1 + 1)) then
        Single (Num.min a.0 b.0, Num.max a.1 b.1)
    else
        Nothing

expect mergePair ((2, 5), (3, 8)) == Single (2, 8)
expect mergePair ((1, 2), (2, 7)) == Single (1, 7)
expect mergePair ((2, 4), (5, 8)) == Single (2, 8)
expect mergePair ((2, 3), (5, 8)) == Nothing

# Find the merge of two sorted lists of ranges
merge : List (Range a) -> List (Range a)
merge = \a -> List.walk a [] mergeHelper

mergeHelper = \x, aa ->
    when x is
        [] -> List.append x aa
        [.., last] ->
            when mergePair (last, aa) is
                Nothing -> List.append x aa
                Single u -> List.set x ((List.len x) - 1) u

expect
    actual = merge [(1, 1), (2, 7), (3, 4), (5, 11), (7, 7), (15, 21), (30, 30)]
    actual == [(1, 11), (15, 21), (30, 30)]

# Find the difference between two sorted ranges
diffPair : (Range a, Range a) -> [Nothing, Single (Range a), Pair (Range a, Range a)]
diffPair = \(a, b) ->
    if !(hasIntersection (a, b)) then
        Single a
    else if a.0 < b.0 && a.1 > b.1 then
        Pair ((a.0, b.0 - 1), (b.1 + 1, a.1))
    else if a.0 < b.0 then
        Single (a.0, Num.min (b.0 - 1) a.1)
    else if a.1 > b.1 then
        Single (Num.max (b.1 + 1) a.0, a.1)
    else
        Nothing

expect diffPair ((3, 7), (11, 13)) == Single (3, 7)
expect diffPair ((3, 7), (5, 7)) == Single (3, 4)
expect diffPair ((3, 11), (5, 7)) == Pair ((3, 4), (8, 11))
expect diffPair ((5, 7), (3, 7)) == Nothing

# Find the difference between two sorted lists of ranges
diff : List (Range a), List (Range a) -> List (Range a)
diff = \a, b ->
    List.walk a [] (\x, aa -> diffHelper1 x (List.walk b [aa] diffHelper2))

diffHelper1 = \x, fn -> x |> List.concat fn

diffHelper2 = \y, bb -> y
    |> List.joinMap \yy ->
        when diffPair (yy, bb) is
            Single d -> [d]
            Pair d -> [d.0, d.1]
            Nothing -> []

expect diff [(1, 11), (15, 21), (30, 30)] [] == [(1, 11), (15, 21), (30, 30)]
expect diff [(1, 11), (15, 21), (30, 30)] [(4, 7), (13, 13)] == [(1, 3), (8, 11), (15, 21), (30, 30)]
expect diff [(4, 7), (13, 13)] [(1, 11), (15, 21), (30, 30)] == [(13, 13)]

expect
    [(30, 30), (7, 7), (2, 7), (3, 4), (1, 2), (5, 11), (21, 15)]
    |> sort
    |> merge
    == [(1, 11), (15, 21), (30, 30)]

expect
    [(4, 4), (4, 7), (13, 13)]
    |> sort
    |> merge
    == [(4, 7), (13, 13)]

sortedMergeDiff = \include, exclude -> [include, exclude]
    |> List.map (\x -> x |> sort |> merge)
    |> \items ->
        when items is
            [a, b] -> a |> diff b
            _ -> [] |> diff []

expect
    sortedMergeDiff [(30, 30), (7, 7), (2, 7), (3, 4), (1, 2), (5, 11), (21, 15)] [(4, 4), (4, 7), (13, 13)]
    == [(1, 3), (8, 11), (15, 21), (30, 30)]

isDigit = sortedMergeDiff [('1', '5'), ('0', '0'), ('9', '6')] []
expect isDigit == [('0', '9')]

isNonZeroDigit = sortedMergeDiff isDigit [('0', '0')]
expect isNonZeroDigit == [('1', '9')]

isNonFiveDigit = sortedMergeDiff isDigit [('5', '5')]
expect isNonFiveDigit == [('0', '4'), ('6', '9')]
