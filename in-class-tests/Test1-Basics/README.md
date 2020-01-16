# In-Class Test 1 -- List functions

Make your changes to the module `Main.hs` in the same directory as this file. 

1. Complete the function
```haskell
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p [] = undefined
myTakeWhile p (x:xs) = undefined
```

where `myTakeWhile p xs` returns elements of `xs` as a list until it reaches
an element of `xs` for which `p` is false. For example, `myTakeWhile (\x ->
x < 3) [1, 2, 3, 4]` returns `[1, 2]`.

2. Find the penultimate (second to last) element in a list.

3. Find the element at index `k` in list `l`. For example: `findK 2 [0,0,1,0,0,0]`
returns `1`.

14. Determine if a list, `l`, is a palindrome.

5. Duplicate the elements in list `xs`. For example `duplicate [1,2,3]` should
give the list `[1,1,2,2,3,3]`. Hint: The `concat` function flattens a
list of lists into a single list. For example: `concat [[1,2,3],[3,4,5]]`
returns `[1,2,3,3,4,5]`.

6. Split a list, `l`, at element `k` into a tuple containing the first part of `l` up
to and including `k`, followed by the second part of `l` after `k`. For example
`splitAtIndex 3 [1,1,1,2,2,2]` returns `([1,1,1],[2,2,2])`.

7. Drop the element at index `k` in list `l`. For example `dropK 3 [0,0,0,1,0,0,0]`
returns `[0,0,0,0,0,0]`.

8. Extract elements between `i`th and `k`th element in list `l`, including `i`, but
not `k`. For example, `slice 3 6 [0,0,0,1,2,3,0,0,0]` returns `[1,2,3]`.

9. Insert element `x` in list `l` at index `k`. For example, `insertElem 2 5
[0,0,0,0,0,0]` returns `[0,0,0,0,0,2,0]`.

10. Rotate list `l` `n` places left. For example, `rotate 2 [1,2,3,4,5]` gives
`[3,4,5,1,2]`.