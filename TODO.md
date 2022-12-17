Planned operations:

[x] Map -> takes two sequences and runs the 2nd on each element of the 1st, returning the mapped sequence

    ```
    [1 2 3 4 5] [dup *] map -- [1 4 9 16 25]
    ```

[x] Filter -> takes two sequences and returns a new sequence of only the elements from the 1st that return true when running the 2nd on each element

    ```
    [1 2 3 4 5] [2 >] filter -- [3 4 5]
    ```

[x] Reduce -> takes two sequences and an accumulator and reduces them into a single value on the stack

    ```
    [1 2 3 4 5] [+] 0 reduce -- 15
    ```

[x] Head + Last -> Returns the first and last element of a sequence respectively

    ```
    [1 2 3 4 5] head -- 5
    [1 2 3 4 5] last -- 1
    ```

[x] Tail + Init -> returns all but the first and the last elements of a sequence respectively

    ```
    [1 2 3 4 5] tail -- [2 3 4 5]
    [1 2 3 4 5] init -- [1 2 3 4]
    ```

[~] Cons -> inserts an element at the top of a sequence, or creates a new sequence

    ```
    [1 2] 3 cons -> [1 2 3]
    [1 2] [3 4] cons -> [1 2 [3 4]]
    1 2 cons -> [1 2]
    ```

[ ] Nth/get (name tbd) -> pushes the nth element of a sequence onto the stack, preserving the original sequence

    ```
    [1 2 3] 1 nth -- [1 2 3] 2
    [1 2 3] 1 get -- [1 2 3] 2
    ```

[ ] Splice (name tbd) -> pushes a subsequence of the original sequence onto the stack, removing it from the original sequence

    ```
    [1 2 3] 0 1 splice -- [1] [2 3]
    ```

[ ] Slice -> pushes a sublist of the original list onto the stack, preserving the original list

    ```
    [1 2 3] 0 1 slice -- [1 2 3] [2 3]
    ```

[ ] Drop -> Takes a sequence and an integer and drops that many elements from the sequence

    ```
    [1 2 3 4 5] 2 drop -- [1 2 3]
    ```

[ ] Take -> Takes a sequence and an integer and forms a new list from that many elements

    ```
    [1 2 3 4 5] 3 take -- [3 4 5]
    ```

[x] Reverse -> reverses a sequence

    ```
    [1 2 3 4 5] reverse -- [5 4 3 2 1]
    ```

[ ] Flatten -> flattens a sequence of sequences (maybe)

    ```
    [1 2 [3 4 [5]]] flatten -- [1 2 3 4 5]
    ```

[ ] Zip -> Joins two equal length sequences into a sequence of pairs

    ```
    [1 2 3 4 5] [1 4 9 16 25] zip -- [[1 1] [2 4] [3 9] [4 16] [5 25]]
    ```

[x] Concat -> concatenates two sequences

    ```
    [1 2 3] [4 5] concat -> [1 2 3 4 5]
    ```

[ ] Range operations -> creates a sequence from a range

    ```
    [1..10] -- [1 2 3 4 5 6 7 8 9]
    [1..=10] -- [1 2 3 4 5 6 7 8 9 10]
    ```

[ ] For (requires variables) -> sugar for a loop

    ```
    [0..10] for i do # assigns the next value in the sequence to i on each iteration
        i print
    end
    ```

# Features

[x] Recursion

Recursive methods:

    ```
    define factorial (int -- int)
        0 =
        [1]
        [dup 1 - factorial *]
        if
    end
    ```

[~] Strings - Should be a list of char to enable len and mapping?

    ```
    "hello" print
    ```

[x] Imports

    ```
    "sugar" import

    [1 2 3] empty? [[print] apply] [pop] if
    ```