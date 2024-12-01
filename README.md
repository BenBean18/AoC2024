# Advent of Code 2024

In Idris 2!

### Day 1
**Part 1: #4177 in 00:09:50**

I opened the input and sighed with relief when I saw it; I haven't found a regex library for Idris2 yet, so this:
```
3   4
4   3
2   5
1   3
3   9
3   3
```
was a nice gentle start. I could just use `words` :)

Then I realized I'd need to zip? unzip? (it's unzip) those lists so I could sort them. I tried to be fancy and use a `Vect 2 Int`, but after wasting a bit of time, I just went with a `(Int, Int)` since that's what `unzip` uses. So I unzipped them to get a `(List Int, List Int)`, then sorted those lists, then rezipped them and took the difference between every pair and added them up with `sum` (I like functional programming :))

**Part 2: #3827 in 00:13:35**

So we have to do something with counting the number of occurrences of an element in a list. I had a vague theory that I could use a `Set` or `HashMap` where all entries being added have `1` for the value or something but didn't know how to make one in Idris yet, so I just did this instead:
```
count : (Eq a) => List a -> a -> Int
count (x :: xs) el = (if x == el then 1 else 0) + count xs el
count [] _ = 0
```
I forgot the `el` in `count b el` here `map (\el => (el * count b el)) a` which was confusing me (it was throwing a type error), but then I realized (I had went from `map (count b) a` to the above statement and forgot to change it :/)

Anyway, not too much that was exciting today, but functional programming is always nice!