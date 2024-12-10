# Advent of Code 2024

In Idris 2!

### Day 1
**Part 1: #4177 in 00:09:50, runtime: 551us**

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

**Part 2: #3827 in 00:13:35, runtime: 10.571ms**

So we have to do something with counting the number of occurrences of an element in a list. I had a vague theory that I could use a `Set` or `HashMap` where all entries being added have `1` for the value or something but didn't know how to make one in Idris yet, so I just did this instead:
```
count : (Eq a) => List a -> a -> Int
count (x :: xs) el = (if x == el then 1 else 0) + count xs el
count [] _ = 0
```
(edit: after discussion, `count xs el = sum (map (\el => if x == el then 1 else 0) xs)` is a LOT nicer)

I forgot the `el` in `count b el` here `map (\el => (el * count b el)) a` which was confusing me (it was throwing a type error), but then I realized (I had went from `map (count b) a` to the above statement and forgot to change it :/)

Anyway, not too much that was exciting today, but functional programming is always nice!

### Day 2
**Part 1: #4301 in 00:13:17, runtime: 1.054ms**

I GOT A GATEWAY TIMEOUT AND A LATE START :sad:

![](gateway_timeout.png)

So after that amazing start, I immediately* figured out the first condition (ascending or descending order): just check if the sorted list is equal to the original list.

For the differences part, I wondered if there was a standard way to group values together (2 at a time), but couldn't find one quickly so wrote a function myself.

I subsequently spent approximately 5 minutes trying to get the minimum/maximum of that differences list to verify that the minimum >= 1 and the maximum <= 3. The only minimum/maximum was in `Data.Nat`? So I converted from `Int` to `Nat` but it still didn't typecheck? I was confused, couldn't figure it out, then realized I could just sort and take the head and last elements.

But then I had to convince Idris the list was non-empty to get the head and last, and I initially chose the wrong list to prove...anyway I figured it out, was feeling good, YOLOed it, and got it...wrong?

I checked it against the sample, and it was in fact not identifying a valid list. ...because...if the list is decreasing...then it'll be equal to the reverse of the sorted list...

After that fix, it worked on the sample and on the real input.

**Part 2: #2530 in 00:18:46, runtime: 3.542ms**
Not too bad, just some old fashioned brute force. The thing that tripped me up the most was I initially summed up ones for *all* lists with one element removed, but it should only be 1 if one of them is valid and 0 otherwise. After that fix, it worked :)

### Day 3

**Part 1: #1939 in 00:05:48**
Python, regex in VS Code, because I thought I could do it fast but really couldn't

`mul\(\d+,\d+\)`

Solved it decently quickly, then

**Part 2: #7331 in 00:37:00, runtime: 168us**
could you tell I switched languages? :)

worked on Idris parsing which took much longer and I'm still cleaning it up, initially read numbers backwards because `(newChar :: existingNumString)` is not `(existingNumString ++ [newChar])` ...oops

TODO use https://github.com/kasiaMarek/TyRE maybe, idk

But in this silly journey we fell down a dependent typing rabbit hole trying to constrain my `Instruction` type to only be numeric, which probably would have happened anyway

I need sleep but this is really interesting

also it might snow but it's not snowing which makes us snow sad (even checked 4W alcove window)

### Day 4
This one was a bit annoying. /shrug

**Part 1: #6614 in 00:32:23, runtime: 7.965ms**

I was happy with counting the `XMAS`es in the lines, just string pattern matching `('X' :: 'M' :: 'A' :: 'S' :: xs)` to see if it exists and then adding 1 if so and if not just recursing to the next character (and checking the reverse as well). Then horizontal checking is just mapping that function over each row and adding up the results (one count for every row), and vertical checking is the transpose.

Major diagonal took me a tiny bit to think about how to do functionally, but it wasn't that bad; just take the top left element and recurse on the rest of the rows of the 2D array with the first column dropped on all of them. 

I did a lot of verification in the REPL, because I was tired and not sure if I did things right. The matrix
```
1 2 3
4 5 6
7 8 9
```
was a useful test case :)

My thought to get the non-major diagonals was to drop each row or column, e.g. dropping rows below
```
4 5 6
7 8 9
```
gives `4 8`

and then dropping the next row gives `7`

Which works, but then you need to consider dropping columns also to get above the diagonal (this only gets below the diagonal). And I have to avoid duplicating the major diagonal, which I tried to do multiple ways for speed (and got it wrong) before actually thinking about it and testing in the REPL and getting the correct answer with a correct deduplication, which was:

`(\list => length list /= length l)` (duh, yeah the major diagonal is the only one with a length equal to the length of a list, was glad to think of this but wished I had thought of it earlier)

I also forgot to get the diagonals going the other way (e.g. `3 5 7`, `6 8`, `9`, etc), so that was a simple `map (\row => reverse row) 2d_array`.

This just wasn't the best approach and had a ton of edge cases and was generally annoying.

**Part 2: #7170 in 00:56:03, runtime: 13.417ms**

I had a clear idea initially for how to solve Part 2: just check the coordinates of every `A` in diagonal `MAS`es, and duplicate coordinates form an `X-MAS`. Attaching coordinates to every item in the 2D array took a bit to code and I made a few other silly mistakes, but I eventually got there.

---

A much better method that I learned about after the fact (from some on-hall discussion once we had both solved it) was padding the start of each row with a number of dummy values corresponding to its index and taking the columns of that new array:
```
1 2 3
. 4 5 6
. . 7 8 9
```
gives `1`, `2 4`, `3 5 7`, `6 8`, `9` which is all the "backwards" diagonals, and we can just transpose to get the normal direction diagonals. That would have been a lot nicer to code.

also uh I should read the standard library especially `Data.List`
```
Data.List.deleteAt : (idx : Nat) -> (xs : List a) -> {auto 0 _ : InBounds idx xs} -> List a
deleteAt 0 (_ :: xs) = xs
deleteAt (S k) (x :: xs) = x :: deleteAt k xs
```
this implementation is so cool! would have been very helpful for Day 2 but like unwrapping a `Nat` until it hits `Z` and then removing once that happens is very cool :)

### Day 5

**Part 1: #6240 in 00:29:18, runtime: 8.476ms**

**Part 2: #5007 in 00:44:21, runtime: initially 385.275ms then 27.353ms**

Notes in source

### Day 6

**Part 1: #7395 in 00:38:57, runtime: 11.773ms**

**Part 2: #4137 in 01:02:14 :(, runtime: 51.905s ugh i need to optimize this**

Some notes in source, I'm tired

### Day 7

**Part 1: #2632 in 00:14:54, runtime: 6.454ms**

**Part 2: #1790 in 00:16:51 (first on NCSSM leaderboard!), runtime: 1.107s (after optimization)**

This went pretty well :)

Parsing was the most annoying part (proving to Idris that the list was `NonEmpty` so I could `init` the first word)

I also wrote a function definition with `::` instead of `:` (Haskell moment) and spent a few minutes confused why it wouldn't compile, womp womp

I'm about to go to sleep, but there are more notes in the source

### Day 8

**Part 1: #3904 in 00:28:19, runtime: 16.908ms**

**Part 2: #5860 in 01:01:41, runtime: 313.331ms**

Commentary in source

### Day 9

**Part 1: #2866 in 00:25:00, runtime: 34.1s**

**Part 2: #9307 in 03:39:36..., runtime (with printouts): 9.552s**

this one was annoying and led to sleep deprivation

when I was testing Part 2, I realized it was getting slower as more blocks were being removed, and eventually tracked it down to tons of noncontiguous whitespace blocks at the end that it has to iterate through every time (and the number of blocks kept increasing).

right when I was about to go to sleep I realized I could compress trailing whitespace into one contiguous block instead of deleting it (which has issues when recursing, since trailing is not always at the end of the whole thing), so then spent ~10min fixing it

### Day 10

**Part 1: #1486 in 00:12:18, runtime: 4.708ms**

**Part 2: #1962 in 00:19:11, runtime: 4.840ms**

Turns out a map lookup is MUCH more efficient (100x speedup) than looking through **every pair**...maybe coding for speed has its drawbacks sometimes. That optimization felt really good.

Also, when you copy a recursive function for Part 2, make sure you change the name of the recursive call :upside_down: