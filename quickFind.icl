/* Простая структура quickFind по курсу Sedgewick'а */
module quickFind
import StdEnv, _SystemArray

// Map and update unique array.
mapU :: (a -> a) *{!a} -> *{!a}
mapU f arr = go 0 arr`
    where (sz, arr`) = usize arr
          go i arr =: {[i] = e} // due to laziness it is safe
            | i < sz = go (i + 1) { arr & [i] = f e }
            | otherwise = arr

// Простая структура QuickFind по курсу Sedgewick'а
makeQuickFind :: Int -> *{!Int}
makeQuickFind n = {e \\ e <- [0..n - 1]}

find :: Int Int *{!Int} -> (Bool, *{!Int})
find p q qf = (p` == q`, qf``)
    where (p`, qf`)  = uselect qf p
          (q`, qf``) = uselect qf` p

// Первоначальная версия union, с выделенной функцией go
union` :: Int Int *{!Int} -> *{!Int}
union` p q qf = go 0 (usize qf``)
    where (p`, qf`)  = uselect qf p
          (q`, qf``) = uselect qf` q
          go i (sz, qf)
            | i < sz = go (i + 1) (sz, replace_if qf i)
            | otherwise = qf
          replace_if qf i
            | i` == p` = { qf` & [i] = q` }
            | otherwise = qf`
            where (i`, qf`)  = uselect qf i

union :: Int Int *{!Int} -> *{!Int}
union p q qf = mapU replace qf``
    where (p`, qf`)  = uselect qf p
          (q`, qf``) = uselect qf` q
          replace e
            | e == p`   = q`
            | otherwise = e

Start :: {!Int}
Start = union 2 3 (makeQuickFind 10)
