/* Простая структура quickUnion по курсу Sedgewick'а */
module quickUnion
import StdEnv, _SystemArray

// Простая структура QuickUnion по курсу Sedgewick'а
makeQuickUnion :: Int -> *{!Int}
makeQuickUnion n = {e \\ e <- [0..n - 1]}

root :: Int *{!Int} -> (Int, *{!Int})
root i qf
    | e == i    = (i, qf`)
    | otherwise = root e qf`
    where (e, qf`) = uselect qf i

find :: Int Int *{!Int} -> (Bool, *{!Int})
find p q qf = (p` == q`, qf``)
    where (p`, qf`) = root p qf
          (q`, qf``) = root q qf`

union :: Int Int *{!Int} -> *{!Int}
union p q qf = snd (replace qf p q)

Start :: {!Int}
Start = union 2 3 (makeQuickUnion 10)
