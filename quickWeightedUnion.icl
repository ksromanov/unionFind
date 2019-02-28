/* Структура quickUnion с весами по курсу Sedgewick'а */
module quickWeightedUnion
import StdEnv, _SystemArray

// Структура QuickWeightedUnion по курсу Sedgewick'а
// При этом мы кодируем в один массив родителей и вес:
//      { p0, w0, p1, w1, ..., pN, wN}
// для локальности.
makeQuickWeightedUnion :: Int -> *{!Int}
makeQuickWeightedUnion n = {f e \\ e <- [0..(2*n - 1)]}
    where f n
            | 2*(n / 2) == n = n/2
            | otherwise = 1

// Селекторы связей
parent :: *{!Int} Int -> (Int, *{!Int})
parent qf i = uselect qf (2*i)

// Устанавливает родителя ячейки i в n
rparent :: *{!Int} Int Int -> *{!Int}
rparent qf i n = snd (replace qf (2*i) n)

// Селекторы весов
weight :: *{!Int} Int -> (Int, *{!Int})
weight qf i = uselect qf (2*i + 1)

rweight :: *{!Int} Int Int -> *{!Int}
rweight qf i w = snd (replace qf (2*i + 1) w)

// Основные процедуры
root :: Int *{!Int} -> (Int, *{!Int})
root i qf
    | e == i    = (i, qf`)
    | otherwise = root e qf`
    where (e, qf`) = parent qf i

find :: Int Int *{!Int} -> (Bool, *{!Int})
find p q qf = (p` == q`, qf``)
    where (p`, qf`)  = root p qf
          (q`, qf``) = root q qf`

union :: Int Int *{!Int} -> *{!Int}
union pC qC qf = if (pW < qW)
                  (union` (p, pW) (q, qW) qf``)
                  (union` (q, qW) (p, pW) qf``)
    where (p, qf1) = root pC qf
          (q, qf2) = root qC qf1
          (pW, qf`)  = weight qf2 p
          (qW, qf``) = weight qf` q
          union` (p, pw) (q, qw) qf = let qf` = rparent qf p q in
                                      rweight qf` q (qw + pw)

Start :: {!Int}
Start = union 6 8 (union 7 8 (union 8 9 (makeQuickWeightedUnion 10)))
