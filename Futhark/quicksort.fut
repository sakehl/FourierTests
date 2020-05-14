-- Quicksort test 100 1
-- ==
-- tags { n100 n1000 n2000 n5000 n10000 m1000 m10000}
-- input  @ ../data/list_100_1.in

-- Quicksort test 100 100
-- ==
-- tags n1 n1000 n2000 n5000 n10000 m1000 m10000
-- input  @ ../data/list_100_100.in

-- Quicksort test 100 1000
-- ==
-- tags n1 n100 n2000 n5000 n10000 m1000 m10000
-- input  @ ../data/list_100_1000.in

-- Quicksort test 100 2000
-- ==
-- tags n1 n100 n1000 n5000 n10000 m1000 m10000
-- input  @ ../data/list_100_2000.in

-- Quicksort test 100 5000
-- ==
-- tags n1 n100 n1000 n2000 n10000 m1000 m10000
-- input  @ ../data/list_100_5000.in

-- Quicksort test 100 10000
-- ==
-- tags n1 n100 n1000 n2000 n5000 m1000 m10000
-- input  @ ../data/list_100_10000.in



type int = i32

let undef : int = 0
let undefi32 : i32 = 0

-- let toi32 (x : i64) : i32 = 
  

let fill 't (n: i32) (x: t): [n]t =
  replicate n x

let scanr 't [n] (comb : t -> t -> t) (init : t) (xs : [n] t) : [] t =
  reverse <| scan (flip comb) init (reverse xs)

-- type state = ([] int, [] bool)

let partitionPermuteIndex (isLarger : bool) (start : i32) 
      (indexIfSmaller : i32) (indexIfLarger : i32)
      (countSmaller : i32) : i32 =
  start + if isLarger then countSmaller + indexIfLarger else indexIfSmaller

let propagateSegmentHead 't [n] (undef : t) (headFlags : [] bool) 
      (values : [n] t) : [n] t =
  let f left (rightValue, rightFlag) =
        if rightFlag then (rightValue, true) else left
  let e = (undef, true)
  let mempty = (undef, false)
  let (res,_) = unzip 
    -- postscanl
    <| map (e `f`) <| scan f mempty 
    <| zip values 
    <| init headFlags
  in res

let propagateSegmentLast [n] (headFlags : [] bool)
      (values : [n] i32) : [n] i32 =
  let f (leftValue, leftFlag) right = 
         if leftFlag then (leftValue, true) else right
  let e = (undefi32, true)
  let mempty = (0, false)
  let (res,_) = unzip 
    -- postscanr
    <| map (`f` e) <| scanr f mempty
    <| zip values 
    <| tail headFlags
  in res

let postscanSegHead [n] (f : i32 -> i32 -> i32) (headFlags : [] bool)
      (values : [n] i32) : [n] i32 =
  let g (leftValue, leftFlag) (rightValue, rightFlag) = 
        (if rightFlag 
          then rightValue 
          else f leftValue rightValue
        , leftFlag || rightFlag)
  let e = (undefi32, true)
  let mempty = (0, false)
  let (res, _) = unzip
      -- postscanl
      <| map (e `g`) <| scan g mempty 
      <| zip values (init headFlags)
  in res

let writeFlags [m] [n] (writes : [m] i32) (flags : *[n] bool): [n] bool =
  scatter flags writes (fill m true)

let step [n] (values :[n] int ,headFlags :[] bool) : ([n] int, [] bool) = 
  let pivots = propagateSegmentHead undef headFlags values
  let isLarger = map2 (>=) values pivots
  let startIndex = propagateSegmentHead undefi32 headFlags (iota n)
  let indicesLarger  = map (\x -> x-1)
    <| postscanSegHead (+) headFlags 
    <| map (\x -> if x then 1 else 0) isLarger
  let indicesSmaller = map (\x -> x-1)
    <| postscanSegHead (+) headFlags 
    <| map (\x -> if x then 0 else 1) isLarger
  let countSmaller = map (+1) 
    <| propagateSegmentLast headFlags indicesSmaller
  let permutation = map5 partitionPermuteIndex isLarger
        startIndex indicesSmaller indicesLarger countSmaller
  let values' = scatter (fill n undef) permutation values
  let f inc headF start countSmall =
        if headF then start + countSmall + inc else -1
  let writes inc = map3 (f inc) (init headFlags) startIndex countSmaller
  let headFlags' = writeFlags (writes 0)
    (writeFlags (writes 1) (copy headFlags))
  in (values', headFlags')

let condition ((_ :[] int ,headFlags :[] bool)) : bool =
  ! (and headFlags)

let quicksort [n] (input : [n] int) : [n] int =
  let emptyFlags = fill (n+1) false
  let initialFlags = emptyFlags with [0] = true with [n] = true
  let (res, _) = iterate_while condition step (input, initialFlags)
  in res

entry quicksort1 [n] (input : [][n] int) : [][n] int =
  unflatten 1 n <| quicksort <| flatten input

-- let xs : [] int = 
--   rotate 4 
--   <| map (\x -> f32.to_i32 <| 100 * f32.cos (f32.from_fraction x 1)) 
--   <| iota 32

-- let xss (n : i32) : [n][] int =
--   -- replicate n xs
--   unflatten n 32
--   <| map (\x -> f32.to_i32 <| 100 * f32.cos (f32.from_fraction x 10)) 
--   <| iota (32*n)

-- entry main2 (n : i32) : [][] int = map quicksort (xss n)

-- entry main3 : [] int = quicksort xs

let main [n] [m] (input : [n][m] int) : [n][m] int = unsafe (map quicksort input)