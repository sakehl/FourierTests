-- How quickly can we reduce arrays?
--
-- ==
-- input @ list_10_10.in
-- input @ list_100_1000.in 

import "lib/github.com/diku-dk/sorts/quick_sort"

let xs : [] i32 = 
  rotate 2 <| map (\x -> f32.to_i32 <| 100 * f32.cos (f32.from_fraction x 1)) <| iota 32

let xss (n : i32) : [][] i32 =
  -- replicate 2 (copy xs)
  unflatten n 32
  <| map (\x -> f32.to_i32 <| 100 * f32.cos (f32.from_fraction x 10)) 
  <| iota (32*100)

-- let main (n : i32) : [][] i32 =
--   map (qsort (<=)) (copy (xss n))
-- let main (n : i32) : [] i32 =
--    (qsort (<=)) (copy xs)
let main (xss : [][] i32) : [][] i32 =
  map (qsort (<=)) xss
