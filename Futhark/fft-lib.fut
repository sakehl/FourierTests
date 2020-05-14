-- Fourier test 1
-- ==
-- entry: fouriertest
-- tags 100 1000 5000 10000 20000
-- input  @ ../data/list_32_32_1.in

-- Fourier test 100
-- ==
-- entry: fouriertest
-- tags 1 1000 5000 10000 20000
-- input  @ ../data/list_32_32_100.in

-- Fourier test 1000
-- ==
-- entry: fouriertest
-- tags 1 100 5000 10000 20000
-- input  @ ../data/list_32_32_1000.in

-- Fourier test 5000
-- ==
-- entry: fouriertest
-- tags 1 100 1000 10000 20000
-- input  @ ../data/list_32_32_5000.in

-- Fourier test 10000
-- ==
-- entry: fouriertest
-- tags 1 100 1000 5000 20000
-- input  @ ../data/list_32_32_10000.in

-- Fourier test 20000
-- ==
-- entry: fouriertest
-- tags 1 100 1000 5000 10000 
-- input  @ ../data/list_32_32_20000.in

import "lib/github.com/diku-dk/fft/stockham-radix-2"

module fft64 = mk_fft f64

type e = f64
type c = (e,e)


entry fft2D [n] [m]  (arr : [n][m] c) : [n][m] c =
  let fft' a = unsafe (transpose  <-< fft64.fft2
               <-< transpose  <-< fft64.fft2
               <| a)
  let go = fft'
  in go arr

entry fouriertest [n][m][k] (input : [n][m][k] e) : [n][m][k] e = 
  let input' = map (map (map (\x -> (x,0)))) <| input
  let res = unsafe (map fft2D) <| input'
  in map (map (map (\(x,_) -> x))) <| res