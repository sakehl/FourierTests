-- Fourier test
-- ==
-- entry: fouriertest
-- input  @ list_32_32_100.in
-- input  @ list_32_32_1000.in
-- input  @ list_32_32_5000.in
-- input  @ list_32_32_10000.in
-- input  @ list_32_32_20000.in


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
  let input' = map (map (map (\x -> (x,x)))) <| input
  let res = unsafe (map fft2D) <| input'
  in map (map (map (\(x,_) -> x))) <| res