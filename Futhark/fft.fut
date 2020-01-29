

-- Single lists
-- /==
-- input  @ list_100_1.in
-- input  @ list_1000_1.in
-- input  @ list_2000_1.in
-- input  @ list_5000_1.in
-- input  @ list_10000_1.in
-- input  @ list_20000_1.in

import "complex"
import "futlib/math"

type e = f64
let pi = f64.pi
let cos = f64.cos
let sin = f64.sin
let fromInt (x : i32) : e = f64.from_fraction x 1


module complex = mk_complex f64
type c = complex.complex

type mode = #Forward | #Reverse | #Inverse

let signOfMode (m : mode) : e =
  match m 
    case #Forward -> -1
    case #Reverse -> 1
    case #Inverse -> 1

let backpermute 't (n :i32) (m : i32) (f : (i32, i32) -> (i32, i32)) (arr : [][] t) : [n][m] t
  = let getval y x = let (newy, newx) = f (y,x)
                     in arr[newy, newx]
    in tabulate_2d n m getval

let backpermute3d 't (n :i32) (m : i32) (o : i32) (f : (i32, i32, i32) -> (i32, i32, i32)) (arr : [][][] t) : [n][m][o] t
  = let getval z y x = let (newz, newy, newx) = f (z, y, x)
                       in arr[newz, newy, newx]
    in tabulate_3d n m o getval

-- TODO: ZipWith with maps?
-- But then it doesn't check bounds correctly
let zipWith 't1 't2 't3 [o1] [o2] [n1] [n2] [m1] [m2] (f : t1 -> t2 -> t3) (xs : [o1][n1][m1] t1) (ys : [o2][n2][m2] t2) : [][][] t3
  = let mino = i32.min o1 o2
    let minn = i32.min n1 n2
    let minm = i32.min m1 m2
    let g z y x = f xs[z,y,x] ys[z,y,x]
    in tabulate_3d mino minn minm g

let concatOn2 't [o1] [o2] [n1] [n2] [m1] [m2] (xs : [o1][n1][m1] t) (ys : [o2][n2][m2] t) : [][][] t
  = let minm = i32.min m1 m2
    let o2 = i32.min o1 o2
    let f z y x = if y < n1 then xs[z, y, x] else ys[z, y - n1, x]
    in tabulate_3d o2 (n1+n2) minm f

let concatOn1 't [o1] [o2] [n1] [n2] [m1] [m2] (xs : [o1][n1][m1] t) (ys : [o2][n2][m2] t) : [][][] t
  = let minn = i32.min n1 n2
    let o2 = i32.min o1 o2
    let f z y x = if x < m1 then xs[z, y, x] else ys[z, y, x - m1]
    in tabulate_3d o2 minn (m1+m2) f

let transform2 [m] [n] (v : c) (xs : [m][n][] c) : [m][n][2] c
  = let f z y k = let x0 = xs[z,y,0]
                  let x1 = xs[z,y,1]
                  in if k==0 then x0 complex.+ x1 else x0 complex.+ v complex.* x1
    in tabulate_3d m n 2 f

let zipWithExtrude1 'a 'b 'c [n] [m] [o1] [o2] (f : a -> b -> c) (xs : [o1] a) (ys : [n][m][o2] b) : [n][m][] c
  = let getval i j k   = f xs[k] ys[i,j,k]
    let mino = i32.min o1 o2
    in tabulate_3d n m mino getval

let headV 't [m] [n] [o] (xs : [m][n][o] t) : [m][o] t
  = xs[:m, 0, :o]

let sieve 't [sh] [m] [n] (fac : i32) (start : i32) (xs : [sh][m][n] t) : [][][] t =
  let f (ix,ix2,j) = (ix, ix2, fac*j + start)
  in backpermute3d sh m (n // fac) f xs

let sieveV 't [sh] [m] [n] (fac : i32) (start : i32) (xs : [sh][m][n] t) : [sh][][n] t =
  let f (ix,j,i) = (ix, fac*j + start, i)
  in backpermute3d sh (m // fac) n f xs

let twist 't [sh] [m] [n] (fac : i32) (xs : [sh][m][n] t) : [sh][][] t =
  let f (ix,j,i) = (ix, j // fac, fac*i + j % fac)
  in backpermute3d sh (fac*m) (n // fac) f xs

let get2D 't [n] (_ : [][n][] t) : i32 = n

let dropV 't (n : i32) (xs : [][][] t) : [][][] t = map (drop n) xs

let ditSplitRadixLoop  [sh] [n] (mode : mode) (arr : [sh][n] c) : [][] c =
  let twiddleSR n4 k j =
    let w = pi * k * (fromInt j) / (2* (fromInt n4))
    in complex.mk (cos w) (signOfMode mode * sin w)

  let twiddle len4 k : [] c =
    tabulate len4 (twiddleSR len4 k)
  
  let step [k] (us : [][][] c, zs : [][][k] c) =
    let tw1 : [] c= twiddle k 1
    let tw3 = twiddle k 3
    --
    let im          = complex.mk_im (signOfMode mode)
    let twidZeven   = zipWithExtrude1 (complex.*) tw1 (sieveV 2 0 zs)
    let twidZodd    = zipWithExtrude1 (complex.*) tw3 (sieveV 2 1 zs)
    
    let zsum        = zipWith (complex.+) twidZeven twidZodd
    let zdiff       = zipWith (\c1 c2 -> im complex.* (c1 complex.- c2)) twidZeven twidZodd
    -- ++ does a concatenate on the outer dimension. Not the inner
    let zcomplete = concatOn1 zsum zdiff
    let n = get2D zcomplete

    in ((zipWith (complex.+) us zcomplete) `concatOn1` (zipWith (complex.-) us zcomplete)
       , dropV n us
       )

  let rebase (xs, ys) = (transform2 (complex.mk_re (-1)) xs, ys)

  let firstPred [s] (_ : ([][][] c, [][][s] c)) : bool =
    s > 1

  let secondPred [s] (_ : ([][][] c, [][s][] c)) : bool =
    s > 1

  let reorder (xs, ys) =
    let evens = sieve 2 0 xs
    let odds  = sieve 2 1 xs
    in (concatOn2 evens ys, twist 2 odds)

  let initial : ([sh][][n] c, [][][] c) =
  -- Unsure if "//" truncates to zero
        (unflatten_3d sh 1 n <| flatten arr, unflatten_3d sh 0 (n // 2) <| replicate 0 (complex.mk_re <| fromInt 0) )
  let (res, _) =
    iterate_while secondPred step 
    <| rebase
    <| iterate_while firstPred reorder 
    <| initial
  in headV res

entry fft2D [n] [m] (mode : mode) (arr : [n][m] c) : [n][m] c =
  let scale = complex.mk_re <-< fromInt <| n*m
  let fft' a = transpose  <-< ditSplitRadixLoop mode
               <-< transpose  <-< ditSplitRadixLoop mode
               <| a
  let go = fft'
  in match mode
       case #Inverse -> map (map (complex./ scale)) (go arr)
       case _        -> go arr
  
entry fft1D [n] [m] (mode : mode) (arr : [n][m] c) : [n][m] c =
  let scale = complex.mk_re <-< fromInt <| m
  let fft' a = ditSplitRadixLoop mode
               <| a
  let go = fft'
  in match mode
       case #Inverse -> map (map (complex./ scale)) (go arr)
       case _        -> go arr

entry test (n: i32) : [n][n] c = tabulate_2d n n (\x y -> complex.mk_re <| fromInt (x + y))
entry test2 (n: i32) : [][] e = map (map (\(x,_) -> x)) <| test n

let main [n] [m] (input : [n][m] c) : [][] c = unsafe (fft1D #Forward input)
entry main2d [n] [m] (input : [n][m] c) : [][] c = unsafe (fft2D #Forward input)

entry main_n (n: i32) : [n][n] e = let (res) = (main <| test n) in map (map (\(x,_) -> x)) <| res
entry mainy_n (n: i32) : [n][n] e = let (res) = (main <| test n) in map (map  (\(_,y) -> y)) <| res

entry main2d_n (n: i32) : [n][n] e = let (res) = (main2d <| test n) in map (map (\(x,_) -> x)) <| res
entry main2dy_n (n: i32) : [n][n] e = let (res) = (main2d <| test n) in map (map  (\(_,y) -> y)) <| res