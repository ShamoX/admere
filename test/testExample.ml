open AdmereTypes
open AdmereImplementation
open AdmereParameter
open Admere;;

Random.init 10 ;;

module TestParameter =
  struct
    let minLevel = 3
    and maxLevel = 9
    and dim = 2
    ;;
  end

module TestImplementation =
  struct
    type t = float
    let maxValue = 5.0;;

    let mayAdjustRefinement v =
      if v > 4.0 then DoRefine
      else if v > 1.0 then DontRefine
      else DoUnrefine
    and getInitialData coor =
      Random.float maxValue
    and getColor v =
      Graphics.rgb 135 135 (int_of_float (255. *. v /. 5.0))
    and toStr v = Printf.sprintf "%f" v
    ;;

  end

module Test = Admere(TestImplementation)(TestParameter)
