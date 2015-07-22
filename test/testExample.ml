open AdmereTypes
open AdmereImplementation
open AdmereParameter
open Admere
open Graphics;;

Random.self_init () ;;

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
    let maxValue = 200000.;;

    let mayAdjustRefinement v =
      if v > 2000.0 then DoRefine
      else if v > 500.0 then DontRefine
      else DoUnrefine
    and getInitialData coor =
      (Random.float maxValue)
    and getColor v =
      let rgb = ((2. ** 24.) *. v) /. maxValue in (* put v between 0 and 2^24 *)
      let r = (int_of_float (rgb /. (2. ** 16.)))
      and g = (int_of_float (mod_float (rgb /. (2. ** 8.)) ((2. ** 16.) -. 1.)))
      and b = (int_of_float (mod_float (rgb /. (2. ** 0.)) ((2. ** 8.) -. 1.)))
      in Graphics.rgb r g b
    and toStr v = Printf.sprintf "%f" v
    and doRefine level v oldCoor newCoor =
      v /. (2. ** (float_of_int TestParameter.dim))
    ;;
    let rec doUnrefine level newCoor = function
      [] -> 0.0
      | (v,oldCoor)::l -> v +. (doUnrefine level newCoor l)
    ;;

  end

module Test = Admere(TestImplementation)(TestParameter);;

Graphics.auto_synchronize false;

Test.drawGrid ~displayGrid:true ();
Sys.command "sleep 3";
while Test.checkRefinement () do
  Test.drawGrid ~displayGrid:true ();
  Graphics.synchronize ();
  Sys.command "sleep 3"
done;
Sys.command "sleep 30"
