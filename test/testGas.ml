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
    type t = {
      density : float; (* gas density *)
      speed : array float * float; (* gas medium velocity express in 2 dim *)
    }
    let maxValue = 200000.;;

    let mayAdjustRefinement v =
      if v > 2000.0 then DoRefine
      else if v > 500.0 then DontRefine
      else DoUnrefine
    and getInitialData coor =
      {
        density = Random.float maxValue;
        speed = [| 0.5 , 0.|]
      }
    and getColor v =
      let gray = (2. ** 8.) *. (1. -. (v /. maxValue))
      in Graphics.rgb gray gray gray
    and toStr {d ; s} = Printf.sprintf "d(%f) v(%f,%f)" d s.(0) s.(1)
    and doRefine level {d ; s} oldCoor relNewCoor =
      {
        (* density is equaly distributed around new sub cells *)
        density = d /. (2. ** (float_of_int TestParameter.dim));
        speed = s (* keep same speed *)
      }
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
Sys.command "sleep 10"
