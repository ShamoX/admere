(*open LibAdmere;;*)
open TestExample;;

begin
try
  Test.drawGrid ~displayGrid:false ();
with Invalid_argument(s) ->
  Printf.printf "An exception occurs : Invalid_argument(%s)" s;
  Pervasives.flush Pervasives.stdout
end;
while true do
  print_string "\rHello";
  Unix.sleep 10
done
