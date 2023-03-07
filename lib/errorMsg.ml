let anyErrors = ref false

let fileName = ref ""

let lineNum = ref 1

let linePos = ref [1]

let sourceStream = ref stdin

let reset () =
  anyErrors := false;
  fileName := "";
  lineNum := 1;
  linePos := [1];
  sourceStream := stdin

exception Error

(* let error (pos : int) (msg : string) =
   let rec look = function
     | a :: rest, n ->
         if a < pos then List.iter print_string [":"; string_of_int n; "."; string_of_int (pos - a)]
         else look (rest, n - 1)
     | _ -> print_string "0.0"
   in
   anyErrors := true;
   print_string !fileName;
   look (!linePos, !lineNum);
   List.iter print_string [": "; msg; "\n"]*)

let error (((sl, sr), (el, er)) : Absyn.pos) (msg : string) =
  anyErrors := true;
  print_string (!fileName ^ ":");
  print_string
    (string_of_int sl ^ ":" ^ string_of_int sr ^ "-" ^ string_of_int el ^ ":" ^ string_of_int er);
  List.iter print_string [": "; msg; "\n"]

let impossible msg =
  print_string !fileName;
  List.iter print_string [": Error: Compiler bug: "; msg; "\n"];
  flush stdout;
  raise Error