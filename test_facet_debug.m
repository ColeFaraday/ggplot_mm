(* Test facetWrap with debugging *)

(* Load the package *)
Get["ggplot/Kernel/init.m"];

(* Load mpg data *)
mpgData = Import["Data/mpg.csv", "Dataset"] // Normal;

Print["mpgData sample: ", Take[mpgData, 3]];
Print["mpgData columns: ", Keys[First[mpgData]]];

(* Create a simple faceted plot *)
Print["Creating faceted plot..."];

result = ggplot[mpgData, aes["displ", "hwy"]] + 
  geomPoint[] + 
  facetWrap["variable" -> "class", "ncol" -> 3];

Print["Final result: ", result];
