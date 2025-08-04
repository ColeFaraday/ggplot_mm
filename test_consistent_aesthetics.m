(* Test consistent aesthetics across facets *)

(* Load the package *)
Get["ggplot/Kernel/init.m"];

(* Load mpg data *)
mpgData = Import["Data/mpg.csv", "Dataset"] // Normal;

Print["Testing consistent aesthetics across facets..."];
Print["Available classes: ", Sort@DeleteDuplicates[mpgData[[All, "class"]]]];
Print["Available drive types: ", Sort@DeleteDuplicates[mpgData[[All, "drv"]]]];

(* Create a faceted plot with color mapping to test consistency *)
Print["Creating faceted plot with color mapping..."];

result = ggplot[mpgData, aes["displ", "hwy", "color" -> "drv"]] + 
  geomPoint[] + 
  facetWrap["variable" -> "class", "ncol" -> 3];

Print["Result: ", result];
