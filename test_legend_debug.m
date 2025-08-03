(* Test legend with faceted plot *)

(* Load the package *)
Get["ggplot/Kernel/init.m"];

(* Load mpg data *)
mpgData = Import["Data/mpg.csv", "Dataset"] // Normal;

Print["Testing legend with faceted plot..."];

(* First test: simple legend without faceting *)
Print["Creating simple plot with legend..."];
simplePlot = ggplot[mpgData, aes["displ", "hwy", "color" -> "drv"]] + geomPoint[];
Print["Simple plot result: ", Head[simplePlot]];

(* Second test: faceted plot with legend *)
Print["Creating faceted plot with legend..."];
facetedPlot = ggplot[mpgData, aes["displ", "hwy", "color" -> "drv"]] + 
  geomPoint[] + 
  facetWrap["variable" -> "class", "ncol" -> 3];
Print["Faceted plot result: ", Head[facetedPlot]];
