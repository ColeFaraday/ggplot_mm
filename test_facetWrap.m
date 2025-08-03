(* Test facetWrap functionality *)

(* First load the ggplot package *)
Get["ggplot`"]

(* Load sample data - we'll need to import the mpg dataset *)
mpgData = Import["/Users/user/Documents/Work/Obsidian/documents/projects/programming/ggplot_mm/Data/mpg.csv"];

(* Convert to the expected format - list of associations *)
mpgHeader = First[mpgData];
mpgRows = Rest[mpgData];
mpg2 = Map[AssociationThread[mpgHeader, #] &, mpgRows];

(* Convert numeric columns *)
mpg2 = mpg2 // Map[
  <|
    "manufacturer" -> #["manufacturer"],
    "model" -> #["model"], 
    "displ" -> ToExpression[#["displ"]],
    "year" -> ToExpression[#["year"]],
    "cyl" -> ToExpression[#["cyl"]],
    "trans" -> #["trans"],
    "drv" -> #["drv"],
    "cty" -> ToExpression[#["cty"]],
    "hwy" -> ToExpression[#["hwy"]],
    "fl" -> #["fl"],
    "class" -> #["class"]
  |> &
];

(* Test basic plot without faceting first *)
basicPlot = mpg2 // ggplot["x" -> "displ", "y" -> "hwy", geomPoint[]]

(* Test faceted plot - equivalent to: *)
(* base <- ggplot(mpg2, aes(displ, hwy)) + geom_point() *)
(* base + facet_wrap(~class, ncol = 3) *)
facetedPlot = mpg2 // ggplot["x" -> "displ", "y" -> "hwy", geomPoint[], facetWrap["variable" -> "class", "ncol" -> 3]]

facetedPlot
