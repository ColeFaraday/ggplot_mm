(* Mathematica Source File *)
(* geomConvexHull: draws the convex hull of a set of points using geomPath *)
BeginPackage["ggplot`"];
Begin["`Private`"];
Options[geomConvexHull] = {"data" -> {}, "x" -> Null, "y" -> Null, "group"->Null, "color" -> Null, "thickness" -> Null, "alpha" -> Null, "stat" -> statConvexHull, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomConvexHull[opts : OptionsPattern[]] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0 := Module[{newDataset, stat, hullPoints, color, thickness, alpha, xScale, yScale, groupbyKeys},

	Print["Testing!"];
	Print[OptionValue["color"]];

  If[OptionValue["x"] === Null || OptionValue["y"] === Null, Message[ggplot::xOrYNotGiven]; Throw[Null];];
  newDataset = OptionValue["data"];
  newDataset = Replace[newDataset, d_?DateObjectQ :> AbsoluteTime[d], Infinity];
  newDataset = reconcileAesthetics[newDataset, OptionValue["color"], "color"];
  newDataset = reconcileAesthetics[newDataset, OptionValue["alpha"], "alpha"];
  newDataset = reconcileAesthetics[newDataset, OptionValue["thickness"], "thickness"];

  groupbyKeys = If[OptionValue["group"] =!= Null,
    Function[{# ["group_aes"], # ["color_aes"], # ["alpha_aes"], # ["thickness_aes"]}],
    Function[{# ["color_aes"], # ["alpha_aes"], # ["thickness_aes"]}]
  ];

	grouped = GroupBy[newDataset, groupbyKeys];

	Print[grouped];

  stat = OptionValue["stat"];
  hullPoints = stat[<|"x" -> #[[OptionValue["x"]]], "y" -> #[[OptionValue["y"]]]|>]&/@grouped;
  color = OptionValue["color"];
  thickness = OptionValue["thickness"];
  alpha = OptionValue["alpha"];
  xScale = OptionValue["xScaleFunc"];
  yScale = OptionValue["yScaleFunc"];

	Print["new dataset: "];
	Print[newDataset];

	Print["hull points: "];
	Print[hullPoints];

  geomPath[
    "data" -> Map[<|"x" -> xScale@#[[1]], "y" -> yScale@#[[2]]|> &, hullPoints],
    "x" -> "x", "y" -> "y", "color" -> color, "thickness" -> thickness, "alpha" -> alpha
  ]
]
End[];
EndPackage[];
