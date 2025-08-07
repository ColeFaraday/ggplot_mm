(* Mathematica Source File *)
(* :Author: colefaraday *)
(* :Date: 2025-08-01 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomErrorBoxes implementation *)
ClearAll[geomErrorBoxes];
geomErrorBoxes[opts:OptionsPattern[] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0] := Module[{
  statFunc, geomFunc
},
  (* Allow overriding default stat and geom *)
  statFunc = Lookup[Association[opts], "stat", statIdentity];
  geomFunc = Lookup[Association[opts], "geom", geomErrorBoxesRender];
  
  <|
    "stat" -> statFunc,
    "geom" -> geomFunc,
    "statParams" -> FilterRules[{opts}, Options[statFunc]],
    "geomParams" -> FilterRules[{opts}, Options[geomFunc]]
  |>
];

Options[geomErrorBoxesRender] = {"data" -> {}, "xmin" -> Null, "xmax" -> Null, "ymin" -> Null, "ymax" -> Null, "color" -> Null, "alpha" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomErrorBoxesRender[statData_, opts : OptionsPattern[]] := Module[{output},
  (* Ensure all required parameters have been given *)
  If[OptionValue["xmin"] === Null || OptionValue["xmax"] === Null || OptionValue["ymin"] === Null || OptionValue["ymax"] === Null, 
    Message[ggplot::errorBoxMissingBounds]; Throw[Null]
  ];

  (* Create error box rectangles for each data point *)
  output = statData // Map[Function[row,
    Module[{colorDir, alphaDir, xmin, xmax, ymin, ymax, xminVal, xmaxVal, yminVal, ymaxVal},
      colorDir = row["color_aes"];
      alphaDir = row["alpha_aes"];
      
      (* Calculate bound values - handle both string keys and functions *)
      xminVal = If[StringQ[OptionValue["xmin"]], row[OptionValue["xmin"]], OptionValue["xmin"][row]];
      xmaxVal = If[StringQ[OptionValue["xmax"]], row[OptionValue["xmax"]], OptionValue["xmax"][row]];
      yminVal = If[StringQ[OptionValue["ymin"]], row[OptionValue["ymin"]], OptionValue["ymin"][row]];
      ymaxVal = If[StringQ[OptionValue["ymax"]], row[OptionValue["ymax"]], OptionValue["ymax"][row]];
      
      (* Apply scaling functions *)
      xmin = OptionValue["xScaleFunc"][xminVal];
      xmax = OptionValue["xScaleFunc"][xmaxVal];
      ymin = OptionValue["yScaleFunc"][yminVal];
      ymax = OptionValue["yScaleFunc"][ymaxVal];
      
      (* Create rectangle from bounds *)
      {colorDir, alphaDir, Rectangle[{xmin, ymin}, {xmax, ymax}]}
    ]
  ]];

  output
];

End[];

EndPackage[];
