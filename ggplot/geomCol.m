(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomCol implementation *)
ClearAll[geomCol];
geomCol[opts:OptionsPattern[] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0] := <|
  "stat" -> statIdentity,
  "geom" -> geomColRender,
  "statParams" -> FilterRules[{opts}, Options[statIdentity]],
  "geomParams" -> FilterRules[{opts}, Options[geomColRender]]
|>;

Options[geomColRender] = {"data" -> {}, "x" -> Null, "y" -> Null, "color" -> Null, "alpha" -> Null, "width" -> 0.9, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomColRender[statData_, opts : OptionsPattern[]] := Module[{output, width},
  (* Ensure X/Y has been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, 
    Message[ggplot::xOrYNotGiven]; Throw[Null]
  ];

  width = OptionValue["width"] / 2;

  (* Create rectangles for each data point *)
  output = statData // Map[Function[row,
    Module[{colorDir, alphaDir, xpos, yval, pos1, pos2},
      colorDir = row["color_aes"];
      alphaDir = row["alpha_aes"];
      xpos = OptionValue["xScaleFunc"][extractMappedValues[{row}, OptionValue["x"]][[1]]];
      yval = OptionValue["yScaleFunc"][extractMappedValues[{row}, OptionValue["y"]][[1]]];
      
      pos1 = {xpos - width, 0};
      pos2 = {xpos + width, yval};
      
      {colorDir, alphaDir, Rectangle[pos1, pos2]}
    ]
  ]];

  output
];

End[];

EndPackage[];
