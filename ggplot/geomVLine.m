(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomVLine implementation *)
ClearAll[geomVLine];
geomVLine[opts:OptionsPattern[] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0] := <|
  "stat" -> statIdentity,
  "geom" -> geomVLineRender,
  "statParams" -> FilterRules[{opts}, Options[statIdentity]],
  "geomParams" -> FilterRules[{opts}, Options[geomVLineRender]]
|>;

Options[geomVLineRender] = {"data" -> {}, "xintercept" -> Null, "color" -> Null, "thickness" -> Null, "alpha" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomVLineRender[statData_, opts : OptionsPattern[]] := Module[{output, xintercept},
  (* Get xintercept value *)
  xintercept = OptionValue["xintercept"];
  If[xintercept === Null,
    Message[ggplot::xInterceptNotGiven]; Throw[Null]
  ];

  (* Create vertical lines for each group *)
  output = statData // Map[Function[row,
    Module[{colorDir, alphaDir, thicknessDir, xpos},
      colorDir = row["color_aes"];
      alphaDir = row["alpha_aes"];
      thicknessDir = row["thickness_aes"];
      
      xpos = OptionValue["xScaleFunc"][xintercept];
      
      (* Return infinite vertical line *)
      {colorDir, alphaDir, thicknessDir,
        InfiniteLine[{{xpos, OptionValue["yScaleFunc"][2]}, {xpos, OptionValue["yScaleFunc"][3]}}]
      }
    ]
  ]];

  output
];

End[];

EndPackage[];
