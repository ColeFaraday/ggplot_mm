(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomHLine implementation *)
ClearAll[geomHLine];
geomHLine[opts:OptionsPattern[] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0] := Module[{
  statFunc, geomFunc
},
  (* Allow overriding default stat and geom *)
  statFunc = Lookup[Association[opts], "stat", statIdentity];
  geomFunc = Lookup[Association[opts], "geom", geomHLineRender];
  
  <|
    "stat" -> statFunc,
    "geom" -> geomFunc,
    "statParams" -> FilterRules[{opts}, Options[statFunc]],
    "geomParams" -> FilterRules[{opts}, Options[geomFunc]]
  |>
];

Options[geomHLineRender] = {"data" -> {}, "yintercept" -> Null, "color" -> Null, "thickness" -> Null, "alpha" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomHLineRender[statData_, opts : OptionsPattern[]] := Module[{output, yintercept},
  (* Get yintercept value *)
  yintercept = OptionValue["yintercept"];
  If[yintercept === Null,
    Message[ggplot::yInterceptNotGiven]; Throw[Null]
  ];

  (* Create horizontal lines for each group *)
  output = statData // Map[Function[row,
    Module[{colorDir, alphaDir, thicknessDir, ypos},
      colorDir = row["color_aes"];
      alphaDir = row["alpha_aes"];
      thicknessDir = row["thickness_aes"];
      
      ypos = OptionValue["yScaleFunc"][yintercept];
      
      (* Return infinite horizontal line *)
      {colorDir, alphaDir, thicknessDir,
        InfiniteLine[{{OptionValue["xScaleFunc"][2], ypos}, {OptionValue["xScaleFunc"][3], ypos}}]
      }
    ]
  ]];

  output
];

End[];

EndPackage[];
