(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomParityLine implementation - draws a y=x diagonal line *)
ClearAll[geomParityLine];
geomParityLine[opts:OptionsPattern[] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0] := Module[{
  statFunc, geomFunc
},
  (* Allow overriding default stat and geom *)
  statFunc = Lookup[Association[opts], "stat", statIdentity];
  geomFunc = Lookup[Association[opts], "geom", geomParityLineRender];
  
  <|
    "stat" -> statFunc,
    "geom" -> geomFunc,
    "statParams" -> FilterRules[{opts}, Options[statFunc]],
    "geomParams" -> FilterRules[{opts}, Options[geomFunc]]
  |>
];

Options[geomParityLineRender] = {"data" -> {}, "color" -> Null, "thickness" -> Null, "alpha" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomParityLineRender[statData_, opts : OptionsPattern[]] := Module[{output},
  (* Create parity lines (y=x diagonal) for each group *)
  output = statData // Map[Function[row,
    Module[{colorDir, alphaDir, thicknessDir},
      colorDir = row["color_aes"];
      alphaDir = row["alpha_aes"];
      thicknessDir = row["thickness_aes"];
      
      (* Return infinite diagonal line (y=x) *)
      {colorDir, alphaDir, thicknessDir,
        InfiniteLine[{{OptionValue["xScaleFunc"][2], OptionValue["yScaleFunc"][2]}, 
                     {OptionValue["xScaleFunc"][3], OptionValue["yScaleFunc"][3]}}]
      }
    ]
  ]];

  output
];

End[];

EndPackage[];
