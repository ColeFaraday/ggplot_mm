(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomHistogram implementation *)
ClearAll[geomHistogram];
geomHistogram[opts:OptionsPattern[] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0] := Module[{
  statFunc, geomFunc
},
  (* Allow overriding default stat and geom *)
  statFunc = Lookup[Association[opts], "stat", statBin];
  geomFunc = Lookup[Association[opts], "geom", geomHistogramRender];
  
  <|
    "stat" -> statFunc,
    "geom" -> geomFunc,
    "statParams" -> FilterRules[{opts}, Options[statFunc]],
    "geomParams" -> FilterRules[{opts}, Options[geomFunc]]
  |>
];

Options[geomHistogramRender] = {"data" -> {}, "x" -> Null, "y" -> "count", "color" -> Null, "alpha" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomHistogramRender[statData_, opts : OptionsPattern[]] := Module[{output},
  (* statData is grouped bin data from statBin *)
  output = statData // Map[Function[row,
    Module[{colorDir, alphaDir, xmin, xmax, yval, pos1, pos2},
      colorDir = row["color_aes"];
      alphaDir = row["alpha_aes"];
      xmin = OptionValue["xScaleFunc"][row["xmin"]];
      xmax = OptionValue["xScaleFunc"][row["xmax"]];
      yval = OptionValue["yScaleFunc"][row["count"]];

      
      pos1 = {xmin, 0};
      pos2 = {xmax, yval};
      
      {colorDir, alphaDir, Rectangle[pos1, pos2]}
    ]
  ]];
  output
];

End[];

EndPackage[];
