(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2025-08-01 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomErrorBar implementation *)
ClearAll[geomErrorBar];
geomErrorBar[opts:OptionsPattern[] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0] := Module[{
  statFunc, geomFunc
},
  (* Allow overriding default stat and geom *)
  statFunc = Lookup[Association[opts], "stat", statIdentity];
  geomFunc = Lookup[Association[opts], "geom", geomErrorBarRender];
  
  <|
    "stat" -> statFunc,
    "geom" -> geomFunc,
    "statParams" -> FilterRules[{opts}, Options[statFunc]],
    "geomParams" -> FilterRules[{opts}, Options[geomFunc]]
  |>
];

Options[geomErrorBarRender] = {"data" -> {}, "xmin" -> Null, "xmax" -> Null, "ymin" -> Null, "ymax" -> Null, "color" -> Null, "thickness" -> Null, "alpha" -> Null, "capSize" -> 0.02, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomErrorBarRender[statData_, opts : OptionsPattern[]] := Module[{output, capSize},
  (* Ensure all required parameters have been given *)
  If[OptionValue["xmin"] === Null || OptionValue["xmax"] === Null || OptionValue["ymin"] === Null || OptionValue["ymax"] === Null, 
    Message[ggplot::errorBarMissingBounds]; Throw[Null]
  ];

  capSize = OptionValue["capSize"];

  (* Create error bar lines for each data point *)
  output = statData // Map[Function[row,
    Module[{colorDir, alphaDir, thicknessDir, xmin, xmax, ymin, ymax, xcenter, ycenter, xminVal, xmaxVal, yminVal, ymaxVal},
      colorDir = row["color_aes"];
      alphaDir = row["alpha_aes"];
      thicknessDir = row["thickness_aes"];
      
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
      xcenter = (xmin + xmax)/2;
      ycenter = (ymin + ymax)/2;
      
      {colorDir, alphaDir, thicknessDir, Sequence @@ {
        (* Horizontal error bar *)
        Line[{{xmin, ycenter}, {xmax, ycenter}}],
        (* Vertical error bar *)
        Line[{{xcenter, ymin}, {xcenter, ymax}}],
        (* Left cap *)
        Line[{{xmin, ycenter - capSize}, {xmin, ycenter + capSize}}],
        (* Right cap *)
        Line[{{xmax, ycenter - capSize}, {xmax, ycenter + capSize}}],
        (* Bottom cap *)
        Line[{{xcenter - capSize, ymin}, {xcenter + capSize, ymin}}],
        (* Top cap *)
        Line[{{xcenter - capSize, ymax}, {xcenter + capSize, ymax}}]
      }}
    ]
  ]];

  output
];

End[];

EndPackage[];
