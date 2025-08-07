(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomLine implementation *)
ClearAll[geomLine];
geomLine[opts:OptionsPattern[] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0] := Module[{
  statFunc, geomFunc
},
  (* Allow overriding default stat and geom *)
  statFunc = Lookup[Association[opts], "stat", statIdentity];
  geomFunc = Lookup[Association[opts], "geom", geomLineRender];
  
  <|
    "stat" -> statFunc,
    "geom" -> geomFunc,
    "statParams" -> FilterRules[{opts}, Options[statFunc]],
    "geomParams" -> FilterRules[{opts}, Options[geomFunc]]
  |>
];

Options[geomLineRender] = {"data" -> {}, "x" -> Null, "y" -> Null, "group" -> Null, "color" -> Null, "thickness" -> Null, "alpha" -> Null, "dashing" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomLineRender[statData_, opts : OptionsPattern[]] := Module[{output, xvals, yvals, pairs, sortedPairs, scaledPairs, connectedSegments},
  (* statData is a single group - a list of associations *)
  
  xvals = extractMappedValues[statData, OptionValue["x"]];
  yvals = extractMappedValues[statData, OptionValue["y"]];
  pairs = Transpose[{xvals, yvals}];
  sortedPairs = SortBy[pairs, First];
  scaledPairs = Map[{OptionValue["xScaleFunc"][#[[1]]], OptionValue["yScaleFunc"][#[[2]]]} &, sortedPairs];
        
  (* Create line segments where each segment takes color from emanating point *)
  connectedSegments = {};
  Do[
    If[i < Length[scaledPairs],
      Module[{point1, color1, alpha1, thickness1},
        point1 = statData[[Ordering[xvals][[i]]]]; (* Get the point data for aesthetics *)
        color1 = point1["color_aes"];
        alpha1 = point1["alpha_aes"]; 
        thickness1 = point1["thickness_aes"];
        AppendTo[connectedSegments, {
          color1,
          alpha1, 
          thickness1,
          Line[{scaledPairs[[i]], scaledPairs[[i + 1]]}]
        }]
      ]
    ],
    {i, Length[scaledPairs]}
  ];
  
  output = connectedSegments;
  output

];

End[];

EndPackage[];
