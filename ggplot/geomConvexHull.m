(* Mathematica Source File *)
(* geomConvexHull: draws the convex hull of a set of points *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomConvexHull implementation *)
ClearAll[geomConvexHull];
geomConvexHull[opts:OptionsPattern[] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0] := Module[{
  statFunc, geomFunc
},
  (* Allow overriding default stat and geom *)
  statFunc = Lookup[Association[opts], "stat", statConvexHull];
  geomFunc = Lookup[Association[opts], "geom", geomConvexHullRender];
  
  <|
    "stat" -> statFunc,
    "geom" -> geomFunc,
    "statParams" -> FilterRules[{opts}, Options[statFunc]],
    "geomParams" -> FilterRules[{opts}, Options[geomFunc]]
  |>
];

(* geomConvexHullRender - dedicated renderer for convex hulls *)
Options[geomConvexHullRender] = {"data" -> {}, "x" -> Null, "y" -> Null, "color" -> Null, "thickness" -> Null, "alpha" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomConvexHullRender[statData_, opts : OptionsPattern[]] := Module[{output, xvals, yvals, pairs, scaledPairs},
  (* Ensure X/Y has been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, 
    Message[ggplot::xOrYNotGiven]; Throw[Null]
  ];

  (* statData is a single group - a list of associations *)
  xvals = extractMappedValues[statData, OptionValue["x"]];
  yvals = extractMappedValues[statData, OptionValue["y"]];
  pairs = Transpose[{xvals, yvals}];
  scaledPairs = Map[{OptionValue["xScaleFunc"][#[[1]]], OptionValue["yScaleFunc"][#[[2]]]} &, pairs];
        
  (* Create a closed polygon for the convex hull *)
  If[Length[scaledPairs] > 0,
    Module[{firstRow, colorDir, alphaDir, thicknessDir, closedPairs},
      firstRow = First[statData];
      colorDir = firstRow["color_aes"];
      alphaDir = firstRow["alpha_aes"];
      thicknessDir = firstRow["thickness_aes"];
      
      (* Close the hull by adding the first point at the end *)
      closedPairs = If[Length[scaledPairs] > 2,
        Append[scaledPairs, First[scaledPairs]],
        scaledPairs
      ];
      
      output = {{colorDir, alphaDir, thicknessDir, Line[closedPairs]}};
    ],
    output = {}
  ];
  
  output
];
End[];
EndPackage[];
