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
Options[geomConvexHullRender] = {"data" -> {}, "x" -> Null, "y" -> Null, "color" -> Null, "fill" -> Null, "thickness" -> Null, "alpha" -> Null, "lineAlpha" -> Null, "filled" -> True, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomConvexHullRender[statData_, opts : OptionsPattern[]] := Module[{output, filled},
  filled = OptionValue["filled"];
  
  (* Ensure X/Y has been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, 
    Message[ggplot::xOrYNotGiven]; Throw[Null]
  ];

  (* statData contains the convex hull points from statConvexHull *)
  If[Length[statData] > 0,
    If[filled,
      (* Filled convex hull *)
      Module[{xvals, yvals, scaledPairs, firstRow, colorDir, alphaDir, fillDir, lineAlphaDir, thicknessDir, closedPairs},
        (* Extract hull points and scale them *)
        xvals = extractMappedValues[statData, OptionValue["x"]];
        yvals = extractMappedValues[statData, OptionValue["y"]];
        scaledPairs = MapThread[
          {OptionValue["xScaleFunc"][#1], OptionValue["yScaleFunc"][#2]} &, 
          {xvals, yvals}
        ];
        
        (* Get aesthetics from first point *)
        firstRow = First[statData];
        colorDir = Lookup[firstRow, "color_aes", Black];
        alphaDir = Lookup[firstRow, "alpha_aes", Opacity[1]];
        lineAlphaDir = Lookup[firstRow, "lineAlpha_aes", Opacity[1]];
        thicknessDir = Lookup[firstRow, "thickness_aes", Automatic];
        
        (* Handle fill aesthetic - inherit from color if not specified *)
        fillDir = Lookup[firstRow, "fill_aes", colorDir];
        
        (* Polygon automatically closes, so no need to append first point *)
        closedPairs = scaledPairs;
        
        (* Create filled polygon with outline *)
        output = {
          EdgeForm[{colorDir, lineAlphaDir, thicknessDir}], (* Polygon outline with color, lineAlpha, and thickness *)
          fillDir, alphaDir, (* Fill color and fill alpha *)
          Polygon[closedPairs]
        };
      ],
      (* Outline-only convex hull *)
      Module[{xvals, yvals, scaledPairs, firstRow, colorDir, lineAlphaDir, thicknessDir, closedPairs},
        (* Extract hull points and scale them *)
        xvals = extractMappedValues[statData, OptionValue["x"]];
        yvals = extractMappedValues[statData, OptionValue["y"]];
        scaledPairs = MapThread[
          {OptionValue["xScaleFunc"][#1], OptionValue["yScaleFunc"][#2]} &, 
          {xvals, yvals}
        ];
        
        (* Get aesthetics from first point *)
        firstRow = First[statData];
        colorDir = Lookup[firstRow, "color_aes", Black];
        lineAlphaDir = Lookup[firstRow, "lineAlpha_aes", Opacity[1]];
        thicknessDir = Lookup[firstRow, "thickness_aes", Automatic];
        
        (* Close the hull by adding the first point at the end for Line *)
        closedPairs = If[Length[scaledPairs] > 2,
          Append[scaledPairs, First[scaledPairs]],
          scaledPairs
        ];
        
        output = {{colorDir, lineAlphaDir, thicknessDir, Line[closedPairs]}};
      ]
    ],
    output = {}
  ];
  
  output
];
End[];
EndPackage[];
