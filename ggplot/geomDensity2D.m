(* Mathematica Source File *)
(* :Author: colefaraday *)
(* :Date: 2025-08-08 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomDensity2D implementation *)
ClearAll[geomDensity2D];
geomDensity2D[opts:OptionsPattern[] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0] := Module[{
  statFunc, geomFunc
},
  (* Allow overriding default stat and geom *)
  statFunc = Lookup[Association[opts], "stat", statDensity2D];
  geomFunc = Lookup[Association[opts], "geom", geomDensity2DRender];
  
  <|
    "stat" -> statFunc,
    "geom" -> geomFunc,
    "statParams" -> FilterRules[{opts}, Options[statFunc]],
    "geomParams" -> FilterRules[{opts}, Options[geomFunc]]
  |>
];

Options[geomDensity2DRender] = {
  "data" -> {}, 
  "x" -> Null, 
  "y" -> Null, 
  "color" -> Null, 
  "lineAlpha" -> Null, 
  "fill" -> Null,
  "thickness" -> Null,
  "xScaleFunc" -> Function[Identity[#]], 
  "yScaleFunc" -> Function[Identity[#]],
  "contours" -> Automatic,
  "contourLabels" -> False
};

geomDensity2DRender[statData_, opts : OptionsPattern[]] := Module[{output, contourData, colorDir, alphaDir, thicknessDir, contourPlot, contourLines},
  (* Ensure X/Y has been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, 
    Message[ggplot::xOrYNotGiven]; Throw[Null]
  ];

  (* statData is a single group - a list of associations from statDensity2D *)
  If[Length[statData] == 0, Return[{}]];
  
  (* Extract aesthetics from the first data point (all should be the same for a group) *)
  colorDir = Lookup[First[statData], "color_aes", Black];
  alphaDir = Lookup[First[statData], "lineAlpha_aes", Opacity[1]];
  thicknessDir = Lookup[First[statData], "thickness_aes", Automatic];
  
  (* Transform data for ListContourPlot: {x, y, density} format *)
  contourData = statData // Map[Function[row,
    Module[{xpos, ypos, density},
      xpos = OptionValue["xScaleFunc"][Lookup[row, OptionValue["x"]]];
      ypos = OptionValue["yScaleFunc"][Lookup[row, OptionValue["y"]]];
      density = Lookup[row, "density", 0];
      
      {xpos, ypos, density}
    ]
  ]];
  
  (* Filter out any non-numeric or zero-density points *)
  contourData = Cases[contourData, {_?NumericQ, _?NumericQ, d_?NumericQ} /; d > 0];

	plotRangeAll = {#[[1]] - 0.2 (#[[2]] - #[[1]]), #[[2]] + 0.2 (#[[2]] - #[[1]])} & /@ (MinMax[#] &) /@ Transpose[contourData];
	plotRangeAll[[3]] = {Max[plotRangeAll[[3,1]], 0.], plotRangeAll[[3,2]]};
  
  If[Length[contourData] == 0, Return[{}]];
  
  (* Create contour plot and extract the contour lines *)
  contourPlot = ListContourPlot[contourData, 
    ContourShading -> False,
    ContourStyle -> {colorDir, alphaDir, thicknessDir},
    ContourLabels -> OptionValue["contourLabels"],
    Contours -> OptionValue["contours"],
    Frame -> False,
    Axes -> False,
    PlotRangePadding -> None,
		PlotRange->All
  ];
  
  (* Extract the contour lines from the contour plot *)

	output = {contourPlot[[1]]};
  
  output
];

End[];

EndPackage[];
