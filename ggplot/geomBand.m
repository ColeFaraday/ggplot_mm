(* Mathematica Source File *)
(* :Author: colefaraday *)
(* :Date: 2025-08-01 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomBand implementation *)
ClearAll[geomBand];
geomBand[opts:OptionsPattern[] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0] := Module[{
  statFunc, geomFunc
},
  (* Allow overriding default stat and geom *)
  statFunc = Lookup[Association[opts], "stat", statIdentity];
  geomFunc = Lookup[Association[opts], "geom", geomBandRender];
  
  <|
    "stat" -> statFunc,
    "geom" -> geomFunc,
    "statParams" -> FilterRules[{opts}, Options[statFunc]],
    "geomParams" -> FilterRules[{opts}, Options[geomFunc]]
  |>
];

Options[geomBandRender] = {"data" -> {}, "x" -> "x", "ymin" -> "ymin", "ymax" -> "ymax", "color" -> Null, "fill" -> Null, "alpha" -> Null, "lineAlpha" -> Null, "thickness" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomBandRender[statData_, opts : OptionsPattern[]] := Module[{output, xVals, yMinVals, yMaxVals, sortedIndices, scaledXVals, scaledYMinVals, scaledYMaxVals, upperPath, lowerPath, polygonPath, colorDir, alphaDir, fillDir, lineAlphaDir, thicknessDir},
  (* Ensure all required parameters have been given *)
  If[OptionValue["x"] === Null || OptionValue["ymin"] === Null || OptionValue["ymax"] === Null, 
    Message[ggplot::errorBandMissingBounds]; Throw[Null]
  ];

  (* statData is a single group - a list of associations *)
  
  (* Extract values using standard method *)
  xVals = extractMappedValues[statData, OptionValue["x"]];
  yMinVals = extractMappedValues[statData, OptionValue["ymin"]];
  yMaxVals = extractMappedValues[statData, OptionValue["ymax"]];
  
  (* Sort by x values for proper polygon construction *)
  sortedIndices = Ordering[xVals];
  scaledXVals = Map[OptionValue["xScaleFunc"], xVals[[sortedIndices]]];
  scaledYMinVals = Map[OptionValue["yScaleFunc"], yMinVals[[sortedIndices]]];
  scaledYMaxVals = Map[OptionValue["yScaleFunc"], yMaxVals[[sortedIndices]]];
  
  (* Get aesthetics from first point in group *)
  colorDir = Lookup[First[statData], "color_aes", Black];
  alphaDir = Lookup[First[statData], "alpha_aes", Opacity[1]];
  
  (* Handle fill aesthetic - inherit from color if not specified *)
  fillDir = Lookup[First[statData], "fill_aes", colorDir];
  
  (* Handle lineAlpha aesthetic for polygon outline *)
  lineAlphaDir = Lookup[First[statData], "lineAlpha_aes", Opacity[1]];
  
  (* Handle thickness aesthetic for polygon outline *)
  thicknessDir = Lookup[First[statData], "thickness_aes", Automatic];
  
  (* Create polygon paths *)
  upperPath = Transpose[{scaledXVals, scaledYMaxVals}];
  lowerPath = Reverse[Transpose[{scaledXVals, scaledYMinVals}]];
  polygonPath = Join[upperPath, lowerPath];

  output = {
    EdgeForm[{colorDir, lineAlphaDir, thicknessDir}], (* Polygon outline with color, lineAlpha, and thickness *)
    fillDir, alphaDir, (* Fill color and fill alpha *)
    Polygon[polygonPath]
  };

  output
];

End[];

EndPackage[];
