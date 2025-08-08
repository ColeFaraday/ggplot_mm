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

Options[geomBandRender] = {"data" -> {}, "x" -> Null, "ymin" -> Null, "ymax" -> Null, "color" -> Null, "alpha" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomBandRender[statData_, opts : OptionsPattern[]] := Module[{output, xVals, yMinVals, yMaxVals, sortedIndices, scaledXVals, scaledYMinVals, scaledYMaxVals, upperPath, lowerPath, polygonPath, colorDir, alphaDir},
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
  colorDir = First[statData]["color_aes"];
  alphaDir = First[statData]["alpha_aes"];
  
  (* Create polygon paths *)
  upperPath = Transpose[{scaledXVals, scaledYMaxVals}];
  lowerPath = Reverse[Transpose[{scaledXVals, scaledYMinVals}]];
  polygonPath = Join[upperPath, lowerPath];

  output = {colorDir, alphaDir, Polygon[polygonPath]};

  output
];

End[];

EndPackage[];
