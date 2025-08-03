(* Mathematica Source File *)
(* :Author: colefaraday *)
(* :Date: 2025-08-01 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomErrorBand implementation *)

Options[geomErrorBand] = {"data" -> {}, "x" -> Null, "ymin" -> Null, "ymax" -> Null, "color" -> Null, "alpha" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomErrorBand[opts : OptionsPattern[]] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0 := Module[{newDataset, groupbyKeys, output},
  (* Ensure all required parameters have been given *)
  If[OptionValue["x"] === Null || OptionValue["ymin"] === Null || OptionValue["ymax"] === Null, 
    Message[ggplot::errorBandMissingBounds]; Throw[Null];];

  newDataset = OptionValue["data"];

  (* Switch dates to absolute times *)
  newDataset = Replace[newDataset, d_?DateObjectQ :> AbsoluteTime[d], Infinity];

  (* For each key necessary, reconcile the aesthetics and append them to the dataset as a column name i.e. "color_aes" -> somecolor *)
  newDataset = reconcileAesthetics[newDataset, OptionValue["color"], "color"];
  newDataset = reconcileAesthetics[newDataset, OptionValue["alpha"], "alpha"];

  (* Group the data based on their aesthetic keys and then apply correct aesthetics while making error band primitives *)
  groupbyKeys = Function[{#["color_aes"], #["alpha_aes"]}];
  output = newDataset //
            GroupBy[groupbyKeys] //
            Values //
            Map[{
              #[[1, "color_aes"]],
              #[[1, "alpha_aes"]],
              (* Create filled polygon band for the grouped data *)
              Module[{sortedData, xVals, yminVals, ymaxVals, upperPath, lowerPath, polygonPath},
                (* Sort data by x values for proper polygon construction *)
                sortedData = SortBy[#, Function[point,
                  If[StringQ[OptionValue["x"]], point[OptionValue["x"]], OptionValue["x"][point]]
                ]];
                
                (* Extract and scale the values *)
                xVals = Map[Function[point,
                  Module[{xVal},
                    xVal = If[StringQ[OptionValue["x"]], point[OptionValue["x"]], OptionValue["x"][point]];
                    OptionValue["xScaleFunc"][xVal]
                  ]
                ], sortedData];
                
                yminVals = Map[Function[point,
                  Module[{yminVal},
                    yminVal = If[StringQ[OptionValue["ymin"]], point[OptionValue["ymin"]], OptionValue["ymin"][point]];
                    OptionValue["yScaleFunc"][yminVal]
                  ]
                ], sortedData];
                
                ymaxVals = Map[Function[point,
                  Module[{ymaxVal},
                    ymaxVal = If[StringQ[OptionValue["ymax"]], point[OptionValue["ymax"]], OptionValue["ymax"][point]];
                    OptionValue["yScaleFunc"][ymaxVal]
                  ]
                ], sortedData];
                
                (* Create paths for upper and lower bounds *)
                upperPath = Transpose[{xVals, ymaxVals}];
                lowerPath = Reverse[Transpose[{xVals, yminVals}]];
                
                (* Combine into closed polygon *)
                polygonPath = Join[upperPath, lowerPath];
                
                Polygon[polygonPath]
              ]
            } &];

  output
];

End[];

EndPackage[];
