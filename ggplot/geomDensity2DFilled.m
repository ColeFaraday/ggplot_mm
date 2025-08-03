(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2025-08-02 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomDensity2DFilled implementation *)

Options[geomDensity2DFilled] = {"data" -> {}, "x" -> Null, "y" -> Null, "color" -> Null, "alpha" -> Null, "levels" -> 10, "bandwidth" -> Automatic, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomDensity2DFilled[opts : OptionsPattern[]] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0 := Module[{newDataset, groupbyKeys, output},
  (* Ensure X/Y has been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, Message[ggplot::xOrYNotGiven]; Throw[Null];];

  newDataset = OptionValue["data"];

  (* Switch dates to absolute times *)
  newDataset = Replace[newDataset, d_?DateObjectQ :> AbsoluteTime[d], Infinity];

  (* For each key necessary, reconcile the aesthetics and append them to the dataset as a column name i.e. "color_aes" -> somecolor *)
  newDataset = reconcileAesthetics[newDataset, OptionValue["color"], "color"];
  newDataset = reconcileAesthetics[newDataset, OptionValue["alpha"], "alpha"];

  (* Group the data based on their aesthetic keys and then apply correct aesthetics while making density primitives *)
  groupbyKeys = Function[{#["color_aes"], #["alpha_aes"]}];
  output = newDataset //
            GroupBy[groupbyKeys] //
            Values //
            Map[{
              #[[1, "color_aes"]],
              #[[1, "alpha_aes"]],
              (* Create density contours for the grouped data *)
              Module[{xVals, yVals, xRange, yRange, kde, contourGraphics, primitives},
                (* Extract x and y values *)
                xVals = Map[Function[point,
                  If[StringQ[OptionValue["x"]], point[OptionValue["x"]], OptionValue["x"][point]]
                ], #];
                yVals = Map[Function[point,
                  If[StringQ[OptionValue["y"]], point[OptionValue["y"]], OptionValue["y"][point]]
                ], #];
                
                (* Apply scaling functions *)
                xVals = Map[OptionValue["xScaleFunc"], xVals];
                yVals = Map[OptionValue["yScaleFunc"], yVals];
                
                (* Determine data ranges and expand them for natural density falloff *)
                xRange = MinMax[xVals];
                yRange = MinMax[yVals];
                
                (* Expand ranges by 20% on each side for natural density tails *)
                xSpan = xRange[[2]] - xRange[[1]];
                ySpan = yRange[[2]] - yRange[[1]];
                xRangeExpanded = {xRange[[1]] - 0.2*xSpan, xRange[[2]] + 0.2*xSpan};
                yRangeExpanded = {yRange[[1]] - 0.2*ySpan, yRange[[2]] + 0.2*ySpan};

                
                (* Create kernel density estimation *)
                kde = SmoothKernelDistribution[Transpose[{xVals, yVals}], 
                  If[OptionValue["bandwidth"] === Automatic, 
                    Automatic, 
                    OptionValue["bandwidth"]]];
                
                (* Generate filled contour plot and extract polygons *)
                contourGraphics = ContourPlot[PDF[kde, {x, y}], 
                  {x, xRangeExpanded[[1]], xRangeExpanded[[2]]}, 
                  {y, yRangeExpanded[[1]], yRangeExpanded[[2]]},
                  Contours -> OptionValue["levels"],
                  ContourShading -> True,
                  ContourLines -> False,
                  PlotPoints -> 50,
                  Frame -> False,
                  Axes -> False
                ];
                
                (* Extract polygon primitives from the contour plot *)
                primitives = FullForm[contourGraphics][[1,1]];

                
                (* Return the polygons *)
                Sequence @@ {primitives}
              ]
            } &];

  output
];

End[];

EndPackage[];
