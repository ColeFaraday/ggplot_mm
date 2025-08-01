(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2025-08-01 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomErrorBoxes implementation *)

Options[geomErrorBoxes] = {"data" -> {}, "xmin" -> Null, "xmax" -> Null, "ymin" -> Null, "ymax" -> Null, "color" -> Null, "alpha" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomErrorBoxes[opts : OptionsPattern[]] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0 := Module[{newDataset, groupbyKeys, output},
  (* Ensure all required parameters have been given *)
  If[OptionValue["xmin"] === Null || OptionValue["xmax"] === Null || OptionValue["ymin"] === Null || OptionValue["ymax"] === Null, 
    Message[ggplot::errorBarMissingBounds]; Throw[Null];];

  newDataset = OptionValue["data"];

  (* Switch dates to absolute times *)
  newDataset = Replace[newDataset, d_?DateObjectQ :> AbsoluteTime[d], Infinity];

  (* For each key necessary, reconcile the aesthetics and append them to the dataset as a column name i.e. "color_aes" -> somecolor *)
  newDataset = reconcileAesthetics[newDataset, OptionValue["color"], "color"];
  newDataset = reconcileAesthetics[newDataset, OptionValue["alpha"], "alpha"];

  (* Group the data based on their aesthetic keys and then apply correct aesthetics while making error box primitives *)
  groupbyKeys = Function[{#["color_aes"], #["alpha_aes"]}];
  output = newDataset //
            GroupBy[groupbyKeys] //
            Values //
            Map[{
              #[[1, "color_aes"]],
              #[[1, "alpha_aes"]],
              (* Create error box rectangles for each data point *)
              Sequence @@ Map[Function[point,
                Module[{xmin, xmax, ymin, ymax, xminVal, xmaxVal, yminVal, ymaxVal},
                  (* Calculate bound values - handle both string keys and functions *)
                  xminVal = If[StringQ[OptionValue["xmin"]], point[OptionValue["xmin"]], OptionValue["xmin"][point]];
                  xmaxVal = If[StringQ[OptionValue["xmax"]], point[OptionValue["xmax"]], OptionValue["xmax"][point]];
                  yminVal = If[StringQ[OptionValue["ymin"]], point[OptionValue["ymin"]], OptionValue["ymin"][point]];
                  ymaxVal = If[StringQ[OptionValue["ymax"]], point[OptionValue["ymax"]], OptionValue["ymax"][point]];
                  
                  (* Apply scaling functions *)
                  xmin = OptionValue["xScaleFunc"][xminVal];
                  xmax = OptionValue["xScaleFunc"][xmaxVal];
                  ymin = OptionValue["yScaleFunc"][yminVal];
                  ymax = OptionValue["yScaleFunc"][ymaxVal];
                  
                  (* Create rectangle from bounds *)
                  Rectangle[{xmin, ymin}, {xmax, ymax}]
                ]
              ], #]
            } &];

  output
];

End[];

EndPackage[];
