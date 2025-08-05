(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomPoint implementation *)

Options[geomPoint] = {"data" -> {}, "x" -> Null, "y" -> Null, "color" -> Null, "size" -> Null, "alpha" -> Null, "shape" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomPoint[opts : OptionsPattern[]] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0 := Module[{newDataset, colorFunc, sizeFunc, alphaFunc, shapeFunc, output},
  (* Ensure X/Y has been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, Message[ggplot::xOrYNotGiven]; Throw[Null];];

  newDataset = OptionValue["data"];

  (* Switch dates to absolute times *)
  newDataset = Replace[newDataset, d_?DateObjectQ :> AbsoluteTime[d], Infinity];

  (* For each key necessary, reconcile the aesthetics and append them to the dataset as a column name i.e. "color_aes" -> somecolor *)
  newDataset = reconcileAesthetics[newDataset, OptionValue["color"], "color"];
  newDataset = reconcileAesthetics[newDataset, OptionValue["size"], "size"];
  newDataset = reconcileAesthetics[newDataset, OptionValue["alpha"], "alpha"];
  newDataset = reconcileAesthetics[newDataset, OptionValue["shape"], "shape"];

  (*Grab the point data and for each Point apply the correct aesthetic*)
  output = newDataset // Map[Function[row,
    Module[{shapeObj, colorDir, alphaDir, sizeDir, pos, processedShape},
      shapeObj = row["shape_aes"];
      colorDir = row["color_aes"];
      alphaDir = row["alpha_aes"];
      sizeDir = row["size_aes"];
      pos = {OptionValue["xScaleFunc"][extractMappedValues[{row}, OptionValue["x"]][[1]]], OptionValue["yScaleFunc"][extractMappedValues[{row}, OptionValue["y"]][[1]]]};

      (* Check if shape has placeholder variables (from FilledMarkers[] or similar) *)
      If[StringContainsQ[ToString[shapeObj], "ggplotColorPlaceholder"],
        (* For markers with placeholders, substitute the actual values *)
        processedShape = shapeObj /. {
          ggplotColorPlaceholder -> colorDir,
          ggplotAlphaPlaceholder -> alphaDir,
          ggplotSizePlaceholder -> sizeDir
        };
        {Directive[], Directive[], Translate[processedShape, pos]},
        (* For simple string/symbol shapes, use the original approach *)
        {colorDir, alphaDir, Inset[Style[shapeObj, sizeDir], pos]}
      ]
    ]
  ]];

  (* Grouping data but doing a GeometricTransformation on similar Inset values to speed up the plotting once inside Graphics *)
  output = output // GroupBy[Function[{#[[1]], #[[2]], Inset[#[[3, 1]], {0, 0}]}] -> Function[#[[3, 2]]]] // Normal // Map[{#[[1, 1]], #[[1, 2]], GeometricTransformation[#[[1, 3]], List /@ #[[2]]]} &];

  output

];

End[];

EndPackage[];
