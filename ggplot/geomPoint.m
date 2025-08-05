(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomPoint implementation *)

Options[geomPointRender] = {"data" -> {}, "x" -> Null, "y" -> Null, "color" -> Null, "size" -> Null, "alpha" -> Null, "shape" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomPointRender[statData_, opts : OptionsPattern[]] := Module[{newDataset, colorFunc, sizeFunc, alphaFunc, shapeFunc, output},
  (* Ensure X/Y has been given *)

  (*Grab the point data and for each Point apply the correct aesthetic*)
  output = statData // Map[Function[row,
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
