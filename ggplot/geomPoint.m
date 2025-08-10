(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomPoint implementation *)
ClearAll[geomPoint];
geomPoint[opts:OptionsPattern[] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0] := Module[{
  statFunc, geomFunc
},
  (* Allow overriding default stat and geom *)
  statFunc = Lookup[Association[opts], "stat", statIdentity];
  geomFunc = Lookup[Association[opts], "geom", geomPointRender];
  
  <|
    "stat" -> statFunc,
    "geom" -> geomFunc,
    "statParams" -> FilterRules[{opts}, Options[statFunc]],
    "geomParams" -> FilterRules[{opts}, Options[geomFunc]]
  |>
];

Options[geomPointRender] = {"data" -> {}, "x" -> Null, "y" -> Null, "color" -> Null, "size" -> Null, "alpha" -> Null, "shape" -> Null, "scales" -> <||>};
geomPointRender[statData_, opts : OptionsPattern[]] := Module[{newDataset, colorFunc, sizeFunc, alphaFunc, shapeFunc, output, scales, xMapping, yMapping, xScaleFunc, yScaleFunc},
  (* Get scales and mappings *)
  scales = OptionValue["scales"];
  xMapping = OptionValue["x"];
  yMapping = OptionValue["y"];
  
  (* Extract scale functions for positional mapping *)
  (* Handle both Association format {"x" -> scale, "y" -> scale} and List format {scale1, scale2, scale3} *)
  xScale = Which[
    AssociationQ[scales] && KeyExistsQ[scales, "x"], scales["x"],
    ListQ[scales], SelectFirst[scales, #["aesthetic"] === "x" &, <||>],
    True, <||>
  ];
  yScale = Which[
    AssociationQ[scales] && KeyExistsQ[scales, "y"], scales["y"],
    ListQ[scales], SelectFirst[scales, #["aesthetic"] === "y" &, <||>],
    True, <||>
  ];
  
  xScaleFunc = If[KeyExistsQ[xScale, "transform"],
    xScale["transform"],
    Function[#]  (* Default identity *)
  ];
  yScaleFunc = If[KeyExistsQ[yScale, "transform"],
    yScale["transform"],
    Function[#]  (* Default identity *)
  ];

  Print["stat:", statData];

  (*Grab the point data and for each Point apply the correct aesthetic*)
  output = statData // Map[Function[row,
    Module[{shapeObj, colorDir, alphaDir, sizeDir, pos, processedShape, xValue, yValue},
    Print[row];
      (* Get aesthetic values from _aes columns, with defaults if missing *)
      shapeObj = Lookup[row, "shape_aes", "\[FilledCircle]"];
      colorDir = Lookup[row, "color_aes", Black];
      alphaDir = Lookup[row, "alpha_aes", Opacity[1]];
      sizeDir = Lookup[row, "size_aes", 12];

      Print[colorDir];
      
      (* Get raw x/y values and apply scale transformations *)
      xValue = If[StringQ[xMapping], row[xMapping], xMapping[row]];
      yValue = If[StringQ[yMapping], row[yMapping], yMapping[row]];
      pos = {xScaleFunc[xValue], yScaleFunc[yValue]};

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

  Print[output];

  output


];

End[];

EndPackage[];