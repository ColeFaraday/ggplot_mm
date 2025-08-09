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

Options[geomPointRender] = {
  "data" -> {}, 
  "x" -> Null, "y" -> Null, 
  "color" -> Null, "size" -> Null, "alpha" -> Null, "shape" -> Null, 
  "xScale" -> None, "yScale" -> None,
  "scales" -> <||>
};

geomPointRender[statData_, opts : OptionsPattern[]] := Module[{
  data, xMapping, yMapping, xScale, yScale, scales,
  points, xValues, yValues, colors, sizes, shapes, alphas, output
},
  data = statData; (* statData is already processed by stat function *)
  xMapping = OptionValue["x"];
  yMapping = OptionValue["y"];
  xScale = OptionValue["xScale"];
  yScale = OptionValue["yScale"];
  scales = OptionValue["scales"];
  
  If[Length[data] == 0, Return[{}]];
  
  (* Transform positions using scales *)
  xValues = If[xScale =!= None,
    Map[applyScale[xScale, #[xMapping]] &, data],
    Map[#[xMapping] &, data] (* fallback *)
  ];
  
  yValues = If[yScale =!= None,
    Map[applyScale[yScale, #[yMapping]] &, data],
    Map[#[yMapping] &, data] (* fallback *)
  ];
  
  (* Extract aesthetic values (already applied by aesthetic system) *)
  colors = Map[Lookup[#, "color_aes", Black] &, data];
  sizes = Map[Lookup[#, "size_aes", 5] &, data];
  shapes = Map[Lookup[#, "shape_aes", "\[FilledCircle]"] &, data];
  alphas = Map[Lookup[#, "alpha_aes", 1.0] &, data];
  
  (* Generate point graphics *)
  output = MapThread[Function[{x, y, color, size, shape, alpha, row},
    Module[{pos, processedShape},
      pos = {x, y};
      
      (* Check if shape has placeholder variables (from FilledMarkers[] or similar) *)
      If[StringContainsQ[ToString[shape], "ggplotColorPlaceholder"],
        (* For markers with placeholders, substitute the actual values *)
        processedShape = shape /. {
          ggplotColorPlaceholder -> color,
          ggplotAlphaPlaceholder -> alpha,
          ggplotSizePlaceholder -> size
        };
        {Directive[], Directive[], Translate[processedShape, pos]},
        (* For simple string/symbol shapes, use the original approach *)
        {color, Opacity[alpha], Inset[Style[shape, size], pos]}
      ]
    ]
  ], {xValues, yValues, colors, sizes, shapes, alphas, data}];

  (* Grouping optimization for performance *)
  output = output // GroupBy[Function[{#[[1]], #[[2]], Inset[#[[3, 1]], {0, 0}]}] -> Function[#[[3, 2]]]] // Normal // Map[{#[[1, 1]], #[[1, 2]], GeometricTransformation[#[[1, 3]], List /@ #[[2]]]} &];

  output
];

End[];

EndPackage[];