(* Mathematica Source File *)
(* :Author: colefaraday *)
(* :Date: 2025-08-01 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomErrorBand implementation *)
ClearAll[geomErrorBand];
geomErrorBand[opts:OptionsPattern[] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0] := <|
  "stat" -> statIdentity,
  "geom" -> geomErrorBandRender,
  "statParams" -> FilterRules[{opts}, Options[statIdentity]],
  "geomParams" -> FilterRules[{opts}, Options[geomErrorBandRender]]
|>;

Options[geomErrorBandRender] = {"data" -> {}, "x" -> Null, "ymin" -> Null, "ymax" -> Null, "color" -> Null, "alpha" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomErrorBandRender[statData_, opts : OptionsPattern[]] := Module[{output},
  (* Ensure all required parameters have been given *)
  If[OptionValue["x"] === Null || OptionValue["ymin"] === Null || OptionValue["ymax"] === Null, 
    Message[ggplot::errorBandMissingBounds]; Throw[Null]
  ];

  (* Create filled polygon bands for each group *)
  output = statData // Map[Function[group,
    Module[{colorDir, alphaDir, sortedGroup, xvals, yminvals, ymaxvals, upperPath, lowerPath, polygonPath},
      (* Get aesthetics from first point in group *)
      colorDir = First[group]["color_aes"];
      alphaDir = First[group]["alpha_aes"];
      
      (* Sort group by x values for proper polygon construction *)
      sortedGroup = SortBy[group, Function[point,
        If[StringQ[OptionValue["x"]], point[OptionValue["x"]], OptionValue["x"][point]]
      ]];
      
      (* Extract and scale the values *)
      xvals = Map[Function[point,
        Module[{xval},
          xval = If[StringQ[OptionValue["x"]], point[OptionValue["x"]], OptionValue["x"][point]];
          OptionValue["xScaleFunc"][xval]
        ]
      ], sortedGroup];
      
      yminvals = Map[Function[point,
        Module[{yminval},
          yminval = If[StringQ[OptionValue["ymin"]], point[OptionValue["ymin"]], OptionValue["ymin"][point]];
          OptionValue["yScaleFunc"][yminval]
        ]
      ], sortedGroup];
      
      ymaxvals = Map[Function[point,
        Module[{ymaxval},
          ymaxval = If[StringQ[OptionValue["ymax"]], point[OptionValue["ymax"]], OptionValue["ymax"][point]];
          OptionValue["yScaleFunc"][ymaxval]
        ]
      ], sortedGroup];
      
      (* Create polygon paths *)
      upperPath = Transpose[{xvals, ymaxvals}];
      lowerPath = Reverse[Transpose[{xvals, yminvals}]];
      polygonPath = Join[upperPath, lowerPath];
      
      {colorDir, alphaDir, Polygon[polygonPath]}
    ]
  ]];

  output
];

End[];

EndPackage[];
