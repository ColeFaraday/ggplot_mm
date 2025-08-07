(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomPath implementation *)
ClearAll[geomPath];
geomPath[opts:OptionsPattern[] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0] := Module[{
  statFunc, geomFunc
},
  (* Allow overriding default stat and geom *)
  statFunc = Lookup[Association[opts], "stat", statIdentity];
  geomFunc = Lookup[Association[opts], "geom", geomPathRender];
  
  <|
    "stat" -> statFunc,
    "geom" -> geomFunc,
    "statParams" -> FilterRules[{opts}, Options[statFunc]],
    "geomParams" -> FilterRules[{opts}, Options[geomFunc]]
  |>
];

Options[geomPathRender] = {"data" -> {}, "x" -> Null, "y" -> Null, "color" -> Null, "thickness" -> Null, "alpha" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomPathRender[statData_, opts : OptionsPattern[]] := Module[{output, xvals, yvals, pairs, scaledPairs},
  (* Ensure X/Y has been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, 
    Message[ggplot::xOrYNotGiven]; Throw[Null]
  ];

  (* statData is a single group - a list of associations *)
  xvals = extractMappedValues[statData, OptionValue["x"]];
  yvals = extractMappedValues[statData, OptionValue["y"]];
  pairs = Transpose[{xvals, yvals}];
  scaledPairs = Map[{OptionValue["xScaleFunc"][#[[1]]], OptionValue["yScaleFunc"][#[[2]]]} &, pairs];
        
  (* Create a single line path using the data order (no sorting) *)
  If[Length[scaledPairs] > 0,
    Module[{firstRow, colorDir, alphaDir, thicknessDir},
      firstRow = First[statData];
      colorDir = firstRow["color_aes"];
      alphaDir = firstRow["alpha_aes"];
      thicknessDir = firstRow["thickness_aes"];
      
      output = {{colorDir, alphaDir, thicknessDir, Line[scaledPairs]}};
    ],
    output = {}
  ];
  
  output
];

End[];

EndPackage[];
