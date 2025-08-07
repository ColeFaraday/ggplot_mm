(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2025-08-04 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomText implementation *)
ClearAll[geomText];
geomText[opts:OptionsPattern[] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0] := Module[{
  statFunc, geomFunc
},
  (* Allow overriding default stat and geom *)
  statFunc = Lookup[Association[opts], "stat", statIdentity];
  geomFunc = Lookup[Association[opts], "geom", geomTextRender];
  
  <|
    "stat" -> statFunc,
    "geom" -> geomFunc,
    "statParams" -> FilterRules[{opts}, Options[statFunc]],
    "geomParams" -> FilterRules[{opts}, Options[geomFunc]]
  |>
];

Options[geomTextRender] = {
  "data" -> {}, 
  "x" -> Null, 
  "y" -> Null, 
  "label" -> Null,
  "color" -> Null, 
  "size" -> Null, 
  "alpha" -> Null, 
  "hjust" -> 0.5,
  "vjust" -> 0.5,
  "angle" -> 0,
  "fontFamily" -> Automatic,
  "fontWeight" -> Plain,
  "xScaleFunc" -> Function[Identity[#]], 
  "yScaleFunc" -> Function[Identity[#]]
};

geomTextRender[statData_, opts : OptionsPattern[]] := Module[{output},
  (* Ensure X/Y and label have been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, 
    Message[ggplot::xOrYNotGiven]; Throw[Null]
  ];
  If[OptionValue["label"] === Null, 
    Message[ggplot::labelNotGiven]; Throw[Null]
  ];

  (* Create text elements for each data point *)
  output = statData // Map[Function[row,
    Module[{colorDir, alphaDir, sizeDir, pos, labelText, hjust, vjust, angle, fontFamily, fontWeight},
      colorDir = row["color_aes"];
      alphaDir = row["alpha_aes"];
      sizeDir = row["size_aes"];
      
      pos = {OptionValue["xScaleFunc"][extractMappedValues[{row}, OptionValue["x"]][[1]]], 
             OptionValue["yScaleFunc"][extractMappedValues[{row}, OptionValue["y"]][[1]]]};
      
      (* Get label text - could be a column name or direct value *)
      labelText = If[StringQ[OptionValue["label"]], 
        row[OptionValue["label"]], 
        OptionValue["label"]
      ];
      
      hjust = OptionValue["hjust"];
      vjust = OptionValue["vjust"];
      angle = OptionValue["angle"];
      fontFamily = OptionValue["fontFamily"];
      fontWeight = OptionValue["fontWeight"];
      
      (* Return text element *)
      {colorDir, alphaDir, Inset[
        Style[ToString[labelText],
          FontSize -> If[NumericQ[sizeDir], sizeDir, 12],
          FontColor -> colorDir,
          FontFamily -> fontFamily,
          FontWeight -> fontWeight
        ], 
        pos, 
        {hjust - 0.5, vjust - 0.5}, (* Convert to Inset offset format *)
        Automatic,
        angle * Degree
      ]}
    ]
  ]];

  output
];

End[];

EndPackage[];
