(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2025-08-04 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomText implementation *)
ClearAll[geomText];
geomText[opts:OptionsPattern[] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0] := <|
  "stat" -> statIdentity,
  "geom" -> geomTextRender,
  "statParams" -> FilterRules[{opts}, Options[statIdentity]],
  "geomParams" -> FilterRules[{opts}, Options[geomTextRender]]
|>;

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
              alphaDir
            ],
            Sequence @@ Scaled /@ pos
          ],
          (* Use regular coordinates for x,y positioning *)
          Inset[
            Style[
              ToString@labelText,
              FontSize -> sizeDir,
              FontColor -> colorDir,
              FontFamily -> fontFamily,
              FontWeight -> fontWeight,
              alphaDir
            ],
            pos[[1]],
            {2*hjust - 1, 2*vjust - 1}, (* Convert 0-1 range to -1 to 1 range for positioning *)
            {Cos[angle*Degree], Sin[angle*Degree]} (* Rotation vector *)
          ]
        ]
			};
    ],
    (* Original dataset mapping case *)
    Module[{},
      (* For each key necessary, reconcile the aesthetics and append them to the dataset as a column name i.e. "color_aes" -> somecolor *)
      newDataset = reconcileAesthetics[newDataset, OptionValue["color"], "color"];
      newDataset = reconcileAesthetics[newDataset, OptionValue["size"], "size"];
      newDataset = reconcileAesthetics[newDataset, OptionValue["alpha"], "alpha"];

      (* Get text data and apply aesthetics *)
      output = newDataset // Map[Function[row,
        Module[{
          colorDir, alphaDir, sizeDir, pos, labelText, hjust, vjust, angle, 
          fontFamily, fontWeight, textStyle
          },
          
          colorDir = row["color_aes"];
          alphaDir = row["alpha_aes"];
          sizeDir = row["size_aes"];
          
          (* Calculate position - either from location string or x,y coordinates *)
          pos = If[OptionValue["location"] =!= Null,
            (* Use location string with GetScaledCoord *)
            GetScaledCoord[OptionValue["location"]],
            (* Handle x/y as either column names or direct values *)
            {{
              If[NumericQ[OptionValue["x"]],
                OptionValue["xScaleFunc"][OptionValue["x"]],
                OptionValue["xScaleFunc"][row[OptionValue["x"]]]
              ],
              If[NumericQ[OptionValue["y"]],
                OptionValue["yScaleFunc"][OptionValue["y"]],
                OptionValue["yScaleFunc"][row[OptionValue["y"]]]
              ]
            }, {0, 0}} (* Format to match GetScaledCoord output *)
          ];
          
          (* Get the label text - handle Text[...] wrapper or column names *)
          labelText = Which[
            MatchQ[OptionValue["label"], Text[_]],
            OptionValue["label"][[1]], (* Extract from Text[...] *)
            StringQ[OptionValue["label"]],
            row[OptionValue["label"]], (* Column name *)
            True,
            OptionValue["label"] (* Direct value *)
          ];
          
          (* Get positioning and styling options *)
          hjust = OptionValue["hjust"];
          vjust = OptionValue["vjust"];
          angle = OptionValue["angle"];
          fontFamily = OptionValue["fontFamily"];
          fontWeight = OptionValue["fontWeight"];
          
          (* Create text graphic with Inset positioning *)
          If[OptionValue["location"] =!= Null,
            (* Use Scaled coordinates for location-based positioning *)
            Inset[
              Style[
                ToString[labelText],
                FontSize -> sizeDir,
                FontColor -> colorDir,
                FontFamily -> fontFamily,
                FontWeight -> fontWeight,
                alphaDir
              ],
              Sequence @@ Scaled /@ pos
            ],
            (* Use regular coordinates for x,y positioning *)
            Inset[
              Style[
                ToString[labelText],
                FontSize -> sizeDir,
                FontColor -> colorDir,
                FontFamily -> fontFamily,
                FontWeight -> fontWeight,
                alphaDir
              ],
              pos[[1]],
              {2*hjust - 1, 2*vjust - 1}, (* Convert 0-1 range to -1 to 1 range for positioning *)
              {Cos[angle*Degree], Sin[angle*Degree]} (* Rotation vector *)
            ]
          ]
        ]
      ]];
    ]
  ];

  output
];

(* Simple geomText for direct coordinate or location specification without requiring dataset *)
geomText[opts : OptionsPattern[]] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] == 0 && 
  ((NumericQ[OptionValue[geomText, opts, "x"]] && NumericQ[OptionValue[geomText, opts, "y"]]) || 
   OptionValue[geomText, opts, "location"] =!= Null) && 
  OptionValue[geomText, opts, "label"] =!= Null := Module[{
  colorDir, alphaDir, sizeDir, pos, labelText, hjust, vjust, angle, 
  fontFamily, fontWeight, textStyle
  },
  
  (* Use default or specified aesthetics *)
  colorDir = If[OptionValue["color"] === Null, Black, OptionValue["color"]];
  alphaDir = If[OptionValue["alpha"] === Null, 1., OptionValue["alpha"]];
  sizeDir = If[OptionValue["size"] === Null, 12, OptionValue["size"]];
  
  (* Calculate position - either from location string or x,y coordinates *)
  pos = If[OptionValue["location"] =!= Null,
    (* Use location string with GetScaledCoord *)
    GetScaledCoord[OptionValue["location"]],
    (* Use x,y coordinates *)
    {{
      OptionValue["xScaleFunc"][OptionValue["x"]], 
      OptionValue["yScaleFunc"][OptionValue["y"]]
    }, {0, 0}} (* Format to match GetScaledCoord output *)
  ];
  
  (* Extract text - handle Text[...] wrapper or direct text *)
  labelText = If[MatchQ[OptionValue["label"], Text[_]],
    OptionValue["label"][[1]], (* Extract from Text[...] *)
    OptionValue["label"] (* Direct value *)
  ];
  
  (* Get positioning and styling options *)
  hjust = OptionValue["hjust"];
  vjust = OptionValue["vjust"];
  angle = OptionValue["angle"];
  fontFamily = OptionValue["fontFamily"];
  fontWeight = OptionValue["fontWeight"];
  
  (* Return single text element using Inset format *)
  {
    If[OptionValue["location"] =!= Null,
      (* Use Scaled coordinates for location-based positioning *)
      Inset[
        Style[
          ToString[labelText],
          FontSize -> sizeDir,
          FontColor -> colorDir,
          FontFamily -> fontFamily,
          FontWeight -> fontWeight,
          alphaDir
        ],
        Sequence @@ Scaled /@ pos
      ],
      (* Use regular coordinates for x,y positioning *)
      Inset[
        Style[
          ToString[labelText],
          FontSize -> sizeDir,
          FontColor -> colorDir,
          FontFamily -> fontFamily,
          FontWeight -> fontWeight,
          alphaDir
        ],
        pos[[1]],
        {2*hjust - 1, 2*vjust - 1}, (* Convert 0-1 range to -1 to 1 range for positioning *)
        {Cos[angle*Degree], Sin[angle*Degree]} (* Rotation vector *)
      ]
    ]
  }
];

End[];

EndPackage[];
