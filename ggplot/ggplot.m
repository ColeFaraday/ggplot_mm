(* :Title: ggplot *)
(* :Context: ggplot` *)
(* :Author: andrewyule *)
(* :Date: 2019-11-10 *)

(* Correct ggplot context declaration *)
BeginPackage["ggplot`"];

(* Replace Print in DebugPrint with Message *)
ClearAll[DebugPrint];
DebugPrint[args___] := If[$DebugMode, Message[args]];

(* Fix directive syntax *)
Begin["`Private`"];

(* Ensure all scales are declared in ggplotSymbolDeclaration *)
ClearAll[ggplotSymbolDeclaration];
ggplotSymbolDeclaration[] := {
  scaleContinuous, scaleDiscrete, scaleLog, scaleDate,
  scaleXContinuous, scaleYContinuous, scaleXDiscrete, scaleYDiscrete,
  scaleXLog, scaleYLog, scaleXDate, scaleYDate,
  scaleColorContinuous, scaleColorDiscrete, scaleColorManual,
  scaleFillContinuous, scaleFillDiscrete, scaleFillManual,
  scaleSizeContinuous, scaleSizeDiscrete, scaleSizeManual,
  scaleShapeDiscrete, scaleShapeManual
};

ggplot::xOrYNotGiven        = "A geom was given without specifying the x or y mapping";
ggplot::xInterceptNotGiven  = "No xIntercept value was given for geomHLine";
ggplot::yInterceptNotGiven  = "No yIntercept value was given for geomHLine";
ggplot::shapeContinuous     = "A continuous variable can not be mapped to a shape";
ggplot::shapeCount          = "More than 7 discrete shapes are present, aborting... (this should be fixed)";
ggplot::errorBarMissingBounds = "geomBar requires all four bounds: xmin, xmax, ymin, ymax";
ggplot::errorBandMissingBounds = "geomBand requires x, ymin, and ymax";
ggplot::labelNotGiven       = "geomText requires a label mapping";
ggplot::keyNotFound         = "Aesthetic `1` refers to key '`2`' which does not exist in the data";
ggplot::aestheticFormatError = "Aesthetic `1` has invalid format for value: `2`";
ggplot::facetNotImplemented = "Faceting is not yet fully implemented";

validDatasetQ[dataset_] := MatchQ[dataset, {_?AssociationQ..}];

(* Legend positioning function *)
defaultOffset = {0.05, 0.05};
GetScaledCoord["BottomLeft", offset_ : defaultOffset] := {{0 + offset[[1]], 0 + offset[[2]]}, {0, 0}};
GetScaledCoord["BottomRight", offset_ : defaultOffset] := {{1 - offset[[1]], 0 + offset[[2]]}, {1, 0}};
GetScaledCoord["TopLeft", offset_ : defaultOffset] := {{0 + offset[[1]], 1 - offset[[2]]}, {0, 1}};
GetScaledCoord["TopRight", offset_ : defaultOffset] := {{1 - offset[[1]], 1 - offset[[2]]}, {1, 1}};
GetScaledCoord["MiddleLeft", offset_ : defaultOffset] := {{0 + offset[[1]], 0.5 + offset[[2]]}, {0, 0.5}};
GetScaledCoord["MiddleRight", offset_ : defaultOffset] := {{1 - offset[[1]], 0.5 + offset[[2]]}, {1, 0.5}};
GetScaledCoord["TopMiddle", offset_ : defaultOffset] := {{0.5 + offset[[1]], 1 - offset[[2]]}, {0.5, 1}};
GetScaledCoord["BottomMiddle", offset_ : defaultOffset] := {{0.5 + offset[[1]], 0 + offset[[2]]}, {0.5, 0}};

(* Outer legend positioning functions - for legends outside the plot area *)
GetScaledCoord["OuterBottomLeft", offset_ : defaultOffset] := {{0 - offset[[1]], 0 - offset[[2]]}, {1, 0}};
GetScaledCoord["OuterBottomRight", offset_ : defaultOffset] := {{1 + offset[[1]], 0 - offset[[2]]}, {0, 0}};
GetScaledCoord["OuterTopLeft", offset_ : defaultOffset] := {{0 - offset[[1]], 1 + offset[[2]]}, {1, 1}};
GetScaledCoord["OuterTopRight", offset_ : defaultOffset] := {{1 + offset[[1]], 1 + offset[[2]]}, {0, 1}};
GetScaledCoord["OuterMiddleLeft", offset_ : defaultOffset] := {{0 - offset[[1]], 0.5 + offset[[2]]}, {1, 0.5}};
GetScaledCoord["OuterMiddleRight", offset_ : defaultOffset] := {{1 + offset[[1]], 0.5 + offset[[2]]}, {0, 0.5}};
GetScaledCoord["OuterTopMiddle", offset_ : defaultOffset] := {{0.5 + offset[[1]], 1 + offset[[2]]}, {0.5, 0}};
GetScaledCoord["OuterBottomMiddle", offset_ : defaultOffset] := {{0.5 + offset[[1]], 0 - offset[[2]]}, {0.5, 1}};

(* Auto-detect scale type from data and create appropriate scale *)
ClearAll[autoDetectScale];
autoDetectScale[aesthetic_, mapping_, data_List] := Module[{
  values, scaleType, scale
},
  values = extractValues[data, mapping];
  
  (* Auto-detect scale type based on data *)
  scaleType = Which[
    (* Check for dates first *)
    AllTrue[values, DateObjectQ] || 
    (AllTrue[values, NumericQ] && (Max[values] - Min[values]) > 365*24*3600),
    "date",
    
    (* Check for log scale candidates *)
    AllTrue[values, NumericQ] && Min[values] > 0 && (Max[values]/Min[values]) > 100,
    "log",
    
    (* Check for continuous numeric *)
    AllTrue[values, NumericQ],
    "continuous", 
    
    (* Everything else is discrete *)
    True,
    "discrete"
  ];
  
  (* Create appropriate scale *)
  scale = Switch[scaleType,
    "continuous", scaleContinuous["aesthetic" -> aesthetic],
    "discrete", scaleDiscrete["aesthetic" -> aesthetic],
    "log", scaleLog["aesthetic" -> aesthetic],  
    "date", scaleDate["aesthetic" -> aesthetic]
  ];
  
  scale
];

(* Collect all scale specifications from ggplot arguments *)
ClearAll[collectScaleSpecs];
collectScaleSpecs[heldArgs_Hold] := Module[{scaleSpecs},
  (* Extract scale constructor calls from arguments *)
  scaleSpecs = Cases[heldArgs,
    (scaleContinuous[opts___] | scaleDiscrete[opts___] | scaleLog[opts___] | scaleDate[opts___] |
     scaleXContinuous[opts___] | scaleYContinuous[opts___] | scaleXDiscrete[opts___] | scaleYDiscrete[opts___] |
     scaleXLog[opts___] | scaleYLog[opts___] | scaleXDate[opts___] | scaleYDate[opts___] |
     scaleColorContinuous[opts___] | scaleColorDiscrete[opts___] | scaleColorManual[opts___] |
     scaleFillContinuous[opts___] | scaleFillDiscrete[opts___] | scaleFillManual[opts___] |
     scaleSizeContinuous[opts___] | scaleSizeDiscrete[opts___] | scaleSizeManual[opts___] |
     scaleShapeDiscrete[opts___] | scaleShapeManual[opts___]),
    {0, Infinity}
  ];
  
  (* Return empty association if no scales are found *)
  If[Length[scaleSpecs] == 0, Return[<||>]];

  (* Convert to association by aesthetic *)
  Association[Map[#"aesthetic" -> # &, scaleSpecs]]
];

(* Create scales for all mapped aesthetics *)
ClearAll[createAllScales];
createAllScales[data_List, heldArgs_Hold, globalAesthetics_Association, layerAesthetics_List] := Module[{
  allMappings, userScales, autoScales, finalScales
},
  (* Collect all aesthetic mappings *)
  allMappings = Join[globalAesthetics, Association@Flatten[layerAesthetics]]; (* Ensure compatibility by converting to Association *)
  allMappings = DeleteCases[allMappings, _ -> Null]; (* Remove null mappings *)

  (* Debug print for allMappings *)
  DebugPrint["Debug: AllMappings = ", allMappings];

  (* Get user-specified scales *)
  userScales = collectScaleSpecs[heldArgs];

  (* Auto-create scales for unmapped aesthetics *)
  autoScales = Association[KeyValueMap[Function[{aesthetic, mapping},
    If[!KeyExistsQ[userScales, aesthetic],
      aesthetic -> autoDetectScale[aesthetic, mapping, data],
      Nothing
    ]
  ], allMappings]];

  (* Debug print for autoScales *)
  DebugPrint["Debug: AutoScales = ", autoScales];

  (* Combine user scales with auto scales *)
  finalScales = Join[autoScales, userScales];

  (* Debug print for finalScales *)
  DebugPrint["Debug: FinalScales = ", finalScales];

  (* Train all scales on data *)
  Association[KeyValueMap[Function[{aesthetic, scale},
    aesthetic -> trainScale[scale, data, allMappings[aesthetic]]
  ], finalScales]]
];

(* ============================== *)
(* AESTHETIC APPLICATION WITH SCALES *)
(* ============================== *)

(* Apply all aesthetic mappings using trained scales *)
ClearAll[applyAestheticsWithScales];
applyAestheticsWithScales[data_List, scales_Association, aestheticMappings_Association] := Module[{
  processedData
},
  DebugPrint["Debug: Entering applyAestheticsWithScales"];
  DebugPrint["Debug: Initial data size = ", Length[data]];
  DebugPrint["Debug: Scales = ", scales];
  DebugPrint["Debug: AestheticMappings = ", aestheticMappings];

  processedData = data;

  (* Apply each aesthetic using its trained scale *)
  KeyValueMap[Function[{aesthetic, mapping},
    If[KeyExistsQ[scales, aesthetic],
      processedData = Map[Function[row,
        Module[{value, scaledValue},
          value = Which[
            StringQ[mapping], row[mapping],
            Head[mapping] === Function, mapping[row],
            True, mapping (* constant *)
          ];

          scaledValue = applyScale[scales[aesthetic], value];
          Append[row, aesthetic <> "_aes" -> scaledValue]
        ]
      ], processedData]
    ]
  ], aestheticMappings];

  DebugPrint["Debug: ProcessedData size = ", Length[processedData]];
  processedData
];

(* ============================== *)
(* LEGEND GENERATION FROM SCALES  *)
(* ============================== *)

(* Generate legends from trained scales *)
ClearAll[generateLegendsFromScales];
generateLegendsFromScales[scales_Association] := Module[{
  legendScales, legends
},
  (* Only create legends for non-position aesthetics *)
  legendScales = KeySelect[scales, !MemberQ[{"x", "y"}, #] &];

  legends = KeyValueMap[Function[{aesthetic, scale},
    createLegendFromScale[aesthetic, scale]
  ], legendScales];

  (* Filter out None values *)
  Select[legends, # =!= None &]
];

(* Create legend from a trained scale *)
ClearAll[createLegendFromScale];
createLegendFromScale[aesthetic_, scale_Association] := Module[{
  scaleType, domain, palette, breaks, labels, name
},
  scaleType = scale["type"];
  domain = scale["domain"];
  palette = scale["palette"];
  breaks = scale["breaks"];
  labels = scale["labels"];
  name = If[scale["name"] === Automatic, ToString[scale["aesthetic"]], scale["name"]];
  
  Switch[{aesthetic, scaleType},
    {"color" | "fill", "discrete"},
    If[ListQ[palette] && ListQ[labels],
      LineLegend[palette, labels, LegendLabel -> name],
      None
    ],
    
    {"color" | "fill", "continuous" | "log"},
    BarLegend[{palette, domain}, LegendLabel -> name],
    
    {"shape", "discrete"},
    If[ListQ[palette] && ListQ[labels],
      PointLegend[palette, labels, LegendLabel -> name],
      None
    ],
    
    {"size", "discrete"},
    If[ListQ[domain] && ListQ[scale["range"]],
      PointLegend[
        ConstantArray["\[FilledCircle]", Length[domain]], 
        labels,
        LegendMarkerSize -> scale["range"],
        LegendLabel -> name
      ],
      None
    ],
    
    {"size", "continuous"},
    Module[{minSize, maxSize},
      {minSize, maxSize} = scale["range"];
      PointLegend[
        {"\[FilledCircle]", "\[FilledCircle]", "\[FilledCircle]"},
        {ToString[domain[[1]]], "Mid", ToString[domain[[2]]]},
        LegendMarkerSize -> {minSize, Mean[{minSize, maxSize}], maxSize},
        LegendLabel -> name
      ]
    ],
    
    {"alpha", "discrete"},
    SwatchLegend[
      Map[Opacity[#, Gray] &, scale["range"]],
      labels,
      LegendLabel -> name
    ],
    
    _, None (* No legend for this combination *)
  ]
];

(* Convert legend info to built-in legend functions *)
convertLegendInfo[legendInfo_] := Module[{legendItems},
  If[Length[legendInfo] == 0, Return[{}]];

  legendItems = KeyValueMap[Function[{aesthetic, data},
    Which[
      (* Handle combined aesthetics (multiple aesthetics for same variable) *)
      KeyExistsQ[data, "aesthetics"], createCombinedLegend[data],
      
      (* Single aesthetic legends *)
      data["aesthetic"] === "color" && data["type"] === "discrete", LineLegend[data["values"], data["labels"], LegendLabel -> data["title"]],
      
      data["aesthetic"] === "color" && data["type"] === "continuous", (* For continuous color, use BarLegend *)
      BarLegend[{data["palette"], data["range"]}, LegendLabel -> data["title"]],
      
      data["aesthetic"] === "shape" && data["type"] === "discrete", PointLegend[ConstantArray[Black, Length[data["labels"]]], data["labels"], LegendLabel -> data["title"]],
      
      data["aesthetic"] === "size" && data["type"] === "discrete", PointLegend[ConstantArray[Graphics[{Black, Disk[]}], Length[data["labels"]]], data["labels"], LegendLabel -> data["title"]],
      
      True, SwatchLegend[data["values"], data["labels"], LegendLabel -> data["title"]]
    ]
  ], legendInfo];

  legendItems
];

End[];
EndPackage[];