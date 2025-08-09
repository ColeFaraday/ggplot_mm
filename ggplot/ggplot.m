(* :Title: ggplot *)
(* :Context: ggplot` *)
(* :Author: andrewyule *)
(* :Date: 2019-11-10 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

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
  
  (* Convert to association by aesthetic *)
  Association[Map[#["aesthetic"] -> # &, scaleSpecs]]
];

(* Create scales for all mapped aesthetics *)
ClearAll[createAllScales];
createAllScales[data_List, heldArgs_Hold, globalAesthetics_Association, layerAesthetics_List] := Module[{
  allMappings, userScales, autoScales, finalScales
},
  (* Collect all aesthetic mappings *)
  allMappings = Join[globalAesthetics, Flatten[layerAesthetics]];
  allMappings = DeleteCases[allMappings, _ -> Null]; (* Remove null mappings *)
  
  (* Get user-specified scales *)
  userScales = collectScaleSpecs[heldArgs];
  
  (* Auto-create scales for unmapped aesthetics *)
  autoScales = Association[KeyValueMap[Function[{aesthetic, mapping},
    If[!KeyExistsQ[userScales, aesthetic],
      aesthetic -> autoDetectScale[aesthetic, mapping, data],
      Nothing
    ]
  ], allMappings]];
  
  (* Combine user scales with auto scales *)
  finalScales = Join[autoScales, userScales];
  
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
  
  processedData
];

(* ============================== *)
(* LEGEND GENERATION FROM SCALES  *)
(* ============================== *)

(* Generate legends from trained scales *)
ClearAll[generateLegendsFromScales];
generateLegendsFromScales[scales_Association] := Module[{
  legendableScales, legends
},
  (* Only create legends for non-position aesthetics *)
  legendableScales = KeySelect[scales, !MemberQ[{"x", "y"}, #] &];
  
  legends = KeyValueMap[Function[{aesthetic, scale},
    createLegendFromScale[aesthetic, scale]
  ], legendableScales];
  
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
      KeyExistsQ[data, "aesthetics"],
      createCombinedLegend[data],
      
      (* Single aesthetic legends *)
      data["aesthetic"] === "color" && data["type"] === "discrete",
      LineLegend[data["values"], data["labels"], LegendLabel -> data["title"]],
      
      data["aesthetic"] === "color" && data["type"] === "continuous",
      (* For continuous color, use BarLegend *)
      BarLegend[{data["palette"], data["range"]}, LegendLabel -> data["title"]],
      
      data["aesthetic"] === "shape" && data["type"] === "discrete",
      PointLegend[ConstantArray[Black, Length[data["labels"]]], data["labels"], LegendLabel -> data["title"], LegendMarkers->(data[["values"]]/.{ggplotSizePlaceholder->12, ggplotAlphaPlaceholder->Opacity[1.], ggplotColorPlaceholder->Black})],
      
      data["aesthetic"] === "size" && data["type"] === "discrete",
      PointLegend[ConstantArray[Graphics[{Black, Disk[]}], Length[data["labels"]]], data["labels"], 
        LegendLabel -> data["title"], LegendMarkerSize -> data["values"]],
      
      True,
      (* Fallback to SwatchLegend *)
      SwatchLegend[data["values"], data["labels"], LegendLabel -> data["title"]]
    ]
  ], legendInfo];
  
  legendItems
];

(* Create a combined legend for multiple aesthetics mapping to the same variable *)
createCombinedLegend[data_] := Module[{
  aesthetics, labels, colors, shapes, sizes, alphas, markers, legendColors
  },
  aesthetics = data["aesthetics"];
  labels = data["labels"];
  
  (* Extract values for each aesthetic *)
  colors = Lookup[data["values"], "color", ConstantArray[Black, Length[labels]]];
  shapes = Lookup[data["values"], "shape", ConstantArray[FilledMarkers[][[1]], Length[labels]]];
  sizes = Lookup[data["values"], "size", ConstantArray[12, Length[labels]]];
  alphas = Lookup[data["values"], "alpha", ConstantArray[1., Length[labels]]];

  (* Create markers that combine shape, size, and alpha *)
  markers = MapThread[Function[{shape, size, alpha, color},
    shape /. {ggplotSizePlaceholder -> size, ggplotAlphaPlaceholder -> Opacity[alpha], ggplotColorPlaceholder -> color}
  ], {shapes, sizes, alphas, colors}];
  
  (* Use colors as the legend colors and shapes/sizes as markers *)
  PointLegend[colors, labels, 
    LegendLabel -> data["title"], 
    LegendMarkers -> markers,
    LegendMarkerSize -> {50, 45},
    LegendLayout -> (Column[Row /@ #, Spacings -> -2] &)
  ]
];

Attributes[argPatternQ] = {HoldAllComplete};
argPatternQ[expr___] := MatchQ[Hold[expr], Hold[(_Rule | geomPoint[___] | geomLine[___] | geomPath[___] | geomSmooth[___] | geomVLine[___] | geomHLine[___] | geomParityLine[___] | geomHistogram[___] | geomCol[___] | geomBar[___] | geomBoxes[___] | geomBand[___] | geomDensity2DFilled[___] | geomDensity2D[___] | geomConvexHull[___] | geomText[___] | scaleXDate2[___] | scaleXLinear2[___] | scaleXLog2[___] | scaleYDate2[___] | scaleYLinear2[___] | scaleYLog2[___] | facetWrap[___]) ...]];

(* Main ggplot method and entry point *)
Options[ggplot] = DeleteDuplicates[Join[{
  "data" -> {},
  "color" -> Null,
  "size" -> Null,
  "alpha" -> Null,
  "shape" -> Null,
  "thickness" -> Null,
  "group" -> Null,
  "categoricalColors" -> Automatic,
  "sequentialColors" -> {Blue, White, Red},
  "divergingColors" -> {Blue, White, Red},
  "continuousColorPalette" -> "auto",
  "categoricalShapes" -> {"\[FilledCircle]", "\[FilledUpTriangle]", "\[FilledSquare]", "\[FivePointedStar]", "\[FilledDiamond]", "\[FilledRectangle]", "\[FilledDownTriangle]"},
  "showLegend" -> Automatic,
  "legendPosition" -> "right",
  "legendSpacing" -> 0.15,
  FrameLabel -> Automatic,
  PlotStyle -> Automatic,
  ImageSize -> Automatic,
  ImageMargins -> Automatic
}, Options[ListLinePlot], Options[ticks2], Options[gridLines2]]];
(* Options for ggplot are set further below in themes *)
Attributes[ggplot] = {HoldAllComplete};
ggplot[ds_?validDatasetQ, args___?argPatternQ] := ggplot["data" -> ds, args];
ggplot[args___?argPatternQ][ds_?validDatasetQ] := ggplot["data" -> ds, args];
ggplot[args___?argPatternQ] /; Count[Hold[args], ("data" -> _), {0, Infinity}] > 0 := Catch[Module[{heldArgs, options, dataset, processedData, allMappings, defaultXLabel, defaultYLabel, frameLabel, points, lines, paths, smoothLines, columns, abLines, hLines, vLines, histograms, graphicsPrimitives, xScaleType, yScaleType, xScaleFunc, yScaleFunc, xDiscreteLabels, yDiscreteLabels, xTickFunc, yTickFunc, xGridLineFunc, yGridLineFunc, legendInfo, legendGraphics, showLegend, graphic, facetInfo, layers, facetSpec, facetResult, globalScales, panelGraphics, allLegendData},
  
  heldArgs = Hold[args];
  options = Cases[heldArgs, _Rule, 1];
  dataset = Lookup[options, "data", {}];

  (* Extract layers (geom constructors) *)
  layers = Cases[heldArgs, 
    (geomPoint[opts___] | geomLine[opts___] | geomPath[opts___] | geomSmooth[opts___] | 
     geomVLine[opts___] | geomHLine[opts___] | geomParityLine[opts___] | 
     geomHistogram[opts___] | geomBar[opts___] | geomBoxes[opts___] | 
     geomBand[opts___] | geomDensity2DFilled[opts___] | geomConvexHull[opts___] | geomText[opts___] | geomDensity2D[opts___]), 
    {0, Infinity}
  ];


  (* Extract facet specification *)
  facetSpec = Cases[heldArgs, facetWrap[variable_, opts___] :> facetWrap[variable, opts], {0, Infinity}];
  facetSpec = If[Length[facetSpec] > 0, First[facetSpec], facetIdentity[]];

  (* 0. Global aesthetic reconciliation before faceting *)
  processedData = dataset;
  
  (* Collect only global aesthetic mappings *)
  allMappings = collectGlobalAestheticMappings[options];

  globalAesthetics = collectGlobalAestheticMappings[options]; (* Your existing function *)
  layerAesthetics = Map[extractLayerAesthetics[Association @@ #] &, layers]; (* Your existing function *)

  scales = createAllScales[dataset, heldArgs, globalAesthetics, layerAesthetics];

  processedData = applyAestheticsWithScales[dataset, scales, globalAesthetics];


  (* 1. Apply faceting to get panel specifications *)
  facetResult = If[facetSpec === facetIdentity[], 
    <|"type" -> "identity", "panels" -> <|"single" -> processedData|>|>,
    facetSpec[processedData]  (* Facet function transforms the data *)
  ];

  (* For faceted plots, don't add frame labels to individual panels *)
  (* For single panels, add frame labels to the panel *)
  panelOptions = If[facetSpec =!= facetIdentity[],
    (* Faceted: remove frame labels from panel options *)
    Append[DeleteCases[options, FrameLabel -> _], FrameLabel -> None],
    (* Single panel: add computed frame labels to panel options *)
    Append[DeleteCases[options, FrameLabel -> _], FrameLabel -> frameLabel]
  ];

(* Process panels with scale-aware layers *)
  panelGraphics = If[facetResult["type"] === "identity",
    {processPanelLayersWithScales[First[Values[facetResult["panels"]]], layers, scales, options]},
    Map[
      Function[panelKey,
        processPanelLayersWithScales[facetResult["panels"][panelKey], layers, scales, options]
      ],
      facetResult["panelOrder"]
    ]
  ];


  legends = generateLegendsFromScales[scales];

  (* 5. Layout panels + legends based on facet type *)
  (* Pass both the original computed frameLabel and the facetResult for layout decisions *)
  frameLabel = calculateFrameLabel[heldArgs, options]; (* You'd need to implement this *)
  graphic = layoutFacetedPlot[panelGraphics, legends, facetResult, options, frameLabel];

  graphic
]];

(* Helper function to collect global aesthetic mappings only *)
collectGlobalAestheticMappings[options_] := Module[{globalMappings},
  
  (* Get only global aesthetic mappings from ggplot options *)
  globalMappings = <|
    "color" -> Lookup[options, "color", Null],
    "fill" -> Lookup[options, "fill", Null],
    "size" -> Lookup[options, "size", Null], 
    "alpha" -> Lookup[options, "alpha", Null],
    "lineAlpha" -> Lookup[options, "lineAlpha", Null],
    "shape" -> Lookup[options, "shape", Null],
    "thickness" -> Lookup[options, "thickness", Null],
    "group" -> Lookup[options, "group", Null]
  |>;
  
  globalMappings
];

(* Calculate consistent plot ranges from the full dataset for faceting *)
calculateGlobalPlotRange[processedData_, heldArgs_, xScaleFunc_, yScaleFunc_] := Module[{
  xMapping, yMapping, xValues, yValues, scaledXValues, scaledYValues, xRange, yRange
},
  (* Extract x and y mappings from arguments *)
  xMapping = First@Cases[heldArgs, ("x" -> x_) :> x, {0, Infinity}, 1];
  yMapping = First@Cases[heldArgs, ("y" -> y_) :> y, {0, Infinity}, 1];
  
  If[xMapping === $Failed || yMapping === $Failed,
    Return[All] (* Fallback if mappings not found *)
  ];
  
  (* Extract x and y values using the mapping functions *)
  xValues = extractMappedValues[processedData, xMapping];
  yValues = extractMappedValues[processedData, yMapping];
  
  If[Length[xValues] == 0 || Length[yValues] == 0,
    Return[All] (* Fallback if no data *)
  ];
  
  (* Apply scale transformations *)
  scaledXValues = xScaleFunc /@ xValues;
  scaledYValues = yScaleFunc /@ yValues;
  
  (* Calculate ranges with small padding *)
  xRange = MinMax[scaledXValues];
  yRange = MinMax[scaledYValues];
  
  (* Add 5% padding to ranges *)
  xRange = xRange + {-1, 1} * 0.1 * (xRange[[2]] - xRange[[1]]);
  yRange = yRange + {-1, 1} * 0.1 * (yRange[[2]] - yRange[[1]]);
  
  {xRange, yRange}
];

(* Helper to extract mapped values for x, y, or any mapping (string or function) *)
extractMappedValues[data_, mapping_] := 
  Which[
    StringQ[mapping], data[[All, mapping]],
    Head[mapping] === Function, mapping /@ data,
    True, Nothing
  ];

(* Enhanced panel processing that passes scales to geoms *)
ClearAll[processPanelLayersWithScales];
processPanelLayersWithScales[panelData_, layers_, scales_, options_] := Module[{
  processedLayers, xScale, yScale
},
  (* Extract position scales *)
  xScale = Lookup[scales, "x", None];
  yScale = Lookup[scales, "y", None];
  
  processedLayers = Map[
    Function[layer,
      Module[{layerHead, layerOpts, mergedLayer, layerAesthetics, resolvedData, 
        statParams, geomParams, statResult, geomResult},
        layerHead = Head[layer];
        layerOpts = List @@ layer;
        
        mergedLayer = layerHead @@ Normal@Join[
          Association@options, Association@layerOpts
        ];
        
        (* Resolve layer-specific aesthetics using scales *)
        layerAesthetics = extractLayerAesthetics[Association@layerOpts];
        resolvedData = applyAestheticsWithScales[panelData, scales, layerAesthetics];
        
        (* Prepare parameters *)
        statParams = Normal@Association[
          Association@mergedLayer["statParams"], 
          <|"data" -> resolvedData|>
        ];
        
        geomParams = Join[mergedLayer["geomParams"], <|
          "xScale" -> xScale,
          "yScale" -> yScale,
          "scales" -> scales
        |>];
        
        (* Run stat → geom pipeline *)
        statResult = mergedLayer["stat"][Sequence @@ statParams];
        geomResult = Values[mergedLayer["geom"][#, Sequence @@ Normal[geomParams]] & /@ statResult];
        
        geomResult
      ]
    ],
    layers
  ];
  
  (* Calculate plot range from position scales *)
  plotRange = {
    If[xScale =!= None, xScale["domain"], All],
    If[yScale =!= None, yScale["domain"], All]
  };
  
  (* Return Graphics object *)
  Graphics[Flatten[processedLayers],
    Frame -> Lookup[options, Frame, True],
    FrameLabel -> Lookup[options, FrameLabel, Automatic],
    PlotRange -> plotRange,
    AspectRatio -> Lookup[options, AspectRatio, 7/10],
    ImageSize -> 150
  ]
];

(* Helper function to calculate frame labels based on ggplot arguments *)
ClearAll[calculateFrameLabel];
calculateFrameLabel[heldArgs_, options_] := Module[{xLabel, yLabel},
  (* Extract x and y labels from options or arguments *)
  xLabel = Lookup[options, "xLabel", First@Cases[heldArgs, ("xLabel" -> lbl_) :> lbl, {0, Infinity}, 1]];
  yLabel = Lookup[options, "yLabel", First@Cases[heldArgs, ("yLabel" -> lbl_) :> lbl, {0, Infinity}, 1]];

  (* Default to automatic labels if not provided *)
  xLabel = If[xLabel === $Failed, "x", xLabel];
  yLabel = If[yLabel === $Failed, "y", yLabel];

  {xLabel, yLabel}
];

End[];

EndPackage[];