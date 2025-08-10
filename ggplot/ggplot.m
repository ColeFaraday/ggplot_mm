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

  (* Apply aesthetic reconciliation globally *)
  processedData = reconcileAesthetics[processedData, allMappings["color"], "color"];
  processedData = reconcileAesthetics[processedData, allMappings["fill"], "fill"];
  processedData = reconcileAesthetics[processedData, allMappings["size"], "size"];
  processedData = reconcileAesthetics[processedData, allMappings["alpha"], "alpha"];
  processedData = reconcileAesthetics[processedData, allMappings["lineAlpha"], "lineAlpha"];
  processedData = reconcileAesthetics[processedData, allMappings["shape"], "shape"];
  processedData = reconcileAesthetics[processedData, allMappings["thickness"], "thickness"];
  processedData = reconcileAesthetics[processedData, allMappings["group"], "group"];


  (* 1. Apply faceting to get panel specifications *)
  facetResult = If[facetSpec === facetIdentity[], 
    <|"type" -> "identity", "panels" -> <|"single" -> processedData|>|>,
    facetSpec[processedData]  (* Facet function transforms the data *)
  ];

  (* 2. Compute global scales for consistency across panels *)
  defaultXLabel = First@Cases[heldArgs, ("x" -> x_) :> ToString[x], {0, Infinity}];
  defaultYLabel = Quiet[Check[First@Cases[heldArgs, ("y" -> y_) :> ToString[y], {0, Infinity}], ""]];
  frameLabel = Lookup[options, FrameLabel, Automatic];
  frameLabel = Which[
    frameLabel === Automatic,
    {defaultXLabel, defaultYLabel} /. str_?StringQ :> Style[str, Opacity[1], FontColor -> Black],
    MatchQ[frameLabel, {{_?StringQ, _?StringQ}, {_?StringQ, _?StringQ}} | {_?StringQ, _?StringQ} | _?StringQ],
    frameLabel /. str_?StringQ :> Style[str, Opacity[1], FontColor -> Black],
    True,
    frameLabel
  ];

  (* For faceted plots, don't add frame labels to individual panels *)
  (* For single panels, add frame labels to the panel *)
  panelOptions = If[facetSpec =!= facetIdentity[],
    (* Faceted: remove frame labels from panel options *)
    Append[DeleteCases[options, FrameLabel -> _], FrameLabel -> None],
    (* Single panel: add computed frame labels to panel options *)
    Append[DeleteCases[options, FrameLabel -> _], FrameLabel -> frameLabel]
  ];

  xScaleType = reconcileXScales[heldArgs];
  yScaleType = reconcileYScales[heldArgs];

  xScaleFunc = If[xScaleType == "Discrete",
    createDiscreteScaleFunc["x", heldArgs],
    With[{f = ToExpression[xScaleType /. "Linear" | "Date" -> "Identity"]}, Function[f[#]]]
  ];
  yScaleFunc = If[yScaleType == "Discrete",
    createDiscreteScaleFunc["y", heldArgs],
    With[{f = ToExpression[yScaleType /. "Linear" | "Date" -> "Identity"]}, Function[f[#]]]
  ];

  If[xScaleType == "Discrete", xDiscreteLabels = createDiscreteScaleLabels["x", heldArgs]];
  If[yScaleType == "Discrete", yDiscreteLabels = createDiscreteScaleLabels["y", heldArgs]];

  (* Calculate consistent plot ranges from full dataset for faceting, unless user provided PlotRange *)
  globalPlotRange = Lookup[options, PlotRange, 
    calculateGlobalPlotRange[processedData, heldArgs, xScaleFunc, yScaleFunc]
  ];

  globalScales = <|
    "xScaleFunc" -> xScaleFunc, 
    "yScaleFunc" -> yScaleFunc,
    "plotRange" -> globalPlotRange
  |>;
  

  (* 3. Process each panel through stat→geom pipeline *)
  {panelGraphics, allLegendData} = Reap[
    If[facetResult["type"] === "identity",
      (* Single panel case - process directly *)
      KeyValueMap[
        Function[{panelKey, panelData},
          processPanelLayers[panelData, layers, globalScales, panelOptions]
        ],
        facetResult["panels"]
      ],
      (* Faceted case - process panels in the correct order to match strip labels *)
      Map[
        Function[panelKey,
          processPanelLayers[facetResult["panels"][panelKey], layers, globalScales, panelOptions]
        ],
        facetResult["panelOrder"]
      ]
    ]
  ];

  (* 4. Generate legends - placeholder for now *)
  legendInfo = {};

  (* 5. Layout panels + legends based on facet type *)
  (* Pass both the original computed frameLabel and the facetResult for layout decisions *)
  graphic = layoutFacetedPlot[panelGraphics, legendInfo, facetResult, options, frameLabel];

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

ClearAll[discoverScales];
discoverScales[dataset_, heldArgs_, options_] := Module[{
  xMapping, yMapping, colorMapping, xValues, yValues, colorValues,
  xScaleType, yScaleType, colorScaleType, scales
},
  (* Extract mappings from arguments *)
  xMapping = First@Cases[heldArgs, ("x" -> x_) :> x, {0, Infinity}, 1];
  yMapping = First@Cases[heldArgs, ("y" -> y_) :> y, {0, Infinity}, 1];
  colorMapping = Lookup[options, "color", Null];
  
  (* Discover scale types by examining the full dataset *)
  (* This is where your existing reconcileXScales/reconcileYScales logic goes *)
  xScaleType = If[xMapping =!= $Failed, 
    determineScaleType[extractMappedValues[dataset, xMapping], "x", heldArgs],
    "identity"
  ];
  yScaleType = If[yMapping =!= $Failed,
    determineScaleType[extractMappedValues[dataset, yMapping], "y", heldArgs], 
    "identity"
  ];
  colorScaleType = determineColorScaleType[dataset, colorMapping];
  
  (* Create scale specifications (not yet applied to data) *)
  scales = <|
    "x" -> <|"type" -> xScaleType, "mapping" -> xMapping, "applied" -> False|>,
    "y" -> <|"type" -> yScaleType, "mapping" -> yMapping, "applied" -> False|>,
    "color" -> <|"type" -> colorScaleType, "mapping" -> colorMapping, "applied" -> False|>
  |>;
  
  scales
];

(* STEP 3: Faceting - Modified to Pass Scales *)
ClearAll[facetWithScales];
facetWithScales[dataset_, facetSpec_, scales_] := Module[{facetResult, enrichedResult},
  (* Your existing faceting logic *)
  facetResult = If[facetSpec === facetIdentity[], 
    <|"type" -> "identity", "panels" -> <|"single" -> dataset|>|>,
    facetSpec[dataset]
  ];
  
  (* Attach scale information to facet result *)
  enrichedResult = Append[facetResult, "scales" -> scales];
  
  enrichedResult
];

(* STEP 4: Scale Application (NEW - MOST IMPORTANT) *)
ClearAll[materializeAndApplyScales];
materializeAndApplyScales[facetResult_, scales_] := Module[{
  allPanelData, materializedScales, panelDataWithScales
},
  (* Collect all panel data to determine global scale domains *)
  allPanelData = Flatten[Values[facetResult["panels"]], 1];
  
  (* Convert scale specifications into actual scale objects with domains *)
  materializedScales = KeyValueMap[
    Function[{aesthetic, scaleSpec},
      If[scaleSpec["applied"] === False,
        createMaterializedScale[aesthetic, scaleSpec, allPanelData],
        scaleSpec
      ]
    ],
    scales
  ];
  
  (* Apply scales to each panel's data *)
  panelDataWithScales = KeyValueMap[
    Function[{panelKey, panelData},
      applyScalesToPanelData[panelData, materializedScales]
    ],
    facetResult["panels"]
  ];
  
  (* Return updated facet result with materialized scales *)
  <|
    "type" -> facetResult["type"],
    "panels" -> panelDataWithScales, 
    "panelOrder" -> facetResult["panelOrder"],
    "scales" -> materializedScales
  |>
];

(* Helper: Create materialized scale from specification *)
ClearAll[createMaterializedScale];
createMaterializedScale[aesthetic_, scaleSpec_, allData_] := Module[{
  values, domain, range, transform, scaleObj
},
  Switch[aesthetic,
    "x" | "y",
    (* For positional scales, determine domain from all data *)
    values = extractMappedValues[allData, scaleSpec["mapping"]];
    domain = If[scaleSpec["type"] === "discrete",
      Sort[DeleteDuplicates[values]],
      MinMax[values]
    ];
    transform = createTransformFunction[scaleSpec["type"], domain];
    
    Scale[aesthetic, scaleSpec["type"], domain, Automatic,
      "transform" -> transform,
      "name" -> ToString[scaleSpec["mapping"]]
    ],
    
    "color",
    (* For aesthetic scales, create the full mapping *)
    createColorScale[allData, scaleSpec["mapping"]],
    
    _,
    (* Default identity scale *)
    Scale[aesthetic, "identity", {}, {}, "name" -> ToString[aesthetic]]
  ]
];

(* Helper: Apply materialized scales to panel data *)
ClearAll[applyScalesToPanelData];
applyScalesToPanelData[panelData_, materializedScales_] := Module[{processedData},
  processedData = panelData;
  
  (* Apply aesthetic scales (color, size, etc.) to add _aes columns *)
  KeyValueMap[
    Function[{aesthetic, scale},
      If[MemberQ[{"color", "size", "shape", "alpha"}, aesthetic],
        processedData = addAestheticColumn[processedData, aesthetic, scale];
      ]
    ],
    materializedScales
  ];
  
  processedData
];

(* STEP 5: Stat→Geom Processing - Modified to Use Scales *)
ClearAll[processPanelLayersWithScales];
processPanelLayersWithScales[panelData_, layers_, scales_, options_] := Module[{
  layerResults
},
  layerResults = Map[
    Function[layer,
      Module[{statResult, geomResult},
        (* Stat processing - unchanged from your current approach *)
        statResult = layer["stat"][Sequence @@ layer["statParams"], "data" -> panelData];
        
        (* Geom processing - now gets scale objects instead of scale functions *)
        geomResult = layer["geom"][statResult,
          Sequence @@ layer["geomParams"],
          "scales" -> scales  (* Pass all scales instead of individual functions *)
        ];
        
        geomResult
      ]
    ],
    layers
  ];
  
  layerResults
];

(* STEP 6: Legend Generation - Now Trivial *)
ClearAll[generateLegendsFromScales];
generateLegendsFromScales[scales_] := Module[{aestheticScales, legendGraphics},
  (* Extract only scales that should have legends *)
  aestheticScales = Select[scales, 
    #["aesthetic"] ∈ {"color", "size", "shape", "alpha"} && 
    #["type"] =!= "identity" && 
    #["type"] =!= "constant" &
  ];
  
  (* Generate legend for each aesthetic scale *)
  legendGraphics = Map[createLegendFromScale, aestheticScales];
  
  (* Layout legends *)
  If[Length[legendGraphics] > 0, 
    Column[legendGraphics, Spacings -> 1],
    Graphics[{}]
  ]
];

(* =============================================================================
   MODIFIED MAIN GGPLOT FUNCTION
   ============================================================================= *)

ClearAll[ggplotWithScalePipeline];
ggplotWithScalePipeline[args___?argPatternQ] := Module[{
  heldArgs, options, dataset, layers, facetSpec,
  discoveredScales, facetResult, materializedFacetResult, 
  panelGraphics, legends, finalGraphic
},
  (* Step 1: Extract arguments (unchanged) *)
  heldArgs = Hold[args];
  options = Cases[heldArgs, _Rule, 1];
  dataset = Lookup[options, "data", {}];
  layers = Cases[heldArgs, _geom, {0, Infinity}]; (* Your geom patterns *)
  facetSpec = Cases[heldArgs, facetWrap[___], {0, Infinity}];
  facetSpec = If[Length[facetSpec] > 0, First[facetSpec], facetIdentity[]];
  
  (* Step 2: Scale Discovery *)
  discoveredScales = discoverScales[dataset, heldArgs, Association[options]];
  
  (* Step 3: Faceting with Scales *)
  facetResult = facetWithScales[dataset, facetSpec, discoveredScales];
  
  (* Step 4: Scale Materialization and Application *)
  materializedFacetResult = materializeAndApplyScales[facetResult, discoveredScales];
  
  (* Step 5: Layer Processing with Scales *)
  panelGraphics = KeyValueMap[
    Function[{panelKey, panelData},
      processPanelLayersWithScales[panelData, layers, 
        materializedFacetResult["scales"], options]
    ],
    materializedFacetResult["panels"]
  ];
  
  (* Step 6: Legend Generation *)
  legends = generateLegendsFromScales[materializedFacetResult["scales"]];
  
  (* Step 7: Layout (unchanged from your current approach) *)
  finalGraphic = layoutFacetedPlot[panelGraphics, legends, 
    materializedFacetResult, options];
  
  finalGraphic
];

End[];

EndPackage[];