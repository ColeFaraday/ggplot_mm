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
ggplot::errorBarMissingBounds = "geomErrorBar requires all four bounds: xmin, xmax, ymin, ymax";
ggplot::errorBandMissingBounds = "geomErrorBand requires x, ymin, and ymax";
ggplot::labelNotGiven       = "geomText requires a label mapping";
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
argPatternQ[expr___] := MatchQ[Hold[expr], Hold[(_Rule | geomPoint[___] | geomLine[___] | geomPath[___] | geomSmooth[___] | geomVLine[___] | geomHLine[___] | geomParityLine[___] | geomHistogram[___] | geomCol[___] | geomErrorBar[___] | geomErrorBoxes[___] | geomErrorBand[___] | geomDensity2DFilled[___] | geomText[___] | scaleXDate2[___] | scaleXLinear2[___] | scaleXLog2[___] | scaleYDate2[___] | scaleYLinear2[___] | scaleYLog2[___] | facetWrap[___]) ...]];

(* Main ggplot method and entry point *)
Options[ggplot] = DeleteDuplicates[Join[{
  "data" -> {},
  "categoricalColors" -> Automatic,
  "sequentialColors" -> {Blue, White, Red},
  "divergingColors" -> {Blue, White, Red},
  "continuousColorPalette" -> "auto",
  "categoricalShapes" -> {"\[FilledCircle]", "\[FilledUpTriangle]", "\[FilledSquare]", "\[FivePointedStar]", "\[FilledDiamond]", "\[FilledRectangle]", "\[FilledDownTriangle]"},
  "showLegend" -> Automatic,
  "legendPosition" -> "right",
  "legendSpacing" -> 0.15
}, Options[ListLinePlot], Options[ticks2], Options[gridLines2]]];
(* Options for ggplot are set further below in themes *)
Attributes[ggplot] = {HoldAllComplete};
ggplot[ds_?validDatasetQ, args___?argPatternQ] := ggplot["data" -> ds, args];
ggplot[args___?argPatternQ][ds_?validDatasetQ] := ggplot["data" -> ds, args];
ggplot[args___?argPatternQ] /; Count[Hold[args], ("data" -> _), {0, Infinity}] > 0 := Catch[Module[{heldArgs, options, dataset, defaultXLabel, defaultYLabel, frameLabel, points, lines, paths, smoothLines, columns, abLines, hLines, vLines, histograms, graphicsPrimitives, xScaleType, yScaleType, xScaleFunc, yScaleFunc, xDiscreteLabels, yDiscreteLabels, xTickFunc, yTickFunc, xGridLineFunc, yGridLineFunc, legendInfo, legendGraphics, showLegend, graphic, facetInfo, layers, facetSpec, facetResult, globalScales, panelGraphics, allLegendData},
  
  Print["[ggplot] Starting new pipeline"];
  heldArgs = Hold[args];
  Print["[ggplot] heldArgs length:", Length[Unevaluated[args]]];
  options = Cases[heldArgs, _Rule, 1];
  Print["[ggplot] options count:", Length[options]];
  dataset = Lookup[options, "data", {}];
  Print["[ggplot] dataset length:", Length[dataset]];

  (* Extract layers (geom constructors) *)
  layers = Cases[heldArgs, 
    (geomPoint[opts___] | geomLine[opts___] | geomPath[opts___] | geomSmooth[opts___] | 
     geomVLine[opts___] | geomHLine[opts___] | geomParityLine[opts___] | 
     geomHistogram[opts___] | geomErrorBar[opts___] | geomErrorBoxes[opts___] | 
     geomErrorBand[opts___] | geomDensity2DFilled[opts___] | geomText[opts___]), 
    {0, Infinity}
  ];
  Print["[ggplot] layers found:", Length[layers], " layers"];
  Print["[ggplot] layer types:", Head /@ layers];

  (* Extract facet specification *)
  facetSpec = Cases[heldArgs, facetWrap[variable_, opts___] :> facetWrap[variable, opts], {0, Infinity}];
  Print["[ggplot] facetWrap cases found:", Length[facetSpec]];
  facetSpec = If[Length[facetSpec] > 0, First[facetSpec], facetIdentity[]];
  Print["[ggplot] facetSpec head:", Head[facetSpec]];

  (* 1. Apply faceting to get panel specifications *)
  Print["[ggplot] Step 1: Applying faceting"];
  facetResult = If[facetSpec === facetIdentity[], 
    Print["[ggplot] Using identity facet"];
    <|"type" -> "identity", "panels" -> <|"single" -> dataset|>|>,
    Print["[ggplot] Calling facet function"];
    facetSpec[dataset]  (* Facet function transforms the data *)
  ];
  Print["[ggplot] facetResult type:", facetResult["type"]];
  Print["[ggplot] facetResult panels keys:", Keys[facetResult["panels"]]];
  Print["[ggplot] facetResult:", facetResult];

  (* 2. Compute global scales for consistency across panels *)
  Print["[ggplot] Step 2: Computing global scales"];
  defaultXLabel = First@Cases[heldArgs, ("x" -> x_) :> ToString[x], {0, Infinity}];
  defaultYLabel = Quiet[Check[First@Cases[heldArgs, ("y" -> y_) :> ToString[y], {0, Infinity}], ""]];
  Print["[ggplot] labels - x:", defaultXLabel, ", y:", defaultYLabel];
  frameLabel = Lookup[options, FrameLabel, OptionValue[ggplot, FrameLabel]];
  frameLabel = Which[
    frameLabel === Automatic,
    {defaultXLabel, defaultYLabel} /. str_?StringQ :> Style[str, Opacity[1], FontColor -> Black],
    MatchQ[frameLabel, {{_?StringQ, _?StringQ}, {_?StringQ, _?StringQ}} | {_?StringQ, _?StringQ} | _?StringQ],
    frameLabel /. str_?StringQ :> Style[str, Opacity[1], FontColor -> Black],
    True,
    frameLabel
  ];
  Print["[ggplot] frameLabel head:", Head[frameLabel]];

  xScaleType = reconcileXScales[heldArgs];
  yScaleType = reconcileYScales[heldArgs];
  Print["[ggplot] scale types - x:", xScaleType, ", y:", yScaleType];

  xScaleFunc = If[xScaleType == "Discrete",
    createDiscreteScaleFunc["x", heldArgs],
    With[{f = ToExpression[xScaleType /. "Linear" | "Date" -> "Identity"]}, Function[f[#]]]
  ];
  yScaleFunc = If[yScaleType == "Discrete",
    createDiscreteScaleFunc["y", heldArgs],
    With[{f = ToExpression[yScaleType /. "Linear" | "Date" -> "Identity"]}, Function[f[#]]]
  ];
  Print["[ggplot] scale functions created"];

  If[xScaleType == "Discrete", xDiscreteLabels = createDiscreteScaleLabels["x", heldArgs]; Print["[ggplot] xDiscreteLabels length:", Length[xDiscreteLabels]]];
  If[yScaleType == "Discrete", yDiscreteLabels = createDiscreteScaleLabels["y", heldArgs]; Print["[ggplot] yDiscreteLabels length:", Length[yDiscreteLabels]]];

  globalScales = <|
    "xScaleFunc" -> xScaleFunc, 
    "yScaleFunc" -> yScaleFunc
  |>;
  Print["[ggplot] globalScales created"];

  (* 3. Process each panel through stat→geom pipeline *)
  Print["[ggplot] Step 3: Processing panels"];
  {panelGraphics, allLegendRequests} = Reap[
    KeyValueMap[
      Function[{panelKey, panelData},
        Print["[ggplot] Processing panel:", panelKey, ", data length:", Length[panelData]];
        processPanelLayers[panelData, layers, globalScales, options]
      ],
      facetResult["panels"]
    ],
    "legendRequests"
  ];
  Print["[ggplot] panelGraphics length:", Length[panelGraphics]];
  Print["[ggplot] panelGraphics heads:", Head /@ panelGraphics];
  Print["[ggplot] allLegendRequests length:", Length[allLegendRequests]];
  Print["[ggplot] allLegendRequests:", allLegendRequests];

  (* 4. Generate legends from collected requests *)
  Print["[ggplot] Step 4: Generating legends from requests"];
  legendInfo = If[Length[allLegendRequests] > 0,
    createLegendsFromRequests[Flatten[allLegendRequests, 1]],
    {}
  ];
  Print["[ggplot] legendInfo length:", Length[legendInfo]];
  Print["[ggplot] legendInfo:", legendInfo];

  (* 5. Layout panels + legends based on facet type *)
  Print["[ggplot] Step 5: Layout"];
  graphic = layoutFacetedPlot[panelGraphics, legendInfo, facetResult, options];
  Print["[ggplot] final graphic:", Head[graphic]];

  graphic
]];

(* Create legends from collected legend requests *)
createLegendsFromRequests[legendRequests_] := Module[{groupedRequests, legends},
  Print["[createLegendsFromRequests] Starting legend creation"];
  Print["[createLegendsFromRequests] Number of requests:", Length[legendRequests]];
  Print["[createLegendsFromRequests] Requests:", legendRequests];
  
  (* For now, just create a simple placeholder legend for each request *)
  legends = Map[createSimpleLegendFromRequest, legendRequests];
  
  Print["[createLegendsFromRequests] Created legends:", legends];
  legends
];

(* Create a simple legend from a single request *)
createSimpleLegendFromRequest[request_] := Module[{geomType, aesthetics, colorAesthetic, colorMapping, uniqueValues, actualColors, legendLabels},
  Print["[createSimpleLegendFromRequest] Processing request:", request];
  
  geomType = Lookup[request, "geomType", "unknown"];
  aesthetics = Lookup[request, "aesthetics", <||>];
  
  Print["[createSimpleLegendFromRequest] GeomType:", geomType, ", Aesthetics:", aesthetics];
  
  (* For now, just handle color aesthetic as a simple example *)
  If[KeyExistsQ[aesthetics, "color"],
    colorAesthetic = aesthetics["color"];
    colorMapping = Lookup[colorAesthetic, "mapping", "Unknown"];
    uniqueValues = Lookup[colorAesthetic, "uniqueValues", {}];
    Print["[createSimpleLegendFromRequest] Creating color legend for mapping:", colorMapping];
    Print["[createSimpleLegendFromRequest] Unique values:", uniqueValues];
    
    (* Use reconcileAesthetics to get the actual colors that would be used *)
    Module[{sampleData, reconciledData, actualColors},
      (* Create sample data with the unique values *)
      sampleData = Table[<|colorMapping -> val|>, {val, uniqueValues}];
      Print["[createSimpleLegendFromRequest] Sample data for reconciliation:", sampleData];
      
      (* Use the same reconcileAesthetics function that the actual plotting uses *)
      reconciledData = reconcileAesthetics[sampleData, colorMapping, "color"];
      actualColors = reconciledData[[All, "color_aes"]];
      Print["[createSimpleLegendFromRequest] Actual colors from reconcileAesthetics:", actualColors];
      
      (* Create legend with actual colors and values *)
      legendLabels = ToString /@ uniqueValues;
      Print["[createSimpleLegendFromRequest] Legend labels:", legendLabels];
      
      PointLegend[
        actualColors,
        legendLabels,
        LegendLabel -> ToString[colorMapping]
      ]
    ],
    (* No aesthetics to legend *)
    Nothing
  ]
];

(* Helper to extract mapped values for x, y, or any mapping (string or function) *)
extractMappedValues[data_, mapping_] := 
  Which[
    StringQ[mapping], data[[All, mapping]],
    Head[mapping] === Function, mapping /@ data,
    True, Nothing
  ];

End[];

EndPackage[]