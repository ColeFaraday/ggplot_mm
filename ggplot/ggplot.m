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
ggplot[args___?argPatternQ] /; Count[Hold[args], ("data" -> _), {0, Infinity}] > 0 := Catch[Module[{heldArgs, options, dataset, defaultXLabel, defaultYLabel, frameLabel, points, lines, paths, smoothLines, columns, abLines, hLines, vLines, histograms, graphicsPrimitives, xScaleType, yScaleType, xScaleFunc, yScaleFunc, xDiscreteLabels, yDiscreteLabels, xTickFunc, yTickFunc, xGridLineFunc, yGridLineFunc, legendInfo, legendGraphics, showLegend, graphic, facetInfo},
  
  Print["[ggplot] Entered main function"];  
  heldArgs = Hold[args];
  Print["[ggplot] heldArgs:", heldArgs];
  options = Cases[heldArgs, _Rule, 1];
  dataset = Lookup[options, "data", {}];
  Print["[ggplot] dataset length:", Length[dataset]];
  (* options = Join[options, {"data" -> dataset, "x" -> Lookup[options, "x", Null], "y" -> Lookup[options, "y", Null]}]; *)

  (* Check for faceting *)
  facetInfo = Cases[heldArgs, facetWrap[opts___] :> facetWrap[opts], {0, Infinity}];
  Print["[ggplot] Faceting requested:", Length[facetInfo] > 0];
  If[Length[facetInfo] > 0,
    Module[{facetSpec},
      facetSpec = First[facetInfo];
      Print["[ggplot] Creating faceted plot"];      
      Return[createFacetedGraphics[dataset, facetSpec, heldArgs, options]]
    ]
  ];

  defaultXLabel = First@Cases[heldArgs, ("x" -> x_) :> ToString[x], {0, Infinity}];
  defaultYLabel = Quiet[Check[First@Cases[heldArgs, ("y" -> y_) :> ToString[y], {0, Infinity}], ""]];
  Print["[ggplot] defaultXLabel:", defaultXLabel, ", defaultYLabel:", defaultYLabel];
  frameLabel = Lookup[options, FrameLabel, OptionValue[ggplot, FrameLabel]];
  frameLabel = Which[
    frameLabel === Automatic,
    {defaultXLabel, defaultYLabel} /. str_?StringQ :> Style[str, Opacity[1], FontColor -> Black],
    MatchQ[frameLabel, {{_?StringQ, _?StringQ}, {_?StringQ, _?StringQ}} | {_?StringQ, _?StringQ} | _?StringQ],
    frameLabel /. str_?StringQ :> Style[str, Opacity[1], FontColor -> Black],
    True,
    frameLabel
  ];
  Print["[ggplot] frameLabel:", frameLabel];

  xScaleType = reconcileXScales[heldArgs];
  yScaleType = reconcileYScales[heldArgs];
  Print["[ggplot] xScaleType:", xScaleType, ", yScaleType:", yScaleType];

  xScaleFunc = If[xScaleType == "Discrete",
    createDiscreteScaleFunc["x", heldArgs],
    With[{f = ToExpression[xScaleType /. "Linear" | "Date" -> "Identity"]}, Function[f[#]]]
  ];
  yScaleFunc = If[yScaleType == "Discrete",
    createDiscreteScaleFunc["y", heldArgs],
    With[{f = ToExpression[yScaleType /. "Linear" | "Date" -> "Identity"]}, Function[f[#]]]
  ];
  Print["[ggplot] xScaleFunc and yScaleFunc created"];

  If[xScaleType == "Discrete", xDiscreteLabels = createDiscreteScaleLabels["x", heldArgs]; Print["[ggplot] xDiscreteLabels:", xDiscreteLabels]];
  If[yScaleType == "Discrete", yDiscreteLabels = createDiscreteScaleLabels["y", heldArgs]; Print["[ggplot] yDiscreteLabels:", yDiscreteLabels]];

  layers = Cases[heldArgs, 
    (geomPoint[opts___] | geomLine[opts___] | geomPath[opts___] | geomSmooth[opts___] | 
     geomVLine[opts___] | geomHLine[opts___] | geomParityLine[opts___] | 
     geomHistogram[opts___] | geomErrorBar[opts___] | geomErrorBoxes[opts___] | 
     geomErrorBand[opts___] | geomDensity2DFilled[opts___] | geomText[opts___]), 
    {0, Infinity}
  ];
  Print["[ggplot] Number of layers:", Length[layers]];
  Print[layers];

  {processedLayers, allStatData} = Reap[
    Map[
      Function[layer,
        Print["[ggplot] Processing layer:", layer];
        Module[{layerHead, layerOpts, mergedLayer},
          layerHead = Head[layer];
          layerOpts = List @@ layer;
          mergedLayer = layerHead@@ Normal@Join[
            Association@options,
            Association@layerOpts
          ];

          statParams = mergedLayer["statParams"];
          
          (* Prepare geom parameters with scaling functions *)
          geomParams = mergedLayer["geomParams"];


          Print["[ggplot] statParams:", statParams[[All,1]]];
          
          (* 1. Run stat *)
          statResult = mergedLayer["stat"][Sequence @@ statParams];
          Sow[statResult, "statData"];

          Print["[ggplot] statResult:", statResult];

          (* 2. Run geom *)
          geomGraphics = Values[mergedLayer["geom"][#, Sequence @@ geomParams] &/@ statResult];

          Print["[ggplot] geomGraphics:\n", geomGraphics];

          geomGraphics
        ]
      ],
      layers
    ]
  ];
  Print["[ggplot] processedLayers length:", Length[processedLayers]];

  graphicsPrimitives = Flatten[processedLayers];
  Print["[ggplot] graphicsPrimitives length:", Length[graphicsPrimitives]];
  Print["[ggplot] graphicsPrimitives head:", graphicsPrimitives[[1]]];

  With[{tickAndGridLineOptions = FilterRules[{options}, {Options[ticks2], Options[gridLines2]}]},
    xTickFunc = If[xScaleType == "Discrete",
      ticks2[xScaleType, xDiscreteLabels, tickAndGridLineOptions],
      Function[{min, max}, ticks2[xScaleType, min, max, tickAndGridLineOptions]]
    ];
    yTickFunc = If[yScaleType == "Discrete",
      ticks2[yScaleType, yDiscreteLabels, tickAndGridLineOptions],
      Function[{min, max}, ticks2[yScaleType, min, max, tickAndGridLineOptions]]
    ];
    xGridLineFunc = If[xScaleType == "Discrete",
      gridLines2[xScaleType, xDiscreteLabels, tickAndGridLineOptions],
      Function[{min, max}, gridLines2[xScaleType, min, max, tickAndGridLineOptions]]
    ];
    yGridLineFunc = If[yScaleType == "Discrete",
      gridLines2[yScaleType, yDiscreteLabels, tickAndGridLineOptions],
      Function[{min, max}, gridLines2[yScaleType, min, max, tickAndGridLineOptions]]
    ];
  ];
  Print["[ggplot] Tick/gridline functions created"];

  showLegend = Lookup[options, "showLegend", OptionValue[ggplot, "showLegend"]];
  legendInfo = {};
  If[showLegend === Automatic || showLegend === True,
    legendInfo = extractLegendInfo[heldArgs, dataset, options];
    Print["[ggplot] legendInfo:", legendInfo];
  ];

  graphic = If[Length[legendInfo] > 0,
    Print["[ggplot] Creating Legended Graphics"];
    Legended[
      Graphics[graphicsPrimitives,
        FrameLabel        -> frameLabel,
        PlotStyle         -> Lookup[options, PlotStyle, OptionValue[ggplot, PlotStyle]],
        ImageSize         -> Lookup[options, ImageSize, OptionValue[ggplot, ImageSize]],
        AspectRatio       -> Lookup[options, AspectRatio, OptionValue[ggplot, AspectRatio]],
        Frame             -> Lookup[options, Frame, OptionValue[ggplot, Frame]],
        Axes              -> Lookup[options, Axes, OptionValue[ggplot, Axes]],
        LabelStyle        -> Lookup[options, LabelStyle, OptionValue[ggplot, LabelStyle]],
        FrameStyle        -> Lookup[options, FrameStyle, OptionValue[ggplot, FrameStyle]],
        FrameTicksStyle   -> Lookup[options, FrameTicksStyle, OptionValue[ggplot, FrameTicksStyle]],
        PlotRange         -> Lookup[options, PlotRange, OptionValue[ggplot, PlotRange]],
        GridLines         -> If[Lookup[options, GridLines, OptionValue[ggplot, GridLines]] === Automatic,
          {xGridLineFunc, yGridLineFunc},
          Lookup[options, GridLines, OptionValue[ggplot, GridLines]]
        ],
        GridLinesStyle    -> Automatic,
        Background        -> Lookup[options, Background, OptionValue[ggplot, Background]],
        ImageMargins      -> Lookup[options, ImageMargins, OptionValue[ggplot, ImageMargins]],
        PlotRangeClipping -> Lookup[options, PlotRangeClipping, OptionValue[ggplot, PlotRangeClipping]],
        Prolog            -> Lookup[options, Prolog, OptionValue[ggplot, Prolog]],
        Method            -> Lookup[options, Method, OptionValue[ggplot, Method]],
        FilterRules[{options}, Options[ListLinePlot]]
      ],
      Placed[
        Row[Join[convertLegendInfo[legendInfo], {Spacer[5]}]],
        GetScaledCoord[Lookup[options, "legendPosition", "OuterMiddleRight"]]
      ]
    ],
    Print["[ggplot] Creating Graphics (no legend)"];
    Graphics[graphicsPrimitives,
      FrameLabel        -> frameLabel,
      PlotStyle         -> Lookup[options, PlotStyle, OptionValue[ggplot, PlotStyle]],
      ImageSize         -> Lookup[options, ImageSize, OptionValue[ggplot, ImageSize]],
      AspectRatio       -> Lookup[options, AspectRatio, OptionValue[ggplot, AspectRatio]],
      Frame             -> Lookup[options, Frame, OptionValue[ggplot, Frame]],
      Axes              -> Lookup[options, Axes, OptionValue[ggplot, Axes]],
      LabelStyle        -> Lookup[options, LabelStyle, OptionValue[ggplot, LabelStyle]],
      FrameStyle        -> Lookup[options, FrameStyle, OptionValue[ggplot, FrameStyle]],
      FrameTicksStyle   -> Lookup[options, FrameTicksStyle, OptionValue[ggplot, FrameTicksStyle]],
      PlotRange         -> Lookup[options, PlotRange, OptionValue[ggplot, PlotRange]],
      GridLines         -> If[Lookup[options, GridLines, OptionValue[ggplot, GridLines]] === Automatic,
        {xGridLineFunc, yGridLineFunc},
        Lookup[options, GridLines, OptionValue[ggplot, GridLines]]
      ],
      GridLinesStyle    -> Automatic,
      Background        -> Lookup[options, Background, OptionValue[ggplot, Background]],
      ImageMargins      -> Lookup[options, ImageMargins, OptionValue[ggplot, ImageMargins]],
      PlotRangeClipping -> Lookup[options, PlotRangeClipping, OptionValue[ggplot, PlotRangeClipping]],
      Prolog            -> Lookup[options, Prolog, OptionValue[ggplot, Prolog]],
      Method            -> Lookup[options, Method, OptionValue[ggplot, Method]],
      FilterRules[{options}, Options[ListLinePlot]]
    ]
  ];
  Print["[ggplot] Returning graphic"];
  graphic
]];

(* Helper to extract mapped values for x, y, or any mapping (string or function) *)
extractMappedValues[data_, mapping_] := 
  Which[
    StringQ[mapping], data[[All, mapping]],
    Head[mapping] === Function, mapping /@ data,
    True, Nothing
  ];

End[];

EndPackage[]