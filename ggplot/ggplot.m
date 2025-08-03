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
  markers = MapThread[Function[{shape, size, alpha, colors},
    shape /. {ggplotSizePlaceholder -> size, ggplotAlphaPlaceholder -> Opacity[alpha], ggplotColorPlaceholder -> colors}
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
argPatternQ[expr___] := MatchQ[Hold[expr], Hold[(_Rule | geomPoint[___] | geomLine[___] | geomPath[___] | geomSmooth[___] | geomVLine[___] | geomHLine[___] | geomParityLine[___] | geomHistogram[___] | geomCol[___] | geomErrorBar[___] | geomErrorBoxes[___] | geomErrorBand[___] | geomDensity2DFilled[___] | scaleXDate2[___] | scaleXLinear2[___] | scaleXLog2[___] | scaleYDate2[___] | scaleYLinear2[___] | scaleYLog2[___]) ...]];

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
ggplot[args___?argPatternQ] /; Count[Hold[args], ("data" -> _), {0, Infinity}] > 0 := Catch[Module[{heldArgs, options, dataset, defaultXLabel, defaultYLabel, frameLabel, points, lines, paths, smoothLines, columns, abLines, hLines, vLines, histograms, graphicsPrimitives, xScaleType, yScaleType, xScaleFunc, yScaleFunc, xDiscreteLabels, yDiscreteLabels, xTickFunc, yTickFunc, xGridLineFunc, yGridLineFunc, legendInfo, legendGraphics, showLegend, graphic},
  
  heldArgs = Hold[args];
  options = Cases[heldArgs, _Rule, 1];
  dataset = Lookup[options, "data", {}];
  options = Join[options, {"data" -> dataset, "x" -> Lookup[options, "x", Null], "y" -> Lookup[options, "y", Null]}];

  (* Default x and y labels *)
  defaultXLabel = First@Cases[heldArgs, ("x" -> xlbl_) :> ToString[xlbl], {0, Infinity}];
  defaultYLabel = Quiet[Check[First@Cases[heldArgs, ("y" -> ylbl_) :> ToString[ylbl], {0, Infinity}], ""]];
  frameLabel = Lookup[options, FrameLabel, OptionValue[ggplot, FrameLabel]]; (* allow default FrameLabel style to be given as well and have it trump any other labeling unless it's 'Automatic'*)
  frameLabel = Which[
    frameLabel === Automatic,
    {defaultXLabel, defaultYLabel} /. str_?StringQ :> Style[str, Opacity[1], FontColor -> Black],
    MatchQ[frameLabel, {{_?StringQ, _?StringQ}, {_?StringQ, _?StringQ}} | {_?StringQ, _?StringQ} | _?StringQ],
    frameLabel /. str_?StringQ :> Style[str, Opacity[1], FontColor -> Black],
    True,
    frameLabel
  ];

  (* Get all scaling information *)
  xScaleType = reconcileXScales[heldArgs]; (* returns Discrete / Linear / Date / Log / Log10 / Log2 *)
  yScaleType = reconcileYScales[heldArgs]; (* returns Discrete / Linear / Date / Log / Log10 / Log2 *)

  (* Creating scaling functions to use for x and y *)
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

  (* Compile all geom information which will create graphics primitives *)
  points      = Cases[heldArgs, geomPoint[opts___]      :> geomPoint[opts,      FilterRules[options, Options[geomPoint]],      "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  lines       = Cases[heldArgs, geomLine[opts___]       :> geomLine[opts,       FilterRules[options, Options[geomLine]],       "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  paths       = Cases[heldArgs, geomPath[opts___]       :> geomPath[opts,       FilterRules[options, Options[geomPath]],       "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  smoothLines = Cases[heldArgs, geomSmooth[opts___]     :> geomSmooth[opts,     FilterRules[options, Options[geomSmooth]],     "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  abLines     = Cases[heldArgs, geomParityLine[opts___] :> geomParityLine[opts, FilterRules[options, Options[geomParityLine]], "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  hLines      = Cases[heldArgs, geomHLine[opts___]      :> geomHLine[opts,      FilterRules[options, Options[geomHLine]],      "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  vLines      = Cases[heldArgs, geomVLine[opts___]      :> geomVLine[opts,      FilterRules[options, Options[geomVLine]],      "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  histograms  = Cases[heldArgs, geomHistogram[opts___]  :> geomHistogram[opts,  FilterRules[options, Options[geomHistogram]],  "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  errorBars   = Cases[heldArgs, geomErrorBar[opts___]   :> geomErrorBar[opts,   FilterRules[options, Options[geomErrorBar]],   "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  errorBoxes  = Cases[heldArgs, geomErrorBoxes[opts___] :> geomErrorBoxes[opts, FilterRules[options, Options[geomErrorBoxes]], "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  errorBands  = Cases[heldArgs, geomErrorBand[opts___]  :> geomErrorBand[opts,  FilterRules[options, Options[geomErrorBand]],  "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  density2D   = Cases[heldArgs, geomDensity2DFilled[opts___] :> geomDensity2DFilled[opts, FilterRules[options, Options[geomDensity2DFilled]], "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  (* columns need a lot more work to sort through *)
  (*columns     = Cases[{geoms}, geomCol[aesthetics__] :> geomCol[dataset, aesthetics, "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];*)

  graphicsPrimitives = {density2D, points, lines, paths, smoothLines, abLines, hLines, vLines, histograms, errorBars, errorBoxes, errorBands} // Flatten;

  (* Tick / GridLine functions passed into ggplot FrameTicks -> _ call *)
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

  (* Create legend if needed *)
  showLegend = Lookup[options, "showLegend", OptionValue[ggplot, "showLegend"]];
  legendInfo = {};
  If[showLegend === Automatic || showLegend === True,
    legendInfo = extractLegendInfo[heldArgs, dataset, options];
  ];

  graphic = If[Length[legendInfo] > 0,
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
        (* FrameTicks        -> If[Lookup[options, FrameTicks, OptionValue[ggplot, FrameTicks]] === Automatic,
          {{yTickFunc, False}, {xTickFunc, False}},
          Lookup[options, FrameTicks, OptionValue[ggplot, FrameTicks]]
        ], *)
        GridLines         -> If[Lookup[options, GridLines, OptionValue[ggplot, GridLines]] === Automatic,
          {xGridLineFunc, yGridLineFunc},
          Lookup[options, GridLines, OptionValue[ggplot, GridLines]]
        ],
        GridLinesStyle    -> Automatic, (* shouldn't need this but do for some reason *)
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
    (* No legend case *)
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
      (* FrameTicks        -> If[Lookup[options, FrameTicks, OptionValue[ggplot, FrameTicks]] === Automatic,
        {{yTickFunc, False}, {xTickFunc, False}},
        Lookup[options, FrameTicks, OptionValue[ggplot, FrameTicks]]
      ], *)
      GridLines         -> If[Lookup[options, GridLines, OptionValue[ggplot, GridLines]] === Automatic,
        {xGridLineFunc, yGridLineFunc},
        Lookup[options, GridLines, OptionValue[ggplot, GridLines]]
      ],
      GridLinesStyle    -> Automatic, (* shouldn't need this but do for some reason *)
      Background        -> Lookup[options, Background, OptionValue[ggplot, Background]],
      ImageMargins      -> Lookup[options, ImageMargins, OptionValue[ggplot, ImageMargins]],
      PlotRangeClipping -> Lookup[options, PlotRangeClipping, OptionValue[ggplot, PlotRangeClipping]],
      Prolog            -> Lookup[options, Prolog, OptionValue[ggplot, Prolog]],
      Method            -> Lookup[options, Method, OptionValue[ggplot, Method]],
      FilterRules[{options}, Options[ListLinePlot]]
    ]
  ];

  graphic
]];

End[];

EndPackage[]