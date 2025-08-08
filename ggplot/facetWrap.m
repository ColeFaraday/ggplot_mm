(* Mathematica Source File *)
(* :Author: colefaraday *)
(* :Date: 2025-08-03 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* facetWrap implementation *)

Options[facetWrap] = {"ncol" -> Automatic, "nrow" -> Automatic, "scales" -> "fixed", "stripPosition" -> "top"};

(* facetWrap returns a function that transforms data into panel specifications *)
facetWrap[variable_, opts : OptionsPattern[]] := Function[dataset,
  Module[{uniqueValues, nPanels, gridDims, panels},
    uniqueValues = Sort[DeleteDuplicates[dataset[[All, variable]]]];
    nPanels = Length[uniqueValues];
    
    (* Determine grid dimensions *)
    gridDims = Module[{ncol, nrow},
      ncol = OptionValue["ncol"];
      nrow = OptionValue["nrow"];
      
      Which[
        IntegerQ[ncol] && IntegerQ[nrow],
        {nrow, ncol},
        IntegerQ[ncol],
        {Ceiling[nPanels/ncol], ncol},
        IntegerQ[nrow],
        {nrow, Ceiling[nPanels/nrow]},
        True,
        Module[{idealCols},
          idealCols = Ceiling[Sqrt[nPanels]];
          {Ceiling[nPanels/idealCols], idealCols}
        ]
      ]
    ];
    
    (* Group data by faceting variable *)
    panels = GroupBy[dataset, #[[variable]]&];

    (* Return panel specification *)
    <|
      "type" -> "wrap",
      "variable" -> variable,
      "gridDims" -> gridDims,
      "panels" -> panels,
      "panelOrder" -> uniqueValues,
      "stripLabels" -> Map[ToString, uniqueValues],
      "options" -> {opts}
    |>
  ]
];

(* Identity facet for non-faceted plots *)
facetIdentity[] := <|
  "type" -> "identity", 
  "panels" -> <|"single" -> "all"|>,
  "panelOrder" -> {"single"}
|>;

(* Shared panel processing function - used by ALL facet types *)
processPanelLayers[panelData_, layers_, globalScales_, options_] := Module[{processedLayers},

  
  processedLayers = Map[
    Function[layer,
      Module[{layerHead, layerOpts, mergedLayer, layerAesthetics, resolvedData, statParams, geomParams, statResult, geomResult},
        layerHead = Head[layer];
        layerOpts = List @@ layer;
        
        mergedLayer = layerHead@@ Normal@Join[
          Association@options, Association@layerOpts
        ];
        
        (* STEP 1: Resolve layer-specific aesthetics BEFORE stat processing *)
        layerAesthetics = extractLayerAesthetics[Association@layerOpts];
        resolvedData = resolveLayerAesthetics[panelData, layerAesthetics, options];

        
        (* STEP 2: Run stat function with resolved data *)
        statParams = Normal@Association[
          Association@mergedLayer["statParams"], 
          <|"data" -> resolvedData|>
        ];
        
        geomParams = mergedLayer["geomParams"];
        
        (* Run stat → geom pipeline *)
        statResult = mergedLayer["stat"][Sequence @@ statParams];
        (* TODO: Sow[statResult, "legendData"] for legend generation *)

        geomResult = Values[mergedLayer["geom"][#, Sequence @@ geomParams] &/@ statResult];
        
        geomResult
      ]
    ],
    layers
  ];
  
  (* Return Graphics object for this panel *)
  Graphics[Flatten[processedLayers],
    (* Core frame and layout options - apply to each panel *)
    Frame -> Lookup[options, Frame, True],
    FrameLabel -> Lookup[options, FrameLabel, Automatic],
    FrameStyle -> Lookup[options, FrameStyle, Automatic],
    FrameTicksStyle -> Lookup[options, FrameTicksStyle, Automatic],
    LabelStyle -> Lookup[options, LabelStyle, Automatic],
    
    (* Plot range and clipping - consistent across panels *)
    PlotRange -> globalScales["plotRange"],
    PlotRangeClipping -> Lookup[options, PlotRangeClipping, True],
    
    (* Panel-specific styling *)
    AspectRatio -> Lookup[options, AspectRatio, 7/10],
    Background -> Lookup[options, Background, None],
    GridLines -> Lookup[options, GridLines, None],
    GridLinesStyle -> Lookup[options, GridLinesStyle, Automatic],
    
    (* Size for individual panels (will be overridden by facet layout) *)
    ImageSize -> 150,
    ImageMargins -> Lookup[options, ImageMargins, Automatic],
    
    (* Content options *)
    Prolog -> Lookup[options, Prolog, {}],
    Method -> Lookup[options, Method, Automatic],
    Axes -> Lookup[options, Axes, False]
  ]
];

(* Extract layer-specific aesthetic mappings *)
extractLayerAesthetics[layerOpts_] := Module[{layerAesthetics},
  layerAesthetics = <|
    "color" -> Lookup[layerOpts, "color", Null],
    "size" -> Lookup[layerOpts, "size", Null], 
    "alpha" -> Lookup[layerOpts, "alpha", Null],
    "shape" -> Lookup[layerOpts, "shape", Null],
    "thickness" -> Lookup[layerOpts, "thickness", Null],
    "lineAlpha" -> Lookup[layerOpts, "lineAlpha", Null],
    "group" -> Lookup[layerOpts, "group", Null]
  |>;
  layerAesthetics
];

(* Resolve layer aesthetics - only override globals if layer specifies something *)
resolveLayerAesthetics[panelData_, layerAesthetics_, globalOptions_] := Module[{processedData},
  processedData = panelData;
  
  (* Apply layer-specific aesthetics that override globals *)
  If[layerAesthetics["color"] =!= Null,
    processedData = reconcileAesthetics[processedData, layerAesthetics["color"], "color"]
  ];
  
  If[layerAesthetics["size"] =!= Null,
    processedData = reconcileAesthetics[processedData, layerAesthetics["size"], "size"]
  ];
  
  If[layerAesthetics["alpha"] =!= Null,
    processedData = reconcileAesthetics[processedData, layerAesthetics["alpha"], "alpha"]
  ];
  
  If[layerAesthetics["shape"] =!= Null,
    processedData = reconcileAesthetics[processedData, layerAesthetics["shape"], "shape"]
  ];
  
  If[layerAesthetics["thickness"] =!= Null,
    processedData = reconcileAesthetics[processedData, layerAesthetics["thickness"], "thickness"]
  ];
  
  If[layerAesthetics["group"] =!= Null,
    processedData = reconcileAesthetics[processedData, layerAesthetics["group"], "group"]
  ];

  If[layerAesthetics["lineAlpha"] =!= Null,
    processedData = reconcileAesthetics[processedData, layerAesthetics["lineAlpha"], "lineAlpha"]
  ];
  
  processedData
];

(* Layout functions for different facet types *)
layoutFacetedPlot[panelGraphics_, legendInfo_, facetResult_, options_, frameLabel_] := (
  
  Which[
    facetResult["type"] === "identity",
    (
     layoutSinglePanel[panelGraphics, legendInfo, options]),
    
    facetResult["type"] === "wrap", 
    (
     layoutWrappedPanels[panelGraphics, legendInfo, facetResult, options, frameLabel]),
    
    True,
    (
     $Failed) (* Unsupported facet type *)
  ]
);

layoutSinglePanel[panelGraphics_, legendInfo_, options_] := Module[{singlePanel, panelOptions, panelPrimitives},
  singlePanel = First[panelGraphics];
  
  (* Extract the graphics primitives and existing options from the panel *)
  panelPrimitives = First[singlePanel];
  panelOptions = Rest[List @@ singlePanel];
  
  (* Rebuild Graphics with user-specified options taking precedence *)
  (* But preserve computed values like PlotRange from the panel *)
  Graphics[
    panelPrimitives,
    (* Keep the panel's computed PlotRange and FrameLabel *)
    PlotRange -> (PlotRange /. panelOptions),
    FrameLabel -> (FrameLabel /. panelOptions), 
    
    (* Apply user-specified options that should override defaults *)
    Frame -> Lookup[options, Frame, Frame /. panelOptions /. Frame -> True],
    FrameStyle -> Lookup[options, FrameStyle, FrameStyle /. panelOptions /. FrameStyle -> Automatic],
    FrameTicksStyle -> Lookup[options, FrameTicksStyle, FrameTicksStyle /. panelOptions /. FrameTicksStyle -> Automatic],
    LabelStyle -> Lookup[options, LabelStyle, LabelStyle /. panelOptions /. LabelStyle -> Automatic],
    PlotRangeClipping -> Lookup[options, PlotRangeClipping, PlotRangeClipping /. panelOptions /. PlotRangeClipping -> True],
    AspectRatio -> Lookup[options, AspectRatio, AspectRatio /. panelOptions /. AspectRatio -> 7/10],
    Background -> Lookup[options, Background, Background /. panelOptions /. Background -> None],
    GridLines -> Lookup[options, GridLines, GridLines /. panelOptions /. GridLines -> None],
    GridLinesStyle -> Lookup[options, GridLinesStyle, GridLinesStyle /. panelOptions /. GridLinesStyle -> Automatic],
    
    (* The key fix: allow user to override ImageSize *)
    ImageSize -> Lookup[options, ImageSize, ImageSize /. panelOptions /. ImageSize -> Automatic],
    ImageMargins -> Lookup[options, ImageMargins, ImageMargins /. panelOptions /. ImageMargins -> Automatic],
    Prolog -> Lookup[options, Prolog, Prolog /. panelOptions /. Prolog -> {}],
    Method -> Lookup[options, Method, Method /. panelOptions /. Method -> Automatic],
    Axes -> Lookup[options, Axes, Axes /. panelOptions /. Axes -> False]
  ]
];

layoutWrappedPanels[panelGraphics_, legendInfo_, facetResult_, options_, frameLabel_] := Module[{
  arrangedPanels, stripLabels, finalGrid, plotGridOptions
},
  
  (* Arrange panels into grid *)
  arrangedPanels = ArrayReshape[panelGraphics, facetResult["gridDims"], None];
  
  stripLabels = facetResult["stripLabels"];
  
  (* TODO: Add legends based on scope - placeholder for now *)
  (* arrangedPanels = addLegendsToArrangement[arrangedPanels, legendInfo, options]; *)
  
  (* Prepare options for the overall plot - these should apply to the final graphic *)
  plotGridOptions = Sequence[
    PlotLabels -> stripLabels, 
    FrameLabel -> frameLabel,
    ImageSize -> Lookup[options, ImageSize, Automatic],
    ImageMargins -> Lookup[options, ImageMargins, Automatic],
    Background -> Lookup[options, Background, Automatic]
  ];
  
  (* Use ResourceFunction with comprehensive options *)
  finalGrid = ResourceFunction["PlotGrid"][arrangedPanels, plotGridOptions];
  finalGrid
];
End[];

EndPackage[];
