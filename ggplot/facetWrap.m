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
  "panels" -> <|"single" -> "all"|>
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
    Frame -> True,
    FrameLabel -> {None, None},
    PlotRange -> Lookup[options, PlotRange, All],
    AspectRatio -> Lookup[options, AspectRatio, 7/10],
    ImageSize -> 150,
    Background -> Lookup[options, Background, None],
    FrameStyle -> Lookup[options, FrameStyle, Automatic],
    GridLines -> None
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
  
  processedData
];

(* Layout functions for different facet types *)
layoutFacetedPlot[panelGraphics_, legendInfo_, facetResult_, options_] := (
  
  Which[
    facetResult["type"] === "identity",
    (
     layoutSinglePanel[panelGraphics, legendInfo, options]),
    
    facetResult["type"] === "wrap", 
    (
     layoutWrappedPanels[panelGraphics, legendInfo, facetResult, options]),
    
    True,
    (
     $Failed) (* Unsupported facet type *)
  ]
);

layoutSinglePanel[panelGraphics_, legendInfo_, options_] := (
 
  First[panelGraphics] (* Just return the single panel *)
);

layoutWrappedPanels[panelGraphics_, legendInfo_, facetResult_, options_] := Module[{
  arrangedPanels, stripLabels, finalGrid
},
  
  (* Arrange panels into grid *)
  arrangedPanels = ArrayReshape[panelGraphics, facetResult["gridDims"], None];
  
  stripLabels = facetResult["stripLabels"];
  
  (* TODO: Add legends based on scope - placeholder for now *)
  (* arrangedPanels = addLegendsToArrangement[arrangedPanels, legendInfo, options]; *)
  
  (* Use ResourceFunction for grid layout *)
  finalGrid = ResourceFunction["PlotGrid"][arrangedPanels, PlotLabels -> stripLabels];
  finalGrid
];
End[];

EndPackage[];
