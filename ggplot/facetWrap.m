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
  Print["[processPanelLayers] Starting panel processing"];
  Print["[processPanelLayers] panelData length:", Length[panelData]];
  Print["[processPanelLayers] layers count:", Length[layers]];
  Print["[processPanelLayers] globalScales keys:", Keys[globalScales]];
  Print["[processPanelLayers] options count:", Length[options]];
  Print[panelData[[1]]];
  
  processedLayers = Map[
    Function[layer,
      Module[{layerHead, layerOpts, mergedLayer, statParams, geomParams, statResult, geomResult},
        layerHead = Head[layer];
        layerOpts = List @@ layer;
        Print["[processPanelLayers] layerHead:", layerHead];
        Print["[processPanelLayers] layerOpts length:", Length[layerOpts]];
        
        mergedLayer = layerHead@@ Normal@Join[
          Association@options, Association@layerOpts
        ];
        Print["[processPanelLayers] mergedLayer keys:", Keys[mergedLayer]];
        
        statParams = Normal@Association[
          Association@mergedLayer["statParams"], 
          <|"data" -> panelData|>
        ];
        Print["[processPanelLayers] statParams count:", Length[statParams]];
        
        geomParams = mergedLayer["geomParams"];
        Print["[processPanelLayers] geomParams count:", Length[geomParams]];
        
        (* Run stat → geom pipeline *)
        Print["[processPanelLayers] Running stat:", mergedLayer["stat"]];
        statResult = mergedLayer["stat"][Sequence @@ statParams];
        Print["[processPanelLayers] statResult length:", Length[statResult]];
        Print["[processPanelLayers] statResult keys:", Keys[statResult]];
        
        (* Generate legend request from stat output and geom type *)
        Print["[processPanelLayers] Generating legend request"];
        legendRequest = createLegendRequest[statParams, statResult, mergedLayer["geom"], panelData];
        Print["[processPanelLayers] legendRequest:", legendRequest];
        Sow[legendRequest, "legendRequests"];
        
        Print["[processPanelLayers] Running geom:", mergedLayer["geom"]];
        geomResult = Values[mergedLayer["geom"][#, Sequence @@ geomParams] &/@ statResult];
        Print["[processPanelLayers] geomResult length:", Length[geomResult]];
        
        geomResult
      ]
    ],
    layers
  ];
  Print["[processPanelLayers] processedLayers length:", Length[processedLayers]];
  
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

(* Create legend request from stat output and geom type *)
createLegendRequest[statParams_, statResult_, geomFunc_, originalData_] := Module[{
  geomType, aestheticMappings, legendRequest
},
  Print["[createLegendRequest] Starting legend request creation"];
  
  (* Determine geom type from function name *)
  geomType = determineGeomType[geomFunc];
  Print["[createLegendRequest] Determined geom type:", geomType];
  
  (* Extract aesthetic mappings from stat params and results *)
  aestheticMappings = extractAestheticMappings[statParams, statResult, originalData];
  Print["[createLegendRequest] Extracted aesthetic mappings:", aestheticMappings];
  
  (* Create legend request *)
  legendRequest = <|
    "geomType" -> geomType,
    "aesthetics" -> aestheticMappings,
    "originalData" -> originalData
  |>;
  
  Print["[createLegendRequest] Final legend request:", legendRequest];
  legendRequest
];

(* Determine geom type from geom function *)
determineGeomType[geomFunc_] := Module[{funcName},
  funcName = ToString[geomFunc];
  Print["[determineGeomType] Function name:", funcName];
  
  Which[
    StringContainsQ[funcName, "geomPointRender"], "point",
    StringContainsQ[funcName, "geomLineRender"], "line", 
    StringContainsQ[funcName, "geomPathRender"], "path",
    StringContainsQ[funcName, "geomColRender"], "bar",
    True, "unknown"
  ]
];

(* Extract aesthetic mappings from stat parameters and results *)
extractAestheticMappings[statParams_, statResult_, originalData_] := Module[{
  aesthetics, result, statAssoc
},
  result = <||>;
  statAssoc = Association[statParams];
  Print["[extractAestheticMappings] statParams keys:", Keys[statAssoc]];
  Print["[extractAestheticMappings] statResult keys (first group):", If[Length[statResult] > 0, Keys[First[statResult]], {}]];
  Print["[extractAestheticMappings] originalData length:", Length[originalData]];
  
  (* Look for aesthetic mappings in statParams *)
  aesthetics = {"color", "size", "shape", "alpha", "thickness"};
  
  Do[
    Module[{mappingValue, uniqueValues, dataValues},
      mappingValue = Lookup[statAssoc, aesthetic, Null];
      Print["[extractAestheticMappings] Checking aesthetic:", aesthetic, ", value:", mappingValue];
      
      If[mappingValue =!= Null,
        Print["[extractAestheticMappings] Found mapping for:", aesthetic, " -> ", mappingValue];
        
        (* Extract unique values from the original data *)
        dataValues = If[StringQ[mappingValue],
          originalData[[All, mappingValue]],
          mappingValue /@ originalData
        ];
        uniqueValues = DeleteDuplicates[dataValues];
        Print["[extractAestheticMappings] Unique values for", aesthetic, ":", uniqueValues];
        
        result[aesthetic] = <|
          "mapping" -> mappingValue,
          "uniqueValues" -> uniqueValues,
          "type" -> If[Length[uniqueValues] <= 10, "discrete", "continuous"]
        |>;
      ];
    ],
    {aesthetic, aesthetics}
  ];
  
  Print["[extractAestheticMappings] Final result:", result];
  result
];

(* Layout functions for different facet types *)
layoutFacetedPlot[panelGraphics_, legendInfo_, facetResult_, options_] := (
  Print["[layoutFacetedPlot] Starting layout"];
  Print["[layoutFacetedPlot] panelGraphics length:", Length[panelGraphics]];
  Print["[layoutFacetedPlot] facetResult type:", facetResult["type"]];
  Print["[layoutFacetedPlot] legendInfo:", legendInfo];
  
  Which[
    facetResult["type"] === "identity",
    (Print["[layoutFacetedPlot] Using single panel layout"]; 
     layoutSinglePanel[panelGraphics, legendInfo, options]),
    
    facetResult["type"] === "wrap", 
    (Print["[layoutFacetedPlot] Using wrapped panel layout"];
     layoutWrappedPanels[panelGraphics, legendInfo, facetResult, options]),
    
    True,
    (Print["[layoutFacetedPlot] ERROR: Unsupported facet type"];
     $Failed) (* Unsupported facet type *)
  ]
);

layoutSinglePanel[panelGraphics_, legendInfo_, options_] := Module[{panel, legends},
  Print["[layoutSinglePanel] panelGraphics length:", Length[panelGraphics]];
  Print["[layoutSinglePanel] First panel head:", Head[First[panelGraphics]]];
  Print["[layoutSinglePanel] legendInfo:", legendInfo];
  
  panel = First[panelGraphics];
  
  (* If we have legends, combine with panel *)
  If[Length[legendInfo] > 0,
    legends = Flatten[{legendInfo}];
    Print["[layoutSinglePanel] Adding", Length[legends], "legend(s) to plot"];
    (* Use Legended to combine the plot with legends *)
    Legended[panel, legends],
    (* No legends, just return the panel *)
    panel
  ]
];

layoutWrappedPanels[panelGraphics_, legendInfo_, facetResult_, options_] := Module[{
  arrangedPanels, stripLabels, finalGrid, legends
},
  Print["[layoutWrappedPanels] Starting wrapped layout"];
  Print["[layoutWrappedPanels] panelGraphics length:", Length[panelGraphics]];
  Print["[layoutWrappedPanels] facetResult gridDims:", facetResult["gridDims"]];
  Print["[layoutWrappedPanels] legendInfo:", legendInfo];
  
  (* Arrange panels into grid *)
  arrangedPanels = ArrayReshape[panelGraphics, facetResult["gridDims"], None];
  Print["[layoutWrappedPanels] arrangedPanels dimensions:", Dimensions[arrangedPanels]];
  
  stripLabels = facetResult["stripLabels"];
  Print["[layoutWrappedPanels] stripLabels length:", Length[stripLabels]];
  
  (* Use ResourceFunction for grid layout *)
  Print["[layoutWrappedPanels] Creating PlotGrid"];
  
  (* Add global legends if we have any *)
  If[Length[legendInfo] > 0,
    legends = Flatten[{legendInfo}];
    Print["[layoutWrappedPanels] Adding", Length[legends], "global legend(s) to faceted plot"];
    arrangedPanels[[1]] = Legended[arrangedPanels[[1]], legends];
  ];

  finalGrid = ResourceFunction["PlotGrid"][arrangedPanels, PlotLabels -> stripLabels];
  finalGrid
];
End[];

EndPackage[];
