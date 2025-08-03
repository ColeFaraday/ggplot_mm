(* Mathematica Source File *)
(* :Author: colefaraday *)
(* :Date: 2025-08-03 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* facetWrap implementation *)

Options[facetWrap] = {"variable" -> Null, "ncol" -> Automatic, "nrow" -> Automatic, "scales" -> "fixed", "stripPosition" -> "top"};

facetWrap[opts : OptionsPattern[]] := Module[{variable, ncol, nrow, scales, stripPosition},
  variable = OptionValue["variable"];
  ncol = OptionValue["ncol"];
  nrow = OptionValue["nrow"];
  scales = OptionValue["scales"];
  stripPosition = OptionValue["stripPosition"];
  
  (* Return an association containing faceting information to be processed by ggplot *)
  <|
    "type" -> "wrap",
    "variable" -> variable,
    "ncol" -> ncol,
    "nrow" -> nrow,
    "scales" -> scales,
    "stripPosition" -> stripPosition
  |>
];

(* Function to create faceted graphics *)
createFacetedGraphics[dataset_, facetInfo_, heldArgs_, options_] := Module[{
  variable, ncol, nrow, scales, stripPosition, uniqueValues, nPanels, 
  gridDims, panels, arrangedPanels, stripLabels, xScaleType, yScaleType, 
  xScaleFunc, yScaleFunc, legendInfo, showLegend, finalGrid, globalDataset
  },
  
  variable = facetInfo["variable"];
  ncol = facetInfo["ncol"];
  nrow = facetInfo["nrow"];
  scales = facetInfo["scales"];
  stripPosition = facetInfo["stripPosition"];
  
  Print["Debug - Faceting by variable: ", variable];
  Print["Debug - X variable: ", Lookup[options, "x", "NOT FOUND"]];
  Print["Debug - Y variable: ", Lookup[options, "y", "NOT FOUND"]];
  
  (* Get unique values for faceting variable *)
  uniqueValues = Sort[DeleteDuplicates[dataset[[All, variable]]]];
  nPanels = Length[uniqueValues];
  
  (* Determine grid dimensions *)
  gridDims = Which[
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
  ];
  
  (* Get scaling information - simplified for now *)
  xScaleType = "Linear"; 
  yScaleType = "Linear";
  xScaleFunc = Function[Identity[#]];
  yScaleFunc = Function[Identity[#]];
  
  (* PRE-COMPUTE GLOBAL AESTHETIC MAPPINGS for consistency across panels *)
  Print["Debug - Pre-computing global aesthetic mappings..."];
  
  (* Extract aesthetic mappings from heldArgs and create globally reconciled dataset *)
  globalDataset = Module[{workingDataset, colorMapping, shapeMapping, sizeMapping, alphaMapping},
    workingDataset = dataset;
    
    (* Find aesthetic mappings *)
    colorMapping = Cases[heldArgs, ("color" -> key_) :> key, {0, Infinity}];
    shapeMapping = Cases[heldArgs, ("shape" -> key_) :> key, {0, Infinity}];
    sizeMapping = Cases[heldArgs, ("size" -> key_) :> key, {0, Infinity}];
    alphaMapping = Cases[heldArgs, ("alpha" -> key_) :> key, {0, Infinity}];
    
    Print["Debug - Found mappings - color: ", colorMapping, ", shape: ", shapeMapping];
    
    (* Apply reconcileAesthetics to the complete dataset for each aesthetic *)
    If[Length[colorMapping] > 0,
      Print["Debug - Reconciling color for complete dataset"];
      workingDataset = reconcileAesthetics[workingDataset, First[colorMapping], "color"];
    ];
    
    If[Length[shapeMapping] > 0,
      Print["Debug - Reconciling shape for complete dataset"];
      workingDataset = reconcileAesthetics[workingDataset, First[shapeMapping], "shape"];
    ];
    
    If[Length[sizeMapping] > 0,
      workingDataset = reconcileAesthetics[workingDataset, First[sizeMapping], "size"];
    ];
    
    If[Length[alphaMapping] > 0,
      workingDataset = reconcileAesthetics[workingDataset, First[alphaMapping], "alpha"];
    ];
    
    Print["Debug - Available keys after reconciliation: ", Keys[First[workingDataset]]];
    
    (* Return the reconciled dataset *)
    workingDataset
  ];
  
  (* Create individual panels *)
  panels = Map[Function[value,
    Module[{subsetData, points, lines, paths, smooths, histograms, primitives, modifiedOptions},
      Print["Debug - Creating panel for value: ", value];
      
      (* Filter data for this facet using the globally reconciled dataset *)
      subsetData = Select[globalDataset, #[variable] === value &];
      Print["Debug - Subset data rows: ", Length[subsetData]];
      
      (* Create geoms with subset data - properly override the data parameter *)
      
      (* Create modified options with subset data *)
      modifiedOptions = Normal@Association[options, {"data" -> subsetData}];
			
			(* Extract geoms from heldArgs and apply modified options *)
      points = Cases[heldArgs, geomPoint[opts___] :> geomPoint[opts, FilterRules[modifiedOptions, Options[geomPoint]], "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
      lines = Cases[heldArgs, geomLine[opts___] :> geomLine[opts, FilterRules[modifiedOptions, Options[geomLine]], "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
      paths = Cases[heldArgs, geomPath[opts___] :> geomPath[opts, FilterRules[modifiedOptions, Options[geomPath]], "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
      smooths = Cases[heldArgs, geomSmooth[opts___] :> geomSmooth[opts, FilterRules[modifiedOptions, Options[geomSmooth]], "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
      histograms = Cases[heldArgs, geomHistogram[opts___] :> geomHistogram[opts, FilterRules[modifiedOptions, Options[geomHistogram]], "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
      
      (* Combine all primitives *)
      primitives = {points, lines, paths, smooths, histograms} // Flatten;
      
      (* Create the individual plot *)
      Graphics[primitives,
        Frame -> True,
        FrameLabel -> {None, None},
        PlotRange -> Lookup[options, PlotRange, All],
        AspectRatio -> Lookup[options, AspectRatio, 7/10],
        ImageSize -> 150, (* Smaller individual panels *)
        Background -> Lookup[options, Background, White],
        FrameStyle -> Lookup[options, FrameStyle, Automatic],
        GridLines -> None
      ]
    ]
  ], uniqueValues];
  
  (* Debug: Print information *)
  Print["Debug - ", Length[uniqueValues], " facet panels created"];
  Print["Debug - Unique values: ", uniqueValues];
  
  (* Create strip labels *)
  stripLabels = Map[Function[value, Style[ToString[value], 12] ], uniqueValues];
  
  (* Arrange panels in grid with labels *)
  arrangedPanels = MapThread[Function[{panel, label},
    Labeled[panel, label, Top]
  ], {panels, stripLabels}];
  
  (* Pad to fill grid if needed *)
  While[Length[arrangedPanels] < gridDims[[1]] * gridDims[[2]],
    AppendTo[arrangedPanels, Graphics[{}, ImageSize -> 150]]
  ];
  
  (* Reshape into grid *)
  arrangedPanels = ArrayReshape[arrangedPanels, gridDims];
  
  (* Create the initial grid *)
  finalGrid = Grid[arrangedPanels, Spacings -> {1, 1}];
  
  (* Create legend if needed *)
  showLegend = Lookup[options, "showLegend", True]; (* Default to True for faceted plots *)
  legendInfo = {};
  If[showLegend === Automatic || showLegend === True,
    Print["Debug - Extracting legend info from global dataset"];
    legendInfo = extractLegendInfo[heldArgs, globalDataset, options];
    Print["Debug - Legend info: ", legendInfo];
  ];
  
  (* Apply legend to the final grid if needed *)
  If[Length[legendInfo] > 0,
    Legended[
      finalGrid,
      Placed[
        Row[Join[convertLegendInfo[legendInfo], {Spacer[5]}]],
        GetScaledCoord[Lookup[options, "legendPosition", "OuterMiddleRight"]]
      ]
    ],
    (* No legend case *)
    finalGrid
  ]
];

End[];

EndPackage[];
