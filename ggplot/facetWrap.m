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
  
  (* Extract aesthetic mappings from heldArgs and create globally reconciled dataset *)
  globalDataset = Module[{workingDataset, colorMapping, shapeMapping, sizeMapping, alphaMapping},
    workingDataset = dataset;
    
    (* Find aesthetic mappings *)
    colorMapping = Cases[heldArgs, ("color" -> key_) :> key, {0, Infinity}];
    shapeMapping = Cases[heldArgs, ("shape" -> key_) :> key, {0, Infinity}];
    sizeMapping = Cases[heldArgs, ("size" -> key_) :> key, {0, Infinity}];
    alphaMapping = Cases[heldArgs, ("alpha" -> key_) :> key, {0, Infinity}];
    
    (* Apply reconcileAesthetics to the complete dataset for each aesthetic *)
    If[Length[colorMapping] > 0,
      workingDataset = reconcileAesthetics[workingDataset, First[colorMapping], "color"];
    ];
    
    If[Length[shapeMapping] > 0,
      workingDataset = reconcileAesthetics[workingDataset, First[shapeMapping], "shape"];
    ];
    
    If[Length[sizeMapping] > 0,
      workingDataset = reconcileAesthetics[workingDataset, First[sizeMapping], "size"];
    ];
    
    If[Length[alphaMapping] > 0,
      workingDataset = reconcileAesthetics[workingDataset, First[alphaMapping], "alpha"];
    ];
    
    (* Return the reconciled dataset *)
    workingDataset
  ];
  
  (* Create individual panels *)
  panels = Map[Function[value,
    Module[{subsetData, points, lines, paths, smooths, histograms, primitives, modifiedOptions},
      
      (* Filter data for this facet using the globally reconciled dataset *)
      subsetData = Select[globalDataset, #[variable] === value &];
      
      (* Create geoms with subset data - properly override the data parameter *)
      
      (* Create modified options with subset data *)
      modifiedOptions = Normal@Association[options, {"data" -> subsetData}];
			
			(* List of all supported geoms *)
      allGeoms = {geomPoint, geomLine, geomPath, geomSmooth, geomHistogram, geomCol, geomDensity2DFilled, geomErrorBand, geomErrorBar, geomErrorBoxes, geomHLine, geomVLine, geomParityLine};
      
      (* Extract geoms from heldArgs and apply modified options for each geom *)
      geomPrimitives = Flatten[
        Table[
          Cases[heldArgs, g[opts___] :> g[opts, FilterRules[modifiedOptions, Options[g]], "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}],
          {g, allGeoms}
        ]
      ];
      
      (* Combine all primitives *)
      primitives = geomPrimitives;
      
			(* TODO: should also inherit theme values *)
      (* Create the individual plot *)
      Graphics[primitives,
        Frame -> True,
        FrameLabel -> {None, None},
        PlotRange -> Lookup[options, PlotRange, All],
        AspectRatio -> Lookup[options, AspectRatio, 7/10],
        ImageSize -> 150, (* Smaller individual panels *)
        Background -> Lookup[options, Background, None], (* Use global background if not specified *)
        FrameStyle -> Lookup[options, FrameStyle, Automatic],
        GridLines -> None
      ]
    ]
  ], uniqueValues];
  
  (* Create strip labels *)
  stripLabels = Map[Function[value, Style[ToString[value], 12] ], uniqueValues];
  

  (* Reshape into grid *)
  arrangedPanels = ArrayReshape[panels, gridDims];
  
  (* Create the initial grid *)
	pg = ResourceFunction["PlotGrid"];

  (* Create legend if needed *)
  showLegend = Lookup[options, "showLegend", True]; (* Default to True for faceted plots *)
  legendInfo = {};
  If[showLegend === Automatic || showLegend === True,
    legendInfo = extractLegendInfo[heldArgs, globalDataset, options];
  ];

	(* make the first panel have the legend and let PlotGrid decide how to place*)
	arrangedPanels[[1,1]] = 
    Legended[
      arrangedPanels[[1,1]],
      Placed[
        Row[Join[convertLegendInfo[legendInfo], {Spacer[5]}]],
        GetScaledCoord[Lookup[options, "legendPosition", "OuterMiddleRight"]]
      ]
    ];

  finalGrid = pg[arrangedPanels, PlotLabels->stripLabels];

  finalGrid
];

End[];

EndPackage[];
