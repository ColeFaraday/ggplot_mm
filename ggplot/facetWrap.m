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
  xScaleFunc, yScaleFunc
  },
  
  variable = facetInfo["variable"];
  ncol = facetInfo["ncol"];
  nrow = facetInfo["nrow"];
  scales = facetInfo["scales"];
  stripPosition = facetInfo["stripPosition"];
  
  Print["Debug - Faceting by variable: ", variable];
  Print["Debug - Available keys in dataset: ", Keys[First[dataset]]];
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
  
  (* Create individual panels *)
  panels = Map[Function[value,
    Module[{subsetData, points, lines, paths, smooths, histograms, primitives, modifiedOptions},
      Print["Debug - Creating panel for value: ", value];
      
      (* Filter data for this facet *)
      subsetData = Select[dataset, #[variable] === value &];
      Print["Debug - Subset data rows: ", Length[subsetData]];
      Print["Debug - First few rows of subset: ", Take[subsetData, UpTo[2]]];
      
      (* Create geoms with subset data - properly override the data parameter *)
      Print["Debug - Processing geoms from heldArgs: ", heldArgs];
      
      (* Create modified options with subset data *)
      modifiedOptions = Normal@Association[options, {"data" -> subsetData}];

	 		Print["Debug - options: ", options];
			Print["Debug - Modified options: ", modifiedOptions];

			Print["Debug - options = modified options : ", options === modifiedOptions];
			
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
  Print["Debug - Number of unique values: ", Length[uniqueValues]];
  Print["Debug - Unique values: ", uniqueValues];
  Print["Debug - Number of panels created: ", Length[panels]];
  Print["Debug - Panel types: ", Map[Head, panels]];

	Print["Debug - panels: ", panels];
  
  (* Create strip labels *)
  stripLabels = Map[Function[value, Style[ToString[value], 12] ], uniqueValues];
  
  Print["Debug - Number of strip labels created: ", Length[stripLabels]];
  Print["Debug - Strip label types: ", Map[Head, stripLabels]];
  
  (* Debug: Check dimensions before transpose *)
  Print["Debug - panels length: ", Length[panels]];
  Print["Debug - stripLabels length: ", Length[stripLabels]];
  
  (* Arrange panels in grid with labels *)
  (* Fix the transpose issue by using MapThread instead *)
  arrangedPanels = MapThread[Function[{panel, label},
    Labeled[panel, label, Top]
  ], {panels, stripLabels}];
  
  Print["Debug - Number of arranged panels: ", Length[arrangedPanels]];
  
  (* Pad to fill grid if needed *)
  While[Length[arrangedPanels] < gridDims[[1]] * gridDims[[2]],
    AppendTo[arrangedPanels, Graphics[{}, ImageSize -> 150]]
  ];
  
  (* Reshape into grid *)
  arrangedPanels = ArrayReshape[arrangedPanels, gridDims];
  
  (* Create final grid *)
  Grid[arrangedPanels, Spacings -> {1, 1}]
];

End[];

EndPackage[];
