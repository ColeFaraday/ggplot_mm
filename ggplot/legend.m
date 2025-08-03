(* Mathematica Source File *)
(* :Author: colefaraday *)
(* :Date: 2025-08-02 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* Legend creation functions *)

(* Extract legend information from aesthetic mappings *)
extractLegendInfo[heldArgs_, dataset_, options_] := Module[{legendInfo, colorLegend, shapeLegend, sizeLegend, alphaLegend, groupedLegends},
  Print["[DEBUG] Extracting legend info..."];
  legendInfo = <||>;
  
  (* Extract individual legend info for each aesthetic *)
  colorLegend = extractColorLegendInfo[heldArgs, dataset, options];
  Print["[DEBUG] Color legend result: ", colorLegend];
  If[colorLegend =!= None, legendInfo["color"] = colorLegend];
  
  shapeLegend = extractShapeLegendInfo[heldArgs, dataset, options];
  Print["[DEBUG] Shape legend result: ", shapeLegend];
  If[shapeLegend =!= None, legendInfo["shape"] = shapeLegend];
  
  sizeLegend = extractSizeLegendInfo[heldArgs, dataset, options];
  Print["[DEBUG] Size legend result: ", sizeLegend];
  If[sizeLegend =!= None, legendInfo["size"] = sizeLegend];
  
  alphaLegend = extractAlphaLegendInfo[heldArgs, dataset, options];
  Print["[DEBUG] Alpha legend result: ", alphaLegend];
  If[alphaLegend =!= None, legendInfo["alpha"] = alphaLegend];
  
  
  (* Group legends by their mapped variable (title) to combine multiple aesthetics *)
  groupedLegends = combineLegendsForSameVariable[legendInfo];
  Print["[DEBUG] Final legend info: ", groupedLegends];
  
  groupedLegends
];

(* Combine legends that map to the same variable into single combined legends *)
combineLegendsForSameVariable[legendInfo_] := Module[{groupedByVariable, combinedLegends},
  Print["[DEBUG] Combining legends for same variable, input: ", legendInfo];
  
  (* Group legend entries by their mapped variable (title) *)
  groupedByVariable = GroupBy[Normal[legendInfo], #[[2]]["title"] &];
  Print["[DEBUG] Grouped by variable: ", groupedByVariable];

	
  (* For each variable, combine all aesthetics that map to it *)
  combinedLegends = Association[KeyValueMap[Function[{variable, aestheticsForVariable},
    Print["[DEBUG] Processing variable: ", variable, " with aesthetics: ", aestheticsForVariable];
    (* If only one aesthetic maps to this variable, keep it as is *)
    If[Length[aestheticsForVariable] == 1,
      Print["[DEBUG] Single aesthetic for ", variable, ", keeping as is"];
      First[aestheticsForVariable],
      (* Otherwise, combine multiple aesthetics into one legend *)
      Print["[DEBUG] Multiple aesthetics for ", variable, ", combining..."];
      variable -> combineLegendAesthetics[aestheticsForVariable]
    ]
  ], groupedByVariable]];

  Print["[DEBUG] Final combined legends: ", combinedLegends];
  
  combinedLegends
];

(* Combine multiple aesthetics for the same variable into a single legend entry *)
combineLegendAesthetics[aestheticsForVariable_] := Module[{
  firstEntry, combinedEntry, aesthetics, labels, isAllDiscrete, isAllContinuous
  },
  Print["[DEBUG] combineLegendAesthetics called with: ", aestheticsForVariable];
  
  (* Get the first entry as the base *)
  firstEntry = First[aestheticsForVariable][[2]];
  Print["[DEBUG] First entry: ", firstEntry];
	
  (* Extract all aesthetic names *)
  aesthetics = #[[1]] & /@ aestheticsForVariable;
  Print["[DEBUG] Aesthetic names: ", aesthetics];

	
  (* Check if all entries are the same type *)
  isAllDiscrete = AllTrue[#[[2]]["type"] & /@ aestheticsForVariable, # === "discrete" &];
  isAllContinuous = AllTrue[#[[2]]["type"] & /@ aestheticsForVariable, # === "continuous" &];
  Print["[DEBUG] All discrete? ", isAllDiscrete, ", All continuous? ", isAllContinuous];
  
  (* Only combine if they're all the same type and have the same labels *)
  If[isAllDiscrete && SameQ @@ (#[[2]]["labels"] & /@ aestheticsForVariable),
    Print["[DEBUG] Creating combined discrete legend"];
    (* Create combined discrete legend *)
    combinedEntry = <|
      "type" -> "discrete",
      "title" -> firstEntry["title"],
      "labels" -> firstEntry["labels"],
      "aesthetics" -> aesthetics,
      "values" -> Association[#[[1]] -> #[[2]]["values"] & /@ aestheticsForVariable]
    |>;
    Print["[DEBUG] Combined entry values: ", combinedEntry["values"]],
    (* For now, if types don't match or labels differ, keep the first one *)
    Print["[DEBUG] Types don't match or labels differ, keeping first entry"];
    combinedEntry = firstEntry
  ];

  Print["[DEBUG] Final combined entry: ", combinedEntry];
  
  combinedEntry
];

(* Create a title from a function by converting it to string *)
createFunctionTitle[func_] := Module[{funcString},
  funcString = ToString[func];
  (* Clean up the string representation *)
  funcString = StringReplace[funcString, {
    "Function[" -> "",
    "]" -> "",
    "Slot[1]" -> "#",
    "Slot[" ~~ n:NumberString ~~ "]" :> "#" <> n,
    "#1" -> "#"
  }];
  (* Limit length to keep legend readable *)
  If[StringLength[funcString] > 40,
    StringTake[funcString, 37] <> "...",
    funcString
  ]
];

(* Extract color legend information using reconcileAesthetics for consistency *)
extractColorLegendInfo[heldArgs_, dataset_, options_] := Module[{colorMappings, colorFunctionMappings, colorMapping, reconciledDataset, uniqueValues, legendTitle, isDiscrete, isContinuous},
  
  (* Look for color mappings - both string keys and functions *)
  colorMappings = Cases[heldArgs, ("color" -> key_?StringQ) :> key, {0, Infinity}];
  colorFunctionMappings = Cases[heldArgs, ("color" -> func_Function) :> func, {0, Infinity}];
  
  If[Length[colorMappings] == 0 && Length[colorFunctionMappings] == 0,
    Return[None]
  ];
  
  (* Use the first mapping found (priority to string mappings) *)
  colorMapping = If[Length[colorMappings] > 0, 
    First[colorMappings], 
    First[colorFunctionMappings]
  ];

	Print["[DEBUG] Color mapping found: ", colorMapping];
  
  (* Use reconcileAesthetics to get the same colors the plot will use *)
  reconciledDataset = reconcileAesthetics[dataset, colorMapping, "color"];
  
  Print["[DEBUG] Color reconciled dataset first 10 rows: ", Take[reconciledDataset, UpTo[10]]];
  Print["[DEBUG] All color_aes values: ", reconciledDataset[[All, "color_aes"]]];
  
  (* Extract unique color values and their corresponding data values *)
  uniqueValues = DeleteDuplicates[reconciledDataset, #1["color_aes"] === #2["color_aes"] &];
  Print["[DEBUG] Unique values count: ", Length[uniqueValues]];
  Print["[DEBUG] Unique values sample: ", Take[uniqueValues, UpTo[10]]];
  
  (* Determine legend title *)
  legendTitle = If[StringQ[colorMapping], 
    colorMapping, 
    createFunctionTitle[colorMapping]
  ];
  
  (* Check if this is discrete or continuous mapping *)
  (* Use the same logic as reconcileAesthetics to determine if the original data is discrete *)
  Module[{originalData},
    originalData = If[StringQ[colorMapping],
      dataset[[All, colorMapping]],
      colorMapping /@ dataset
    ];
    isDiscrete = isDiscreteDataQ[originalData];
    isContinuous = !isDiscrete;
  ];
  
  If[isDiscrete,
    (* Create discrete legend with actual colors from reconcileAesthetics *)
    Module[{labels, colors},
      Print["[DEBUG] Creating discrete color legend"];
      (* For string mappings, use the original data values as labels *)
      labels = If[StringQ[colorMapping],
        Sort[DeleteDuplicates[reconciledDataset[[All, colorMapping]]]],
        (* For function mappings, use the function results as labels *)
        Sort[DeleteDuplicates[colorMapping /@ dataset]]
      ];
      Print["[DEBUG] Color legend labels: ", labels];
      Print["[DEBUG] Length of labels: ", Length[labels]];
      
      (* Get the corresponding colors by finding the first occurrence of each label *)
      colors = labels /. Association[
        If[StringQ[colorMapping],
          (#[colorMapping] -> #["color_aes"]) & /@ uniqueValues,
          (colorMapping[#] -> #["color_aes"]) & /@ uniqueValues
        ]
      ];
      Print["[DEBUG] Color mapping association: ", Association[
        If[StringQ[colorMapping],
          (#[colorMapping] -> #["color_aes"]) & /@ uniqueValues,
          (colorMapping[#] -> #["color_aes"]) & /@ uniqueValues
        ]
      ]];
      Print["[DEBUG] Unique values for color mapping: ", uniqueValues];
      Print["[DEBUG] Color legend colors: ", colors];
      Print["[DEBUG] Length of colors: ", Length[colors]];
      Print["[DEBUG] Color mapping type check - each color: ", Head /@ colors];
      
      (* Safety check: if any colors are not graphics directives, replace with Black *)
      colors = colors /. x_ /; !MatchQ[x, _RGBColor | _GrayLevel | _Hue | _XYZColor | _LABColor | _LCHColor | _LUVColor | Black | White | Red | Green | Blue | Yellow | Magenta | Cyan | Orange | Pink | Purple | Brown | Gray | LightGray | DarkGray] :> (Print["[DEBUG] Found non-color value: ", x, " replacing with Black"]; Black);
      
      <|"type" -> "discrete", "title" -> legendTitle, "labels" -> labels, "values" -> colors, "aesthetic" -> "color"|>
    ],
    (* For continuous mapping, extract range and palette information *)
    Module[{dataRange, colorPalette},
      Print["[DEBUG] Creating continuous color legend data"];
      dataRange = If[StringQ[colorMapping],
        MinMax[reconciledDataset[[All, colorMapping]]],
        MinMax[colorMapping /@ dataset]
      ];
      (* Get the color palette used for this continuous mapping *)
      colorPalette = getContinuousColorPalette[If[StringQ[colorMapping],
        reconciledDataset[[All, colorMapping]],
        colorMapping /@ dataset
      ]];
      Print["[DEBUG] Continuous color range: ", dataRange];
      Print["[DEBUG] Continuous color palette: ", colorPalette];
      
      <|"type" -> "continuous", "title" -> legendTitle, "range" -> dataRange, "palette" -> colorPalette, "aesthetic" -> "color"|>
    ]
  ]
];

(* Extract shape legend information using reconcileAesthetics for consistency *)
extractShapeLegendInfo[heldArgs_, dataset_, options_] := Module[{shapeMappings, shapeFunctionMappings, shapeMapping, reconciledDataset, uniqueValues, legendTitle, labels, shapes},
  shapeMappings = Cases[heldArgs, ("shape" -> key_?StringQ) :> key, {0, Infinity}];
  shapeFunctionMappings = Cases[heldArgs, ("shape" -> func_Function) :> func, {0, Infinity}];
  
  If[Length[shapeMappings] == 0 && Length[shapeFunctionMappings] == 0, Return[None]];
  
  (* Use the first mapping found (priority to string mappings) *)
  shapeMapping = If[Length[shapeMappings] > 0, 
    First[shapeMappings], 
    First[shapeFunctionMappings]
  ];
  
  (* Use reconcileAesthetics to get the same shapes the plot will use *)
  reconciledDataset = reconcileAesthetics[dataset, shapeMapping, "shape"];
  
  (* Extract unique shape values *)
  uniqueValues = DeleteDuplicates[reconciledDataset, #1["shape_aes"] === #2["shape_aes"] &];
  
  (* Determine legend title *)
  legendTitle = If[StringQ[shapeMapping], 
    shapeMapping, 
    createFunctionTitle[shapeMapping]
  ];
  
  (* Shapes are always discrete *)
  labels = If[StringQ[shapeMapping],
    Sort[DeleteDuplicates[reconciledDataset[[All, shapeMapping]]]],
    Sort[DeleteDuplicates[shapeMapping /@ dataset]]
  ];
  
  (* Get the corresponding shapes *)
  shapes = labels /. Association[
    If[StringQ[shapeMapping],
      (#[shapeMapping] -> #["shape_aes"]) & /@ uniqueValues,
      (shapeMapping[#] -> #["shape_aes"]) & /@ uniqueValues
    ]
  ];
  
  <|"type" -> "discrete", "title" -> legendTitle, "labels" -> labels, "values" -> shapes, "aesthetic" -> "shape"|>
];

(* Extract size legend information using reconcileAesthetics for consistency *)
extractSizeLegendInfo[heldArgs_, dataset_, options_] := Module[{sizeMappings, sizeFunctionMappings, sizeMapping, reconciledDataset, uniqueValues, legendTitle, labels, sizes, isDiscrete, isContinuous},
  sizeMappings = Cases[heldArgs, ("size" -> key_?StringQ) :> key, {0, Infinity}];
  sizeFunctionMappings = Cases[heldArgs, ("size" -> func_Function) :> func, {0, Infinity}];
  
  If[Length[sizeMappings] == 0 && Length[sizeFunctionMappings] == 0, Return[None]];
  
  (* Use the first mapping found (priority to string mappings) *)
  sizeMapping = If[Length[sizeMappings] > 0, 
    First[sizeMappings], 
    First[sizeFunctionMappings]
  ];
  
  (* Use reconcileAesthetics to get the same sizes the plot will use *)
  reconciledDataset = reconcileAesthetics[dataset, sizeMapping, "size"];
  
  (* Extract unique size values *)
  uniqueValues = DeleteDuplicates[reconciledDataset, #1["size_aes"] === #2["size_aes"] &];
  
  (* Determine legend title *)
  legendTitle = If[StringQ[sizeMapping], 
    sizeMapping, 
    createFunctionTitle[sizeMapping]
  ];
  
  (* Check if this is discrete or continuous mapping *)
  isDiscrete = Length[uniqueValues] <= 10; (* Assume discrete if <= 10 unique sizes *)
  
  If[isDiscrete,
    (* Create discrete legend *)
    labels = If[StringQ[sizeMapping],
      Sort[DeleteDuplicates[reconciledDataset[[All, sizeMapping]]]],
      Sort[DeleteDuplicates[sizeMapping /@ dataset]]
    ];
    
    sizes = labels /. Association[
      If[StringQ[sizeMapping],
        (#[sizeMapping] -> #["size_aes"]) & /@ uniqueValues,
        (sizeMapping[#] -> #["size_aes"]) & /@ uniqueValues
      ]
    ];
    
    <|"type" -> "discrete", "title" -> legendTitle, "labels" -> labels, "values" -> sizes, "aesthetic" -> "size"|>,
    
    (* Create continuous legend *)
    Module[{dataRange, sizeRange},
      dataRange = If[StringQ[sizeMapping],
        MinMax[reconciledDataset[[All, sizeMapping]]],
        MinMax[sizeMapping /@ dataset]
      ];
      sizeRange = MinMax[reconciledDataset[[All, "size_aes"]]];
      
      <|"type" -> "continuous", "title" -> legendTitle, "range" -> dataRange, "sizeRange" -> sizeRange, "aesthetic" -> "size"|>
    ]
  ]
];


(* Extract alpha legend information using reconcileAesthetics for consistency *)
extractAlphaLegendInfo[heldArgs_, dataset_, options_] := Module[{alphaMappings, alphaFunctionMappings, alphaMapping, reconciledDataset, uniqueValues, legendTitle, labels, alphas, isDiscrete},
  alphaMappings = Cases[heldArgs, ("alpha" -> key_?StringQ) :> key, {0, Infinity}];
  alphaFunctionMappings = Cases[heldArgs, ("alpha" -> func_Function) :> func, {0, Infinity}];
  
  If[Length[alphaMappings] == 0 && Length[alphaFunctionMappings] == 0, Return[None]];
  
  (* Use the first mapping found (priority to string mappings) *)
  alphaMapping = If[Length[alphaMappings] > 0, 
    First[alphaMappings], 
    First[alphaFunctionMappings]
  ];
  
  (* Use reconcileAesthetics to get the same alphas the plot will use *)
  reconciledDataset = reconcileAesthetics[dataset, alphaMapping, "alpha"];
  
  (* Extract unique alpha values *)
  uniqueValues = DeleteDuplicates[reconciledDataset, #1["alpha_aes"] === #2["alpha_aes"] &];
  
  (* Determine legend title *)
  legendTitle = If[StringQ[alphaMapping], 
    alphaMapping, 
    createFunctionTitle[alphaMapping]
  ];
  
  (* Check if this is discrete or continuous mapping *)
  isDiscrete = Length[uniqueValues] <= 10; (* Assume discrete if <= 10 unique alphas *)
  
  If[isDiscrete,
    (* Create discrete legend *)
    labels = If[StringQ[alphaMapping],
      Sort[DeleteDuplicates[reconciledDataset[[All, alphaMapping]]]],
      Sort[DeleteDuplicates[alphaMapping /@ dataset]]
    ];
    
    alphas = labels /. Association[
      If[StringQ[alphaMapping],
        (#[alphaMapping] -> #["alpha_aes"]) & /@ uniqueValues,
        (alphaMapping[#] -> #["alpha_aes"]) & /@ uniqueValues
      ]
    ];
    
    <|"type" -> "discrete", "title" -> legendTitle, "labels" -> labels, "values" -> alphas, "aesthetic" -> "alpha"|>,
    
    (* Create continuous legend *)
    Module[{dataRange, alphaRange},
      dataRange = If[StringQ[alphaMapping],
        MinMax[reconciledDataset[[All, alphaMapping]]],
        MinMax[alphaMapping /@ dataset]
      ];
      alphaRange = MinMax[reconciledDataset[[All, "alpha_aes"]]];
      
      <|"type" -> "continuous", "title" -> legendTitle, "range" -> dataRange, "alphaRange" -> alphaRange, "aesthetic" -> "alpha"|>
    ]
  ]
];
End[];

EndPackage[];
