(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-05-10 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* Legend creation functions *)

(* Extract legend information from aesthetic mappings *)
extractLegendInfo[heldArgs_, dataset_, options_] := Module[{legendInfo, colorLegend, shapeLegend, sizeLegend, alphaLegend, groupedLegends},
  legendInfo = <||>;
  
  (* Extract individual legend info for each aesthetic *)
  colorLegend = extractColorLegendInfo[heldArgs, dataset, options];
  If[colorLegend =!= None, legendInfo["color"] = colorLegend];
  
  shapeLegend = extractShapeLegendInfo[heldArgs, dataset, options];
  If[shapeLegend =!= None, legendInfo["shape"] = shapeLegend];
  
  sizeLegend = extractSizeLegendInfo[heldArgs, dataset, options];
  If[sizeLegend =!= None, legendInfo["size"] = sizeLegend];
  
  alphaLegend = extractAlphaLegendInfo[heldArgs, dataset, options];
  If[alphaLegend =!= None, legendInfo["alpha"] = alphaLegend];
  
  
  (* Group legends by their mapped variable (title) to combine multiple aesthetics *)
  groupedLegends = combineLegendsForSameVariable[legendInfo];
  
  groupedLegends
];

(* Combine legends that map to the same variable into single combined legends *)
combineLegendsForSameVariable[legendInfo_] := Module[{groupedByVariable, combinedLegends},
  (* Group legend entries by their mapped variable (title) *)
  groupedByVariable = GroupBy[Normal[legendInfo], #[[2]]["title"] &];

	
  (* For each variable, combine all aesthetics that map to it *)
  combinedLegends = Association[KeyValueMap[Function[{variable, aestheticsForVariable},
    (* If only one aesthetic maps to this variable, keep it as is *)
    If[Length[aestheticsForVariable] == 1,
      First[aestheticsForVariable],
      (* Otherwise, combine multiple aesthetics into one legend *)
      variable -> combineLegendAesthetics[aestheticsForVariable]
    ]
  ], groupedByVariable]];

  
  combinedLegends
];

(* Combine multiple aesthetics for the same variable into a single legend entry *)
combineLegendAesthetics[aestheticsForVariable_] := Module[{
  firstEntry, combinedEntry, aesthetics, labels, isAllDiscrete, isAllContinuous
  },
  (* Get the first entry as the base *)
  firstEntry = First[aestheticsForVariable][[2]];
	
  (* Extract all aesthetic names *)
  aesthetics = #[[1]] & /@ aestheticsForVariable;

	
  (* Check if all entries are the same type *)
  isAllDiscrete = AllTrue[#[[2]]["type"] & /@ aestheticsForVariable, # === "discrete" &];
  isAllContinuous = AllTrue[#[[2]]["type"] & /@ aestheticsForVariable, # === "continuous" &];
  
  (* Only combine if they're all the same type and have the same labels *)
  If[isAllDiscrete && SameQ @@ (#[[2]]["labels"] & /@ aestheticsForVariable),
    (* Create combined discrete legend *)
    combinedEntry = <|
      "type" -> "discrete",
      "title" -> firstEntry["title"],
      "labels" -> firstEntry["labels"],
      "aesthetics" -> aesthetics,
      "values" -> Association[#[[1]] -> #[[2]]["values"] & /@ aestheticsForVariable]
    |>,
    (* For now, if types don't match or labels differ, keep the first one *)
    combinedEntry = firstEntry
  ];

  
  combinedEntry
];

(* Extract color legend information *)
extractColorLegendInfo[heldArgs_, dataset_, options_] := Module[{colorMappings, colorFunctionMappings, colorKey, data, discreteDataQ, keys, colors, categoricalColors, groupedDataset, functionValues},
  
  (* Look for color mappings in geom functions *)
  colorMappings = Cases[heldArgs, ("color" -> key_?StringQ) :> key, {0, Infinity}];
  
  (* Also look for function-based color mappings *)
  colorFunctionMappings = Cases[heldArgs, ("color" -> func_Function) :> func, {0, Infinity}];
  
  (* Check for color mappings in the main ggplot call - look at all color assignments *)
  allColorMappings = Cases[heldArgs, ("color" -> value_) :> value, {0, Infinity}];
  
  If[Length[colorMappings] == 0 && Length[colorFunctionMappings] == 0,
    Return[None]
  ];
  
  (* Handle string-based mappings *)
  If[Length[colorMappings] > 0,
    colorKey = First[colorMappings];
    If[!keyExistsQAll[dataset, colorKey], 
      Return[None]
    ];
    
    data = dataset[[All, colorKey]];
    discreteDataQ = isDiscreteDataQ[data];
    
    Return[If[discreteDataQ,
      keys = Sort[getDiscreteKeys[data]];
      categoricalColors = Lookup[options, "categoricalColors", OptionValue[ggplot, "categoricalColors"]];
      If[categoricalColors === Automatic,
        colors = ggplotColorsFunc[Length[keys]],
        colors = Take[Flatten[ConstantArray[categoricalColors, Ceiling[Length[keys]/Length[categoricalColors]]]], Length[keys]]
      ];
      <|"type" -> "discrete", "title" -> colorKey, "labels" -> keys, "values" -> colors, "aesthetic" -> "color"|>,
      (* For continuous color, create a color bar *)
      <|"type" -> "continuous", "title" -> colorKey, "range" -> MinMax[data], "palette" -> getContinuousColorPalette[data], "aesthetic" -> "color"|>
    ]]
  ];
  
  (* Handle function-based mappings *)
  If[Length[colorFunctionMappings] > 0,
    Module[{func, groupedData, functionKeys, functionColors},
      func = First[colorFunctionMappings];
      
      (* Apply the function to dataset to get the values it produces *)
      functionValues = func /@ dataset;
      
      (* Group by function result to see what categories we get *)
      groupedData = GroupBy[dataset, func] // KeySort;
      functionKeys = Keys[groupedData];
      
      (* Get colors for these categories *)
      categoricalColors = Lookup[options, "categoricalColors", OptionValue[ggplot, "categoricalColors"]];
      If[categoricalColors === Automatic,
        functionColors = ggplotColorsFunc[Length[functionKeys]],
        functionColors = Take[Flatten[ConstantArray[categoricalColors, Ceiling[Length[functionKeys]/Length[categoricalColors]]]], Length[functionKeys]]
      ];
      
      (* Check if function produces continuous or discrete values *)
      discreteDataQ = isDiscreteDataQ[functionValues];
      
      Return[If[discreteDataQ,
        <|"type" -> "discrete", "title" -> "color", "labels" -> functionKeys, "values" -> functionColors, "aesthetic" -> "color"|>,
        (* For continuous function output, treat as continuous color scale *)
        <|"type" -> "continuous", "title" -> "color", "range" -> MinMax[functionValues], "palette" -> getContinuousColorPalette[functionValues], "aesthetic" -> "color"|>
      ]]
    ]
  ];
  
  (* If we get here, no mappings were found *)
  None
];

(* Extract shape legend information *)
extractShapeLegendInfo[heldArgs_, dataset_, options_] := Module[{shapeMappings, shapeKey, data, discreteDataQ, keys, shapes, categoricalShapes},
  shapeMappings = Cases[heldArgs, ("shape" -> key_?StringQ) :> key, {0, Infinity}];
  
  If[Length[shapeMappings] == 0, Return[None]];
  
  shapeKey = First[shapeMappings];
  If[!keyExistsQAll[dataset, shapeKey], Return[None]];
  
  data = dataset[[All, shapeKey]];
  discreteDataQ = isDiscreteDataQ[data];
  
  If[discreteDataQ,
    keys = Sort[getDiscreteKeys[data]];
    categoricalShapes = Lookup[options, "categoricalShapes", OptionValue[ggplot, "categoricalShapes"]];
    shapes = Take[Flatten[ConstantArray[categoricalShapes, Ceiling[Length[keys]/Length[categoricalShapes]]]], Length[keys]];
    <|"type" -> "discrete", "title" -> shapeKey, "labels" -> keys, "values" -> shapes, "aesthetic" -> "shape"|>,
    None (* Shapes are only discrete *)
  ]
];

(* Extract size legend information *)
extractSizeLegendInfo[heldArgs_, dataset_, options_] := Module[{sizeMappings, sizeKey, data, discreteDataQ, keys, sizes},
  sizeMappings = Cases[heldArgs, ("size" -> key_?StringQ) :> key, {0, Infinity}];
  
  If[Length[sizeMappings] == 0, Return[None]];
  
  sizeKey = First[sizeMappings];
  If[!keyExistsQAll[dataset, sizeKey], Return[None]];
  
  data = dataset[[All, sizeKey]];
  discreteDataQ = isDiscreteDataQ[data];
  
  If[discreteDataQ,
    keys = Sort[getDiscreteKeys[data]];
    sizes = Rescale[Range[Length[keys]], {1, Length[keys]}, {8, 16}]; (* Default size range *)
    <|"type" -> "discrete", "title" -> sizeKey, "labels" -> keys, "values" -> sizes, "aesthetic" -> "size"|>,
    <|"type" -> "continuous", "title" -> sizeKey, "range" -> MinMax[data], "sizeRange" -> {8, 16}, "aesthetic" -> "size"|>
  ]
];

(* Extract alpha legend information *)
extractAlphaLegendInfo[heldArgs_, dataset_, options_] := Module[{alphaMappings, alphaKey, data, discreteDataQ, keys, alphas},
  alphaMappings = Cases[heldArgs, ("alpha" -> key_?StringQ) :> key, {0, Infinity}];
  
  If[Length[alphaMappings] == 0, Return[None]];
  
  alphaKey = First[alphaMappings];
  If[!keyExistsQAll[dataset, alphaKey], Return[None]];
  
  data = dataset[[All, alphaKey]];
  discreteDataQ = isDiscreteDataQ[data];
  
  If[discreteDataQ,
    keys = Sort[getDiscreteKeys[data]];
    alphas = Rescale[Range[Length[keys]], {1, Length[keys]}, {0.3, 1.0}]; (* Default alpha range *)
    <|"type" -> "discrete", "title" -> alphaKey, "labels" -> keys, "values" -> alphas, "aesthetic" -> "alpha"|>,
    <|"type" -> "continuous", "title" -> alphaKey, "range" -> MinMax[data], "alphaRange" -> {0.3, 1.0}, "aesthetic" -> "alpha"|>
  ]
];

End[];

EndPackage[];
