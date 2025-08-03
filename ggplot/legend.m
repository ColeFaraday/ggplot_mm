(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-05-10 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* Legend creation functions *)

(* Extract legend information from aesthetic mappings *)
extractLegendInfo[heldArgs_, dataset_, options_] := Module[{legendInfo, colorLegend, shapeLegend, sizeLegend, alphaLegend},
  Print["[DEBUG] Extracting legend info..."];
  Print["[DEBUG] Dataset length: ", Length[dataset]];
  Print["[DEBUG] HeldArgs: ", heldArgs];
  
  legendInfo = <||>;
  
  (* Extract color legend info *)
  colorLegend = extractColorLegendInfo[heldArgs, dataset, options];
  Print["[DEBUG] Color legend result: ", colorLegend];
  If[colorLegend =!= None, legendInfo["color"] = colorLegend];
  
  (* Extract shape legend info *)
  shapeLegend = extractShapeLegendInfo[heldArgs, dataset, options];
  Print["[DEBUG] Shape legend result: ", shapeLegend];
  If[shapeLegend =!= None, legendInfo["shape"] = shapeLegend];
  
  (* Extract size legend info *)
  sizeLegend = extractSizeLegendInfo[heldArgs, dataset, options];
  Print["[DEBUG] Size legend result: ", sizeLegend];
  If[sizeLegend =!= None, legendInfo["size"] = sizeLegend];
  
  (* Extract alpha legend info *)
  alphaLegend = extractAlphaLegendInfo[heldArgs, dataset, options];
  Print["[DEBUG] Alpha legend result: ", alphaLegend];
  If[alphaLegend =!= None, legendInfo["alpha"] = alphaLegend];
  
  Print["[DEBUG] Final legend info: ", legendInfo];
  legendInfo
];

(* Extract color legend information *)
extractColorLegendInfo[heldArgs_, dataset_, options_] := Module[{colorMappings, colorFunctionMappings, colorKey, data, discreteDataQ, keys, colors, categoricalColors, groupedDataset, functionValues},
  Print["[DEBUG] Extracting color legend info..."];
  
  (* Look for color mappings in geom functions *)
  colorMappings = Cases[heldArgs, ("color" -> key_?StringQ) :> key, {0, Infinity}];
  Print["[DEBUG] String-based color mappings found: ", colorMappings];
  
  (* Also look for function-based color mappings *)
  colorFunctionMappings = Cases[heldArgs, ("color" -> func_Function) :> func, {0, Infinity}];
  Print["[DEBUG] Function-based color mappings found: ", colorFunctionMappings];
  
  (* Check for color mappings in the main ggplot call - look at all color assignments *)
  allColorMappings = Cases[heldArgs, ("color" -> value_) :> value, {0, Infinity}];
  Print["[DEBUG] All color mappings: ", allColorMappings];
  
  If[Length[colorMappings] == 0 && Length[colorFunctionMappings] == 0,
    Print["[DEBUG] No color mappings found, returning None"];
    Return[None]
  ];
  
  (* Handle string-based mappings *)
  If[Length[colorMappings] > 0,
    colorKey = First[colorMappings];
    Print["[DEBUG] Using string-based color key: ", colorKey];
    If[!keyExistsQAll[dataset, colorKey], 
      Print["[DEBUG] Color key doesn't exist in all dataset entries"];
      Return[None]
    ];
    
    data = dataset[[All, colorKey]];
    Print["[DEBUG] Color data: ", Take[data, Min[10, Length[data]]]];
    discreteDataQ = isDiscreteDataQ[data];
    Print["[DEBUG] Is discrete data: ", discreteDataQ];
    
    Return[If[discreteDataQ,
      keys = Sort[getDiscreteKeys[data]];
      categoricalColors = Lookup[options, "categoricalColors", OptionValue[ggplot, "categoricalColors"]];
      If[categoricalColors === Automatic,
        colors = ggplotColorsFunc[Length[keys]],
        colors = Take[Flatten[ConstantArray[categoricalColors, Ceiling[Length[keys]/Length[categoricalColors]]]], Length[keys]]
      ];
      <|"type" -> "discrete", "title" -> colorKey, "labels" -> keys, "values" -> colors, "aesthetic" -> "color"|>,
      (* For continuous color, create a color bar *)
      Print["[DEBUG] Creating continuous color legend data"];
      <|"type" -> "continuous", "title" -> colorKey, "range" -> MinMax[data], "palette" -> getContinuousColorPalette[data], "aesthetic" -> "color"|>
    ]]
  ];
  
  (* Handle function-based mappings *)
  If[Length[colorFunctionMappings] > 0,
    Print["[DEBUG] Processing function-based color mapping"];
    Module[{func, groupedData, functionKeys, functionColors},
      func = First[colorFunctionMappings];
      Print["[DEBUG] Color function: ", func];
      
      (* Apply the function to dataset to get the values it produces *)
      functionValues = func /@ dataset;
      Print["[DEBUG] Function values: ", Take[functionValues, Min[10, Length[functionValues]]]];
      
      (* Group by function result to see what categories we get *)
      groupedData = GroupBy[dataset, func] // KeySort;
      functionKeys = Keys[groupedData];
      Print["[DEBUG] Function produces these unique values: ", functionKeys];
      
      (* Get colors for these categories *)
      categoricalColors = Lookup[options, "categoricalColors", OptionValue[ggplot, "categoricalColors"]];
      If[categoricalColors === Automatic,
        functionColors = ggplotColorsFunc[Length[functionKeys]],
        functionColors = Take[Flatten[ConstantArray[categoricalColors, Ceiling[Length[functionKeys]/Length[categoricalColors]]]], Length[functionKeys]]
      ];
      
      (* Check if function produces continuous or discrete values *)
      discreteDataQ = isDiscreteDataQ[functionValues];
      Print["[DEBUG] Function produces discrete data: ", discreteDataQ];
      
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

(* Create legend graphics *)
createLegendGraphics[legendInfo_, options_] := Module[{legendPosition, legendSpacing, legendItems},
  Print["[DEBUG] Creating legend graphics for: ", Keys[legendInfo]];
  If[Length[legendInfo] == 0, 
    Print["[DEBUG] No legend info, returning empty list"];
    Return[{}]
  ];

	Print["[DEBUG] Legend info: ", legendInfo];
	
  legendPosition = Lookup[options, "legendPosition", "right"];
  legendSpacing = Lookup[options, "legendSpacing", 0.15];
  
  legendItems = KeyValueMap[createSingleLegend, legendInfo];
  Print["[DEBUG] Created ", Length[legendItems], " legend items"];
  Print["[DEBUG] Legend items structure: ", Map[Head, legendItems, {2}]];
	Print["[DEBUG] Legend items: ", legendItems];
	
  (* Position legends vertically and flatten *)
  Module[{positioned},
    positioned = MapIndexed[Function[{legend, index}, 
      Translate[Flatten[legend], {0, -(First[index] - 1) * legendSpacing}]
    ], legendItems];
    Print["[DEBUG] Positioned legend graphics: ", Length[positioned], " items"];
    Flatten[positioned]
  ]
];

(* Create a single legend for one aesthetic *)
createSingleLegend[aesthetic_, legendData_] := Module[{title, legendType, legendGraphics},
  Print["[DEBUG] Creating single legend for aesthetic: ", aesthetic];
  Print["[DEBUG] Legend data: ", legendData];
  
  title = legendData["title"];
  legendType = legendData["type"];
  
  legendGraphics = Which[
    legendType === "discrete" && legendData["aesthetic"] === "color",
    createDiscreteColorLegend[title, legendData["labels"], legendData["values"]],
    
    legendType === "discrete" && legendData["aesthetic"] === "shape",
    createDiscreteShapeLegend[title, legendData["labels"], legendData["values"]],
    
    legendType === "discrete" && legendData["aesthetic"] === "size",
    createDiscreteSizeLegend[title, legendData["labels"], legendData["values"]],
    
    legendType === "discrete" && legendData["aesthetic"] === "alpha",
    createDiscreteAlphaLegend[title, legendData["labels"], legendData["values"]],
    
    legendType === "continuous" && legendData["aesthetic"] === "color",
    createContinuousColorLegend[title, legendData["range"], legendData["palette"]],
    
    legendType === "continuous" && legendData["aesthetic"] === "size",
    createContinuousSizeLegend[title, legendData["range"], legendData["sizeRange"]],
    
    legendType === "continuous" && legendData["aesthetic"] === "alpha",
    createContinuousAlphaLegend[title, legendData["range"], legendData["alphaRange"]],
    
    True,
    {} (* Default empty legend *)
  ];
  
  Print["[DEBUG] Created legend graphics: ", Head[legendGraphics]];
  legendGraphics
];

(* Discrete color legend *)
createDiscreteColorLegend[title_, labels_, colors_] := Module[{legendItems, titleText},
  titleText = Text[Style[title, Bold, 12], {0, 0.1}];
  legendItems = MapIndexed[Function[{label, index},
    {
      colors[[First[index]]],
      Rectangle[{-0.05, -First[index] * 0.04}, {-0.02, -First[index] * 0.04 + 0.02}],
      Text[Style[ToString[label], 10], {0, -First[index] * 0.04 + 0.01}]
    }
  ], labels];
  
  {titleText, legendItems}
];

(* Discrete shape legend *)
createDiscreteShapeLegend[title_, labels_, shapes_] := Module[{legendItems, titleText},
  titleText = Text[Style[title, Bold, 12], {0, 0.1}];
  legendItems = MapIndexed[Function[{label, index},
    {
      Black,
      Inset[Style[shapes[[First[index]]], 12], {-0.035, -First[index] * 0.04 + 0.01}],
      Text[Style[ToString[label], 10], {0, -First[index] * 0.04 + 0.01}]
    }
  ], labels];
  
  {titleText, legendItems}
];

(* Discrete size legend *)
createDiscreteSizeLegend[title_, labels_, sizes_] := Module[{legendItems, titleText},
  titleText = Text[Style[title, Bold, 12], {0, 0.1}];
  legendItems = MapIndexed[Function[{label, index},
    {
      Black,
      Inset[Style["\[FilledCircle]", sizes[[First[index]]]], {-0.035, -First[index] * 0.04 + 0.01}],
      Text[Style[ToString[label], 10], {0, -First[index] * 0.04 + 0.01}]
    }
  ], labels];
  
  {titleText, legendItems}
];

(* Discrete alpha legend *)
createDiscreteAlphaLegend[title_, labels_, alphas_] := Module[{legendItems, titleText},
  titleText = Text[Style[title, Bold, 12], {0, 0.1}];
  legendItems = MapIndexed[Function[{label, index},
    {
      Directive[Black, Opacity[alphas[[First[index]]]]],
      Rectangle[{-0.05, -First[index] * 0.04}, {-0.02, -First[index] * 0.04 + 0.02}],
      Text[Style[ToString[label], 10], {0, -First[index] * 0.04 + 0.01}]
    }
  ], labels];
  
  {titleText, legendItems}
];

(* Continuous color legend (color bar) *)
createContinuousColorLegend[title_, {min_, max_}, palette_] := Module[{titleText, colorBar, tickLabels, nSteps, colorSteps, rectangles},
  Print["[DEBUG] Creating continuous color legend for range: ", {min, max}];
  titleText = Text[Style[title, Bold, 12], {0, 0.1}];
  
  (* Create a simple color bar using rectangles instead of DensityPlot *)
  nSteps = 20; (* Number of color steps in the bar *)
  colorSteps = Table[Blend[palette, i/(nSteps-1)], {i, 0, nSteps-1}];
  rectangles = Table[
    {colorSteps[[i]], 
     Rectangle[{-0.05, min + (i-1)*(max-min)/nSteps}, {-0.02, min + i*(max-min)/nSteps}]},
    {i, 1, nSteps}
  ];
  
  (* Add border around color bar *)
  border = {Black, Thickness[0.001], 
    Line[{{-0.05, min}, {-0.05, max}, {-0.02, max}, {-0.02, min}, {-0.05, min}}]};
  
  tickLabels = {
    Text[Style[ToString[NumberForm[min, 3]], 9], {0.01, min}],
    Text[Style[ToString[NumberForm[max, 3]], 9], {0.01, max}]
  };

  Print["[DEBUG] Created continuous color legend with ", Length[rectangles], " rectangles"];
  {titleText, rectangles, border, tickLabels}
];

(* Continuous size legend *)
createContinuousSizeLegend[title_, {min_, max_}, {minSize_, maxSize_}] := Module[{titleText, sizeItems},
  titleText = Text[Style[title, Bold, 12], {0, 0.1}];
  sizeItems = {
    Inset[Style["\[FilledCircle]", minSize], {-0.035, -0.02}],
    Text[Style[ToString[NumberForm[min, 3]], 9], {0, -0.02}],
    Inset[Style["\[FilledCircle]", maxSize], {-0.035, -0.06}],
    Text[Style[ToString[NumberForm[max, 3]], 9], {0, -0.06}]
  };
  
  {titleText, sizeItems}
];

(* Continuous alpha legend *)
createContinuousAlphaLegend[title_, {min_, max_}, {minAlpha_, maxAlpha_}] := Module[{titleText, alphaItems},
  titleText = Text[Style[title, Bold, 12], {0, 0.1}];
  alphaItems = {
    Directive[Black, Opacity[minAlpha]],
    Rectangle[{-0.05, -0.04}, {-0.02, -0.02}],
    Text[Style[ToString[NumberForm[min, 3]], 9], {0, -0.03}],
    Directive[Black, Opacity[maxAlpha]],
    Rectangle[{-0.05, -0.08}, {-0.02, -0.06}],
    Text[Style[ToString[NumberForm[max, 3]], 9], {0, -0.07}]
  };
  
  {titleText, alphaItems}
];

End[];

EndPackage[];
