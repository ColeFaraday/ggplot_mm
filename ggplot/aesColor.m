(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* Default function if color is not being used as an aesthetic *)
reconcileAesthetics[dataset_, Null, "color"] := Module[{newDataset},
  newDataset = dataset;
  (* Only add color_aes if it doesn't already exist (for faceting compatibility) *)
  If[!KeyExistsQ[First[newDataset], "color_aes"],
    newDataset = newDataset // Map[Append[#, "color_aes" -> Black] &]
  ];
  newDataset
];

(* If color is given as an actual color, then assume that's the color the user wants everything to be *)
reconcileAesthetics[dataset_, color_?ColorQ, "color"] := Module[{newDataset},
  newDataset = dataset;
  newDataset = newDataset // Map[Append[#, "color_aes" -> color] &];
  newDataset
];

(* If a string is passed in, then assume that's the key in the dataset on how to color the data. Then must determine whether the data is discrete or not *)
reconcileAesthetics[dataset_, key_?StringQ, "color"] /; keyExistsQAll[dataset, key] := Module[{newDataset, data, colorFunc, discreteDataQ, keys, minMax, categoricalColors, continuousColors},
  newDataset = dataset;
  
  (* If color_aes already exists, don't override it (for faceting compatibility) *)
  If[KeyExistsQ[First[newDataset], "color_aes"],
    Return[newDataset]
  ];
  
  data = newDataset[[All, key]];
  discreteDataQ = isDiscreteDataQ[data];
  If[discreteDataQ,
    keys = Sort[getDiscreteKeys[data]];
    (* Get categorical colors from theme *)
    categoricalColors = OptionValue[ggplot, "categoricalColors"];
    If[categoricalColors === Automatic,
      categoricalColors = ggplotColorsFunc[Length[keys]],
      (* If user provided specific colors, use them or cycle through them *)
      categoricalColors = Take[Flatten[ConstantArray[categoricalColors, Ceiling[Length[keys]/Length[categoricalColors]]]], Length[keys]]
    ];
    colorFunc = Function[AssociationThread[keys, categoricalColors][#]];
  ];
  If[!discreteDataQ,
    minMax = getContinuousRange[data];
    (* Get appropriate continuous color palette from theme *)
    continuousColors = getContinuousColorPalette[data];
    colorFunc = With[{minMax = minMax, colors = continuousColors}, Function[Blend[colors, Rescale[#, minMax]]]];
  ];
  newDataset = newDataset // Map[Append[#, "color_aes" -> colorFunc[#[key]]] &];
  newDataset
];

(* If a function is passed in, then use it to determine how to color the data assuming the function will be applied "row-wise" to the dataset, as an example "color" -> Function[#somegroup < 10] *)
reconcileAesthetics[dataset_, func_Function, "color"] := Module[{newDataset, data, discreteDataQ, groupedDataset, colors, categoricalColors, colorFunc, minMax, continuousColors},
  newDataset = dataset;
  
  (* Extract the data that results from applying the function *)
  data = Map[func, dataset];
  discreteDataQ = isDiscreteDataQ[data];
  
  If[discreteDataQ,
    (* Handle as discrete/categorical data *)
    groupedDataset = GroupBy[dataset, func] // KeySort;
    (* Get categorical colors from theme *)
    categoricalColors = OptionValue[ggplot, "categoricalColors"];
    If[categoricalColors === Automatic,
      colors = ggplotColorsFunc[Length[groupedDataset]],
      (* If user provided specific colors, use them or cycle through them *)
      colors = Take[Flatten[ConstantArray[categoricalColors, Ceiling[Length[groupedDataset]/Length[categoricalColors]]]], Length[groupedDataset]]
    ];
    newDataset = groupedDataset // Values // MapIndexed[Function[{group, index}, Map[Function[row, Append[row, "color_aes" -> colors[[First@index]]]], group]]] // Flatten;
  ,
    (* Handle as continuous data *)
    minMax = getContinuousRange[data];
    (* Get appropriate continuous color palette from theme *)
    continuousColors = getContinuousColorPalette[data];
    colorFunc = With[{minMax = minMax, colors = continuousColors}, Function[Blend[colors, Rescale[#, minMax]]]];
    newDataset = newDataset // Map[Append[#, "color_aes" -> colorFunc[func[#]]] &];
  ];
  
  newDataset
];

reconcileAesthetics[dataset_, _, "color"] := Throw[Echo["Unclear on how to determine the color"];Null];

(* Helper functions for colors *)
ggplotColorsFunc[1] := {Black};
ggplotColorsFunc[numberOfSeries_?IntegerQ] /; numberOfSeries > 1 := Drop[LCHColor[0.65, 0.6, #] & /@ (Subdivide[30, 390, numberOfSeries]/390), -1];
ggplotColorsFunc[___] := ggplotColorsFunc[1];

(* Helper function to determine if data should use diverging color palette *)
shouldUseDivergingPalette[data_List] := Module[{minVal, maxVal, zeroInRange},
  {minVal, maxVal} = MinMax[data];
  zeroInRange = minVal < 0 < maxVal;
  zeroInRange
];

(* Helper function to get appropriate continuous color palette from theme *)
getContinuousColorPalette[data_List] := Module[{paletteType, sequentialColors, divergingColors},
  paletteType = OptionValue[ggplot, "continuousColorPalette"];
  sequentialColors = OptionValue[ggplot, "sequentialColors"];
  divergingColors = OptionValue[ggplot, "divergingColors"];
  
  Which[
    paletteType === "diverging", divergingColors,
    paletteType === "sequential", sequentialColors,
    paletteType === "auto" && shouldUseDivergingPalette[data], divergingColors,
    True, sequentialColors
  ]
];

End[];

EndPackage[];
