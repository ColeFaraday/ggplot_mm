(* Mathematica Source File *)
(* :Author: colefaraday *)
(* :Date: 2025-08-08 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* Default function if fill is not being used as an aesthetic *)
reconcileAesthetics[dataset_, Null, "fill"] := Module[{newDataset},
  newDataset = dataset;
  newDataset
];

(* Handle Missing[KeyAbsent, "fill"] case - treat same as Null *)
reconcileAesthetics[dataset_, _Missing, "fill"] := reconcileAesthetics[dataset, Null, "fill"];

(* If fill is given as an actual color, then assume that's the fill color the user wants everything to be *)
reconcileAesthetics[dataset_, color_?ColorQ, "fill"] := Module[{newDataset},
  Print["Setting fill to color: ", color];
  newDataset = dataset;
  newDataset = newDataset // Map[Append[#, "fill_aes" -> color] &];
  newDataset
];

(* If a string is passed in, then assume that's the key in the dataset on how to fill the data. Then must determine whether the data is discrete or not *)
reconcileAesthetics[dataset_, key_?StringQ, "fill"] /; keyExistsQAll[dataset, key] := Module[{newDataset, data, fillFunc, discreteDataQ, keys, minMax, categoricalColors, continuousColors},
  newDataset = dataset;

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
    fillFunc = Function[AssociationThread[keys, categoricalColors][#]];
  ];
  If[!discreteDataQ,
    minMax = getContinuousRange[data];
    (* Get appropriate continuous color palette from theme *)
    continuousColors = getContinuousColorPalette[data];
    fillFunc = With[{minMax = minMax, colors = continuousColors}, Function[Blend[colors, Rescale[#, minMax]]]];
  ];
  newDataset = newDataset // Map[Append[#, "fill_aes" -> fillFunc[#[key]]] &];
  newDataset
];

(* If fill is a function, apply to data *)
reconcileAesthetics[dataset_, func_Function, "fill"] := Module[{newDataset},
  newDataset = dataset;
  newDataset = newDataset // Map[Append[#, "fill_aes" -> func[#]] &];
  newDataset
];

(* Generic missing key warning - only for strings that don't exist as keys *)
reconcileAesthetics[dataset_, key_?StringQ, "fill"] /; !keyExistsQAll[dataset, key] := Module[{newDataset},
  Message[ggplot::keyNotFound, "fill", key]; 
  newDataset = reconcileAesthetics[dataset, Null, "fill"];
  newDataset
];

(* Missing error fallback *)
reconcileAesthetics[dataset_, key_, "fill"] := Module[{newDataset},
  Message[ggplot::aestheticFormatError, "fill", key]; 
  newDataset = reconcileAesthetics[dataset, Null, "fill"];
  newDataset
];

End[];

EndPackage[];
