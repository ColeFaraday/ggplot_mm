(* Mathematica Source File *)
(* :Author: colefaraday *)
(* :Date: 2025-08-08 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* Default function if lineAlpha is not being used as an aesthetic *)
reconcileAesthetics[dataset_, Null, "lineAlpha"] := Module[{newDataset},
  newDataset = dataset;
  (* Default: full opacity for lines *)
  newDataset = newDataset // Map[Append[#, "lineAlpha_aes" -> Opacity[1]] &];
  newDataset
];

(* Handle Missing[KeyAbsent, "lineAlpha"] case - treat same as Null *)
reconcileAesthetics[dataset_, _Missing, "lineAlpha"] := reconcileAesthetics[dataset, Null, "lineAlpha"];

(* If lineAlpha is given as a number between 0 and 1 *)
reconcileAesthetics[dataset_, alpha_?NumericQ, "lineAlpha"] /; 0 <= alpha <= 1 := Module[{newDataset},
  newDataset = dataset;
  newDataset = newDataset // Map[Append[#, "lineAlpha_aes" -> Opacity[alpha]] &];
  newDataset
];

(* If lineAlpha is given as an Opacity directive *)
reconcileAesthetics[dataset_, opacity_Opacity, "lineAlpha"] := Module[{newDataset},
  newDataset = dataset;
  newDataset = newDataset // Map[Append[#, "lineAlpha_aes" -> opacity] &];
  newDataset
];

(* If a string is passed in, then assume that's the key in the dataset for mapping lineAlpha *)
reconcileAesthetics[dataset_, key_?StringQ, "lineAlpha"] /; keyExistsQAll[dataset, key] := Module[{newDataset, data, alphaFunc, discreteDataQ, keys, minMax},
  newDataset = dataset;

  data = newDataset[[All, key]];
  discreteDataQ = isDiscreteDataQ[data];
  
  If[discreteDataQ,
    keys = Sort[getDiscreteKeys[data]];
    (* For discrete data, create distinct alpha levels *)
    alphaFunc = Function[
      Opacity[0.3 + 0.6 * (Position[keys, #][[1, 1]] - 1)/(Length[keys] - 1)]
    ],
    
    (* For continuous data, map to alpha range *)
    minMax = getContinuousRange[data];
    alphaFunc = Function[Opacity[0.3 + 0.6 * Rescale[#, minMax]]]
  ];
  
  newDataset = newDataset // Map[Append[#, "lineAlpha_aes" -> alphaFunc[#[key]]] &];
  newDataset
];

(* If lineAlpha is a function, apply to data *)
reconcileAesthetics[dataset_, func_Function, "lineAlpha"] := Module[{newDataset},
  newDataset = dataset;
  newDataset = newDataset // Map[Append[#, "lineAlpha_aes" -> func[#]] &];
  newDataset
];

(* Generic missing key warning - only for strings that don't exist as keys *)
reconcileAesthetics[dataset_, key_?StringQ, "lineAlpha"] /; !keyExistsQAll[dataset, key] := Module[{newDataset},
  Message[ggplot::keyNotFound, "lineAlpha", key]; 
  newDataset = reconcileAesthetics[dataset, Null, "lineAlpha"];
  newDataset
];

(* Missing error fallback *)
reconcileAesthetics[dataset_, key_, "lineAlpha"] := Module[{newDataset},
  Message[ggplot::aestheticFormatError, "lineAlpha", key]; 
  newDataset = reconcileAesthetics[dataset, Null, "lineAlpha"];
  newDataset
];

End[];

EndPackage[];
