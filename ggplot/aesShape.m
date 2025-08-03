(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* Default function if shape is not being used as an aesthetic *)
reconcileAesthetics[dataset_, Null, "shape"] := Module[{newDataset},
  newDataset = dataset;
  (* Only add shape_aes if it doesn't already exist (for faceting compatibility) *)
  If[!KeyExistsQ[First[newDataset], "shape_aes"],
    newDataset = newDataset // Map[Append[#, "shape_aes" -> "\[FilledCircle]"] &]
  ];
  newDataset
];

(* If shape is given as an actual shape, then assume that's the shape the user wants everything to be *)
reconcileAesthetics[dataset_, shape_Symbol, "shape"] := Module[{newDataset},
  newDataset = dataset;
  newDataset = newDataset // Map[Append[#, "shape_aes" -> shape] &];
  newDataset
];

(* If a string is passed in, then assume that's the key in the dataset on how to shape the data. Then must determine whether the data is discrete or not *)
reconcileAesthetics[dataset_, key_?StringQ, "shape"] /; keyExistsQAll[dataset, key] := Module[{newDataset, data, shapeFunc, discreteDataQ, keys, minMax, categoricalShapes},
  newDataset = dataset;
  
  (* If shape_aes already exists, don't override it (for faceting compatibility) *)
  If[KeyExistsQ[First[newDataset], "shape_aes"],
    Return[newDataset]
  ];
  
  data = newDataset[[All, key]];
  discreteDataQ = isDiscreteDataQ[data];
  If[discreteDataQ,
    keys = Sort[getDiscreteKeys[data]];
    categoricalShapes = OptionValue[ggplot, "categoricalShapes"];
    (* Repeat shapes if we need more than available *)
    categoricalShapes = Take[Flatten[ConstantArray[categoricalShapes, Ceiling[Length[keys]/Length[categoricalShapes]]]], Length[keys]];
    shapeFunc = Function[AssociationThread[keys, categoricalShapes][#]];
  ];
  If[!discreteDataQ,
    minMax = getContinuousRange[data];
    shapeFunc = Message[ggplot::shapeContinuous]; Throw[Null];
  ];
  newDataset = newDataset // Map[Append[#, "shape_aes" -> shapeFunc[#[key]]] &];
  newDataset
];

(* If a function is passed in, then use it to determine how to shape the data assuming the function will be applied "row-wise" to the dataset, as an example "shape" -> Function[#somegroup < 10] *)
reconcileAesthetics[dataset_, func_Function, "shape"] := Module[{newDataset, groupedDataset, shapes, categoricalShapes},
  newDataset = dataset;
  groupedDataset = GroupBy[dataset, func] // KeySort;
  categoricalShapes = OptionValue[ggplot, "categoricalShapes"];
  (* Repeat shapes if we need more than available *)
  shapes = Take[Flatten[ConstantArray[categoricalShapes, Ceiling[Length[groupedDataset]/Length[categoricalShapes]]]], Length[groupedDataset]];
  newDataset = groupedDataset // Values // MapIndexed[Function[{group, index}, Map[Function[row, Append[row, "shape_aes" -> shapes[[First@index]]]], group]]] // Flatten;
  newDataset
];

reconcileAesthetics[dataset_, _, "shape"] := Throw[Echo["Unclear on how to determine the shape"];Null];

End[];

EndPackage[];
