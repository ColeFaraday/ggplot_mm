(* Mathematica Source File *)
(* :Author: colefaraday *)
(* :Date: 2025-08-04 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* Default function if group is not being used as an aesthetic *)
reconcileAesthetics[dataset_, Null, "group"] := Module[{newDataset},
  newDataset = dataset;
  newDataset
];

(* If a string is passed in, use that column as the grouping variable *)
reconcileAesthetics[dataset_, key_?StringQ, "group"] /; keyExistsQAll[dataset, key] := Module[{newDataset},
  newDataset = dataset;
  
  (* Simply use the column values as group identifiers *)
  newDataset = newDataset // Map[Append[#, "group_aes" -> #[key]] &];
  newDataset
];

(* If a function is passed in, apply it row-wise to determine groups *)
reconcileAesthetics[dataset_, func_Function, "group"] := Module[{newDataset},
  newDataset = dataset;
  
  (* Apply the function to each row to determine group membership *)
  newDataset = newDataset // Map[Append[#, "group_aes" -> func[#]] &];
  newDataset
];

(* If a direct value is passed in, use it as the group for all rows *)
reconcileAesthetics[dataset_, value_, "group"] := Module[{newDataset},
  newDataset = dataset;
  
  (* All rows get the same group value *)
  newDataset = newDataset // Map[Append[#, "group_aes" -> value] &];
  newDataset
];

End[];

EndPackage[];
