(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* Helper functions for aesthetics *)

isDiscreteDataQ[data_] := Module[{uniqueData},
  uniqueData = DeleteDuplicates[data];
  Which[
    (* String and Boolean data are always discrete *)
    MatchQ[uniqueData, {_?StringQ ..} | {_?BooleanQ ..}], True,
    
    (* For numeric data, consider it discrete if:
       1. There are 12 or fewer unique values, OR
       2. All values are integers and there are 20 or fewer unique values *)
    MatchQ[uniqueData, {_?NumericQ ..}],
    Length[uniqueData] <= 12 || (AllTrue[uniqueData, IntegerQ] && Length[uniqueData] <= 20),
    
    (* Mixed types or other cases: default to discrete *)
    True, True
  ]
];
getAllKeys[data_]           := data // Keys /* Flatten /* DeleteDuplicates;
getDiscreteKeys[data_]      := Sort[DeleteDuplicates[data]];
getContinuousRange[data_]   := MinMax[data];
keyExistsQAll[data_, key_]  := data // Map[KeyExistsQ[key]] // MatchQ[{True ..}];

End[];

EndPackage[];
