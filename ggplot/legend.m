(* Mathematica Source File *)
(* :Author: colefaraday *)
(* :Date: 2025-08-02 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* New Legend System - Geom-based Legend Requests *)

(* Legacy function kept for backward compatibility with other geoms *)
extractLegendInfo[heldArgs_, dataset_, options_] := Module[{},
  (* For now, return empty - other geoms will be migrated to new system later *)
  <||>
];

(* Create legend from geom requests *)
createLegendFromRequests[legendRequests_] := Module[{groupedRequests, legendItems},
  If[Length[legendRequests] == 0, Return[{}]];
  
  (* Group requests by title (variable name) *)
  groupedRequests = GroupBy[legendRequests, #["title"] &];
  
  (* Create legend items for each group *)
  legendItems = KeyValueMap[Function[{title, requests},
    Module[{request},
      (* For now, just use the first request if multiple aesthetics map to same variable *)
      request = First[requests];
      
      Which[
        (* Line legend with discrete color *)
        request["type"] === "line" && request["aesthetic"] === "color" && request["isDiscrete"],
        createLineLegend[request["values"], request["labels"], request["title"]],
        
        (* Fallback *)
        True,
        SwatchLegend[request["values"], request["labels"], LegendLabel -> request["title"]]
      ]
    ]
  ], groupedRequests];
  
  legendItems
];

(* Create a line legend with the specified colors and labels *)
createLineLegend[colors_, labels_, title_] := Module[{},
  LineLegend[colors, labels, LegendLabel -> title]
];

End[];

EndPackage[];
