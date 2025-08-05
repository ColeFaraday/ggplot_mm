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

  Print[groupedRequests];
  
  (* Create legend items for each group *)
  legendItems = KeyValueMap[Function[{title, requests},
    Module[{request, hasLine, hasPoint, lineRequest, pointRequest},
      (* Check if we have multiple types for the same variable *)
      hasLine = AnyTrue[requests, #["type"] === "line" &];
      hasPoint = AnyTrue[requests, #["type"] === "point" &];
      
      Which[
        (* Point + Line legend (for same variable) - render as points for now *)
        hasLine && hasPoint,
        Module[{combinedRequest},
          (* Use the point request as the base, but indicate it's a combined legend *)
          pointRequest = First[Select[requests, #["type"] === "point" &]];
          createLegend[pointRequest["values"], pointRequest["labels"], pointRequest["title"], pointRequest["aesthetic"], "point"]
        ],
        
        (* Single geom legend *)
        Length[requests] == 1 && First[requests]["isDiscrete"],
        Module[{req}, 
          req = First[requests];
          createLegend[req["values"], req["labels"], req["title"], req["aesthetic"], req["type"]]
        ],
        
        (* Fallback *)
        True,
        Module[{req}, 
          req = First[requests];
          SwatchLegend[req["values"], req["labels"], LegendLabel -> req["title"]]
        ]
      ]
    ]
  ], groupedRequests];
  
  legendItems
];

(* Unified legend creation function *)
createLegend[values_, labels_, title_, aesthetic_, type_] := Module[{
  lineMarker = Graphics[{Line[{{-2, 0}, {2, 0}}]}],
  markers, options
},

  (* Determine markers based on type and aesthetic *)
  markers = Which[
    (* Line legends always use line markers *)
    type === "line",
    ConstantArray[lineMarker, Length[labels]],
    
    (* Point legends with shape aesthetic use the actual shapes *)
    type === "point" && aesthetic === "shape",
    values,
    
    (* All other point legends use default markers *)
    True,
    Automatic
  ];

  Print[markers];
  
  (* Build options list *)
  options = {LegendLabel -> title};
  If[markers =!= Automatic, AppendTo[options, LegendMarkers -> markers]];
  
  (* Create the PointLegend - let it handle colors, alpha, sizes, etc. naturally *)
  PointLegend[values, labels, Sequence @@ options]
];

End[];

EndPackage[];
