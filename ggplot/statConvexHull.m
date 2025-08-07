BeginPackage["ggplot`"];

Begin["`Private`"];

ClearAll[statConvexHull];
Options[statConvexHull] = {
  "data" -> {},
  "x" -> Null,
  "y" -> Null,
  "color" -> Null,
  "alpha" -> Null,
  "thickness" -> Null,
  "group" -> Null
};

statConvexHull[opts : OptionsPattern[]] := Module[{
  dataset, processedData, groupedData, hullData
},
  dataset = OptionValue["data"];
  
  (* Ensure X/Y has been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, 
    Message[ggplot::xOrYNotGiven]; 
    Throw[Null]
  ];
  
  processedData = dataset;
  
  (* Switch dates to absolute times *)
  processedData = Replace[processedData, d_?DateObjectQ :> AbsoluteTime[d], Infinity];

  (* Group data by aesthetics for hull computation *)
  groupedData = If[KeyExistsQ[First[processedData, <||>], "group_aes"],
    (* Group only by the group aesthetic *)
    GroupBy[processedData, Function[row, row["group_aes"]]],
    (* Group by all aesthetic values *)
    GroupBy[processedData, 
      Function[row,
        {Lookup[row, "color_aes", Black], Lookup[row, "alpha_aes", Opacity[1]], 
         Lookup[row, "thickness_aes", Automatic]}
      ]
    ]
  ];

	Print[groupedData];
  
  (* Compute convex hulls for each group *)
  hullData = KeyValueMap[Function[{groupKey, groupData},
    Module[{xvals, yvals, points, hullPoints, hullResult},
      xvals = extractMappedValues[groupData, OptionValue["x"]];
      yvals = extractMappedValues[groupData, OptionValue["y"]];
      points = Transpose[{xvals, yvals}];
      
      (* Remove non-numeric points *)
      points = Cases[points, {_?NumericQ, _?NumericQ}];
      
      If[Length[points] < 3,
        {}, (* Return empty if insufficient points for hull *)
        hullPoints = ConvexHullRegion[points][[1]];
        
        (* Create data points for the hull vertices *)
        MapIndexed[Function[{point, index},
          Association[
            OptionValue["x"] -> point[[1]],
            OptionValue["y"] -> point[[2]], 
            "index" -> index[[1]],
            (* Preserve aesthetics from the group *)
            "color_aes" -> Lookup[First[groupData], "color_aes", Black],
            "alpha_aes" -> Lookup[First[groupData], "alpha_aes", Opacity[1]],
            "thickness_aes" -> Lookup[First[groupData], "thickness_aes", Automatic]
          ]
        ], hullPoints]
      ]
    ]
  ], groupedData];
  
  (* Return grouped results *)
  hullData
];

End[];

EndPackage[];
