BeginPackage["ggplot`"];

Begin["`Private`"];

ClearAll[statConvexHull];
Options[statConvexHull] = {
  "data" -> {},
  "x" -> Null,
  "y" -> Null,
  "color" -> Null,
  "alpha" -> Null,
  "fill" -> Null,
  "lineAlpha" -> Null,
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
  
  (* Compute convex hulls for each group *)
  hullData = Association[KeyValueMap[Function[{groupKey, groupData},
    Module[{xvals, yvals, points, hullPoints},
      xvals = extractMappedValues[groupData, OptionValue["x"]];
      yvals = extractMappedValues[groupData, OptionValue["y"]];
      points = Transpose[{xvals, yvals}];
      
      (* Remove non-numeric points *)
      points = Cases[points, {_?NumericQ, _?NumericQ}];
      
      If[Length[points] < 3,
        groupKey -> {}, (* Return empty if insufficient points for hull *)
        hullPoints = ConvexHullRegion[points][[1]];
        
        (* Create data points for the hull vertices *)
        groupKey -> Map[Function[point,
          Module[{baseData, aestheticKeys, aestheticData},
            (* Base data with coordinates *)
            baseData = Association[
              OptionValue["x"] -> point[[1]], 
              OptionValue["y"] -> point[[2]]
            ];
            
            (* Extract all aesthetic keys from the group *)
            aestheticKeys = Select[Keys[First[groupData]], StringEndsQ[#, "_aes"] &];
            
            (* Create aesthetic data with appropriate defaults *)
            aestheticData = Association[Table[
              key -> Switch[key,
                "color_aes", Lookup[First[groupData], key, Black],
                "alpha_aes", Lookup[First[groupData], key, Opacity[1]],
                "thickness_aes", Lookup[First[groupData], key, Automatic],
                "fill_aes", Lookup[First[groupData], key, Lookup[First[groupData], "color_aes", Black]],
                "lineAlpha_aes", Lookup[First[groupData], key, Opacity[1]],
                "size_aes", Lookup[First[groupData], key, 1],
                "shape_aes", Lookup[First[groupData], key, "\[FilledCircle]"],
                "group_aes", Lookup[First[groupData], key, Null],
                _, Lookup[First[groupData], key, Missing["NotAvailable"]]
              ],
              {key, aestheticKeys}
            ]];
            
            (* Merge base data with aesthetics *)
            Join[baseData, aestheticData]
          ]
        ], hullPoints]
      ]
    ]
  ], groupedData]];
  
  (* Return grouped results *)
  hullData
];

End[];

EndPackage[];
