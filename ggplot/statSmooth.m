BeginPackage["ggplot`"];

Begin["`Private`"];

ClearAll[statSmooth];
Options[statSmooth] = {
  "data" -> {},
  "x" -> Null,
  "y" -> Null,
  "color" -> Null,
  "alpha" -> Null,
  "thickness" -> Null,
  "group" -> Null,
  "method" -> "lm",
  "se" -> False,
  "level" -> 0.95,
  "n" -> 80
};

statSmooth[opts : OptionsPattern[]] := Module[{
  dataset, processedData, groupedData, smoothedData
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

  (* Group data by aesthetics for smoothing *)
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
  
  (* Compute smoothed lines for each group *)
  smoothedData = KeyValueMap[Function[{groupKey, groupData},
    Module[{xvals, yvals, xyPairs, sortedPairs, smoothedPoints, fitFunc},
      xvals = extractMappedValues[groupData, OptionValue["x"]];
      yvals = extractMappedValues[groupData, OptionValue["y"]];
      xyPairs = Transpose[{xvals, yvals}];
      
      (* Remove non-numeric pairs *)
      xyPairs = Cases[xyPairs, {_?NumericQ, _?NumericQ}];
      
      If[Length[xyPairs] < 2,
        {}, (* Return empty if insufficient data *)
        sortedPairs = SortBy[xyPairs, First];
        
        (* Create smooth fit based on method *)
        Switch[OptionValue["method"],
          "lm",
          fitFunc = LinearModelFit[sortedPairs, x, x];
          smoothedPoints = Table[{xval, fitFunc[xval]}, {xval, sortedPairs[[All, 1]]}],
          
          "loess", (* Placeholder - could implement LOESS *)
          fitFunc = LinearModelFit[sortedPairs, x, x];
          smoothedPoints = Table[{xval, fitFunc[xval]}, {xval, sortedPairs[[All, 1]]}],
          
          _, (* Default to linear *)
          fitFunc = LinearModelFit[sortedPairs, x, x];
          smoothedPoints = Table[{xval, fitFunc[xval]}, {xval, sortedPairs[[All, 1]]}]
        ];
        
        (* Create data points for the smoothed line *)
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
        ], smoothedPoints]
      ]
    ]
  ], groupedData];
  
  (* Return grouped results *)
  smoothedData
];

End[];

EndPackage[];
