BeginPackage["ggplot`"];

Begin["`Private`"];

ClearAll[statDensity2D];
Options[statDensity2D] = {
  "data" -> {},
  "x" -> Null,
  "y" -> Null,
  "color" -> Null,
  "alpha" -> Null,
  "group" -> Null,
  "levels" -> 10,
  "bandwidth" -> Automatic
};

statDensity2D[opts : OptionsPattern[]] := Module[{
  dataset, processedData, groupedData, densityData
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

  (* Group data by aesthetics for density computation *)
  groupedData = If[KeyExistsQ[First[processedData, <||>], "group_aes"],
    (* Group only by the group aesthetic *)
    GroupBy[processedData, Function[row, row["group_aes"]]],
    (* Group by all aesthetic values *)
    GroupBy[processedData, 
      Function[row,
        {Lookup[row, "color_aes", Black], Lookup[row, "alpha_aes", Opacity[1]]}
      ]
    ]
  ];
  
  (* Compute 2D density contours for each group *)
  densityData = KeyValueMap[Function[{groupKey, groupData},
    Module[{xvals, yvals, points, densityContours, contourResults},
      xvals = extractMappedValues[groupData, OptionValue["x"]];
      yvals = extractMappedValues[groupData, OptionValue["y"]];
      points = Transpose[{xvals, yvals}];
      
      (* Remove non-numeric points *)
      points = Cases[points, {_?NumericQ, _?NumericQ}];
      
      If[Length[points] < 4,
        {}, (* Return empty if insufficient points *)
        (* Create simple density contours using HistogramList *)
        Module[{xbins, ybins, densityMatrix, contourLevels, contours},
          {xbins, ybins, densityMatrix} = HistogramList[points, {20, 20}];
          contourLevels = Range[0.1, 1, 0.1] * Max[densityMatrix];
          
          (* Generate contour polygons - simplified placeholder *)
          contours = {};
          Do[
            AppendTo[contours, 
              Association[
                "x" -> Mean[xbins[[i ;; i+1]]],
                "y" -> Mean[ybins[[j ;; j+1]]], 
                "density" -> densityMatrix[[i, j]],
                "level" -> Floor[densityMatrix[[i, j]] / Max[densityMatrix] * OptionValue["levels"]],
                (* Preserve aesthetics from the group *)
                "color_aes" -> Lookup[First[groupData], "color_aes", Black],
                "alpha_aes" -> Lookup[First[groupData], "alpha_aes", Opacity[1]]
              ]
            ],
            {i, Length[xbins] - 1}, {j, Length[ybins] - 1}
          ];
          contours
        ]
      ]
    ]
  ], groupedData];
  
  (* Return grouped results *)
  densityData
];

End[];

EndPackage[];
