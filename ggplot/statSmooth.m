BeginPackage["ggplot`"];

Begin["`Private`"];

ClearAll[statSmooth];
Options[statSmooth] = {
  "data" -> {},
  "x" -> Null,
  "y" -> Null,
  "color" -> Null,
  "alpha" -> Null,
	"fill"->Null,
  "thickness" -> Null,
  "group" -> Null,
  "method" -> "lm",
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
  smoothedData = Association[KeyValueMap[Function[{groupKey, groupData},
    Module[{xvals, yvals, xyPairs, sortedPairs, smoothedPoints, fitFunc},
      xvals = extractMappedValues[groupData, OptionValue["x"]];
      yvals = extractMappedValues[groupData, OptionValue["y"]];
      xyPairs = Transpose[{xvals, yvals}];
      
      (* Remove non-numeric pairs *)
      xyPairs = Cases[xyPairs, {_?NumericQ, _?NumericQ}];
      
      If[Length[xyPairs] < 2,
        groupKey -> {}, (* Return empty if insufficient data *)
        sortedPairs = SortBy[xyPairs, First];
        
        (* Create dense sampling of x values for smooth curve *)
        Module[{xmin, xmax, xDense, fitFunc, predictions, confidenceData},
          xmin = Min[sortedPairs[[All, 1]]];
          xmax = Max[sortedPairs[[All, 1]]];
          xDense = Table[x, {x, xmin, xmax, (xmax - xmin)/(OptionValue["n"] - 1)}];
          
          (* Create smooth fit based on method *)
          Switch[OptionValue["method"],
            "lm",
            fitFunc = LinearModelFit[sortedPairs, x, x, ConfidenceLevel -> OptionValue["level"]];
            predictions = fitFunc /@ xDense;
            Module[{predictionIntervals},
              predictionIntervals = fitFunc["MeanPredictionBands", ConfidenceLevel -> OptionValue["level"]] /. {x ->#} & /@ xDense;
              confidenceData = Transpose[{xDense, predictions, predictionIntervals[[All, 1]], predictionIntervals[[All, 2]]}]
            ],
            
            "loess", (* Placeholder - simple polynomial for now *)
            fitFunc = LinearModelFit[sortedPairs, {1, x, x^2}, x, ConfidenceLevel -> OptionValue["level"]];
            predictions = fitFunc /@ xDense;
            Module[{predictionIntervals},
              predictionIntervals = fitFunc["MeanPredictionBands", ConfidenceLevel -> OptionValue["level"]] /@ xDense;
              confidenceData = Transpose[{xDense, predictions, predictionIntervals[[All, 1]], predictionIntervals[[All, 2]]}]
            ],
            
            _, (* Default to linear *)
            fitFunc = LinearModelFit[sortedPairs, x, x, ConfidenceLevel -> OptionValue["level"]];
            predictions = fitFunc /@ xDense;
            Module[{predictionIntervals},
              predictionIntervals = fitFunc["MeanPredictionBands", ConfidenceLevel -> OptionValue["level"]] /@ xDense;
              confidenceData = Transpose[{xDense, predictions, predictionIntervals[[All, 1]], predictionIntervals[[All, 2]]}]
            ]
          ];
          
          (* Create data points for the smoothed line *)
          groupKey -> Map[Function[dataPoint,
            Association[
              OptionValue["x"] -> dataPoint[[1]], (* x_smooth *)
              OptionValue["y"] -> dataPoint[[2]], (* y_predicted *)
              "ymin" -> dataPoint[[3]], (* ymin_ci *)
              "ymax" -> dataPoint[[4]], (* ymax_ci *) 
              (* Preserve aesthetics from the group *)
              "color_aes" -> Lookup[First[groupData], "color_aes", Black],
              "alpha_aes" -> Lookup[First[groupData], "alpha_aes", Opacity[1]],
              "thickness_aes" -> Lookup[First[groupData], "thickness_aes", Automatic],
							"fill_aes" -> Lookup[First[groupData], "fill_aes", Lookup[First[groupData], "color_aes", Black]]
            ]
          ], confidenceData]
        ]
      ]
    ]
  ], groupedData]];
  
  (* Return grouped results *)
  smoothedData
];

End[];

EndPackage[];
