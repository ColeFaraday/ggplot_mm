BeginPackage["ggplot`"];

Begin["`Private`"];

ClearAll[statBin];
Options[statBin] = {
  "data" -> {},
  "x" -> Null,
  "y" -> Null,
  "color" -> Null,
  "alpha" -> Null,
  "group" -> Null,
  "bins" -> 30,
  "binwidth" -> Automatic,
  "center" -> Automatic,
  "boundary" -> Automatic
};

statBin[opts : OptionsPattern[]] := Module[{
  dataset, processedData, groupedData, computedBins
},
  dataset = OptionValue["data"];
  
  (* Ensure X has been given *)
  If[OptionValue["x"] === Null, 
    Message[ggplot::xOrYNotGiven]; 
    Throw[Null]
  ];
  
  processedData = dataset;
  
  (* Switch dates to absolute times *)
  processedData = Replace[processedData, d_?DateObjectQ :> AbsoluteTime[d], Infinity];

  (* Group data by aesthetics for binning *)
  groupedData = If[KeyExistsQ[First[processedData, <||>], "group_aes"],
    (* Group only by the group aesthetic *)
    GroupBy[processedData, Function[row, row["group_aes"]]],
    (* Group by all aesthetic values - rows with same aesthetics = same group *)
    GroupBy[processedData, 
      Function[row,
        {Lookup[row, "color_aes", Black], Lookup[row, "alpha_aes", Opacity[1]]}
      ]
    ]
  ];

  (* Compute bins for each group *)
  computedBins = Association[KeyValueMap[Function[{groupKey, groupData},
    Module[{xvals, bins, counts, binCenters, binResults},
      xvals = extractMappedValues[groupData, OptionValue["x"]];
      
      (* Clean the data for binning *)
      xvals = Cases[xvals, _?NumericQ];
      
      If[Length[xvals] == 0, 
        groupKey -> {}, (* Return empty if no numeric data *)
        (* Create histogram bins *)
        {bins, counts} = HistogramList[xvals, 
          If[OptionValue["binwidth"] =!= Automatic,
            {Min[xvals], Max[xvals], OptionValue["binwidth"]},
            OptionValue["bins"]
          ]
        ];
        
        (* Create bin centers and build result data *)
        binCenters = MovingAverage[bins, 2];
        binResults = MapThread[Function[{center, count, leftEdge, rightEdge},
          (* Create a new data point for each bin *)
          Association[
            OptionValue["x"] -> center,
            OptionValue["y"] -> count,
            "count" -> count,
            "density" -> count / (Total[counts] * (rightEdge - leftEdge)),
            "binwidth" -> rightEdge - leftEdge,
            "xmin" -> leftEdge,
            "xmax" -> rightEdge,
            "x" -> center,  (* Default x mapping *)
            "y" -> count,  (* Default y mapping *)
            (* Preserve aesthetics from the group *)
            "color_aes" -> Lookup[First[groupData], "color_aes", Black],
            "alpha_aes" -> Lookup[First[groupData], "alpha_aes", Opacity[1]],
						"shape_aes" -> Lookup[First[groupData], "shape_aes", "\[FilledCircle]"],
						"thickness_aes" -> Lookup[First[groupData], "thickness_aes", Automatic],
						"group_aes" -> Lookup[First[groupData], "group_aes", Null],
						"size_aes" -> Lookup[First[groupData], "size_aes", 1]
          ]
        ], {binCenters, counts, Most[bins], Rest[bins]}];
        
        groupKey -> binResults
      ]
    ]
  ], groupedData]];
  
  (* Return grouped results like other stat functions *)
  computedBins
];

End[];

EndPackage[];
