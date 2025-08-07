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
  Print["statBin called with options: ", opts];
  dataset = OptionValue["data"];
  Print["statBin dataset length: ", Length[dataset]];
  
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
  computedBins = KeyValueMap[Function[{groupKey, groupData},
    Module[{xvals, bins, counts, binCenters, binResults},
      xvals = extractMappedValues[groupData, OptionValue["x"]];
      
      (* Clean the data for binning *)
      xvals = Cases[xvals, _?NumericQ];
      
      If[Length[xvals] == 0, 
        {}, (* Return empty if no numeric data *)
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
            "count" -> count,
            "density" -> count / (Total[counts] * (rightEdge - leftEdge)),
            "binwidth" -> rightEdge - leftEdge,
            "xmin" -> leftEdge,
            "xmax" -> rightEdge,
            "y" -> count,  (* Default y mapping *)
            (* Preserve aesthetics from the group *)
            "color_aes" -> Lookup[First[groupData], "color_aes", Black],
            "alpha_aes" -> Lookup[First[groupData], "alpha_aes", Opacity[1]]
          ]
        ], {binCenters, counts, Most[bins], Rest[bins]}];
        
        binResults
      ]
    ]
  ], groupedData];
  
  (* Flatten results while preserving groups *)
  computedBins = KeyValueMap[Function[{groupKey, binData}, groupKey -> binData], computedBins];
  Association[computedBins]
];

End[];

EndPackage[];
