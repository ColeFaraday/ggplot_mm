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
  "n" -> 100,
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
    (* Group by all aesthetic values like statSmooth *)
    GroupBy[processedData, 
      Function[row,
        {Lookup[row, "color_aes", Black], Lookup[row, "alpha_aes", Opacity[1]], 
         Lookup[row, "fill_aes", Black]}
      ]
    ]
  ];

  (* Compute 2D density for each group *)
  densityData = Association[KeyValueMap[Function[{groupKey, groupData},
    Module[{xvals, yvals, xyPairs, xRange, yRange, xGrid, yGrid, densityResults},
      xvals = extractMappedValues[groupData, OptionValue["x"]];
      yvals = extractMappedValues[groupData, OptionValue["y"]];
      xyPairs = Transpose[{xvals, yvals}];
      
      (* Remove non-numeric pairs *)
      xyPairs = Cases[xyPairs, {_?NumericQ, _?NumericQ}];
      
      If[Length[xyPairs] < 4,
        groupKey -> {}, (* Return empty if insufficient data *)
        (* Create density grid using Mathematica's built-in SmoothKernelDistribution *)
        Module[{xmin, xmax, ymin, ymax, gridSize, xGrid, yGrid, densityPoints, kde},
          xmin = Min[xyPairs[[All, 1]]];
          xmax = Max[xyPairs[[All, 1]]];
          ymin = Min[xyPairs[[All, 2]]];
          ymax = Max[xyPairs[[All, 2]]];
          
          gridSize = OptionValue["n"]; (* n=100 -> 10x10 grid *)
          
          (* Create 2D kernel density estimator *)
          kde = If[OptionValue["bandwidth"] === Automatic,
            SmoothKernelDistribution[xyPairs],
            SmoothKernelDistribution[xyPairs, OptionValue["bandwidth"]]
          ];
          
          (* Extend bounds slightly for better visualization *)
          Module[{xrange, yrange, extend},
            xrange = xmax - xmin;
            yrange = ymax - ymin;
            extend = 0.1; (* Extend by 10% on each side *)
            xmin = xmin - extend * xrange;
            xmax = xmax + extend * xrange;
            ymin = ymin - extend * yrange;
            ymax = ymax + extend * yrange
          ];
          
          (* Create grid points *)
          xGrid = Subdivide[xmin, xmax, gridSize - 1];
          yGrid = Subdivide[ymin, ymax, gridSize - 1];
          
          (* Evaluate KDE at each grid point *)
          densityPoints = {};
          Do[
            Module[{gridX, gridY, density, baseData, aestheticKeys, aestheticData},
              gridX = xGrid[[i]];
              gridY = yGrid[[j]];
              
              (* Evaluate the KDE at this grid point *)
              density = PDF[kde, {gridX, gridY}];
              
              (* Base data with coordinates *)
              baseData = Association[
                OptionValue["x"] -> gridX,
                OptionValue["y"] -> gridY,
                "density" -> density,
                "scaled" -> density, (* Will be rescaled after computing all densities *)
                "level" -> 1 (* Will be computed after all densities *)
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
              AppendTo[densityPoints, Join[baseData, aestheticData]]
            ],
            {i, Length[xGrid]}, {j, Length[yGrid]}
          ];
          
          (* Post-process: compute scaled densities and levels *)
          Module[{allDensities, maxDensity, quantiles},
            allDensities = #["density"] & /@ densityPoints;
            maxDensity = Max[allDensities];
            
            (* Compute quantiles for contour levels *)
            quantiles = Quantile[allDensities, Range[0.1, 1.0, 0.1]];
            
            (* Update scaled and level values *)
            densityPoints = Map[Function[point,
              Module[{scaledDensity, level},
                scaledDensity = point["density"] / maxDensity;
                level = Length[Select[quantiles, # <= point["density"] &]] + 1;
                
                <|point, "scaled" -> scaledDensity, "level" -> level|>
              ]
            ], densityPoints]
          ];
          
          groupKey -> N@densityPoints
        ]
      ]
    ]
  ], groupedData]];

  
  (* Return grouped results like other stat functions *)
  densityData
];

End[];

EndPackage[];
