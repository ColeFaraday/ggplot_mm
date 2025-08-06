BeginPackage["ggplot`"];

Begin["`Private`"];

ClearAll[statIdentity];
Options[statIdentity] = {
  "data" -> {},
  "x" -> Null,
  "y" -> Null, 
  "color" -> Null,
  "size" -> Null,
  "alpha" -> Null,
  "shape" -> Null,
	"thickness" -> Null,
  "group" -> Null
};

statIdentity[opts : OptionsPattern[]] := Module[{
  dataset, processedData, groupedData
},
  Print["[statIdentity] Starting statIdentity"];
  dataset = OptionValue["data"];
  Print["[statIdentity] dataset length:", Length[dataset]];
  Print["[statIdentity] x mapping head:", Head[OptionValue["x"]]];
  Print["[statIdentity] y mapping head:", Head[OptionValue["y"]]];
  
  (* Ensure X/Y has been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, 
    Print["[statIdentity] ERROR: x or y not given"];
    Message[ggplot::xOrYNotGiven]; 
    Throw[Null]
  ];
  
  processedData = dataset;
  Print["[statIdentity] Initial processedData length:", Length[processedData]];
  Print["[statIdentity] First row example:", First[processedData, <||>]];
  
  (* Switch dates to absolute times *)
  processedData = Replace[processedData, d_?DateObjectQ :> AbsoluteTime[d], Infinity];

  (* SIMPLIFIED: statIdentity is now purely about data transformation *)
  (* All aesthetic reconciliation should have been done before this point *)
  
  (* Group data: if group aesthetic is specified, only group by that; otherwise group by all aesthetics *)
  Print["[statIdentity] Grouping data by aesthetic combinations"];
  groupedData = If[KeyExistsQ[First[processedData, <||>], "group_aes"],
    Print["[statIdentity] Grouping by explicit group aesthetic"];
    (* Group only by the group aesthetic *)
    GroupBy[processedData, Function[row, row["group_aes"]]],
    Print["[statIdentity] Grouping by all aesthetic combinations"];
    (* Group by all aesthetic values - rows with same aesthetics = same group *)
    GroupBy[processedData, 
      Function[row,
        {Lookup[row, "color_aes", Black], Lookup[row, "size_aes", 1], 
         Lookup[row, "alpha_aes", Opacity[1]], Lookup[row, "shape_aes", "\[FilledCircle]"],
         Lookup[row, "thickness_aes", Automatic]}
      ]
    ]
  ];
  Print["[statIdentity] groupedData keys count:", Length[Keys[groupedData]]];
  Print["[statIdentity] groupedData group sizes:", Length /@ groupedData];
  Print["[statIdentity] Returning groupedData"];
  groupedData
]

End[];

EndPackage[];