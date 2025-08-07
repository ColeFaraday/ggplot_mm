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
  dataset = OptionValue["data"];
  
  (* Ensure X/Y has been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, 
    Message[ggplot::xOrYNotGiven]; 
    Throw[Null]
  ];
  
  processedData = dataset;
  
  (* Switch dates to absolute times *)
  processedData = Replace[processedData, d_?DateObjectQ :> AbsoluteTime[d], Infinity];

  (* SIMPLIFIED: statIdentity is now purely about data transformation *)
  (* All aesthetic reconciliation should have been done before this point *)
  
  (* Group data: if group aesthetic is specified, only group by that; otherwise group by all aesthetics *)
  groupedData = If[KeyExistsQ[First[processedData, <||>], "group_aes"],
    (* Group only by the group aesthetic *)
    GroupBy[processedData, Function[row, row["group_aes"]]],
    (* Group by all aesthetic values - rows with same aesthetics = same group *)
    GroupBy[processedData, 
      Function[row,
        {Lookup[row, "color_aes", Black], Lookup[row, "size_aes", 1], 
         Lookup[row, "alpha_aes", Opacity[1]], Lookup[row, "shape_aes", "\[FilledCircle]"],
         Lookup[row, "thickness_aes", Automatic]}
      ]
    ]
  ];
  groupedData
]

End[];

EndPackage[];