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
  dataset, processedData
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

  (* Apply aesthetic reconciliation - this is the grouping logic *)
  processedData = reconcileAesthetics[processedData, OptionValue["color"], "color"];
  processedData = reconcileAesthetics[processedData, OptionValue["size"], "size"];
  processedData = reconcileAesthetics[processedData, OptionValue["alpha"], "alpha"];
  processedData = reconcileAesthetics[processedData, OptionValue["shape"], "shape"];
  processedData = reconcileAesthetics[processedData, OptionValue["thickness"], "thickness"];
  processedData = reconcileAesthetics[processedData, OptionValue["group"], "group"];

  (* Group data: if group aesthetic is specified, only group by that; otherwise group by all aesthetics *)
  groupedData = If[OptionValue["group"] =!= Null,
    (* Group only by the explicit group aesthetic *)
    GroupBy[processedData, Function[row, row["group_aes"]]],
    (* Group by all aesthetic values - rows with same aesthetics = same group *)
    GroupBy[processedData, 
      Function[row,
        {row["color_aes"], row["size_aes"], row["alpha_aes"], row["shape_aes"]}
      ]
    ]
  ];
  groupedData
]

End[];

EndPackage[];