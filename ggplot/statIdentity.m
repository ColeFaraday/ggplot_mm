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
  "shape" -> Null
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
  
  (* Return the processed dataset with aesthetic columns *)
	groupedData = GroupBy[processedData, 
			Function[row,
				(* Group by all aesthetic values - rows with same aesthetics = same group *)
				{row["color_aes"], row["size_aes"], row["alpha_aes"], row["shape_aes"]}
			]
		];
	groupedData
]

End[];

EndPackage[];