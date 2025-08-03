(* :Title: ggplot *)
(* :Context: ggplot` *)
(* :Author: andrewyule *)
(* :Date: 2019-11-10 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* Theme setting *)

ggplotSetTheme[ggplotThemeWhite] := Module[{},
  SetOptions[ggplot,
    ImageSize                       -> 400,
    AspectRatio                     -> 7/10,
    Frame                           -> True,
    Axes                            -> False,
    ImageMargins                    -> Automatic,
    LabelStyle                      -> Directive[12, FontFamily -> "Arial"],
    FrameLabel                      -> Automatic,
    FrameStyle                      -> Directive[GrayLevel[0.6], Thickness[0.0008`]],
    FrameTicksStyle                 -> Directive[Black, Opacity[1]],
    FrameTicks                      -> Automatic,
    GridLines                       -> Automatic,
    Background                      -> White,
    PlotRange                       -> All,
    PlotRangeClipping               -> True,
    Method                          -> Automatic,
    Prolog                          -> {},
    (* Color palette settings *)
    "categoricalColors"             -> Automatic,
    "sequentialColors"              -> {Blue, White, Red},
    "divergingColors"               -> {Blue, White, Red},
    "continuousColorPalette"        -> "auto",
    (* Shape palette settings *)
    "categoricalShapes"             -> {"\[FilledCircle]", "\[FilledUpTriangle]", "\[FilledSquare]", "\[FivePointedStar]", "\[FilledDiamond]", "\[FilledRectangle]", "\[FilledDownTriangle]"}
  ];
  SetOptions[ticks2,
    numberOfMajorTicks2             -> 8,
    numberOfMinorTicksPerMajorTick2 -> 1,
    majorTickStyle2                 -> Directive[GrayLevel[0], Thickness[0.001]],
    minorTickStyle2                 -> Directive[GrayLevel[0], Thickness[0.001]],
    majorTickLength2                -> {0.03, 0.},
    minorTickLength2                -> {0.012, 0.},
    DateTicksFormat                 -> Automatic,
    majorGridLineStyle2             -> Directive[GrayLevel[0.8], Thickness[0.002]],
    minorGridLineStyle2             -> Directive[GrayLevel[0.9], Thickness[0.001]]
  ];
  Options[gridLines2] = Options[ticks2];
  Options[formatTicks] = Options[ticks2];
  Options[formatGridLines] = Options[ticks2];
  $ggplotTheme = ggplotThemeWhite;
];


ggplotSetTheme[ggplotThemePub] := Module[{},
  SetOptions[ggplot,
    ImageSize                       -> 300,
    AspectRatio                     -> 7/10,
    Frame                           -> True,
    Axes                            -> False,
    ImageMargins                    -> Automatic,
    LabelStyle                      -> Directive[12, FontFamily -> "Arial"],
    FrameLabel                      -> Automatic,
    FrameStyle                      -> Directive[Black, Thickness[0.0008`]],
    FrameTicksStyle                 -> Directive[Black, Opacity[1]],
    FrameTicks                      -> Automatic,
    GridLines                       -> None,
    Background                      -> White,
    PlotRange                       -> All,
    PlotRangeClipping               -> True,
    Method                          -> Automatic,
    Prolog                          -> {},
    (* Color palette settings *)
    "categoricalColors"             -> (RGBColor[#] & /@ {"#000000", "#DF536B", "#2297E6", "#61D04F", "#F5C710","#FA00F6", "#E69F00","#7A68A6"}), (* edited version of R4 colors *)
    "sequentialColors"              -> (RGBColor[#] & /@ {"#ccece6","#99d8c9","#66c2a4","#41ae76","#238b45","#006d2c","#00441b", "#111111", "#222222"}),
    "divergingColors"               -> (RGBColor[#] & /@ {"#b2182b","#d6604d","#f4a582","#fddbc7","#f7f7f7","#d1e5f0","#92c5de","#4393c3","#2166ac", "#111111", "#222222"}),
    "continuousColorPalette"        -> "auto",
    (* Shape palette settings *)
    "categoricalShapes"             -> FilledMarkers[]
  ];
  SetOptions[ticks2,
    numberOfMajorTicks2             -> 3,
    numberOfMinorTicksPerMajorTick2 -> 2,
    majorTickStyle2                 -> Directive[GrayLevel[0], Thickness[0.001]],
    minorTickStyle2                 -> Directive[GrayLevel[0], Thickness[0.001]],
    majorTickLength2                -> {0.03, 0.},
    minorTickLength2                -> {0.012, 0.},
    DateTicksFormat                 -> Automatic,
    majorGridLineStyle2             -> Directive[GrayLevel[0.8], Thickness[0.002]],
    minorGridLineStyle2             -> Directive[GrayLevel[0.9], Thickness[0.001]]
  ];
  Options[gridLines2] = Options[ticks2];
  Options[formatTicks] = Options[ticks2];
  Options[formatGridLines] = Options[ticks2];
  $ggplotTheme = ggplotThemePub;
  Print["Set theme to ggplotThemePub"]
];

ggplotSetTheme[ggplotThemeGray] := Module[{},
  SetOptions[ggplot,
    ImageSize                       -> 400,
    AspectRatio                     -> 7/10,
    Frame                           -> True,
    Axes                            -> False,
    ImageMargins                    -> Automatic,
    LabelStyle                      -> Directive[12, FontFamily -> "Arial"],
    FrameLabel                      -> Automatic,
    FrameStyle                      -> Directive[Opacity[0]],
    FrameTicksStyle                 -> Directive[Black, Opacity[1]],
    FrameTicks                      -> Automatic,
    GridLines                       -> Automatic,
    Background                      -> White,
    PlotRange                       -> All,
    PlotRangeClipping               -> True,
    Method                          -> {"GridLinesInFront" -> True}, (* important to have this for gray background *)
    Prolog                          -> {RGBColor[0.92, 0.92, 0.92, 1.], Rectangle[Scaled[{0, 0}], Scaled[{1, 1}]]},
    (* Color palette settings *)
    "categoricalColors"             -> Automatic,
    "sequentialColors"              -> {Blue, White, Red},
    "divergingColors"               -> {Blue, White, Red},
    "continuousColorPalette"        -> "auto",
    (* Shape palette settings *)
    "categoricalShapes"             -> {"\[FilledCircle]", "\[FilledUpTriangle]", "\[FilledSquare]", "\[FivePointedStar]", "\[FilledDiamond]", "\[FilledRectangle]", "\[FilledDownTriangle]"}
  ];
  SetOptions[ticks2,
    numberOfMajorTicks2             -> 6,
    numberOfMinorTicksPerMajorTick2 -> 2,
    majorTickStyle2                 -> Directive[Opacity[1], Thickness[0.002], Black],
    minorTickStyle2                 -> Directive[GrayLevel[0], Thickness[0.001]],
    majorTickLength2                -> {0., 0.0075},
    minorTickLength2                -> {0., 0.},
    DateTicksFormat                 -> Automatic,
    majorGridLineStyle2             -> Directive[White, Thickness[0.002]],
    minorGridLineStyle2             -> Directive[White, Thickness[0.001]]
  ];
  Options[gridLines2] = Options[ticks2];
  Options[formatTicks] = Options[ticks2];
  Options[formatGridLines] = Options[ticks2];
  $ggplotTheme = ggplotThemeGray;
];

filledMarker[name_String, size_ : 4] := ResourceFunction["PolygonMarker"][name, Offset[ggplotSizePlaceholder/2], {FaceForm[ggplotColorPlaceholder], ggplotAlphaPlaceholder}, ImagePadding -> 6];

FilledMarkers[size_ : 4] := filledMarker[#, size]& /@ {  "Circle", "Diamond", "Square", "Triangle",  "FivePointedStarThick", "Pentagon", "TripleCross", "SixPointedStar","EightfoldCross", "Y", "DiagonalFourPointedStar"};

openMarker[name_String, size_ : 4] := ResourceFunction["PolygonMarker"][name, Offset[ggplotSizePlaceholder], {EdgeForm[{ggplotColorPlaceholder, JoinForm["Round"], AbsoluteThickness[1], ggplotAlphaPlaceholder}], FaceForm[White]}, ImagePadding -> 6];

OpenMarkers[size_ : 4] := (openMarker[#, size]& /@ {  "Circle", "Diamond", "Square", "Triangle", "FivePointedStarThick", "Pentagon","TripleCross", "SixPointedStar","EightfoldCross", "Y", "DiagonalFourPointedStar"} /. {p : _Disk | _Polygon :> {FaceForm[], p}}); (* the replacement makes the interior transparent. See https://mathematica.stackexchange.com/questions/219573/open-plot-markers-without-background*)

End[];

EndPackage[]