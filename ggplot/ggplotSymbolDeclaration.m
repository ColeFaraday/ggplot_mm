(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

ggplot::usage             = "TBD";
ggplotThemeWhite::usage   = "TBD";
ggplotThemePub::usage   = "TBD";
ggplotThemeGray::usage    = "TBD";
ggplotSetTheme::usage     = "TBD";
$ggplotTheme::usage       = "TBD";

(* Geoms *)
geomPoint::usage        = "TBD";
geomPointRender::usage        = "TBD";
geomLine::usage         = "TBD";
geomLineRender::usage         = "TBD";
geomPath::usage         = "TBD";
geomPathRender::usage         = "TBD";
geomSmooth::usage       = "TBD";
geomSmoothRender::usage       = "TBD";
geomCol::usage          = "TBD";
geomColRender::usage          = "TBD";
geomParityLine::usage   = "TBD";
geomParityLineRender::usage   = "TBD";
geomHLine::usage        = "TBD";
geomHLineRender::usage        = "TBD";
geomVLine::usage        = "TBD";
geomVLineRender::usage        = "TBD";
geomHistogram::usage    = "TBD";
geomHistogramRender::usage    = "TBD";
geomErrorBar::usage    = "TBD";
geomErrorBarRender::usage    = "TBD";
geomErrorBoxes::usage    = "TBD";
geomErrorBoxesRender::usage    = "TBD";
geomBand::usage    = "TBD";
geomBandRender::usage    = "TBD";
geomDensity2DFilled::usage    = "TBD";
geomDensity2DFilledRender::usage    = "TBD";
geomConvexHull::usage    = "TBD";
geomText::usage  = "TBD";
geomTextRender::usage  = "TBD";

(* Stats *)
statIdentity::usage = "TBD";
statBin::usage = "TBD";
statSmooth::usage = "TBD";
statConvexHull::usage = "TBD";
statDensity2D::usage = "TBD";

(* Scales *)
scaleXLinear2::usage   = "TBD";
scaleYLinear2::usage   = "TBD";
scaleXDate2::usage     = "TBD";
scaleYDate2::usage     = "TBD";
scaleXLog2::usage      = "TBD";
scaleYLog2::usage      = "TBD";

(* ticks2 *)
ticks2::usage                            = "TBD";
numberOfMajorTicks2::usage              = "TBD";
numberOfMinorTicksPerMajorTick2::usage  = "TBD";
majorTickStyle2::usage                  = "TBD";
minorTickStyle2::usage                  = "TBD";
majorTickLength2::usage                 = "TBD";
minorTickLength2::usage                 = "TBD";

Options[ticks2] = {numberOfMajorTicks2 -> 8, numberOfMinorTicksPerMajorTick2 -> 1, majorTickStyle2 -> Directive[GrayLevel[0], Thickness[0.001`]], minorTickStyle2 -> Directive[GrayLevel[0], Thickness[0.001`]], majorTickLength2 -> {0., 0.}, minorTickLength2 -> {0., 0.}, DateTicksFormat -> Automatic, majorGridLineStyle2 -> Directive[GrayLevel[0.6], Thickness[0.0008]], minorGridLineStyle2 -> Directive[GrayLevel[0.85], Thickness[0.0008]]};

(* gridLines2*)
gridLines2::usage            = "TBD";
majorGridLineStyle2::usage  = "TBD";
minorGridLineStyle2::usage  = "TBD";

extractMappedValues::usage = "TBD";


Options[gridLines2] = Options[ticks2];

(* Faceting *)
facetWrap::usage         = "TBD";

Begin["`Private`"];

End[];

EndPackage[];
