(* Simple test to verify facet data filtering *)

(* Create minimal test data *)
testData = {
  <|"x" -> 1, "y" -> 2, "class" -> "A"|>,
  <|"x" -> 2, "y" -> 3, "class" -> "A"|>,
  <|"x" -> 3, "y" -> 1, "class" -> "B"|>,
  <|"x" -> 4, "y" -> 4, "class" -> "B"|>,
  <|"x" -> 5, "y" -> 2, "class" -> "C"|>
};

Print["Test data: ", testData];

(* Test the faceting with simple data *)
Get["ggplot/Kernel/init.m"];

result = ggplot[testData, aes["x", "y"]] + 
  geomPoint[] + 
  facetWrap["variable" -> "class", "ncol" -> 2];

Print["Result: ", result];
