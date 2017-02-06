import Data.List

-- note that the 9 x 9 board is numbered sequentially from left to right,
-- top to bottom using 0..80.  Thus , the rowNo of cells 0..8 is 0,
-- 9..17 is 1, etc.  colNo is similar, except that colNo of 0, 9, ... is 0,
-- 1, 10, ... is 1, etc.

rowNo  sq =  sq `div` 9

colNo  sq =  sq `mod` 9

boxNo  sq =  (sq `div` 9 `div` 3 ) * 3
    + (sq `div` 3) `mod` 3

-- Two squares sq1 and sq2 are in the same neighborhood if their row,
-- column, or box numbers are the same.

isNeighbor sq1 sq2 =
    (rowNo sq1 == rowNo sq2||
    colNo sq1 == colNo sq2 ||
    boxNo sq1 == boxNo sq2) &&
    sq1 /= sq2


modifyCandidList cell1 cell2 =
    if isNeighbor sq1 sq2 then (fst(cell2), delete a cell2List) else cell2
    where
    	sq1 = fst(cell1)
    	sq2 = fst(cell2)
    	a = snd(cell1)!!0
    	cell2List = snd(cell2)

delCandidateValue cell1 bd = map (modifyCandidList cell1) bd