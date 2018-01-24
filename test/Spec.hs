import Test.HUnit
import Lib

testBoardSize = 14 --actually is 15x15, 0-indexing

getTestBoard1 :: [[Int]]
getTestBoard1 = [[if (i==j && i<5) then 1 else -50 | i <- [0..testBoardSize]] | j <- [0..testBoardSize]]

getTestBoard2 :: [[Int]]
getTestBoard2 = [[if (j==0 && i<5) then 1 else -50 | i <- [0..testBoardSize]] | j <- [0..testBoardSize]]

getTestBoard3 :: [[Int]]
getTestBoard3 = [[if (j<5 && i==5) then 1 else -50 | i <- [0..testBoardSize]] | j <- [0..testBoardSize]]

getTestBoard4 :: [[Int]]
getTestBoard4 = [[-50 | i <- [0..testBoardSize]] | j <- [0..testBoardSize]]

getTestBoard5 :: [[Int]]
getTestBoard5 = [if (idx `mod` 2 == 0) then row1 else row2 | idx <- [0..testBoardSize]]
  where row1 = [1,1,0,0,1,1,0,0,1,1,0,0,1,1,0]
        row2 = [0,0,1,1,0,0,1,1,0,0,1,1,0,0,1]


test1 :: Test
test1 = TestCase (assertEqual "Cross check test" True (checkForVictory getTestBoard1 1))

test2 :: Test
test2 = TestCase (assertEqual "Horizontal check test" True (checkForVictory getTestBoard2 1))

test3 :: Test
test3 = TestCase (assertEqual "Vertical check test" True (checkForVictory getTestBoard3 1))

test4 :: Test
test4 = TestCase (assertEqual "Empty board check test" False (checkForVictory getTestBoard4 1))

test5 :: Test
test5 = TestCase (assertEqual "Full board no winner check test" False (checkForVictory getTestBoard5 1))


tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4, TestLabel "test5" test5]


main :: IO Counts
main = runTestTT tests
