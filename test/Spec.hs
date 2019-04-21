import Test.Hspec
import Test.QuickCheck
import EasyRewriting.Check (Judgment(SeemsLegit, Incorrect), check, checkAt, checkOn, equalityOfFunctions)
import Examples.Trivial (empty, singleton)
import Examples.Arithmetic (twelve, incorrectSimplification)
import Examples.Function (identity, incorrectIdentity)
import Examples.AbstractSyntax (Expr(Const, (:+:)), eval, associativity)
import Examples.Monad (defineBindUsingJoin, defineJoinUsingBind)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "check" $ do
    it "should accept the empty list" $ do
      accept $ check empty
    it "should accept a singleton list" $ do
      accept $ check singleton
    it "should accept twelve" $ do
      accept $ check twelve
    it "should reject incorrectSimplification" $ do
      reject (Incorrect 2 20 10) $ check incorrectSimplification
    it "should reject associativity" $ do
      reject (Incorrect 2 leftAssoc rightAssoc) $ check associativity

  describe "checkOn" $ do
    it "should accept the empty list on any function" $ do
      accept $ checkOn anyFunction empty
    it "should accept a singleton list on any function" $ do
      accept $ checkOn anyFunction singleton
    it "should accept twelve on id" $ do
      accept $ checkOn id twelve
    it "should accept associativity on eval" $ do
      accept $ checkOn eval associativity
    it "should reject associativity on id" $ do
      reject (Incorrect 2 leftAssoc rightAssoc) $ checkOn id associativity

  describe "checkAt" $ do
    it "should accept the empty list at any point" $ do
      accept $ checkAt anyPoint (map const empty)
    it "should accept a singleton list at any point" $ do
      accept $ checkAt anyPoint (map const singleton)
    it "should accept identity at 1" $ do
      accept $ checkAt 1 identity
    it "should accept defineJoinUsingBind for Maybe" $ do
      accept $ checkAt (Nothing :: Maybe (Maybe Int)) defineJoinUsingBind
      accept $ checkAt (Just (Nothing :: Maybe Int)) defineJoinUsingBind
      accept $ checkAt (Just (Just 5 :: Maybe Int)) defineJoinUsingBind
    it "should accept defineJoinUsingBind for []" $ do
      accept $ checkAt ([] :: [[Int]]) defineJoinUsingBind
      accept $ checkAt ([[], []] :: [[Int]]) defineJoinUsingBind
      accept $ checkAt ([[5], [8]] :: [[Int]]) defineJoinUsingBind
    it "should accept defineBindUsingJoin for Maybe" $ do
      accept $ checkAt just $ map ($ Just 5) defineBindUsingJoin
      accept $ checkAt just $ map ($ Nothing) defineBindUsingJoin
    it "should accept defineBindUsingJoin for []" $ do
      accept $ checkAt single $ map ($ [1..3]) defineBindUsingJoin
      accept $ checkAt single $ map ($ []) defineBindUsingJoin
    it "should reject incorrectIdentity at 1" $ do
      reject (Incorrect 2 1 0) $ checkAt 1 incorrectIdentity

  describe "equalityOfFunctions" $ do
    it "should pass QuickCheck with identity" $ property $ equalityOfFunctions identity

  where
    leftAssoc = (Const 3 :+: Const 5) :+: Const 8
    rightAssoc = Const 3 :+: (Const 5 :+: Const 8)
    just = pure :: Int -> Maybe Int
    single = pure :: Int -> [Int]
    accept :: (Eq a, Show a) => Judgment a -> Expectation
    accept = flip shouldBe SeemsLegit
    reject :: (Eq a, Show a) => Judgment a -> Judgment a -> Expectation
    reject = flip shouldBe
    anyPoint :: a
    anyPoint = error "any point"
    anyFunction :: a -> Int
    anyFunction = error "any function"
