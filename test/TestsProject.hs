import Test.Hspec
import DataStructures
import Logica


main :: IO ()
main = hspec $ do
    it "every card in mkstack is a card of a normal card deck" $ do
        reducelist mkStack examplestack `shouldBe` True

    it "return the correct new coordinates after move in direction D/U/R/L" $ do
        movedirection D (0,0) `shouldBe` (0,1)
        movedirection R (0,0)  `shouldBe` (1,0)
        movedirection L (0,0)  `shouldBe` (-1,0)
        movedirection U (0,0)  `shouldBe` (0,-1)
    
    it "returns the last card of mkstack and flips it" $ do
        last (showLowestCard mkStack) `shouldBe` (Spade, K, Visible)

    it "check if card with ace can be placed on empty endingstack" $ do
        canEnd (Spade, A, Visible) [] `shouldBe` True
    
    it "check if matchingtype returns false with 2 cards of the same color" $ do
        matchingType (getCardType  (Spade, A, Visible)) (getCardType (Club, A, Visible)) `shouldBe` False




reducelist :: Stack -> Stack -> Bool
reducelist stack compareto= length [card `elem` compareto |card<-stack] == length stack

examplestack :: Stack
examplestack = [(Club, A, Hidden), (Club, Two, Hidden), (Club, Three, Hidden), (Club, Four, Hidden), (Club, Five, Hidden), (Club, Six, Hidden), (Club, Seven, Hidden), (Club, Eight, Hidden), (Club, Nine, Hidden), (Club, Ten, Hidden), (Club, J, Hidden), (Club, Q, Hidden), (Club, K, Hidden), (Diamond, A, Hidden), (Diamond, Two, Hidden), (Diamond, Three, Hidden), (Diamond, Four, Hidden), (Diamond, Five, Hidden), (Diamond, Six, Hidden), (Diamond, Seven, Hidden), (Diamond, Eight, Hidden), (Diamond, Nine, Hidden), (Diamond, Ten, Hidden), (Diamond, J, Hidden), (Diamond, Q, Hidden), (Diamond, K, Hidden), (Heart, A, Hidden), (Heart, Two, Hidden), (Heart, Three, Hidden), (Heart, Four, Hidden), (Heart, Five, Hidden), (Heart, Six, Hidden), (Heart, Seven, Hidden), (Heart, Eight, Hidden), (Heart, Nine, Hidden), (Heart, Ten, Hidden), (Heart, J, Hidden), (Heart, Q, Hidden), (Heart, K, Hidden), (Spade, A, Hidden), (Spade, Two, Hidden), (Spade, Three, Hidden), (Spade, Four, Hidden), (Spade, Five, Hidden), (Spade, Six, Hidden), (Spade, Seven, Hidden), (Spade, Eight, Hidden), (Spade, Nine, Hidden), (Spade, Ten, Hidden), (Spade, J, Hidden), (Spade, Q, Hidden), (Spade, K, Hidden)]