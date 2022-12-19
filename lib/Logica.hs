module Logica where

import Data.Char (toLower)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import DataStructures
import GHC.IO (unsafePerformIO)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy
import System.Random
import System.Random.Shuffle

-------------------
-- settings of game
-------------------

-- Initiele positie van het Gloss venster.
windowPosition :: (Int, Int)
windowPosition = (200, 100)

-- Seed voor de random generator.
seed :: Int
seed = 45

-- Het Gloss venster
window :: Display
window = InWindow "Patience" (900, 600) windowPosition

-- Framerate van het spel.
fps :: Int
fps = 60

-- size of card
cardSize :: (Int, Int)
cardSize = (100, 150)

-- spacing between cards
inset :: Int
inset = 20

--------------------
-- initialising game
--------------------

-- make a stack of all possible card
mkStack :: Stack
mkStack = [(t, value, Hidden) | t <- [Club .. Spade], value <- [A .. K]]

-- shuffle the stack
shuffleCards :: Stack -> Stack
shuffleCards cards = shuffle' cards (length cards) (mkStdGen seed)

-- make the 7 initial gamestacks
initStacks :: Stack -> Int -> [Stack]
initStacks cards n
  | n == 0 = []
  | otherwise = take n cards : initStacks (drop n cards) (n - 1)

-- initial board, 7 filled gamestacks(has to reverse because initstacks function goes from stack with size 7 to 0), 4 empty endingstacks and a pile with 3 turned cards
initBoard :: Board
initBoard = Board {gameStacks = map showLowestCard (reverse (initStacks shuffled 7)), endingStacks = [[], [], [], []], pile = turnLast3 (drop 28 shuffled)}
  where
    shuffled = shuffleCards mkStack

-- initial game
initGame :: Game
initGame = Game {board = initBoard, selector = initSelector}

-- initial selector
initSelector :: Selector
initSelector = Selector {position = (0, 0), selected = Nothing}

------------------------------------------------------------------
-- functions to prevent memory overflow, render pictures beforehand
------------------------------------------------------------------

-- lookuptable with pictures of the cards to prevent memory overflow
pictureMap :: [(Card, Picture)]
pictureMap = [(card, loadPicture t v) | card@(t, v, _) <- [(typ, val, Visible) | (typ, val, _) <- mkStack]]

loadPicture :: CardType -> CardValue -> Picture
loadPicture t v = fromMaybe Blank (unsafePerformIO (loadJuicyPNG ("lib/assets/" ++ map toLower (show t) ++ "s/" ++ map toLower (show t) ++ "-" ++ convertVal v ++ ".png")))

-- array with the other pictures of the game
symbols :: [Picture]
symbols = [loadsymbol "back", loadsymbol "placeholder", loadsymbol "selected", loadsymbol "selector"]

loadsymbol :: String -> Picture
loadsymbol val = fromMaybe Blank (unsafePerformIO (loadJuicyPNG ("lib/assets/" ++ val ++ ".png")))

lookupPicture :: Card -> Picture
lookupPicture c = fromJust (lookup c pictureMap)

-------------------------------------------------------------------------------------
-- helper functions to convert the cardvalues for filenames and arithmemtic operations
-------------------------------------------------------------------------------------
-- convert the values of the cards to correct file names
convertVal :: CardValue -> [Char]
convertVal v
  | v == Two = show 2
  | v == Three = show 3
  | v == Four = show 4
  | v == Five = show 5
  | v == Six = show 6
  | v == Seven = show 7
  | v == Eight = show 8
  | v == Nine = show 9
  | v == Ten = show 10
  | otherwise = show v

valToNumber :: CardValue -> Int
valToNumber v
  | v == A = 1
  | v == Two = 2
  | v == Three = 3
  | v == Four = 4
  | v == Five = 5
  | v == Six = 6
  | v == Seven = 7
  | v == Eight = 8
  | v == Nine = 9
  | v == Ten = 10
  | v == J = 11
  | v == Q = 12
  | v == K = 13

------------------------------------------------
-- helper functions for moving cards and turning
------------------------------------------------

-- it makes sense to turn the y-axis upside down in this program so that (0,0) is in the upper left corner
-- function to change the coordinate to given direction
movedirection :: Direction -> Coordinate -> Coordinate
movedirection d (x, y)
  | d == U = (x, y - 1)
  | d == D = (x, y + 1)
  | d == L = (x - 1, y)
  | d == R = (x + 1, y)

-- get card argument helper functions

getCardValue :: Card -> CardValue
getCardValue (_, v, _) = v

getCardType :: Card -> CardType
getCardType (t, _, _) = t

getStatus :: Card -> CardStatus
getStatus (_, _, s) = s

-- return if cardtypes are of opposing colors, check if card can move to other stack
matchingType :: CardType -> CardType -> Bool
matchingType t1 t2
  | t1 == t2 = False
  | t1 == Spade || t1 == Club = (t1 == Spade || t1 == Club) && (t2 == Heart || t2 == Diamond)
  | otherwise = (t1 == Heart || t1 == Diamond) && (t2 == Spade || t2 == Club)

-- namechange of length because easier to read
stacksize :: Stack -> Int
stacksize = length

-- turn a card
turnCard :: Card -> Card
turnCard (t, v, Hidden) = (t, v, Visible)
turnCard (t, v, Visible) = (t, v, Hidden)

-- check if the move to a specific direction is possible
canMove :: Coordinate -> Board -> Direction -> Bool
canMove (x, y) board d
  | d == U = y > 0
  | d == D = y < stacksize (gameStacks board !! x) - 1
  | d == L = x > 0
  | d == R = x < 6
  | otherwise = False

-- move the selector to the correct stack and card (= the lowest card visible or the highest card visible)
moveSelector :: Board -> Direction -> Selector -> Selector
moveSelector board d selector
  | canMove (position selector) board d && snd (position selector) == highestShownCard (gameStacks board !! fst (position selector)) && d == D = selector {position = moveLowestShown d (position selector) board}
  | canMove (position selector) board d && d == U && snd (position selector) == (stacksize (gameStacks board !! fst (position selector)) - 1) = selector {position = moveHighestShown d (position selector) board}
  | canMove (position selector) board d = selector {position = (fst (movedirection d (position selector)), snd (moveLowestShown d (position selector) board))}
  | otherwise = selector

-- move the selector to the lowest shown card in a stack
moveLowestShown :: Direction -> Coordinate -> Board -> Coordinate
moveLowestShown d (x, y) board
  | null (gameStacks board !! fst (movedirection d (x, y))) = (fst (movedirection d (x, y)), 0)
  | otherwise = (fst (movedirection d (x, y)), stacksize (gameStacks board !! fst (movedirection d (x, y))) - 1)

-- move the selector to the highest shown card in a stack
moveHighestShown :: Direction -> Coordinate -> Board -> Coordinate
moveHighestShown d (x, y) board = (fst (movedirection d (x, y)), highestShownCard (gameStacks board !! fst (movedirection d (x, y))))

-- give the coordinate of the highest shown card
highestShownCard :: Stack -> Int
highestShownCard stack = stacksize stack - length (filter (\x -> getStatus x == Visible) stack)

-- turn the lowest card in a stack, used to initialize the board
showLowestCard :: Stack -> Stack
showLowestCard [] = []
showLowestCard stack
  | getStatus (stack !! (stacksize stack - 1)) == Hidden = init stack ++ [turnCard (last stack)]
  | otherwise = stack

-- empty the selector
emptySelector :: Selector -> Selector
emptySelector s = s {selected = Nothing}

-- Update board
step :: Float -> Game -> Game
step _ b = b

-- helper functions to determine the ending stack of a card
endTypeStack :: CardType -> [Stack] -> Stack
endTypeStack t stacks
  | t == Spade = head stacks
  | t == Heart = stacks !! 1
  | t == Diamond = stacks !! 2
  | t == Club = stacks !! 3

endStackNumber :: CardType -> Int
endStackNumber t
  | t == Spade = 0
  | t == Heart = 1
  | t == Diamond = 2
  | t == Club = 3

------------------
-- input functions
------------------

-- detect if key pressed is one of special keys
isSpecialKey :: SpecialKey -> Event -> Bool
isSpecialKey k1 (EventKey (SpecialKey k2) Down _ _) = k1 == k2
isSpecialKey _ _ = False

-- detect if key pressed is a normal letter
isKey :: Char -> Event -> Bool
isKey k1 (EventKey (Char k2) Down _ _) = k1 == k2
isKey _ _ = False

-- use arrow keys to move the selector
-- use space to select a card and move cards
-- use r to rotate the drawpile in steps of 3
-- use d to try to draw a card from the drawpile to the current selector or instantly to the endstack
-- use e to try to send cards from gamestacks to  the matching endstacks
-- make sure to always move to lowest card shown after move (causes index problems otherwise)
-- TODO: dont't draw if drawpile is empty
handleInput :: Event -> Game -> Game
handleInput ev game
  | isSpecialKey KeyDown ev = game {selector = moveSelector cboard D cselector}
  | isSpecialKey KeyLeft ev = game {selector = moveSelector cboard L cselector}
  | isSpecialKey KeyRight ev = game {selector = moveSelector cboard R cselector}
  | isSpecialKey KeyUp ev = game {selector = moveSelector cboard U cselector}
  | isSpecialKey KeySpace ev && isNothing (selected cselector) && stacksize (gameStacks cboard !! fst (position cselector)) /=0 = game {selector = cselector {selected = Just (position cselector)}}
  | isSpecialKey KeySpace ev && isNothing (selected cselector) && null (gameStacks cboard !! fst (position cselector)) = game
  | isSpecialKey KeySpace ev && selected cselector == Just (position cselector) = game {selector = emptySelector cselector}
  | isSpecialKey KeySpace ev && not (canplaceCard (getCard (fromJust (selected cselector)) cboard) (gameStacks cboard !! fst (position cselector))) = game {selector = emptySelector cselector}
  | isSpecialKey KeySpace ev && isJust (selected cselector) && canplaceCard (getCard (fromJust (selected cselector)) cboard) (gameStacks cboard!! fst (position cselector)) = game {board = board (moveCards game), selector = (selector game) {position = moveLowestShown D (position (selector game)) (board (moveCards game)),selected=Nothing}}
  | isKey 'r' ev = game {board = (board game) {pile = turnLast3 (showncardsToBack (pile (board game)))}}
  | isKey 'd' ev && stacksize (pile cboard)/=0 = game {board = drawCard cboard (position (selector game)), selector = (selector game){position = moveLowestShown D (position (selector game)) (drawCard (board game) (position (selector game)))}}
  | isKey 'e' ev && stacksize (gameStacks cboard!! fst(position cselector))/=0 && canEnd (getCard (position cselector ) cboard) (endTypeStack (getCardType (getCard (position cselector) cboard)) (endingStacks cboard)) = (stackToEnd game) {selector = (selector (stackToEnd game)) {position = moveLowestShown D (position (selector (stackToEnd game))) (board (stackToEnd game)), selected = Nothing}}
  | otherwise = game
  where cboard = board game
        cselector = selector game
-------------------
-- move cards around
-------------------
-- get card from gamestacks board according to coordinate
getCard:: Coordinate -> Board -> Card
getCard (x,y) board = (gameStacks board !! x) !! y
  

-- turn the last 3 cards in the drawpile
turnLast3 :: Stack -> Stack
turnLast3 stack
    | stacksize stack ==1 = [turnCard (head stack)]
  | stacksize stack == 2 = [turnCard (head stack), turnCard (last stack)]
  | stacksize stack == 3 = tail (map turnCard stack) ++ [head (map turnCard stack)]
  | otherwise = init (init (init stack)) ++ [turnCard (stack !! (stacksize stack - 1))] ++ [turnCard (stack !! (stacksize stack - 2))] ++ [turnCard (stack !! (stacksize stack - 3))]

-- move the shown cards to the back of the drawpile
showncardsToBack :: Stack -> Stack
showncardsToBack stack
    | stacksize stack <=3 = [(t,v,Hidden) | (t,v,_)<-stack]
    | getStatus (stack !! (stacksize stack - 3)) == Visible = concat (map turnCard (take 3 (reverse stack)) : [[(t, v, Hidden) | (t, v, _) <- reverse (drop 3 (reverse stack))]])
    | getStatus (stack !! (stacksize stack - 2)) == Visible = concat (map turnCard (take 2 (reverse stack)) : [[(t, v, Hidden) | (t, v, _) <- reverse (drop 2 (reverse stack))]])
    | getStatus (stack !! (stacksize stack - 1)) == Visible = concat (map turnCard (take 1 (reverse stack)) : [[(t, v, Hidden) | (t, v, _) <- reverse (drop 1 (reverse stack))]])
    | otherwise = stack

-- check if a card can be placed on another stack
canplaceCard :: Card -> Stack -> Bool
canplaceCard card stack
  | null stack && getCardValue card == K = True
  | null stack = False
  | otherwise = valToNumber (getCardValue card) == pred (valToNumber (getCardValue (last stack))) && matchingType (getCardType card) (getCardType (last stack))

-- place current shown card of drawpile on current location of selector
drawCard :: Board -> Coordinate -> Board
drawCard board (x, y)
  | canEnd (last (pile board)) (endTypeStack (getCardType (last (pile board))) (endingStacks board)) = board {pile = init (pile board), endingStacks = toEnd (last (pile board)) (endingStacks board)}
  | getStatus (last (pile board)) == Hidden = board
  | canplaceCard (last (pile board)) (gameStacks board !! x) = board {pile = init (pile board), gameStacks = replaceNth x (moveTo (gameStacks board !! x) [last (pile board)]) (gameStacks board)}
  | otherwise = board

-- move card to endingstack
toEnd :: Card -> [Stack] -> [Stack]
toEnd card stacks
  | canEnd card (endTypeStack (getCardType card) stacks) = replaceNth (endStackNumber (getCardType card)) (moveTo (endTypeStack (getCardType card) stacks) [card]) stacks
  | otherwise = stacks

-- check if a card can be moved to its corresponding endingstack
canEnd :: Card -> Stack -> Bool
canEnd card stack
  | null stack = getCardValue card == A
  | otherwise = valToNumber (getCardValue card) == succ (valToNumber (getCardValue (last stack))) && getCardType card == getCardType (last stack)

-- move cards from the gamestacks to the endingstacks
stackToEnd :: Game -> Game
stackToEnd game = game {board = (board game) {gameStacks = replaceNth (fst (position (selector game))) (moveFrom (gameStacks (board game) !! fst (position (selector game))) (stacksize (gameStacks (board game) !! fst (position (selector game))) - 1)) (gameStacks (board game)), endingStacks = toEnd (gameStacks (board game) !! fst (position (selector game)) !! snd (position (selector game))) (endingStacks (board game))}}

--------------------------------------
-- move cards from one stack to another
--------------------------------------
moveCards :: Game -> Game
moveCards game = game {board = cboard {gameStacks = swapcards (gameStacks cboard) (fromJust (selected cselector)) (fst (position cselector))}, selector = cselector {selected = Nothing}}
  where
    cboard = board game
    cselector = selector game

swapcards :: [Stack] -> Coordinate -> Int -> [Stack]
swapcards stacks (x, y) z = replaceNth z (moveTo (stacks !! z) (reverse (take (stacksize (stacks !! x) - y) (reverse (stacks !! x))))) (replaceNth x (moveFrom (stacks !! x) y) stacks)

moveFrom :: Stack -> Int -> Stack
moveFrom stack h = showLowestCard (take h  stack)

moveTo :: Stack -> Stack -> Stack
moveTo stack1 stack2 = stack1 ++ stack2

--function to replace the nth stack with a new stack
replaceNth :: Int -> Stack -> [Stack] -> [Stack]
replaceNth _ _ [] = []
replaceNth n newVal (x : xs)
  | n == 0 = newVal : xs
  | otherwise = x : replaceNth (n - 1) newVal xs

------------------------------------------------------------------------
-- functions to place the elements at the correct location on the screen
------------------------------------------------------------------------

convertx :: Int -> Int -> Float
convertx location axis = ((fromIntegral location - fromIntegral axis / 2) + 1 / 2) * (fromIntegral (fst cardSize) + fromIntegral inset)

converty :: Int -> Int -> Float
converty location axis = (-(-(fromIntegral (snd cardSize - inset) / 2) + (fromIntegral location - fromIntegral axis / 2) * 20)) - 20

--------------------------------------------------------
-- rendering functions for every visual part of the game
--------------------------------------------------------
-- show back of card if hidden
renderCard :: Card -> Picture
renderCard card@(t, v, s)
  | s == Hidden = head symbols
  | s == Visible = lookupPicture card

-- show placehgolder if stack is empty
renderStack :: Stack -> Int -> Int -> Picture
renderStack stack x y
  | stacksize stack == 0 = translate (convertx x 7) (converty y 4) (symbols !! 1)
  | otherwise = pictures [translate (convertx x 7) (converty (y + i) 4) (renderCard (stack !! i)) | i <- [0 .. length stack - 1]]

renderGameStacks :: [Stack] -> Picture
renderGameStacks stacks = pictures [renderStack (stacks !! i) (0 + i) 0 | i <- [0 .. length stacks - 1]]

renderEndingStack :: Stack -> Int -> Picture
renderEndingStack stack x
  | stacksize stack == 0 = translate (convertx x 7) 225 (symbols !! 1)
  | otherwise = translate (convertx x 7) 225 (renderCard (last (showLowestCard stack)))

renderEndingStacks :: [Stack] -> Int -> Picture
renderEndingStacks endingstacks x = pictures [renderEndingStack (endingstacks !! i) (x + i) | i <- [0 .. length endingstacks - 1]]

renderPile :: Stack -> Picture
renderPile stack
  | stacksize stack == 0 = translate (convertx 0 7) 225 (symbols !! 1)
  | otherwise = translate (convertx 0 7) 225 (renderCard (last stack))

renderSelector :: Selector -> Picture
renderSelector selector= translate (convertx (fst (position selector)) 7) (converty (snd (position selector)) 4) (symbols !! 3)

renderSelected :: Selector -> Picture
renderSelected selector
  | isNothing (selected selector) = blank
  | otherwise = translate (convertx (fst (fromJust (selected selector))) 7) (converty (snd (fromJust (selected selector))) 4) (symbols !! 2)

renderBoard :: Board -> Picture
renderBoard board = pictures [renderGameStacks (gameStacks board), renderEndingStacks (endingStacks board) 3, renderPile (pile board)]

render :: Game -> Picture
render game = pictures [renderBoard (board game), renderSelector (selector game),renderSelected (selector game)]