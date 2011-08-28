module Main where

import Data.Random
import Data.Random.Source.DevRandom
import System.IO
import Control.Monad
import Control.Monad.Loops

data Suit = Hearts | Spades | Diamonds | Clubs
          deriving(Show)

data CardValue = Ace | King | Queen | Jack | NumCard Int
               deriving(Eq)

data Card = Card { suit :: Suit
                 , value :: CardValue
                 }

type Deck = [Card]
type Hand = [Card]

data CurrentHand = FirstHand | SplitHand
                 deriving(Show, Eq)

-- I am only allowing one level of splitting
data CardPlayer = CardPlayer { name :: String
                             ,  hand :: Hand
                             ,  splitHand :: Hand
                             ,  currentHand :: CurrentHand
                             ,  money :: Int
                             ,  bet :: Int
                             }
                deriving(Show)

instance Show CardValue where
  show Ace = "Ace"
  show King = "King"
  show Queen = "Queen"
  show Jack = "Jack"
  show (NumCard x) = show x

instance Show Card where
  show card = show (value card) ++ " of " ++ show (suit card)

prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine

getValidChoice msg options =
  iterateUntil p x
  where
    x = prompt msg
    p = (`elem` options)

-- Apparently I should avoid I shouldn't be taking the length when unnecessary?
lengthIs [] 0 = True
lengthIs _ 0 = False
lengthIs [] _ = False
lengthIs (x:xs) l = lengthIs xs (l-1)

count p = length . filter p

-- Black Jack card values.. Ace is 11 unless changed down
cardValueWorth :: CardValue -> Int
cardValueWorth Ace = 11
cardValueWorth (NumCard x) = x
cardValueWorth _ = 10

cardWorth :: Card -> Int
cardWorth = cardValueWorth . value

handWorth :: Hand -> Int
handWorth hand =
  helper naiveWorth numAces
  where
    numAces = count (==Ace) (map value hand)
    naiveWorth = sum $ map cardWorth hand
    helper worth aces
      | aces == 0 = worth
      | worth <= 21 = worth
      | otherwise = helper (worth - 10) (aces - 1)

-- Unshuffled
standardDeck :: Deck
standardDeck = [ Card {suit=s, value=v} | s<-[Hearts,Spades,Diamonds,Clubs], v<-map NumCard [2..10] ++ [Jack,Queen,King,Ace]]

createShuffledDeck :: IO Deck
createShuffledDeck = sampleFrom DevURandom (shuffle standardDeck)

drawCard :: CardPlayer -> Card -> CardPlayer
drawCard player card =
  case currentHand player of
    FirstHand -> player { hand=card:hand player }
    _ -> player { splitHand=card:splitHand player }

dealOutCards :: [CardPlayer] -> Deck -> [CardPlayer]
dealOutCards = zipWith drawCard

readableList [] = "empty"
readableList [x] = show x
readableList (x:xs) =
  show x ++ concatMap ((',':) . (' ':) . show) (init xs) ++ " and " ++ show (last xs)

getHand player =
  case currentHand player of
    FirstHand -> hand player
    _ -> splitHand player

getPlayerNames =
  f `untilM` p
  where
    f = prompt "Enter the player's name: "
    p = (=="n") `liftM` getValidChoice "Any more players? (y/n): " ["y","n"]

takeBet player =
  iterateUntil p x
  where
    x = (read `liftM` prompt ("How much would you like to bet " ++ name player ++ "? ")) :: IO Int
    p bet = bet >= 0 && bet <= money player

turnAction player deck "hit" = ( drawCard player (head deck), tail deck )
turnAction player deck "stay" = ( player, deck )
turnAction player deck "double down" = ( np, tail deck )
  where
    p = drawCard player (head deck)
    -- Note if bet is 0 then p = np
    np = p { bet=(2 * bet p) }
turnAction player deck "split" = ( np, drop 2 deck )
  where
    np = player { hand=(head deck):(drop 1 $ hand player), splitHand=[(head $ hand player),(deck !! 2)] }

showMove choice player
  | choice == "stay" = name player ++ " stays with a total of " ++ show playerHandWorth
  | choice == "hit" && playerHandWorth > 21 = name player ++ " drew a " ++ show mostRecentCard ++ " and went Bust"
  | choice == "hit" = name player ++ " drew a " ++ show mostRecentCard
  | choice == "double down" = name player ++ " doubles down, drawing one card and ending the turn"
  | otherwise = name player ++ " splits"
  where
    playerHand = getHand player
    mostRecentCard = head playerHand
    playerHandWorth = handWorth playerHand

endMove choice player deck
  | stopping && FirstHand == currentHand player && not (null (splitHand player)) =
    takeTurn (player { currentHand = SplitHand } ) deck
  | stopping = return (player, deck)
  | otherwise = takeTurn player deck
  where
    stopping = choice == "stay" || choice == "double down" || handWorth (getHand player) >= 21
               
takeTurn player deck = do
  putStrLn $ "You have a " ++ readableList playerHand ++ "."
  putStrLn $ "This totals to " ++ show playerHandWorth ++ "."
  putStrLn $ "Your options are to " ++ readableList availableOptions ++ "."
  choice <- getValidChoice "What would you like to do? " availableOptions
  let (player', deck') = turnAction player deck choice
  putStrLn $ showMove choice player'
  putStrLn ""
  endMove choice player' deck'
  where
    playerHand = getHand player
    playerHandWorth = handWorth playerHand
    allOptions = [ (playerHandWorth < 21, "hit"),
                   (playerHandWorth <= 21, "stay"),
                   (playerHandWorth < 21 && playerHand `lengthIs` 2 && 2 * bet player <= money player, "double down"),
                   (FirstHand == currentHand player
                    && playerHand `lengthIs` 2
                    && cardWorth (head playerHand) == cardWorth (playerHand !! 1),
                    "split")
                 ]
    availableOptions = map snd $ filter fst allOptions

takeTurnsHelper accum (p:players) deck = do
  putStrLn $ "It is your turn " ++ name p ++ "."
  (p', deck') <- takeTurn p deck
  takeTurnsHelper (p':accum) players deck'
takeTurnsHelper accum [] deck = return accum

takeTurns = takeTurnsHelper []  

startingMoney = 100

setupGame = do
  names <- getPlayerNames
  shuffled <- createShuffledDeck
  return (names, shuffled)

playGame names shuffled = do
  putStrLn $ "Each player starts with " ++ show startingMoney ++ " dollars"
  bets <- mapM takeBet players''
  let players''' = zipWith (\p b->p { bet=b }) players'' bets
  putStrLn $ "The dealer has a " ++ show (head $ hand $ head players') ++ " face up."
  let deck = (drop (2 * length players) shuffled)
  takeTurns players''' deck

  where
    defaultPlayer n = CardPlayer { name=n, hand=[], splitHand=[], currentHand=FirstHand, money=startingMoney, bet=0 }
    players = map defaultPlayer ("Dealer":names)
    players' = players `dealOutCards` shuffled `dealOutCards` drop (length players) shuffled
    players'' = reverse $ tail players'
    
main = do
  putStrLn " -- Black Jack -- "
  (n, s) <- setupGame
  playGame n s
