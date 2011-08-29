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

getValidChoice :: String -> [String] -> IO String
getValidChoice msg options =
  iterateUntil p x
  where
    x = prompt msg
    p = (`elem` options)

-- Apparently I should avoid I shouldn't be taking the length when unnecessary?
lengthIs :: Num a => [t] -> a -> Bool
lengthIs [] 0 = True
lengthIs _ 0 = False
lengthIs [] _ = False
lengthIs (x:xs) l = lengthIs xs (l-1)

count :: (a -> Bool) -> [a] -> Int
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

readableList :: Show a => [a] -> String
readableList [] = "empty"
readableList [x] = show x
readableList (x:xs) =
  show x ++ concatMap ((',':) . (' ':) . show) (init xs) ++ " and " ++ show (last xs)

getHand :: CardPlayer -> Hand
getHand player =
  case currentHand player of
    FirstHand -> hand player
    _ -> splitHand player

getPlayerNames :: IO [String]
getPlayerNames =
  f `untilM` p
  where
    f = prompt "Enter the player's name: "
    p = (=="n") `liftM` getValidChoice "Any more players? (y/n): " ["y","n"]

takeBet :: CardPlayer -> IO Int
takeBet player =
  iterateUntil p x
  where
    x = (read `liftM` prompt ("How much would you like to bet " ++ name player ++ "? ")) :: IO Int
    p bet = bet >= 0 && bet <= money player

turnAction :: CardPlayer -> Deck -> String -> (CardPlayer, Deck)
turnAction player deck "hit" = ( drawCard player (head deck), tail deck )
turnAction player deck "stay" = ( player, deck )
turnAction player deck "double down" = ( np, tail deck )
  where
    p = drawCard player (head deck)
    -- Note if bet is 0 then p = np
    np = p { bet=2 * bet p }
turnAction player deck "split" = ( np, drop 2 deck )
  where
    np = player { hand=head deck:drop 1 (hand player), splitHand=[head $ hand player,deck !! 2] }

showMove :: String -> CardPlayer -> String
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

endMove :: String -> CardPlayer -> Deck -> IO (CardPlayer, Deck)
endMove choice player deck
  | stopping && FirstHand == currentHand player && not (null (splitHand player)) =
    takeTurn (player { currentHand = SplitHand } ) deck
  | stopping = return (player, deck)
  | otherwise = takeTurn player deck
  where
    stopping = choice == "stay" || choice == "double down" || handWorth (getHand player) >= 21
               
takeTurn :: CardPlayer -> Deck -> IO (CardPlayer, Deck)
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

takeTurns' ::  [CardPlayer] -> [CardPlayer] -> Deck -> IO ([CardPlayer], Deck)
takeTurns' accum (p:players) deck = do
  putStrLn $ "It is your turn " ++ name p ++ "."
  (p', deck') <- takeTurn p deck
  takeTurns' (p':accum) players deck'
takeTurns' accum [] deck = return (accum, deck)

takeTurns ::  [CardPlayer] -> Deck -> IO ([CardPlayer], Deck)
takeTurns = takeTurns' []

startingMoney = 100

dealerTurn' :: Hand -> Deck -> IO Hand
dealerTurn' dhand (c:deck)
  | worth > 21 = putStrLn "The dealer bust!" >> return dhand
  | worth >= 17 = putStrLn "The dealer stays" >> return dhand
  | otherwise = do
    putStrLn "The dealer hits"
    putStrLn $ "He draws a " ++ show c ++ " bringing him to a total of " ++ show (handWorth (c:dhand))
    dealerTurn' (c:dhand) deck
  where
    worth = handWorth dhand

dealerTurn :: CardPlayer -> [Card] -> IO Hand
dealerTurn dealer deck = do
  putStrLn $ "The dealer has a " ++ readableList dealerHand ++ "."
  dealerTurn' dealerHand deck
    where
      dealerHand = hand dealer

payout' dhand phand pbet
  | pworth > 21 = -pbet
  | dworth > 21 = pbet
  | pworth > dworth = pbet
  | pworth < dworth = -pbet
  | otherwise = 0
  where
    dworth = handWorth dhand
    pworth = handWorth phand

describePayout p
  | p > 0 = " won " ++ show p
  | p < 0 = " lost " ++ show (-p)
  | otherwise = " pushed"

dhand `payout` player = do
  putStr $ name player ++ describePayout pay
  if null (splitHand player)
    then putStrLn "" >> return player { money=pay + money player }
    else do
    putStrLn $ " and with their split hand" ++ describePayout spay
    return player { money=pay + spay + money player }
  where
    pay = payout' dhand (hand player) pbet
    spay = payout' dhand (splitHand player) pbet
    pbet = bet player

gameIteration d ps shuffled = do
  mapM (\p -> putStrLn $ name p ++ " has " ++ show (money p) ++ " dollars") players
  bets <- mapM takeBet players
  let players' = zipWith (\p b->p { bet=b }) players bets
  putStrLn $ "The dealer has a " ++ show (head $ hand dealer) ++ " face up."
  (players'', deck') <- takeTurns players' deck
  dhand <- dealerTurn dealer deck'
  mapM (dhand `payout`) players''
  where
    numPlayers = length (d:ps)
    deck = drop (2 * numPlayers) shuffled
    dealer:players = (d:ps) `dealOutCards` shuffled `dealOutCards` drop numPlayers shuffled    
    
createPlayers names =
  return (map defaultPlayer ("Dealer":names))
  where
    defaultPlayer n = CardPlayer { name=n, hand=[], splitHand=[], currentHand=FirstHand, money=startingMoney, bet=0 }
  
gameLoop (dealer:players) = do
  np <- f
  done <- p
  unless done (gameLoop (dealer:np))
  where
    f = createShuffledDeck >>= (gameIteration dealer players)
    p = (=="n") `liftM` getValidChoice "Play again? (y/n): " ["y","n"]

main = do
  putStrLn " -- Black Jack -- "
  getPlayerNames >>= createPlayers >>= gameLoop
