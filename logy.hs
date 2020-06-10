import Data.List

type Castka = Integer

data Operace
  = Prihlaseni
  | Odhlaseni
  | Vyber Castka
  | Pripis Castka
  deriving (Show, Read, Eq)

type Cas = Integer

type Uzivatel = String

data Zaznam =
  Zaznam Cas
         Uzivatel
         Operace
  deriving (Show, Read, Eq)

type Zaznamy = [Zaznam]

main = do
  log <- (map read . lines <$> readFile "banka.log") :: IO [Zaznam] --nacteni a rozparsovani logu
  let result cmt f --pomocna funkce na vypisovani vysledku
       = do
        putStrLn (cmt ++ ":")
        print (f log)
        putChar '\n'
  {- pocitani a vypisovani vysledku zacina zde -}
  result
    "DEMO1 -- jmeno prvniho uzivatele v souboru se smichanymi zaznamy"
    demoPrvniZaznam
  result
    "DEMO2 -- pocet zaznamu se jmenem Marie"
    demoPocetMarie
  result "Seznam uzivatelu serazenych podle abecedy" serazeniUzivatele
  result "Casy top 10 nejvetsich vyberu" top10vyber
  result "Jmena uzivatelu 10 nejmensich pripisu" top10pripis
  result "Nejaktivnejsi uzivatel" topUzivatel
  result "Uzivatel ktery vydelal nejvic penez" topPrirustek
  result "BONUS: Prumerna vybrana castka uzivatelu zacinajicich od J" prumerVyberuJ
  result
    "BONUS: Uzivatel s nejdelsi posloupnosti akci nerusenou v logu jinymi uzivateli"
    nejdelsiSingleRun

-- příklad 1: Jméno uživatele prvního záznamu v logu
demoPrvniZaznam :: Zaznamy -> Uzivatel
demoPrvniZaznam ((Zaznam _ jm _):_) = jm

-- příklad 2: Počet záznamů se jménem Marie
demoPocetMarie :: Zaznamy -> Int
demoPocetMarie = length . filter uzivatelMarie
  where
    uzivatelMarie (Zaznam _ "Marie" _) = True
    uzivatelMarie _ = False
-- ekvivalentně:
-- demoPocetMarie zaznamy = length $ filter uzivatelMarie zaznamy
-- nebo:
-- demoPocetMarie zaznamy = length (filter uzivatelMarie zaznamy)

{- Ukol zacina tady. Misto `undefined` dodejte definice funkci, ktere z logu
 - vytahnou pozadovany vysledek. -}

-- Seznam uživatelů (bez duplicit), seřazený podle abecedy
serazeniUzivatele :: Zaznamy -> [Uzivatel]
serazeniUzivatele zaznamy = (sortUniq . (map getUser)) zaznamy

-- Časy deseti největších výběrů
top10vyber :: Zaznamy -> [Cas]
top10vyber zaznamy = take 10 (map getTime (sortBy (\ (Zaznam _ _ (Vyber aAmount)) (Zaznam _ _ (Vyber bAmount)) -> compare bAmount aAmount) (filterWithdraw zaznamy)))

-- Jména uživatelů, kterým přišlo deset nejmenších přípisů (bez opakování jmen)
top10pripis :: Zaznamy -> [Uzivatel]
top10pripis zaznamy = nub (take 10 (map getUser (sortBy (\ (Zaznam _ _ (Pripis aAmount)) (Zaznam _ _ (Pripis bAmount)) -> compare aAmount bAmount) (filterDeposit zaznamy))))

-- Jméno uživatele, který je nejaktivnější (tj. má v logu nejvíc záznamů)
topUzivatel :: Zaznamy -> Uzivatel
topUzivatel zaznamy = getUser (head (head(map (\ (_, a) -> a) (sortByLength (mapWithLength (groupSortedUsers zaznamy))))))

-- Jméno uživatele, kterému na účtu přibylo nejvíc peněz (tj. má maximální součet příjmů mínus součet výdajů)
topPrirustek :: Zaznamy -> Uzivatel
topPrirustek zaznamy =  getUser (head (head(map (\ (_, a) -> a) (sortByLength (mapWithMoney (groupSortedUsers (filterMoneyOperations zaznamy)))))))

-- BONUS: Průměrná částka (oříznutá na celé číslo), kterou vybrali uživatelé začínající od J
prumerVyberuJ :: Zaznamy -> Castka
prumerVyberuJ zaznamy = countAvarege (map getMoney (filterWithdraw (filterNameAfter zaznamy 'J')))

-- BONUS: Jméno uživatele, který provedl nejvíc akcí za sebou bez toho, aby jakýkoliv jiný uživatel cokoliv udělal (tj. po seřazení logu podle času bude mít “nejvíc řádků po sobě”)
nejdelsiSingleRun :: Zaznamy -> Uzivatel
nejdelsiSingleRun zaznamy = getUser (head (mapToZaznamy (head (sortByLength (mapCount (groupUsers (sortByTime zaznamy)))))))
  where
    mapCount zaznamy = map (\ zaznam -> (length zaznam, zaznam)) zaznamy
    mapToZaznamy (number, zaznamy) = zaznamy






getTime :: Zaznam -> Cas
getTime (Zaznam time _ _) = time

getUser :: Zaznam -> Uzivatel
getUser (Zaznam _ user _) = user

getOperation :: Zaznam -> Operace
getOperation (Zaznam _ _ operation) = operation

getMoney :: Zaznam -> Castka
getMoney (Zaznam _ _ (Vyber money)) = money
getMoney (Zaznam _ _ (Pripis money)) = money


groupSortedUsers :: [Zaznam] -> [[Zaznam]]
groupSortedUsers zaznamy = groupBy (\ (Zaznam _ userA _) (Zaznam _ userB _) -> userA == userB) (sortBy (\ (Zaznam _ userA _) (Zaznam _ userB _) -> compare userA userB) zaznamy)

groupUsers :: [Zaznam] -> [[Zaznam]]
groupUsers zaznamy = groupBy (\ (Zaznam _ userA _) (Zaznam _ userB _) -> userA == userB) zaznamy

mapWithLength :: [[Zaznam]] -> [(Int, [Zaznam])]
mapWithLength zaznamy = map (\ zaznam -> (length zaznam, zaznam)) zaznamy

mapWithMoney :: [[Zaznam]] -> [(Int, [Zaznam])]
mapWithMoney zaznamy = map (\ zaznam -> (fromIntegral(foldl suprSum 0 zaznam), zaznam)) zaznamy
  where
    suprSum sum (Zaznam _ _ (Vyber number)) = sum - number
    suprSum sum (Zaznam _ _ (Pripis number)) = sum + number


sortByLength :: [(Int, [Zaznam])] -> [(Int, [Zaznam])]
sortByLength zaznamy = sortBy (\ (numberA, _) (numberB, _) -> compare numberB numberA) zaznamy

sortByTime :: [Zaznam] -> [Zaznam]
sortByTime zaznamy = sortBy (\ (Zaznam timeA _ _) (Zaznam timeB _ _) -> compare timeA timeB) zaznamy

sortUniq :: (Ord a) => [a] -> [a]
sortUniq a = (nub . sort) a

filterWithdraw :: [Zaznam] -> [Zaznam]
filterWithdraw zaznamy = filter (\ (Zaznam _ _ operace) -> matchOperation operace) zaznamy
  where
    matchOperation (Vyber _) = True
    matchOperation _ = False 

filterDeposit :: [Zaznam] -> [Zaznam]
filterDeposit zaznamy = filter (\ (Zaznam _ _ operace) -> matchOperation operace) zaznamy
  where
    matchOperation (Pripis _) = True
    matchOperation _ = False 

filterMoneyOperations :: [Zaznam] -> [Zaznam]
filterMoneyOperations zaznamy = filter (\ (Zaznam _ _ operace) -> matchOperation operace) zaznamy
  where
    matchOperation (Pripis _) = True
    matchOperation (Vyber _) = True
    matchOperation _ = False 

filterNameAfter :: [Zaznam] -> Char -> [Zaznam]
filterNameAfter zaznamy char = filter (\ (Zaznam _ (chars:_) _) -> chars >= char) zaznamy

countAvarege :: [Castka] -> Castka
countAvarege castky = div (foldl (\ sum number -> number + sum ) 0 castky) (toInteger(length castky))