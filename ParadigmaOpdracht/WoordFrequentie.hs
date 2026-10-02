-- Commando:  runghc WoordFrequentie.hs tekst.txt


-- isLetter controleert of een teken een letter is; toLower maakt van een letter een kleine letter. 
import Data.Char (isLetter, toLower)

-- sortBy sorteert een lijst op basis van een eigen vergelijkfunctie.
import Data.List (sortBy)

-- qualified betekent dat de functies alleen bereikbaar zijn via die naam. Dus Map.insertWith en niet gewoon insertWith.
-- Dat is nodig omdat Data.Map.Strict functies bevat die dezelfde naam hebben als standaardfuncties. 
import qualified Data.Map.Strict as Map

-- comparing en Down worden gebruikt bij het sorteren.
-- comparing maakt een vergelijkfunctie die twee elementen vergelijkt op
-- het onderdeel dat meegegeven wordt.
-- Down draait de vergelijking om, zodat de hoogste aantallen eerst komen.
import Data.Ord (Down (..), comparing)

-- getArgs geeft de argumenten van het commando als een lijst terug.
import System.Environment (getArgs)

-- System.IO bevat functies om bestanden te openen en lezen.
import System.IO 

--Functie splitst de tekst op in woorden met kleine letters.
woorden :: String -> [String] 
woorden tekst =
  -- dropWhile gooit tekens weg zolang deze voorwaarde klopt: "not . isLetter" betekent dat het geen letter is
  case dropWhile (not . isLetter) tekst of 
    []   -> [] -- Niets over, tekst is klaar.
    rest -> -- Nu begint "rest" met een letter.
      let (woord, overig) = neemWoord rest -- neemWoord knipt het eerste woord af en geeft een paar terug: het woord en de rest van de tekst.
       -- "map toLower woord" maakt het woord klein. 
       -- De operator : zet dat woord vooraan een lijst, 
       -- de lijst is het resultaat van dezelfde functie woorden op de rest. 
       -- Zo bouwt de recursie de lijst woord voor woord op.
       in map toLower woord : woorden overig 

--Functie haalt een woord uit de tekst
neemWoord :: String -> (String, String)
neemWoord (teken : rest) -- (teken : rest) splitst de tekst in het eerste teken en de rest.
-- | is een voorwaarde. 
-- Is het teken een letter, dan hoort het bij het woord. Recursief wordt de rest van het woord opgehaald met de overgebleven tekst, 
-- en het teken wordt er weer voor gezet.
  | isLetter teken = let (woord, overig) = neemWoord rest in (teken : woord, overig) 
  -- Dit zijn drie voorwaarden gescheiden door kommas, en ze moeten alle drie kloppen:
  -- 1. teken is een apostrof
  -- 2. "(volgTeken : _) <- rest" kijkt of er na de apostrof nog een teken komt. Zo ja,
  --    dan heet dat teken volgTeken. _ betekent: alles wat na het volgteken komt wordt genegeert. Is rest leeg,
  --    dan past het patroon niet en klopt deze voorwaarde niet.
  -- 3. Het volgende teken volgTeken is een letter.
  -- Dan hoort de apostrof bij het woord en wordt hij eraan vastgezet.
  | teken == '\'', (volgTeken : _) <- rest, isLetter volgTeken = 
      let (woord, overig) = neemWoord rest in (teken : woord, overig)
-- Is de tekst leeg, of klopt geen enkele voorwaarde hierboven, 
-- dan valt Haskell door naar deze regel: het woord is afgelopen.
neemWoord tekst = ([], tekst) 

--Functie telt hoe vaak elk woord voorkomt.
telWoorden :: [String] -> Map.Map String Int
telWoorden [] = Map.empty
-- (w : ws) splitst de lijst in het eerste woord w en de rest ws
-- "telWoorden ws" is een recursieve aanroep. Deze telt alle woorden na het eerste, en geeft daarvoor een map terug.
-- "Map.insertWith (+) w 1" voegt het woord w toe aan de map. 
-- Staat het woord er nog niet in, dan komt het erbij met aantal 1. Staat het er al in, dan telt (+) er 1 bij 
-- het bestaande aantal op. 
telWoorden (w : ws) = Map.insertWith (+) w 1 (telWoorden ws)

--Functie sorteert op aantal
sorteerOpAantal :: Map.Map String Int -> [(String, Int)]
-- 1. Map.toList zet de map om in een lijst van paren (woord, aantal), alfabetisch op woord.
-- 2. snd pakt het tweede deel van een paar, het aantal. 
--    Down draait de volgorde om (hoog naar laag). comparing maakt van die functie een vergelijking van twee paren.
-- 3. sortBy sorteert met die vergelijking.
sorteerOpAantal = sortBy (comparing (Down . snd)) . Map.toList

--Funcite stelt de codering in op UTF-8 en leest de inhoud van een bestand.
leesInhoud :: Handle -> IO String
leesInhoud handle = do
  hSetEncoding handle utf8
  hGetContents' handle

--Functie opent een bestand, leest de inhoud en sluit het bestand weer.
leesBestand :: FilePath -> IO String
  -- withFile opent een bestand, voert er iets mee uit en sluit het daarna automatisch.
  -- Krijgt drie argumenten: pad (het bestand), ReadMode (Bestand moet gelezen worden) 
  -- en een functie die zegt wat er met het bestand moet gebeuren.
leesBestand pad = withFile pad ReadMode leesInhoud

--Functie maakt van een paar een tekstregel
toonRegel :: (String, Int) -> String
toonRegel (woord, aantal) = woord ++ ": " ++ show aantal

main :: IO ()
main = do
  [pad] <- getArgs
  -- Bestand wordt gelezen en een lijst van de woorden gesorteerd op aantal wordt gemaakt
  tekst <- leesBestand pad
  let resultaat = sorteerOpAantal (telWoorden (woorden tekst))
  -- mapM_ voert een actie uit voor elk element van de lijst: het printen van het element
  mapM_ (putStrLn . toonRegel) resultaat