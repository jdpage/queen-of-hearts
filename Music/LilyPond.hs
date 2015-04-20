module Music.LilyPond where

import Control.Monad.State.Lazy
import qualified Control.Monad.Trans.Class as Trans (lift)
import Music
import System.IO

type Engraver a = StateT (Key, Handle) IO a

data Clef = Treble | Bass deriving (Eq, Show)

engrave :: Engraver a -> Handle -> IO a
engrave e h = evalStateT e (keyC, h)

emit :: String -> Engraver ()
emit s = do
    (_, h) <- get
    Trans.lift $ hPutStr h s

emitLn :: String -> Engraver ()
emitLn s = do
    (_, h) <- get
    Trans.lift $ hPutStrLn h s

getKey :: Engraver Key
getKey = gets $ \(key, _) -> key

putKey :: Key -> Engraver ()
putKey k = do
    (_, h) <- get
    put (k, h)

key :: Key -> Engraver ()
key k = do
    putKey k
    emit "\\key "
    let s = sharpCount k
    let pc = pitchClassOf k
    if pc `elem` naturals then
        emit $ nameOf pc
    else if s < 0 then
        emit $ nameOfFlat pc
    else
        emit $ nameOfSharp pc
    emitLn " \\major"

clef :: Clef -> Engraver ()
clef c
    | c == Treble   = clef' "treble"
    | c == Bass     = clef' "bass"
    where
        clef' c = emitLn $ "\\clef " ++ c

version :: String -> Engraver ()
version v = emitLn $ "\\version \"" ++ v ++ "\""

bar :: String -> Engraver ()
bar b = emit $ " \\bar \"" ++ b ++ "\" "

showOctave :: Octave -> Engraver String
showOctave n
    | n == 3   = return ""
    | n < 3    = return $ replicate (abs $ n - 3) ','
    | n > 3    = return $ replicate (n - 3) '\''

nameOf :: PitchClass -> String
nameOf p
    | p == pcC   = "c"
    | p == pcD   = "d"
    | p == pcE   = "e"
    | p == pcF   = "f"
    | p == pcG   = "g"
    | p == pcA   = "a"
    | p == pcB   = "b"
    | otherwise  = error "not natural"

nameOfSharp :: PitchClass -> String
nameOfSharp pc = (nameOf $ naturalOfSharp pc) ++ "is"

nameOfFlat :: PitchClass -> String
nameOfFlat pc = (nameOf $ naturalOfFlat pc) ++ "es"

showPitch :: Pitch -> Engraver String
showPitch p = do
    oct <- showOctave $ octaveOf p
    let pc = pitchClassOf p
    if pc `elem` naturals then
        return $ nameOf pc ++ oct
    else do
        key <- getKey
        let s = sharpCount key
        if p `inKey` key then
            if s > 0 then
                return $ nameOfSharp pc ++ oct
            else
                return $ nameOfFlat pc ++ oct
        else
            if s > 0 then
                return $ nameOfFlat pc ++ oct
            else
                return $ nameOfSharp pc ++ oct

part :: Engraver a -> Engraver a
part notes = do
    key <- getKey
    emitLn "{"
    x <- notes
    emitLn "}"
    putKey key
    return x
