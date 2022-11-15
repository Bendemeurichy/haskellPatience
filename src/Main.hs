import Logica
import Graphics.Gloss

main :: IO ()
main = play window green fps initGame render handleInput step