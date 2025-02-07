import Board (fenToimage)

main :: IO ()
main = do
    let fen = "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"
    let move = ("g1", "f3") 
    let filePath = "board.png"
    let width = 800
    fenToimage fen move width filePath

