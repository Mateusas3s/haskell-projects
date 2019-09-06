import Control.Exception
import System.IO
import System.IO.Error
import System.Process


type Players = [Player]
type Name = String
type Points = Int
type ToPlay = Int
type Table = [Char]
data Player = Player Name Points
            deriving(Show, Read)

getString :: String -> IO String
getString str = do
    putStr str
    res <- getLine
    return res

start :: IO ()
start = do 
    { catch (read_file) fix_error;}
    where
        read_file = do
        {
            file <- openFile "data.txt" ReadMode;
            dat <- hGetLine file;
            hClose file;
            menu (read dat);
            return()
        }
        fix_error error = if isDoesNotExistError error then do
        {
            file <- openFile "data.txt" WriteMode;
            hPutStrLn file "[]";
            hClose file;
            menu [];
            return()
        }
        else
            ioError error

menu :: Players -> IO Players
menu dat = do
    system "clear"
    putStrLn "----------------------------------------- JOGO DA VELHA -----------------------------------------"
    putStrLn "\nDigite 1 para cadastrar o jogador"
    putStrLn "Digite 2 para jogar"
    putStrLn "Digite 3 para visualizar o ranking"
    putStrLn "Digite 0 para sair"
    putStr "Opção: " 
    op <- getChar
    getChar
    execOption dat op

execOption :: Players -> Char -> IO Players
execOption dat '1' = registerPlayer dat


execOption dat '0' = do
    putStrLn("\nTchau!")
    system "clear"
    return dat

execOption dat _ = do
    putStrLn("\nOpção inválida! Digite uma opção válida.")
    putStr ("Pressione <Enter> para voltar ao menu...")
    getChar
    menu dat

registerPlayer :: Players -> IO Players
registerPlayer dat = do
    name <- getString "\nDigite um nome de usuário: "
    if(thereIsPlayer dat name) then do
        putStrLn "\nEsse nome já existe, escolha outro."
        putStr "\nPressione <Enter> para continuar..."
        getChar
        menu dat
    else do
        file <- openFile "data.txt" WriteMode
        hPutStrLn file (show((Player name 0): dat))
        hClose file
        putStrLn ("\nUsuário " ++ name ++ " cadastrado com sucesso.")
        putStr "\nPressione <Enter> para continuar..."
        getChar
        menu ((Player name 0): dat)

thereIsPlayer :: Players -> Name -> Bool
thereIsPlayer [] _ = False
thereIsPlayer ((Player n p):xs) name
                | (n == name) = True
                | otherwise = thereIsPlayer xs name
