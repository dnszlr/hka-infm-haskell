import Data.IORef (newIORef, writeIORef, readIORef)
-- Vorlesung vom 24.03.2022

main = do {
    print "What is your name?";
    name <- getLine;
    print("Hello " ++ name ++ "!");
    print "Your age?";
    age <- getLine;
    x <- newIORef(read age :: Int); -- x = x - 5; Beispiel in Java
    v <- readIORef x;
    writeIORef x (v - 5);
    v <- readIORef x; -- End Beispiel
    print("Your actual age: " ++ show v)
}