
main::IO()
main = do
	putStrLn "hello"

sq :: String -> String
sq s@[c]                     = s
sq ('"':s)  | last s == '"'  = init s
        | otherwise          = s
sq s        | last s == '"'  = init s
        | otherwise          = s