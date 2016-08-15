import Control.Concurrent

validCharsFor :: Char -> [Char]
validCharsFor '('  = "{"
validCharsFor '{'  = "["
validCharsFor '['  = "{[("
validCharsFor _ = " "

correspondingChar :: Char -> Char
correspondingChar '(' = ')'
correspondingChar '{' = '}'
correspondingChar '[' = ']'
correspondingChar _ = ' '

solve :: String -> Bool
solve (str) = innerSolve str ""
					where	innerSolve [] [] = True;
							innerSolve [] _ = False;
							innerSolve (c:cs) "" = innerSolve cs [c];
							innerSolve (c:cs) (s:stack) 
								| closeBraces = innerSolve cs stack
								| newBrace = if c `elem` validCharsFor s
												then innerSolve cs ([c] ++ [s] ++ stack)
												else False
									where 	closeBraces = c == correspondingChar s;
											newBrace = not closeBraces

main = do
		getLine >>= (\str -> forkIO (putStrLn (str ++ "  " ++ (show $ solve str )  )))
		main