import Sound.SC3 {- hsc3 -}

main :: IO ()
main = withSC3 serverStatus >>= mapM_ putStrLn
