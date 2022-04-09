import Distribution.Simple.Setup (trueArg)
myNull xs = case xs of
                []  -> True
                _   -> False                