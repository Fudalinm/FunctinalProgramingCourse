import qualified Data.ByteString as B
readAndWrite:: IO()
readAndWrite = do
		contents<- B.getContents
		B.putStr contents