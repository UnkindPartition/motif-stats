module CmdLine (main) where

import Options.Applicative
import Control.Monad (join)
import qualified Control.Monad.Trans.Resource as R
import Streaming as S
import qualified Streaming.Prelude as S
import qualified Data.ByteString.Streaming.Char8 as SB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lex.Integral as L
import System.IO
import IUPAC
import DFA

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> progDesc "Calculate the probability of a motif occurrence in an interval"
  )
  where
    parser :: Parser (IO ())
    parser =
      work
        <$> switch
            (  long "raw"
            <> short 'r'
            <> help "take a list of interval lengths as an input (the default is BED file)"
            )
        <*> flag RC NoRC
            ( long "norc"
            <> help "consider only motif itself (default: consider also its reverse complement"
            )
        <*> optional (strOption
            ( short 'i'
            <> help "the input file (default: stdin)"
            ))
        <*> optional (strOption
            ( short 'o'
            <> help "the output file (default: stdout)"
            ))
        <*> strArgument
            ( metavar "MOTIF"
            <> help "motif represented as a string of IUPAC codes"
            )

rawLengths, bedLengths :: MonadIO m => Handle -> Stream (Of Int) m ()
rawLengths = S.map readNum . mapsM SB.toStrict . SB.lines . SB.hGetContents
bedLengths = S.map fromBed . mapsM SB.toStrict . SB.lines . SB.hGetContents
  where
    fromBed bs =
      let fields = BS.split '\t' bs
      in case map readNum . take 2 . drop 1 $ fields of
        [start, end] -> end - start
        _ -> error $ "Not enough fields in a BED record: " ++ show (length fields)

readNum :: BS.ByteString -> Int
readNum bs =
  case L.readDecimal bs of
    Just (n, rest) | BS.null rest -> n
    _ -> error $ "Invalid number: " ++ show bs

work :: Bool -> RC -> Maybe FilePath -> Maybe FilePath -> String -> IO ()
work raw rc inp_file outp_file motif = R.runResourceT $ do
  inp_h <- case inp_file of
    Nothing -> return stdin
    Just path -> snd <$> R.allocate (openFile path ReadMode) hClose
  outp_h <- case outp_file of
    Nothing -> return stdout
    Just path -> snd <$> R.allocate (openFile path ReadMode) hClose
  let tm = dfaTransferMatrix . nfaToDfa primitiveCodes . motifToNFA rc $ motif
      inp_s
        | raw = rawLengths inp_h
        | otherwise = bedLengths inp_h
  S.mapM_ (liftIO . hPrint outp_h . dfaProbability tm) inp_s
