{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Time.Git (approxidate, approxidateIO) where

import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Foreign hiding (unsafePerformIO)
import Foreign.C.String
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)
import UnexceptionalIO (Unexceptional)
import qualified UnexceptionalIO as UIO

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS (fromString)

foreign import ccall unsafe "date.c approxidate_careful"
	c_approxidate_careful ::
	CString -> Ptr CInt ->
	IO CULong

-- | Parse a date/time string and return Just a Unix timestamp.
--   Return Nothing if the string could not be interpreted.
--   If no timezone is in the string, the local timezone is used.
approxidate :: String -> Maybe UTCTime
approxidate = unsafePerformIO . approxidateIO

-- | Parse a date/time string and return Just a Unix timestamp.
--   Return Nothing if the string could not be interpreted.
--   If no timezone is in the string, the local timezone is used.
--   This is in IO so that you can ask it to parse strings like
--   \"now\" and \"yesterday\".
approxidateIO :: (Unexceptional m) => String -> m (Maybe UTCTime)
approxidateIO input =
	(fmap . fmap) (posixSecondsToUTCTime . realToFrac) $
	UIO.unsafeFromIO $
	BS.useAsCString (BS.fromString input) (\c_input ->
		alloca (\c_error_ret -> do
			poke c_error_ret 0
			time <- c_approxidate_careful c_input c_error_ret
			err <- peek c_error_ret
			if err == 0 then return $ Just (toInteger time) else
				return Nothing
		)
	)
