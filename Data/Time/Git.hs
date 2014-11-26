{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Time.Git (approxidate, io_approxidate, posixToUTC) where

import           Data.Time
import           Data.Time.Clock.POSIX

import           Foreign               hiding (unsafePerformIO)
import           Foreign.C.String
import           Foreign.C.Types

import           System.IO.Unsafe

import qualified Data.ByteString       as BS
import qualified Data.ByteString.UTF8  as BS (fromString)

foreign import ccall unsafe "date.c approxidate_careful"
	c_approxidate_careful ::
	CString -> Ptr CInt ->
	IO CULong

-- | Parse a date/time string and return Just a Unix timestamp.
--   Return Nothing if the string could not be interpreted.
--   If no timezone is in the string, the local timezone is used.
--   This is not in IO and you cannot safely ask it to parse strings like
--   \"now\" and \"yesterday\".
approxidate :: String -> Maybe Integer
approxidate = unsafePerformIO . io_approxidate

-- | Convert a Unix timestamp to a UTCTime
posixToUTC :: Integer -> UTCTime
posixToUTC = posixSecondsToUTCTime . realToFrac

-- | Parse a date/time string and return Just a Unix timestamp.
--   Return Nothing if the string could not be interpreted.
--   If no timezone is in the string, the local timezone is used.
--   This is in IO so that you can safely ask it to parse strings like
--   \"now\" and \"yesterday\".
io_approxidate :: String -> IO (Maybe Integer)
io_approxidate input =
	BS.useAsCString (BS.fromString input) (\c_input ->
		alloca (\c_error_ret -> do
			poke c_error_ret 0
			time <- c_approxidate_careful c_input c_error_ret
			err <- peek c_error_ret
			if err == 0 then return $ Just (toInteger time) else
				return Nothing
		)
	)
