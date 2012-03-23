{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Time.Git (approxidate, approxidateUTC) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Data.Time
import Data.Time.Clock.POSIX

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS (fromString)

-- | Parse a date/time string and return Just a Unix timestamp.
--   Return Nothing if the string could not be interpreted.
--   If no timezone is in the string, the local timezone is used.
approxidate :: String -> Maybe Integer
approxidate = unsafePerformIO . io_approxidate_careful

-- | The same as approxidate, but returns a UTCTime instead of a raw
--   timestamp.
approxidateUTC :: String -> Maybe UTCTime
approxidateUTC input =
	fmap (posixSecondsToUTCTime . realToFrac) (approxidate input)

foreign import ccall unsafe "date.c approxidate_careful"
	c_approxidate_careful ::
	CString -> Ptr CInt ->
	IO CULong

io_approxidate_careful :: String -> IO (Maybe Integer)
io_approxidate_careful input =
	BS.useAsCString (BS.fromString input) (\c_input ->
		alloca (\c_error_ret -> do
			poke c_error_ret 0
			time <- c_approxidate_careful c_input c_error_ret
			err <- peek c_error_ret
			if err == 0 then return $ Just (toInteger time) else
				return Nothing
		)
	)
