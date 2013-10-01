import Test.Framework (defaultMain, Test)
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Data.Int
import Data.Time.Git
import System.Locale
import Data.Time
import Data.Time.Clock.POSIX
import System.IO.Unsafe

-- utcToLocalZonedTime is safeish because timezone won't change during testing
toLocal :: UTCTime -> ZonedTime
toLocal = unsafePerformIO . utcToLocalZonedTime

newtype PositiveTime = PositiveTime ZonedTime deriving (Show)

instance Arbitrary PositiveTime where
	arbitrary =
		fmap (PositiveTime . toLocal . posixSecondsToUTCTime . fromIntegral) $
		choose (0, maxBound::Int32) -- Should be able to use CULong, but that failed

newtype DateFormat = DateFormat String deriving (Show, Eq)

instance Arbitrary DateFormat where
	arbitrary = elements $ map DateFormat $ [
			"%c",
			"%Y-%m-%d %H:%M:%S%Q%z",
			"%B %d, %Y %l:%M:%S %P",
			"%B %d, %Y %l:%M:%S %p %Z",
			"%l:%M:%S %p %Y %B %d",
			rfc822DateFormat,
			iso8601DateFormat (Just "%H:%M:%S%Q"),
			iso8601DateFormat (Just "%H:%M:%S"),
			iso8601DateFormat (Just "%H:%M:%S%Q%z"),
			iso8601DateFormat (Just "%H:%M:%S%Q%Z")
		]

prop_format :: DateFormat -> PositiveTime -> Bool
prop_format (DateFormat fmt) (PositiveTime time) =
	approxidate formatted == Just posix
	where
	posix = floor $ utcTimeToPOSIXSeconds $ zonedTimeToUTC time
	formatted = formatTime defaultTimeLocale fmt time

tests :: [Test]
tests =
	[
		testProperty "Arbitrary date formats work" prop_format
	]

main :: IO ()
main = defaultMain tests
