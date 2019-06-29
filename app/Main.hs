{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Except
import           Data.Foldable                 (traverse_)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (fromMaybe, isNothing)
import           Data.Text                     (Text, pack)
import           Data.Time
import           Data.Time.Calendar
import           Data.Vector                   (Vector, filter)
import           GitHub.Auth
import           GitHub.Data.Definitions
import           GitHub.Data.Name              (Name (N))
import           GitHub.Data.Options
import           GitHub.Endpoints.Issues
import           GitHub.Endpoints.PullRequests
import           GitHub.Request
import           Lib
import           Options.Applicative
import           Safe.Foldable
import           System.ReadEnvVar

data AppArguments = AppArguments { orgToLookup :: String, repoToLookup :: String }

-- NoTokenFound is for when a Github token can't be found from the GITHUB_TOKEN environmental variable
-- GitHubError is for any error that occurs when requesting data from Github's API
data MyError = NoTokenFound | GitHubError Error deriving Show

-- All the data necessary to generate the graph
data AllData = AllData (Vector SimplePullRequest) (Vector Issue)

-- How many open pull requests and issues there were on a given Day
data DayData = DayData { openPRs :: Int, openIssues :: Int }

type DayHistogram = Map.Map Day Int
newtype PRHistogram = PRHistogram DayHistogram
newtype IssueHistogram = IssueHistogram DayHistogram

-- Require a Github API token to use the app, otherwise it's too easy
-- to hit the rate limit
lookupToken :: ExceptT MyError IO Auth
lookupToken = ExceptT $ do
  maybeToken <- lookupEnv "GITHUB_TOKEN"
  return $ fmap OAuth (maybeToEither NoTokenFound maybeToken)

-- Github's API counts pull requests as issues (i.e. pull requests
-- are returned by the route that lists issues). We want to filter these
-- out since we treat pull requests completely separately
filterOutPullRequests :: Vector Issue -> Vector Issue
filterOutPullRequests = Data.Vector.filter (isNothing . issuePullRequest)

getDataFromAuth :: Auth -> Text -> Text -> ExceptT MyError IO AllData
getDataFromAuth auth owner repoName = do
  let prRequest = pullRequestsForR (N owner) (N repoName) stateAll FetchAll

  -- `executeRequest`'s error case is the Error type from the Github API package.
  -- use `withExceptT` to wrap it inside our own Error type
  pullRequests <- withExceptT GitHubError $ ExceptT $ executeRequest auth prRequest
  let issueRequest = issuesForRepoR (N owner) (N repoName) stateAll FetchAll
  issues <- withExceptT GitHubError $ ExceptT $ executeRequest auth issueRequest
  return $ AllData pullRequests (filterOutPullRequests issues)

getDataFromArguments :: AppArguments -> ExceptT MyError IO AllData
getDataFromArguments args = do
  auth <- lookupToken
  getDataFromAuth auth (pack . orgToLookup $ args) (pack . repoToLookup $ args)

-- Construct a DayHistgram by folding over the provided Vector of objects
-- that have an instance of the DayRange typeclass. The `Day` parameter is
-- the value to use as the "ending" day for any `a` whose DayRange is missing
-- and ending day (i.e. it should be set to today's date, since in practice
-- these values with missing ending days are PRs or Issues that are still open,
-- so their "ending day" is today)
getHistogram :: DayRange a => Day -> Vector a -> DayHistogram
getHistogram today = foldr addCounts Map.empty
  where
    addCounts :: DayRange a => a -> DayHistogram -> DayHistogram
    addCounts item hist =
      let openDay :: Day = startDay item
          closedDay :: Day = fromMaybe today (endDay item)
          days :: [Day] = enumFromTo openDay closedDay

          -- `subfold` is a function that, when given a Day and a DayHistogram,
          -- will set that Day's value to 1 (if it's not already present), or
          -- increase it by 1 (if it is present)
          subfold :: Day -> DayHistogram -> DayHistogram
          subfold = Map.alter updater
            where
              updater :: Maybe Int -> Maybe Int
              updater Nothing  = Just 1
              updater (Just x) = Just (x + 1)
       -- Folding `subfold` over each Day in the current `a`'s range of "open" days will
       -- cause the current `a` to be represented in the final DayHistogram.
       in foldr subfold hist days

getDayData :: DayHistogram -> DayHistogram -> Day -> DayData
getDayData prHist issueHist day =
  let
    prs :: Int = fromMaybe 0 $ Map.lookup day prHist
    issues :: Int = fromMaybe 0 $ Map.lookup day issueHist
  in
    DayData prs issues

-- Construct the line of symbols that represents a given day
getLineForDay :: DayData -> String
getLineForDay dayData =
  let
      -- Symbols that represent open pull requests / issues. A bit abstract...
      justPR = '$'
      justIssue = '%'
      both = '#'
      prs = openPRs dayData
      issues = openIssues dayData

       -- largest `x` such that there are at least x open PRs and x open issues.
       -- these will be represented by the `both` symbol above
      bothCount = min prs issues

      -- the `tailCount` for a day is the number of symbols at the end that are NOT the "both" symbol.
      -- these will be represented by either the `justPR` symbol or the `justIssue` symbol.
      tailCount = max prs issues - bothCount
      tailChar =
        (if prs <= issues
           then justIssue
           else justPR)
   in replicate bothCount both ++ replicate tailCount tailChar

-- Given an ending day (i.e. today), along with histograms for PRs and Issues, construct the
-- full "chart"
getChart :: Day -> PRHistogram -> IssueHistogram -> [String]
getChart today (PRHistogram prHist) (IssueHistogram issueHist) =
  let allDays = Map.keys prHist ++ Map.keys issueHist
      (earliestDay, latestDay) = (fromMaybe today $ minimumMay allDays, fromMaybe today $ maximumMay allDays)
      days :: [Day] = enumFromTo earliestDay latestDay
   in foldr addLineForDay [] days
  where
    addLineForDay :: Day -> [String] -> [String]
    addLineForDay day currentLines =
      let dayData = getDayData prHist issueHist day
          textForDay = show day ++ " " ++ getLineForDay dayData
       in currentLines ++ [textForDay]

runWithArguments :: AppArguments -> IO ()
runWithArguments args = do
  currentDay <- fmap utctDay getCurrentTime
  result <- runExceptT $ getDataFromArguments args
  case result of
    Left error -> print error
    Right (AllData prs issues) -> do
      putStrLn $ "Got " ++ show (length prs) ++ " pull requests and " ++ show (length issues) ++ " issues."
      let prHist = PRHistogram $ getHistogram currentDay prs
          issueHist = IssueHistogram $ getHistogram currentDay issues
          chart = getChart currentDay prHist issueHist
      traverse_ putStrLn chart

main :: IO ()
main = execParser opts >>= runWithArguments
  where
   parser = AppArguments <$> argument str (metavar "ORG") <*> argument str (metavar "REPO")
   opts = info parser mempty
