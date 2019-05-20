{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GitHub.Endpoints.PullRequests
import GitHub.Endpoints.Issues
import GitHub.Data.Name ( Name ( N))
import GitHub.Data.Options
import GitHub.Request
import GitHub.Data.Definitions
import GitHub.Auth
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Maybe (isNothing, fromMaybe)
import Data.Text (Text)
import Data.Vector (Vector, filter)
import Data.Foldable (traverse_)
import Data.Time
import Data.Time.Calendar
import qualified Data.Map.Strict as Map
import System.ReadEnvVar
import Lib

data MyError = NoTokenFound | GitHubError Error deriving Show

data AllData = AllData (Vector SimplePullRequest) (Vector Issue)
type DayHistogram = Map.Map Day Int

lookupToken :: ExceptT MyError IO Auth
lookupToken = ExceptT $ do
  maybeToken <- lookupEnv "GITHUB_TOKEN"
  return $ fmap OAuth (maybeToEither NoTokenFound maybeToken)

getData' :: Auth -> Text -> Text -> ExceptT MyError IO AllData
getData' auth owner repoName = do
  let prRequest = pullRequestsForR (N owner) (N repoName) stateAll FetchAll
  pullRequests <- withExceptT errorMapper $ ExceptT $ executeRequest auth prRequest
  let issueRequest = issuesForRepoR　(N owner) (N repoName) stateAll FetchAll
  issues <- withExceptT errorMapper $ ExceptT $ executeRequest auth issueRequest
  return $ AllData pullRequests (filterOutPullRequests issues)
  where
    errorMapper :: Error -> MyError
    errorMapper e = GitHubError e

getData :: ExceptT MyError IO AllData
getData = do
  auth <- lookupToken
  getData' auth "octocat" "hello-world"

filterOutPullRequests :: Vector Issue -> Vector Issue
filterOutPullRequests issues = Data.Vector.filter (isNothing . issuePullRequest) issues

getPRHistogram :: Day -> Vector SimplePullRequest -> DayHistogram
getPRHistogram today prs =
  foldr foldfn Map.empty prs
    where
      foldfn :: SimplePullRequest -> DayHistogram -> DayHistogram
      foldfn pr hist =
        let openDay :: Day = utctDay (simplePullRequestCreatedAt pr)
            closedDay :: Day = fromMaybe today $ fmap utctDay (simplePullRequestClosedAt pr)
            days :: [Day] = enumFromThen openDay closedDay
            subfold :: Day -> DayHistogram -> DayHistogram
            subfold day h = Map.alter updater day h
              where
                updater :: Maybe Int -> Maybe Int
                updater Nothing = Just 1
                updater (Just x) = Just (x + 1)
        in foldr subfold hist days
  
main :: IO ()
main = do
  result <- runExceptT getData
  case result of
    Left error -> putStrLn (show error)
    Right (AllData prs issues) -> do
      putStrLn $ "Got " ++ (show (length prs)) ++ " pull requests and " ++ (show (length issues)) ++ " issues."
      --traverse_ (putStrLn . show) titles
      --where titles :: Vector Text = fmap issueTitle issues