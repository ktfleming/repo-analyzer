{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified GitHub.Endpoints.PullRequests as PR
import qualified GitHub.Endpoints.Issues as Issues
import qualified GitHub.Data.Options as Options
import qualified GitHub.Request as R
import qualified GitHub.Data.Definitions as Definitions
import GitHub.Auth
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Vector (Vector, filter)
import Data.Foldable (traverse_)
import System.ReadEnvVar
import Lib

data MyError = NoTokenFound | GitHubError Definitions.Error deriving Show

data AllData = AllData (Vector PR.SimplePullRequest) (Vector Issues.Issue)

lookupToken :: ExceptT MyError IO Auth
lookupToken = ExceptT $ do
  maybeToken <- lookupEnv "GITHUB_TOKEN"
  return $ fmap OAuth (maybeToEither NoTokenFound maybeToken)

getData' :: Auth -> Text -> Text -> ExceptT MyError IO AllData
getData' auth owner repoName = do
  let prRequest = PR.pullRequestsForR (PR.mkOwnerName owner) (PR.mkRepoName repoName) Options.stateAll PR.FetchAll
  pullRequests <- withExceptT errorMapper $ ExceptT $ R.executeRequest auth prRequest
  let issueRequest = Issues.issuesForRepoR　(Issues.mkOwnerName owner) (Issues.mkRepoName repoName) Options.stateAll Issues.FetchAll
  issues <- withExceptT errorMapper $ ExceptT $ R.executeRequest auth issueRequest
  return $ AllData pullRequests (filterOutPullRequests issues)
  where
    errorMapper :: Definitions.Error -> MyError
    errorMapper e = GitHubError e

getData :: ExceptT MyError IO AllData
getData = do
  auth <- lookupToken
  getData' auth "octocat" "hello-world"

filterOutPullRequests :: Vector Issues.Issue -> Vector Issues.Issue
filterOutPullRequests issues = Data.Vector.filter (isNothing . Issues.issuePullRequest) issues

main :: IO ()
main = do
  result <- runExceptT getData
  case result of
    Left error -> putStrLn (show error)
    Right (AllData prs issues) -> do
      putStrLn $ "Got " ++ (show (length prs)) ++ " pull requests and " ++ (show (length issues)) ++ " issues."
      traverse_ (putStrLn . show) titles
      where titles :: Vector Text = fmap Issues.issueTitle issues