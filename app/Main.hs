{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified GitHub.Endpoints.PullRequests as PR
import qualified GitHub.Endpoints.Issues as Issues
import qualified GitHub.Data.Options as Options
import qualified GitHub.Request as R
import GitHub.Auth
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Text (Text)
import Data.Vector (Vector)
import System.ReadEnvVar
import Lib

getToken :: ExceptT String IO Auth
getToken = ExceptT $ do
  maybeToken <- lookupEnv "GITHUB_TOKEN"
  return $ fmap OAuth (maybeToEither "Not found" maybeToken)

getData :: Text -> Text -> ExceptT PR.Error IO (Vector PR.SimplePullRequest, Vector Issues.Issue)
getData owner repoName = do
  let prRequest = PR.pullRequestsForR (PR.mkOwnerName owner) (PR.mkRepoName repoName) mempty PR.FetchAll
  pullRequests <- ExceptT $ R.executeRequest' prRequest
  let issueRequest = Issues.issuesForRepoR　(Issues.mkOwnerName owner) (Issues.mkRepoName repoName) mempty Issues.FetchAll
  issues <- ExceptT $ R.executeRequest' issueRequest
  return (pullRequests, issues)

main :: IO ()
main = do
  result <- runExceptT $ getData "octocat" "hello-world"
  case result of
    Left error ->
      putStrLn (show error)
    Right (prs, issues) ->
      putStrLn $ "Got " ++ (show (length prs)) ++ " pull requests and " ++ (show (length issues)) ++ " issues."