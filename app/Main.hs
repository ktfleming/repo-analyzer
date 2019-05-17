{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified GitHub.Endpoints.PullRequests as PR
import qualified GitHub.Endpoints.Issues as Issues
import qualified GitHub.Data.Options as Options
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Text (Text)
import Data.Vector (Vector)


getData :: Text -> Text -> ExceptT PR.Error IO (Vector PR.SimplePullRequest, Vector Issues.Issue)
getData owner repoName = do
  pullRequests <- ExceptT $ PR.pullRequestsFor (PR.mkOwnerName owner) (PR.mkRepoName repoName)
  issues <- ExceptT $ Issues.issuesForRepo　(Issues.mkOwnerName owner) (Issues.mkRepoName repoName) mempty
  return (pullRequests, issues)

main :: IO ()
main = do
  result <- runExceptT $ getData "octocat" "hello-world"
  case result of
    Left error -> putStrLn (show error)
    Right (prs, issues) ->
      putStrLn $ "Got " ++ (show (length prs)) ++ " pull requests and " ++ (show (length issues)) ++ " issues."