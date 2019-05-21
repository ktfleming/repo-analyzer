module Lib where

import Data.Time (utctDay)
import Data.Time.Calendar (Day)
import GitHub.Endpoints.PullRequests
import GitHub.Endpoints.Issues

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither = flip maybe Right . Left

class DayRange a where
    startDay :: a -> Day
    endDay :: a -> Maybe Day

instance DayRange SimplePullRequest where
    startDay = utctDay . simplePullRequestCreatedAt
    endDay = (fmap utctDay) . simplePullRequestClosedAt

instance DayRange Issue where
    startDay = utctDay . issueCreatedAt
    endDay = (fmap utctDay) . issueClosedAt