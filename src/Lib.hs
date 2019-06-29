module Lib where

import Data.Time (utctDay)
import Data.Time.Calendar (Day)
import GitHub.Endpoints.PullRequests
import GitHub.Endpoints.Issues

-- Just a becomes Right a
-- Nothing becomes Left e
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither = flip maybe Right . Left

-- Anything that can represent a range of Days with a defined start point
-- and an optional end point
class DayRange a where
    startDay :: a -> Day
    endDay :: a -> Maybe Day

instance DayRange SimplePullRequest where
    startDay = utctDay . simplePullRequestCreatedAt
    endDay = (fmap utctDay) . simplePullRequestClosedAt

instance DayRange Issue where
    startDay = utctDay . issueCreatedAt
    endDay = (fmap utctDay) . issueClosedAt