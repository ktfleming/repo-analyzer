# repo-analyzer

A tool to visualize the history of open pull requests and issues on a given GitHub repo.

## Usage

After building the project with `stack build`, you can then run the executable with

`stack exec repo-analyzer-exe org-name repo-name`

This will display a list of lines in this format:

```
2017-12-02 %%%
2017-12-01 ##%
2017-11-30 #%
2017-11-29 %
2017-11-28 $
2017-11-27 $$$$
```

A `$` symbol represents an open pull request on the given day, while a `%` symbol represents an issue, and a `#` symbol represents both an open pull request and an open issue.

## GitHub API Token

To use this tool, you must create a GitHub API access token and set its value in the `GITHUB_TOKEN` environmental variable. This is because without an access token, it's very easy to exceed the GitHub API rate limit. The token doesn't need any particular OAuth scope.
