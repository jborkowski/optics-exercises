{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module SimpleServer where

import Control.Lens
import Data.List

type Path = [String]

type Body = String

data Request =
    Post Path Body
  | Get Path
  | Delete Path
  deriving (Show)

makePrisms ''Request

path :: Lens' Request Path
path = lens getter setter
  where
    getter (Post p body) = p
    getter (Get p) = p
    getter (Delete p) = p
    setter (Post _ body) p = Post p body
    setter (Get _) p = Get p
    setter (Delete _) p = Delete p

ex1 = Get ["posts", "123456"] ^. path

ex2 = Post ["posts", "123456"] "My new post" ^. path

ex3 = Get ["posts", "123456"] & path .~ ["hello"]

serverRequest :: Request -> String
serverRequest _ = "404 Not Found"

_PathPrefix :: String -> Prism' Request Request
_PathPrefix prefix = prism' embed match
  where
    embed :: Request -> Request
    embed req = req & path %~ (prefix :)
    match :: Request -> (Maybe Request)
    match req
      | has (path . _head . only prefix) req = Just (req & path %~ drop 1)
    match _ = Nothing

------------------------------------------------------------------------------------

safeTail :: [a] -> [a]
safeTail = tail & outside _Empty .~ const []

------------------------------------------------------------------------------------

userHandler :: Request -> String
userHandler req =
  "User Handler! Remaining path: " <> intercalate "/" (req ^. path)


server :: Request -> String
server = serverRequest
  & outside (_PathPrefix "users") .~ userHandler
  & outside (_PathPrefix "posts") .~ const "Post Handler!"
  & outside (_PathPrefix "posts") .~ postsHandler

postsHandler :: Request -> String
postsHandler = const "404 Not Found"
    & outside _Post
        .~ (\(path', body) -> "Created post with body: " <> body)
    & outside _Get
        .~ (\path' -> "Fetching post at path: " <> intercalate "/" path')
    & outside _Delete
        .~ (\path' -> "Deleting post at path: " <> intercalate "/" path')

postHandler' :: Request -> String
postHandler' (Post path' body) =
  "Created post with body: " <> body
postHandler' (Get path') =
  "Fetching post at path: " <> intercalate "/" path'
postHandler' (Delete path') =
  "Deleting post at path: " <> intercalate "/" path'
