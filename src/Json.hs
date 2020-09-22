{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Json where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Text.RawString.QQ (r)
import qualified Data.ByteString as BS
import Data.Text (Text)


pods :: BS.ByteString
pods = [r|
{
    "kind": "List",
    "apiVersion": "v1",
    "items": [
        {
            "kind": "Pod",
            "apiVersion": "v1",
            "metadata": {
                "name": "redis-h315w",
                "creationTimestamp": "2019-03-23T19:42:21Z",
                "labels": {
                    "name": "redis",
                    "region": "usa"
                }
            },
            "spec": {
                "containers": [
                    {
                        "name": "redis",
                        "image": "redis",
                        "ports": [
                            {
                                "name": "redis",
                                "hostPort": 27017,
                                "containerPort": 27017,
                                "protocol": "TCP"
                            }
                        ],
                        "resources": {
                            "requests": {
                                "cpu": "100m"
                            }
                        }
                    }
                ]
            }
        },
        {
            "kind": "Pod",
            "apiVersion": "v1",
            "metadata": {
                "name": "web-4c5bj",
                "creationTimestamp": "2019-02-24T20:23:56Z",
                "labels": {
                    "name": "web",
                    "region": "usa"
                }
            },
            "spec": {
                "containers": [
                    {
                        "name": "web",
                        "image": "server",
                        "ports": [
                            {
                                "name": "http-server",
                                "containerPort": 3000,
                                "protocol": "TCP"
                            }
                        ],
                        "resources": {
                            "requests": {
                                "cpu": "100m"
                            }
                        }
                    }
                ]
            }
        }
    ]
}
|]

version :: Maybe Text
version = pods ^? key "apiVersion" . _String

numberOfContainers :: Int
numberOfContainers = lengthOf (key "items" . values . key "spec" .  key "containers" . values) pods

sameNameFilter :: AsValue s => s -> Bool
sameNameFilter json = json  ^? key "name" == json ^? key "image"

returnTheSameNameAndImage :: [Text]
returnTheSameNameAndImage = toListOf (key "items" . values . key "spec" . key "containers" . values . filtered sameNameFilter . key "name" . _String) pods
