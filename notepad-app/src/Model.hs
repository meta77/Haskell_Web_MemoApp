{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveGeneric              #-}

module Model where

import Database.Persist.TH
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

-- データベーススキーマを定義します。
-- "Note" というテーブル名で、タイトル(Text)、本文(Text)、
-- 作成日時(UTCTime)、更新日時(UTCTime)のカラムを持ちます。
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Note json -- 'json' を追加すると、Aesonのインスタンスが自動生成されます
    title Text
    content Text Maybe -- Maybe をつけると NULL 許容になります
    createdAt UTCTime
    updatedAt UTCTime
    deriving Show
|]

-- APIで受け取るためのデータ型 (IDなし)
data NoteInput = NoteInput
    { noteInputTitle :: Text
    , noteInputContent :: Maybe Text
    } deriving (Show, Generic)

instance FromJSON NoteInput
instance ToJSON NoteInput

-- Note 型は mkPersist で ToJSON/FromJSON が自動生成されるので不要