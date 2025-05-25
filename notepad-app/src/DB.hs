{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE FlexibleContexts  #-}

module DB where

import           Control.Monad.Reader         (MonadIO, MonadReader, asks, liftIO)
import           Control.Monad.Logger         (runStdoutLoggingT, LoggingT, MonadLogger)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.Text                    (Text)
import           Data.Time                    (getCurrentTime, UTCTime)
import           Data.Pool                    (Pool)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)

import Model

-- データベースファイル名
dbFile :: Text
dbFile = "notes.db"

-- コネクションプールを作成する関数
createPool :: MonadIO m => m (Pool SqlBackend)
createPool = liftIO $ runStdoutLoggingT $ createSqlitePool dbFile 5

-- データベース操作を実行する関数
runDb :: (MonadReader (Pool SqlBackend) m, MonadIO m) => SqlPersistT IO a -> m a
runDb query = do
    pool <- asks id
    liftIO $ runSqlPool query pool

-- データベースのマイグレーションを実行する関数
runDBMigration :: MonadIO m => Pool SqlBackend -> m ()
runDBMigration pool = liftIO $ runSqlPool (runMigration migrateAll) pool

-- 全てのメモを取得
getAllNotes :: (MonadReader (Pool SqlBackend) m, MonadIO m) => m [Entity Note]
getAllNotes = runDb $ selectList [] [Asc NoteId]

-- IDでメモを取得
getNoteById :: (MonadReader (Pool SqlBackend) m, MonadIO m) => Key Note -> m (Maybe (Entity Note))
getNoteById noteId = runDb $ getEntity noteId

-- メモを作成
createNote :: (MonadReader (Pool SqlBackend) m, MonadIO m) => NoteInput -> m (Entity Note)
createNote input = do
    now <- liftIO getCurrentTime
    let note = Note
            { noteTitle = noteInputTitle input
            , noteContent = noteInputContent input
            , noteCreatedAt = now
            , noteUpdatedAt = now
            }
    noteId <- runDb $ insert note
    return $ Entity noteId note

-- メモを更新
updateNoteById :: (MonadReader (Pool SqlBackend) m, MonadIO m) => Key Note -> NoteInput -> m (Maybe (Entity Note))
updateNoteById noteId input = do
    now <- liftIO getCurrentTime
    runDb $ do
        mNote <- get noteId
        case mNote of
            Nothing -> return Nothing
            Just _  -> do
                update noteId
                    [ NoteTitle =. noteInputTitle input
                    , NoteContent =. noteInputContent input
                    , NoteUpdatedAt =. now
                    ]
                getEntity noteId -- 更新後のデータを取得して返す

-- メモを削除
deleteNoteById :: (MonadReader (Pool SqlBackend) m, MonadIO m) => Key Note -> m ()
deleteNoteById noteId = runDb $ delete noteId