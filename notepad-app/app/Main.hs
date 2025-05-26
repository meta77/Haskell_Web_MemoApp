{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeApplications  #-}

import qualified Web.Scotty.Trans as Scotty

import Network.HTTP.Types.Status (status200, status201, status204, status404, status500)
import Data.Aeson (object, (.=))
import Database.Persist (Entity, Key)
import Database.Persist.Sqlite (fromSqlKey, toSqlKey, SqlBackend)
import Control.Monad.Reader (ReaderT, runReaderT, lift)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Pool (Pool)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.Text (Text, pack, unpack)
import Data.Int (Int64)

import Model (Note(..), NoteInput(..), NoteId)
import qualified DB as DB

-- AppM の定義
type AppM = ReaderT (Pool SqlBackend) IO

-- ScottyAction の型エイリアスを削除

-- DBアクションを実行し、ScottyT IO にリフトするヘルパー
-- 戻り値の型を Scotty.ScottyT Text IO a に変更
runDB :: Pool SqlBackend -> ReaderT (Pool SqlBackend) IO a -> Scotty.ActionT Text IO a
runDB pool action = liftIO $ runReaderT action pool

main :: IO ()
main = do
    pool <- DB.createPool
    DB.runDBMigration pool
    putStrLn "Starting server on port 3000..."

    Scotty.scottyT 3000 id $ do

        Scotty.middleware logStdoutDev

        Scotty.defaultHandler $ \e -> do
            Scotty.status status500
            Scotty.json $ object ["error" .= e]

        -- GET /notes: メモ一覧取得
        Scotty.get "/notes" $ do
            notes <- runDB pool DB.getAllNotes
            Scotty.json notes

        -- POST /notes: メモ作成
        Scotty.post "/notes" $ do
            -- Scotty.ScottyT Text IO NoteInput に変更
            input <- Scotty.jsonData :: Scotty.ActionT Text IO NoteInput
            newNote <- runDB pool (DB.createNote input)
            Scotty.status status201
            Scotty.json newNote

        -- GET /notes/:id: メモ詳細取得
        Scotty.get "/notes/:id" $ do
            -- Scotty.ScottyT Text IO Int64 に変更
            noteId' <- Scotty.param "id" :: Scotty.ActionT Text IO Int64
            let noteId = toSqlKey noteId'
            mNote <- runDB pool (DB.getNoteById noteId)
            case mNote of
                Just note -> Scotty.json note
                Nothing   -> do
                    Scotty.status status404
                    Scotty.json $ object ["error" .= ("Note not found" :: Text)]

        -- PUT /notes/:id: メモ更新
        Scotty.put "/notes/:id" $ do
            -- Scotty.ScottyT Text IO Int64 に変更
            noteId' <- Scotty.param "id" :: Scotty.ActionT Text IO Int64
            let noteId = toSqlKey noteId'
            -- Scotty.ScottyT Text IO NoteInput に変更
            input <- Scotty.jsonData :: Scotty.ActionT Text IO NoteInput
            mUpdatedNote <- runDB pool (DB.updateNoteById noteId input)
            case mUpdatedNote of
                Just updatedNote -> Scotty.json updatedNote
                Nothing          -> do
                    Scotty.status status404
                    Scotty.json $ object ["error" .= ("Note not found" :: Text)]

        -- DELETE /notes/:id: メモ削除
        Scotty.delete "/notes/:id" $ do
            -- Scotty.ScottyT Text IO Int64 に変更
            noteId' <- Scotty.param "id" :: Scotty.ActionT Text IO Int64
            let noteId = toSqlKey noteId'
            mNote <- runDB pool (DB.getNoteById noteId)
            case mNote of
                Just _ -> do
                    runDB pool (DB.deleteNoteById noteId)
                    Scotty.status status204
                Nothing -> do
                    Scotty.status status404
                    Scotty.json $ object ["error" .= ("Note not found" :: Text)]