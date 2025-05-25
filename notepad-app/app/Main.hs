{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeApplications  #-}

-- Scottyを 'Scotty' という名前で修飾インポートします
import qualified Web.Scotty.Trans as Scotty

import Network.HTTP.Types.Status (status200, status201, status204, status404, status500)
import Data.Aeson (object, (.=))
import Database.Persist (Entity, Key)
-- SqlBackend をインポートリストに追加します
import Database.Persist.Sqlite (fromSqlKey, toSqlKey, SqlBackend)
import Control.Monad.Reader (ReaderT, runReaderT, lift)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Pool (Pool)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
-- Text, pack, unpack をインポートします
import Data.Text (Text, pack, unpack)
-- Int64 をインポートします
import Data.Int (Int64)

import Model (Note(..), NoteInput(..), NoteId)
import qualified DB as DB

-- AppM の定義 (ScottyT の型引数を修正)
type AppM = ReaderT (Pool SqlBackend) IO

-- Scotty.ActionT を使うように修正
runIO :: Pool SqlBackend -> ReaderT (Pool SqlBackend) IO a -> Scotty.ActionT Text IO a
runIO pool action = liftIO $ runReaderT action pool

main :: IO ()
main = do
    pool <- DB.createDbPool
    DB.runDbMigration pool -- ← DB.runDbMigration になっていることを確認
    putStrLn "Starting server on port 3000..."

    -- Scotty.scotty ではなく Scotty.scottyT を使います
    -- また、ReaderT を実行する方法を ScottyT に渡します
    Scotty.scotty 3000 $ do

        let runAction = runIO pool -- このヘルパーはもう使わないかも...
                        -- 直接 liftIO で実行するか、ScottyT/ReaderT を使う

        -- Scotty.middleware を使います
        Scotty.middleware logStdoutDev

        -- Scotty.defaultHandler を使います
        Scotty.defaultHandler $ \e -> do
            Scotty.status status500
            Scotty.json $ object ["error" .= e] -- エラーメッセージの型を Text に

        -- GET /notes: メモ一覧取得 (Scotty.get を使う)
        Scotty.get "/notes" $ do
            notes <- liftIO $ runReaderT DB.getAllNotes pool
            Scotty.json notes

        -- POST /notes: メモ作成 (Scotty.post を使う)
        Scotty.post "/notes" $ do
            input <- Scotty.jsonData :: Scotty.ActionT Text IO NoteInput
            newNote <- liftIO $ runReaderT (DB.createNote input) pool
            Scotty.status status201
            Scotty.json newNote

        -- GET /notes/:id: メモ詳細取得 (Scotty.get を使う)
        Scotty.get "/notes/:id" $ do
            noteId' <- Scotty.param "id" :: Scotty.ActionT Text IO Int64 -- Int64 を使う
            let noteId = toSqlKey noteId'
            mNote <- liftIO $ runReaderT (DB.getNoteById noteId) pool
            case mNote of
                Just note -> Scotty.json note
                Nothing   -> do
                    Scotty.status status404
                    Scotty.json $ object ["error" .= ("Note not found" :: Text)]

        -- PUT /notes/:id: メモ更新 (Scotty.put を使う)
        Scotty.put "/notes/:id" $ do
            noteId' <- Scotty.param "id" :: Scotty.ActionT Text IO Int64 -- Int64 を使う
            let noteId = toSqlKey noteId'
            input <- Scotty.jsonData :: Scotty.ActionT Text IO NoteInput
            mUpdatedNote <- liftIO $ runReaderT (DB.updateNoteById noteId input) pool
            case mUpdatedNote of
                Just updatedNote -> Scotty.json updatedNote
                Nothing          -> do
                    Scotty.status status404
                    Scotty.json $ object ["error" .= ("Note not found" :: Text)]

        -- DELETE /notes/:id: メモ削除 (Scotty.delete を使う)
        Scotty.delete "/notes/:id" $ do
            noteId' <- Scotty.param "id" :: Scotty.ActionT Text IO Int64 -- Int64 を使う
            let noteId = toSqlKey noteId'
            mNote <- liftIO $ runReaderT (DB.getNoteById noteId) pool
            case mNote of
                Just _ -> do
                    liftIO $ runReaderT (DB.deleteNoteById noteId) pool
                    Scotty.status status204
                Nothing -> do
                    Scotty.status status404
                    Scotty.json $ object ["error" .= ("Note not found" :: Text)]