{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeApplications  #-}

import Web.Scotty
import Data.Aeson (object, (.=), FromJSON, ToJSON)
import Database.Persist
import Database.Persist.Sqlite (fromSqlKey, toSqlKey)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.IO.Class (liftIO)
import Data.Pool (Pool)
import Network.Wai.Middleware.RequestLogger (logStdoutDev) -- リクエストログ用

import Model
import DB

-- ReaderT を使ってコネクションプールをアプリケーション全体で共有します。
-- ScottyM は Scotty の Monad、IO は入出力、() は状態なし。
type AppM = ReaderT (Pool SqlBackend) IO

-- ReaderT を ScottyM に変換するヘルパー関数
runIO :: Pool SqlBackend -> AppM a -> ActionM a
runIO pool action = liftIO $ runReaderT action pool

main :: IO ()
main = do
    -- データベースコネクションプールを作成
    pool <- createPool
    -- データベースマイグレーションを実行
    runDBMigration pool
    putStrLn "Starting server on port 3000..."

    -- Scottyアプリケーションを起動
    scotty 3000 $ do
        -- リクエストログを出力するミドルウェアを追加
        middleware logStdoutDev

        -- ヘルパー関数を使って AppM を ActionM に変換
        let runAction = runIO pool

        -- JSON エラーハンドリング
        defaultHandler $ \e -> do
            status status500
            json $ object ["error" .= show e]

        -- GET /notes: メモ一覧取得
        get "/notes" $ do
            notes <- runAction getAllNotes
            json notes

        -- POST /notes: メモ作成
        post "/notes" $ do
            input <- jsonData :: ActionM NoteInput
            newNote <- runAction $ createNote input
            status status201 -- Created
            json newNote

        -- GET /notes/:id: メモ詳細取得
        get "/notes/:id" $ do
            noteId' <- param "id" :: ActionM Int64
            let noteId = toSqlKey noteId'
            mNote <- runAction $ getNoteById noteId
            case mNote of
                Just note -> json note
                Nothing   -> do
                    status status404 -- Not Found
                    json $ object ["error" .= ("Note not found" :: String)]

        -- PUT /notes/:id: メモ更新
        put "/notes/:id" $ do
            noteId' <- param "id" :: ActionM Int64
            let noteId = toSqlKey noteId'
            input <- jsonData :: ActionM NoteInput
            mUpdatedNote <- runAction $ updateNoteById noteId input
            case mUpdatedNote of
                Just updatedNote -> json updatedNote
                Nothing          -> do
                    status status404
                    json $ object ["error" .= ("Note not found" :: String)]

        -- DELETE /notes/:id: メモ削除
        delete "/notes/:id" $ do
            noteId' <- param "id" :: ActionM Int64
            let noteId = toSqlKey noteId'
            -- 削除前に存在確認 (任意)
            mNote <- runAction $ getNoteById noteId
            case mNote of
                Just _ -> do
                    runAction $ deleteNoteById noteId
                    status status204 -- No Content
                Nothing -> do
                    status status404
                    json $ object ["error" .= ("Note not found" :: String)]