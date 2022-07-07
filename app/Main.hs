module Main where

import Book
import System.IO

main :: IO ()
main = do
    -- contents <- readFile "db/books.txt"
    -- putStrLn contents

    -- handle <- openFile "db/books.txt" ReadMode
    -- contents <- hGetContents handle
    -- putStrLn contents

    -- hClose handle
    listBooks <- fmap readListBooksFromDB (readFile "db/books.txt")
    -- listBooks <- fmap readListBooksFromDB contents
    -- hClose handle
    -- putStrLn "debug---list"
    -- putStrLn (show listBooks)
    -- putStrLn "debug---list"
    process listBooks

process :: [Book] -> IO ()
process listBooks = do
    putStrLn $ "\nMENU:" ++
               "\n" ++ replicate 20 '-' ++ 
               "\n1. Add a new book" ++
               "\n2. Edit a book" ++
               "\n3. Delete a book" ++
               "\n4. Show all books" ++
               "\n5. Exit" ++
               "\n" ++ replicate 20 '-'

    action <- getInput "\nWhat do you want to do?: "

    case action of
        "1" -> do
            bookTitle <- getInput "\nBook Title: "

            newListBooks <- addNewBook listBooks bookTitle
            writeListBooksToDB newListBooks

            process newListBooks

        "2" -> do
            _bookIdStr <- getInput "\nInput your book id to EDIT: "
            let _bookId = read _bookIdStr

            _bookTitle <- getInput "\nInput new title: "

            newListBooks <- editBook listBooks _bookId _bookTitle
            writeListBooksToDB newListBooks

            process newListBooks

        "3" -> do
            _bookIdStr <- getInput "\nInput your book id to DELETE: "
            let _bookId = read _bookIdStr

            newListBooks <- deleteBook listBooks _bookId
            writeListBooksToDB newListBooks

            process newListBooks
            -- process listBooks

        "4" -> do
            putStrLn "\nALL BOOKS:"
            putStrLn $ showListBooks listBooks
            process listBooks

        "5" -> do
            putStrLn "\nEnd!"

        _ -> do
            putStrLn "\nPlease input a valid action!"
            process listBooks
