module Main where

import Book

main :: IO ()
main = do
    listBooks <- fmap readListBooksFromDB (readFile "db/books.txt")
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

    action <- getStringInput "\nWhat do you want to do?: "

    case action of
        "1" -> do
            bookTitle <- getStringInput "\nTitle: "
            bookAuthor <- getStringInput "\nAuthor: "
            publishingYear <- getIntInput "\nPublishing Year: "
            price <- getFloatInput "\nPrice: "
            quantity <- getIntInput "\nQuantity: "

            newListBooks <- addNewBook listBooks bookTitle bookAuthor publishingYear price quantity
            writeListBooksToDB newListBooks

            process newListBooks

        "2" -> do
            _bookId <- getIntInput "\nInput your book id to EDIT: "

            newListBooks <- editBook listBooks _bookId
            writeListBooksToDB newListBooks

            process newListBooks

        "3" -> do
            _bookId <- getIntInput "\nInput your book id to DELETE: "

            newListBooks <- deleteBook listBooks _bookId
            writeListBooksToDB newListBooks

            process newListBooks

        "4" -> do
            putStrLn "\nALL BOOKS:"
            putStrLn $ showListBooks listBooks
            process listBooks

        "5" -> do
            putStrLn "\nEnd!"

        _ -> do
            putStrLn "\nPlease input a valid action!"
            process listBooks
